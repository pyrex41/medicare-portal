import * as p from '@clack/prompts';
import { z } from 'zod';
import { exit } from 'process';
import chalk from 'chalk';
import { Database } from '../src/database';
import { TURSO_CONFIG } from '../src/config/turso';
import fetch from 'node-fetch';

interface Organization {
  id: number;
  name: string;
  turso_db_url: string | null;
}

class TursoManager {
  private headers = {
    'Authorization': `Bearer ${TURSO_CONFIG.API_TOKEN}`,
    'Content-Type': 'application/json'
  };

  async listLocations(): Promise<string[]> {
    const response = await fetch(`${TURSO_CONFIG.API_URL}/locations`, {
      headers: this.headers
    });

    if (!response.ok) {
      throw new Error(`Failed to fetch locations: ${await response.text()}`);
    }

    const data = await response.json() as { locations: Array<{ name: string }> };
    return data.locations.map(l => l.name);
  }

  async listDatabases(): Promise<Array<{name: string, hostname: string}>> {
    const response = await fetch(
      `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases`,
      { headers: this.headers }
    );

    if (!response.ok) {
      throw new Error(`Failed to fetch databases: ${await response.text()}`);
    }

    const data = await response.json() as { databases: Array<{Name: string, Hostname: string}> };
    return data.databases.map(db => ({
      name: db.Name,
      hostname: db.Hostname
    }));
  }

  async updateGroup(name: string, location: string): Promise<void> {
    const response = await fetch(
      `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/groups/${TURSO_CONFIG.GROUP_NAME}`,
      {
        method: 'PATCH',
        headers: this.headers,
        body: JSON.stringify({ name, location })
      }
    );

    if (!response.ok) {
      throw new Error(`Failed to update group: ${await response.text()}`);
    }
  }

  async deleteDatabase(name: string): Promise<void> {
    const response = await fetch(
      `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases/${name}`,
      {
        method: 'DELETE',
        headers: this.headers
      }
    );

    if (!response.ok) {
      throw new Error(`Failed to delete database: ${await response.text()}`);
    }
  }
}

async function main() {
  p.intro(chalk.green('✨ Turso Database Manager ✨'));

  const db = new Database();
  const turso = new TursoManager();

  while (true) {
    const action = await p.select({
      message: 'What would you like to do?',
      options: [
        { value: 'list', label: 'List all databases and organizations' },
        { value: 'update-group', label: 'Update group configuration' },
        { value: 'delete-db', label: 'Delete a customer database' },
        { value: 'exit', label: 'Exit' },
      ],
    });

    if (p.isCancel(action) || action === 'exit') {
      p.outro(chalk.yellow('Goodbye!'));
      exit(0);
    }

    try {
      switch (action) {
        case 'list':
          await listDatabasesAndOrgs(db, turso);
          break;
        case 'update-group':
          await updateGroupConfig(turso);
          break;
        case 'delete-db':
          await deleteCustomerDatabase(db, turso);
          break;
      }
    } catch (error) {
      p.outro(chalk.red(`Error: ${error}`));
      exit(1);
    }
  }
}

async function listDatabasesAndOrgs(db: Database, turso: TursoManager) {
  const databases = await turso.listDatabases();
  const orgs = await db.execute<Organization>(
    'SELECT id, name, turso_db_url FROM organizations WHERE turso_db_url IS NOT NULL'
  );

  const mappedDatabases = databases.map(database => {
    const org = orgs.find(o => o.turso_db_url && o.turso_db_url.toString().includes(database.hostname));
    return {
      name: database.name,
      hostname: database.hostname,
      organization: org ? org.name : 'Not associated with any organization'
    };
  });

  if (mappedDatabases.length === 0) {
    p.note('No databases found', 'Empty');
    return;
  }

  p.note(
    mappedDatabases
      .map(db => `${db.name}:\n  URL: ${db.hostname}\n  Organization: ${db.organization}`)
      .join('\n\n'),
    'Current Databases'
  );
}

async function updateGroupConfig(turso: TursoManager) {
  // Get available locations
  const locations = await turso.listLocations();

  p.note(
    `Current Configuration:\n` +
    `  Group Name: ${TURSO_CONFIG.GROUP_NAME}\n` +
    `  Location: ${TURSO_CONFIG.LOCATION}`,
    'Current Settings'
  );

  const newGroupName = await p.text({
    message: 'Enter new group name (or press enter to keep current):',
    initial: TURSO_CONFIG.GROUP_NAME,
  });

  if (p.isCancel(newGroupName)) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  const newLocation = await p.select({
    message: 'Select new location:',
    options: locations.map(loc => ({
      value: loc,
      label: loc,
    })),
    initialValue: TURSO_CONFIG.LOCATION,
  });

  if (p.isCancel(newLocation)) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  const confirm = await p.confirm({
    message: `Update group "${TURSO_CONFIG.GROUP_NAME}" to:\n` +
             `  Name: ${newGroupName}\n` +
             `  Location: ${newLocation}\n` +
             `Are you sure?`,
  });

  if (p.isCancel(confirm) || !confirm) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  await turso.updateGroup(newGroupName, newLocation as string);
  
  p.note(
    chalk.green(
      `Successfully updated group configuration.\n` +
      `Remember to update TURSO_CONFIG in your code!`
    ),
    'Success'
  );
}

async function deleteCustomerDatabase(db: Database, turso: TursoManager) {
  const databases = await turso.listDatabases();
  const orgs = await db.execute<Organization>(
    'SELECT id, name, turso_db_url FROM organizations WHERE turso_db_url IS NOT NULL'
  );

  if (databases.length === 0) {
    p.note('No databases found to delete', 'Empty');
    return;
  }

  const options = databases.map(database => {
    const org = orgs.find(o => o.turso_db_url && o.turso_db_url.toString().includes(database.hostname));
    return {
      value: database.name,
      label: `${database.name} (${org ? org.name : 'Unassociated'})`,
    };
  });

  const selection = await p.select({
    message: 'Select database to delete:',
    options,
  });

  if (p.isCancel(selection)) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  const confirm = await p.confirm({
    message: chalk.red(`Are you sure you want to delete database "${selection}"?\nThis action cannot be undone!`),
  });

  if (p.isCancel(confirm) || !confirm) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  await turso.deleteDatabase(selection as string);

  // Update organization record
  const org = orgs.find(o => 
    o.turso_db_url && 
    o.turso_db_url.toString().includes(
      databases.find(d => d.name === selection)?.hostname || ''
    )
  );

  if (org) {
    await db.execute(
      'UPDATE organizations SET turso_db_url = NULL, turso_auth_token = NULL WHERE id = ?',
      [org.id]
    );
  }

  p.note(
    chalk.green(`Successfully deleted database "${selection}"`),
    'Success'
  );
}

main().catch((error) => {
  p.outro(chalk.red(`Fatal error: ${error}`));
  exit(1);
}); 