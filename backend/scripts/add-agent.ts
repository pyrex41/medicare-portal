import * as p from '@clack/prompts';
import { z } from 'zod';
import { exit } from 'process';
import chalk from 'chalk';
import { Database } from '../src/database';

// Schema validation for agent data
const AgentSchema = z.object({
  first_name: z.string().min(1),
  last_name: z.string().min(1),
  email: z.string().email(),
  phone: z.string().min(1),
  organization_id: z.number(),
});

async function main() {
  p.intro(chalk.green('✨ Interactive Agent Creation Tool ✨'));

  const db = new Database();

  // Get all organizations
  const orgs = await db.fetchAll<{ id: number; name: string }>('SELECT id, name FROM organizations');
  
  if (orgs.length === 0) {
    p.outro(chalk.red('No organizations found. Please create an organization first.'));
    exit(1);
  }

  // Gather information
  const firstName = await p.text({
    message: 'Enter agent first name:',
    validate: (value) => {
      if (value.length === 0) return 'First name cannot be empty';
    },
  });

  const lastName = await p.text({
    message: 'Enter agent last name:',
    validate: (value) => {
      if (value.length === 0) return 'Last name cannot be empty';
    },
  });

  const email = await p.text({
    message: 'Enter agent email:',
    validate: (value) => {
      if (!value.includes('@')) return 'Invalid email address';
    },
  });

  const phone = await p.text({
    message: 'Enter agent phone:',
    validate: (value) => {
      if (value.length === 0) return 'Phone cannot be empty';
    },
  });

  const organization = await p.select({
    message: 'Select organization:',
    options: orgs.map(org => ({
      value: org.id,
      label: org.name,
    })),
  });

  if (p.isCancel(firstName) || p.isCancel(lastName) || p.isCancel(email) || p.isCancel(phone) || p.isCancel(organization)) {
    p.outro(chalk.yellow('Operation cancelled'));
    exit(0);
  }

  try {
    // Validate data
    const agentData = AgentSchema.parse({
      first_name: firstName,
      last_name: lastName,
      email,
      phone,
      organization_id: organization,
    });

    // Start a transaction
    await db.execute('BEGIN TRANSACTION');

    try {
      // Insert into agents table
      await db.execute(
        `INSERT INTO agents (first_name, last_name, email, phone, organization_id)
         VALUES (?, ?, ?, ?, ?)`,
        [
          agentData.first_name,
          agentData.last_name,
          agentData.email,
          agentData.phone,
          agentData.organization_id
        ]
      );

      // Insert into users table
      await db.execute(
        `INSERT INTO users (email, organization_id, is_admin, is_agent, is_active)
         VALUES (?, ?, ?, ?, true)`,
        [agentData.email, agentData.organization_id, false, true]
      );

      // Commit the transaction
      await db.execute('COMMIT');

      p.outro(chalk.green('Agent created successfully! 🎉'));

    } catch (error) {
      // Rollback on error
      await db.execute('ROLLBACK');
      throw error;
    }

  } catch (error) {
    if (error instanceof z.ZodError) {
      p.outro(chalk.red(`Validation error: ${error.errors.map(e => e.message).join(', ')}`));
    } else {
      p.outro(chalk.red(`Error creating agent: ${error}`));
    }
    exit(1);
  }
}

main().catch((error) => {
  p.outro(chalk.red(`Fatal error: ${error}`));
  exit(1);
}); 