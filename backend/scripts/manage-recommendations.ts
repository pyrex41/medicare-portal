import * as p from '@clack/prompts';
import { z } from 'zod';
import { exit } from 'process';
import chalk from 'chalk';
import { Database } from '../src/database';

// Schema validation for GI recommendation data
const RecommendationSchema = z.object({
  state: z.string().length(2),
  carrier: z.string().min(1),
});

async function main() {
  p.intro(chalk.green('✨ Guaranteed Issue Recommendations Manager ✨'));

  const db = new Database();
  await db.init();

  while (true) {
    const action = await p.select({
      message: 'What would you like to do?',
      options: [
        { value: 'list', label: 'List all GI recommendations' },
        { value: 'add', label: 'Add new GI recommendation' },
        { value: 'remove', label: 'Remove GI recommendation' },
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
          await listRecommendations(db);
          break;
        case 'add':
          await addRecommendation(db);
          break;
        case 'remove':
          await removeRecommendation(db);
          break;
      }
    } catch (error) {
      p.outro(chalk.red(`Error: ${error}`));
      exit(1);
    }
  }
}

async function listRecommendations(db: Database) {
  const results = await db.fetchAll<{ state: string; carrier: string; created_at: string }>(
    `SELECT state, carrier, created_at 
     FROM guaranteed_issue_recommendations 
     ORDER BY carrier, state`
  );

  if (results.length === 0) {
    p.note('No GI recommendations found', 'Empty');
    return;
  }

  const groupedByCarrier = results.reduce((acc, curr) => {
    if (!acc[curr.carrier]) {
      acc[curr.carrier] = [];
    }
    acc[curr.carrier].push(curr.state);
    return acc;
  }, {} as Record<string, string[]>);

  p.note(
    Object.entries(groupedByCarrier)
      .map(([carrier, states]) => `${carrier}:\n  ${states.sort().join(', ')}`)
      .join('\n\n'),
    'Current GI Recommendations'
  );
}

async function addRecommendation(db: Database) {
  // Get available carriers first
  const carriers = await db.fetchAll<{ name: string }>(
    'SELECT name FROM carriers ORDER BY name'
  );

  if (carriers.length === 0) {
    p.note(chalk.red('No carriers found in database'));
    return;
  }

  const carrier = await p.select({
    message: 'Select carrier:',
    options: carriers.map(c => ({
      value: c.name,
      label: c.name,
    })),
  });

  if (p.isCancel(carrier)) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  const states = await p.text({
    message: 'Enter state codes (comma/space separated, e.g., TX CA FL):',
    validate: (value) => {
      const stateList = value.toUpperCase().split(/[\s,]+/).filter(Boolean);
      if (stateList.length === 0) return 'At least one state code is required';
      const invalidStates = stateList.filter(s => s.length !== 2);
      if (invalidStates.length > 0) {
        return `Invalid state codes: ${invalidStates.join(', ')}`;
      }
    },
  });

  if (p.isCancel(states)) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  const stateList = states.toUpperCase().split(/[\s,]+/).filter(Boolean);
  const results: { state: string; success: boolean; message: string }[] = [];

  for (const state of stateList) {
    try {
      const data = RecommendationSchema.parse({
        state,
        carrier,
      });

      // Check if recommendation already exists
      const existing = await db.fetchOne(
        'SELECT 1 FROM guaranteed_issue_recommendations WHERE state = ? AND carrier = ?',
        [data.state, data.carrier]
      );

      if (existing) {
        results.push({
          state: data.state,
          success: false,
          message: 'Already exists'
        });
        continue;
      }

      await db.execute(
        'INSERT INTO guaranteed_issue_recommendations (state, carrier) VALUES (?, ?)',
        [data.state, data.carrier]
      );

      results.push({
        state: data.state,
        success: true,
        message: 'Added successfully'
      });

    } catch (error) {
      results.push({
        state,
        success: false,
        message: error instanceof z.ZodError 
          ? error.errors.map(e => e.message).join(', ')
          : 'Unknown error'
      });
    }
  }

  // Display results summary
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);

  if (successful.length > 0) {
    p.note(
      chalk.green(
        `Successfully added GI recommendations for ${carrier}:\n` +
        successful.map(r => `  ${r.state}`).join('\n')
      ),
      'Success'
    );
  }

  if (failed.length > 0) {
    p.note(
      chalk.yellow(
        `Failed to add some recommendations for ${carrier}:\n` +
        failed.map(r => `  ${r.state}: ${r.message}`).join('\n')
      ),
      'Warnings'
    );
  }
}

async function removeRecommendation(db: Database) {
  const recommendations = await db.fetchAll<{ id: number; state: string; carrier: string }>(
    'SELECT id, state, carrier FROM guaranteed_issue_recommendations ORDER BY carrier, state'
  );

  if (recommendations.length === 0) {
    p.note(chalk.yellow('No recommendations to remove'));
    return;
  }

  const selections = await p.multiselect({
    message: 'Select recommendations to remove (space to select/deselect):',
    options: recommendations
      .sort((a, b) => a.carrier.localeCompare(b.carrier) || a.state.localeCompare(b.state))
      .map(rec => ({
        value: rec.id.toString(),
        label: `${rec.carrier} - ${rec.state}`,
      })),
    required: true,
  });

  if (p.isCancel(selections)) {
    p.note(chalk.yellow('Operation cancelled'));
    return;
  }

  const idsToRemove = (selections as string[]).map(Number);

  if (idsToRemove.length === 0) {
    p.note(chalk.yellow('No recommendations selected for removal'));
    return;
  }

  await db.execute(
    `DELETE FROM guaranteed_issue_recommendations WHERE id IN (${idsToRemove.join(',')})`,
  );

  const removedItems = recommendations
    .filter(r => idsToRemove.includes(r.id))
    .reduce((acc, curr) => {
      if (!acc[curr.carrier]) {
        acc[curr.carrier] = [];
      }
      acc[curr.carrier].push(curr.state);
      return acc;
    }, {} as Record<string, string[]>);

  p.note(
    chalk.green(
      'Successfully removed recommendations:\n\n' +
      Object.entries(removedItems)
        .map(([carrier, states]) => `${carrier}:\n  ${states.sort().join(', ')}`)
        .join('\n\n')
    ),
    'Success'
  );
}

main().catch((error) => {
  p.outro(chalk.red(`Fatal error: ${error}`));
  exit(1);
}); 