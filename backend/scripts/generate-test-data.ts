import fs from 'fs/promises';
import { stringify } from 'csv-stringify/sync';
import path from 'path';

/**
 * Script to generate a large dataset of test contacts for performance testing
 * 
 * Usage:
 *   bun run scripts/generate-test-data.ts <count> [output-file]
 * 
 * Where:
 *   <count> is the number of contacts to generate
 *   [output-file] is the optional output file path (defaults to ./scripts/test-contacts-{count}.csv)
 */

// Sample data for random generation
const firstNames = ['John', 'Jane', 'Robert', 'Mary', 'Michael', 'Jennifer', 'William', 'Patricia', 'Richard', 'Linda', 
  'David', 'Elizabeth', 'Joseph', 'Barbara', 'Thomas', 'Susan', 'Charles', 'Jessica', 'Daniel', 'Sarah', 'Matthew', 'Karen', 
  'Anthony', 'Nancy', 'Mark', 'Lisa', 'Donald', 'Betty', 'Steven', 'Dorothy'];

const lastNames = ['Smith', 'Johnson', 'Williams', 'Jones', 'Brown', 'Davis', 'Miller', 'Wilson', 'Moore', 'Taylor', 
  'Anderson', 'Thomas', 'Jackson', 'White', 'Harris', 'Martin', 'Thompson', 'Garcia', 'Martinez', 'Robinson', 'Clark', 
  'Rodriguez', 'Lewis', 'Lee', 'Walker', 'Hall', 'Allen', 'Young', 'Hernandez', 'King'];

const carriers = ['Aetna', 'Humana', 'UnitedHealthcare', 'Cigna', 'Blue Cross Blue Shield', 'Kaiser Permanente', 
  'Anthem', 'Wellcare', 'Centene', 'Molina Healthcare'];

const planTypes = ['A', 'B', 'C', 'D', 'F', 'G', 'K', 'L', 'M', 'N'];

const states = ['AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 
  'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 
  'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY'];

// Helper functions for random data generation
function getRandomItem<T>(array: T[]): T {
  return array[Math.floor(Math.random() * array.length)];
}

function getRandomDate(start: Date, end: Date): string {
  const date = new Date(start.getTime() + Math.random() * (end.getTime() - start.getTime()));
  return date.toISOString().split('T')[0];
}

function getRandomPhone(): string {
  return `${Math.floor(Math.random() * 900) + 100}-${Math.floor(Math.random() * 900) + 100}-${Math.floor(Math.random() * 9000) + 1000}`;
}

function getRandomZip(): string {
  return `${Math.floor(Math.random() * 90000) + 10000}`;
}

// Main function
async function main() {
  try {
    const args = process.argv.slice(2);
    
    if (args.length < 1) {
      console.error('Usage: bun run scripts/generate-test-data.ts <count> [output-file]');
      process.exit(1);
    }
    
    const count = parseInt(args[0], 10);
    
    if (isNaN(count) || count <= 0) {
      console.error('Count must be a positive number');
      process.exit(1);
    }
    
    const outputFile = args[1] || path.join(process.cwd(), 'scripts', `test-contacts-${count}.csv`);
    
    console.log(`Generating ${count} test contacts...`);
    
    // Generate the contacts
    const contacts = [];
    
    for (let i = 0; i < count; i++) {
      const firstName = getRandomItem(firstNames);
      const lastName = getRandomItem(lastNames);
      const email = `${firstName.toLowerCase()}.${lastName.toLowerCase()}${Math.floor(Math.random() * 10000)}@example.com`;
      
      contacts.push({
        first_name: firstName,
        last_name: lastName,
        email: email,
        current_carrier: getRandomItem(carriers),
        plan_type: getRandomItem(planTypes),
        effective_date: getRandomDate(new Date('2022-01-01'), new Date('2023-12-31')),
        birth_date: getRandomDate(new Date('1940-01-01'), new Date('1965-12-31')),
        tobacco_user: Math.random() > 0.8 ? 1 : 0, // 20% are tobacco users
        gender: Math.random() > 0.5 ? 'Male' : 'Female',
        state: getRandomItem(states),
        zip_code: getRandomZip(),
        phone_number: getRandomPhone()
      });
      
      // Log progress for large datasets
      if (i > 0 && i % 10000 === 0) {
        console.log(`Generated ${i} contacts...`);
      }
    }
    
    // Convert to CSV
    console.log('Converting to CSV...');
    const csv = stringify(contacts, { header: true });
    
    // Write to file
    console.log(`Writing to ${outputFile}...`);
    await fs.writeFile(outputFile, csv);
    
    console.log(`Successfully generated ${count} contacts in ${outputFile}`);
    
  } catch (error) {
    console.error(`Error generating test data: ${error}`);
    process.exit(1);
  }
}

main();