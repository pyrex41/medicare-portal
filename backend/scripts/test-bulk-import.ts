import { Database } from '../src/database';
import { ContactCreate } from '../src/types';
import { logger } from '../src/logger';
import fs from 'fs/promises';
import { parse } from 'csv-parse/sync';

/**
 * Test utility for the bulk contact import feature
 * 
 * Usage:
 *   bun run scripts/test-bulk-import.ts <org_id> <csv_file> [--overwrite]
 * 
 * Where:
 *   <org_id> is the organization ID to import contacts for
 *   <csv_file> is the path to a CSV file containing contacts
 *   --overwrite is an optional flag to overwrite existing contacts
 * 
 * The CSV file should have headers that match the ContactCreate interface properties
 * Columns can be in either camelCase (firstName) or snake_case (first_name) format
 */

async function main() {
  try {
    // Parse command line arguments
    const args = process.argv.slice(2);
    
    if (args.length < 2) {
      console.error('Usage: bun run scripts/test-bulk-import.ts <org_id> <csv_file> [--overwrite]');
      process.exit(1);
    }
    
    const orgId = args[0];
    const csvPath = args[1];
    const overwriteExisting = args.includes('--overwrite');
    
    // Read and parse the CSV file
    logger.info(`Reading contacts from ${csvPath}`);
    const csvContent = await fs.readFile(csvPath, 'utf-8');
    
    // Parse CSV with header row
    const records = parse(csvContent, {
      columns: true,
      skip_empty_lines: true,
      trim: true
    });
    
    logger.info(`Parsed ${records.length} records from CSV`);
    
    // Convert records to ContactCreate objects
    const contacts: ContactCreate[] = records.map((record: any) => {
      return {
        first_name: record.first_name || record.firstName || '',
        last_name: record.last_name || record.lastName || '',
        email: record.email || '',
        current_carrier: record.current_carrier || record.currentCarrier || '',
        plan_type: record.plan_type || record.planType || '',
        effective_date: record.effective_date || record.effectiveDate || '',
        birth_date: record.birth_date || record.birthDate || '',
        tobacco_user: record.tobacco_user === '1' || record.tobacco_user === 'true' || record.tobaccoUser === true || record.tobaccoUser === '1' || record.tobaccoUser === 'true',
        gender: record.gender || '',
        state: record.state || '',
        zip_code: record.zip_code || record.zipCode || '',
        phone_number: record.phone_number || record.phoneNumber || '',
        agent_id: record.agent_id || record.agentId || null
      };
    });
    
    // Log a sample contact
    logger.info(`Sample contact: ${JSON.stringify(contacts[0])}`);
    
    // Validate contacts
    const validContacts = contacts.filter(contact => {
      return contact.first_name && contact.last_name && contact.email;
    });
    
    logger.info(`Found ${validContacts.length} valid contacts out of ${contacts.length}`);
    
    // Confirm with user
    console.log(`Ready to import ${validContacts.length} contacts for organization ${orgId}.`);
    console.log(`Overwrite existing: ${overwriteExisting}`);
    
    // Add a delay to allow cancellation
    console.log('Starting import in 5 seconds... Press Ctrl+C to cancel');
    await new Promise(resolve => setTimeout(resolve, 5000));
    
    // Execute bulk import
    const startTime = Date.now();
    logger.info('Starting bulk import...');
    
    await Database.bulkImportContacts(orgId, validContacts, overwriteExisting);
    
    const totalTime = (Date.now() - startTime) / 1000;
    logger.info(`Bulk import completed in ${totalTime.toFixed(2)} seconds`);
    
  } catch (error) {
    logger.error(`Error in test-bulk-import: ${error}`);
    process.exit(1);
  }
}

main();