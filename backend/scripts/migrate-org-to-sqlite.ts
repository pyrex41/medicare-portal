#!/usr/bin/env bun

import { Database } from '../src/database';
import { logger } from '../src/logger';
import { select, confirm, text } from '@clack/prompts';

/**
 * Migration script to move an organization from Turso to SQLite/GCS architecture
 * This script helps with the transition to the new on-demand Litestream replication system
 */

async function migrateOrganization() {
  console.log('🔄 Organization Migration: Turso → SQLite/GCS\n');
  
  try {
    const mainDb = new Database();
    
    // Get list of organizations using Turso
    const tursoOrgs = await mainDb.query<{ id: number; name: string; turso_db_url: string }>(
      `SELECT id, name, turso_db_url 
       FROM organizations 
       WHERE (db_type IS NULL OR db_type = 'turso') 
       AND turso_db_url IS NOT NULL 
       ORDER BY id`
    );
    
    if (tursoOrgs.length === 0) {
      console.log('✅ No organizations found using Turso architecture.');
      return;
    }
    
    console.log(`Found ${tursoOrgs.length} organizations using Turso:\n`);
    tursoOrgs.forEach(org => {
      console.log(`  ${org.id}: ${org.name}`);
    });
    console.log();
    
    const selectedOrgId = await select({
      message: 'Which organization would you like to migrate?',
      options: [
        ...tursoOrgs.map(org => ({
          value: org.id.toString(),
          label: `${org.id}: ${org.name}`,
        })),
        {
          value: 'cancel',
          label: 'Cancel migration',
        }
      ],
    });
    
    if (selectedOrgId === 'cancel') {
      console.log('Migration cancelled.');
      return;
    }
    
    const orgId = String(selectedOrgId);
    const selectedOrg = tursoOrgs.find(org => org.id.toString() === orgId);
    
    if (!selectedOrg) {
      console.log('❌ Organization not found.');
      return;
    }
    
    console.log(`\n📋 Migration Plan for: ${selectedOrg.name} (ID: ${selectedOrg.id})`);
    console.log('   1. Mark organization as using SQLite/GCS architecture');
    console.log('   2. Set GCS bucket and replica path configuration');
    console.log('   3. Existing Turso data will remain accessible during transition');
    console.log('   4. New bulk imports will use the SQLite/GCS system\n');
    
    const gcsBucket = await text({
      message: 'GCS Bucket Name:',
      placeholder: 'replit-object-storage',
      defaultValue: 'replit-object-storage',
    });
    
    const confirmMigration = await confirm({
      message: `Proceed with migration for "${selectedOrg.name}"?`,
    });
    
    if (!confirmMigration) {
      console.log('Migration cancelled.');
      return;
    }
    
    console.log('\n🚀 Starting migration...\n');
    
    // Update organization to use SQLite/GCS
    await Database.markOrgAsUsingGCS(
      orgId,
      `litestream-replicas/${orgId}`
    );
    
    // Update the bucket name if different from default
    if (gcsBucket && gcsBucket !== 'replit-object-storage') {
      await mainDb.execute(
        'UPDATE organizations SET gcs_bucket_name = ? WHERE id = ?',
        [gcsBucket, orgId]
      );
    }
    
    console.log(`✅ Successfully migrated organization "${selectedOrg.name}" to SQLite/GCS`);
    console.log(`   - Database type: sqlite`);
    console.log(`   - GCS Bucket: ${String(gcsBucket)}`);
    console.log(`   - Replica path: litestream-replicas/${orgId}`);
    console.log('\n📝 Next steps:');
    console.log('   1. Test bulk imports with the new architecture');
    console.log('   2. Monitor Litestream replication in application logs');
    console.log('   3. Verify GCS bucket access and permissions');
    console.log('   4. Consider data migration if needed\n');
    
  } catch (error) {
    logger.error(`Migration failed: ${error instanceof Error ? error.message : String(error)}`);
    console.log('❌ Migration failed. Check logs for details.');
  }
}

async function listOrganizations() {
  console.log('📊 Organization Database Architecture Status\n');
  
  try {
    const mainDb = new Database();
    
    const orgs = await mainDb.query<{
      id: number;
      name: string;
      db_type: string | null;
      gcs_bucket_name: string | null;
      turso_db_url: string | null;
    }>(
      `SELECT id, name, db_type, gcs_bucket_name, turso_db_url 
       FROM organizations 
       ORDER BY id`
    );
    
    console.log('Organizations:');
    console.log('ID | Name | Architecture | Details');
    console.log('---|------|--------------|--------');
    
    for (const org of orgs) {
      const architecture = org.db_type === 'sqlite' ? '🆕 SQLite/GCS' : '🔄 Turso (Legacy)';
      const details = org.db_type === 'sqlite' 
        ? `Bucket: ${org.gcs_bucket_name || 'default'}`
        : `URL: ${org.turso_db_url ? '✅' : '❌'}`;
      
      console.log(`${org.id.toString().padEnd(3)}| ${org.name.padEnd(20)}| ${architecture.padEnd(15)}| ${details}`);
    }
    
    console.log('\n📈 Summary:');
    const sqliteCount = orgs.filter(org => org.db_type === 'sqlite').length;
    const tursoCount = orgs.length - sqliteCount;
    console.log(`   SQLite/GCS: ${sqliteCount} organizations`);
    console.log(`   Turso: ${tursoCount} organizations`);
    
  } catch (error) {
    logger.error(`Failed to list organizations: ${error instanceof Error ? error.message : String(error)}`);
    console.log('❌ Failed to list organizations. Check logs for details.');
  }
}

async function main() {
  const action = await select({
    message: 'What would you like to do?',
    options: [
      {
        value: 'list',
        label: '📊 List all organizations and their database architecture',
      },
      {
        value: 'migrate',
        label: '🔄 Migrate an organization from Turso to SQLite/GCS',
      },
      {
        value: 'exit',
        label: '🚪 Exit',
      },
    ],
  });
  
  switch (action) {
    case 'list':
      await listOrganizations();
      break;
    case 'migrate':
      await migrateOrganization();
      break;
    case 'exit':
      console.log('Goodbye! 👋');
      break;
  }
}

if (import.meta.main) {
  main().catch(console.error);
}