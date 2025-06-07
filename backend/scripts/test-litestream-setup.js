#!/usr/bin/env node

/**
 * Simple validation script to test Litestream setup and error handling
 */

import { spawn } from 'child_process';
import fs from 'fs';

async function testLitestreamBinary() {
  return new Promise((resolve) => {
    const process = spawn('litestream', ['version']);
    
    let stdout = '';
    let stderr = '';
    
    process.stdout?.on('data', (data) => stdout += data);
    process.stderr?.on('data', (data) => stderr += data);
    
    process.on('close', (code) => {
      if (code === 0) {
        console.log('✅ Litestream binary found and working');
        console.log(`   Version info: ${stdout.trim()}`);
        resolve(true);
      } else {
        console.log('❌ Litestream binary found but returned error');
        console.log(`   Error: ${stderr.trim()}`);
        resolve(false);
      }
    });

    process.on('error', (error) => {
      if (error.code === 'ENOENT') {
        console.log('❌ Litestream binary not found in PATH');
        console.log('   Please install litestream (see documentation)');
      } else {
        console.log('❌ Error running litestream:', error.message);
      }
      resolve(false);
    });
  });
}

async function testEnvironmentSetup() {
  console.log('🔍 Testing Litestream Setup\n');
  
  // Test binary availability
  const binaryAvailable = await testLitestreamBinary();
  
  // Test environment variables
  const gcsBucket = process.env.GCS_BUCKET_NAME;
  if (gcsBucket) {
    console.log(`✅ GCS_BUCKET_NAME configured: ${gcsBucket}`);
  } else {
    console.log('⚠️  GCS_BUCKET_NAME not set (will use default: replit-object-storage)');
  }
  
  // Test path to config file
  const configPath = '/workspace/litestream-single-db.yml';
  if (fs.existsSync(configPath)) {
    console.log('✅ Litestream config template found');
  } else {
    console.log('❌ Litestream config template not found at:', configPath);
  }
  
  console.log('\n📋 Setup Summary:');
  console.log(`   Binary Available: ${binaryAvailable ? '✅' : '❌'}`);
  console.log(`   Config Template: ${fs.existsSync(configPath) ? '✅' : '❌'}`);
  console.log(`   GCS Bucket: ${gcsBucket || 'default'}`);
  
  if (binaryAvailable && fs.existsSync(configPath)) {
    console.log('\n🎉 Litestream setup is ready for SQLite/GCS architecture!');
  } else {
    console.log('\n⚠️  Setup incomplete - see errors above');
  }
}

// Run if this is the main module
if (import.meta.url === `file://${process.argv[1]}`) {
  testEnvironmentSetup().catch(console.error);
}