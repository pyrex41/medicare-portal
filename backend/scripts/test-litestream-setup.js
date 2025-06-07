#!/usr/bin/env node

/**
 * Enhanced validation script to test Litestream setup, error handling, and Replit SDK integration
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

async function testReplitStorageSDK() {
  try {
    // Dynamic import to handle potential missing dependency
    const { Client } = await import('@replit/object-storage');
    const client = new Client();
    
    console.log('✅ Replit Object Storage SDK available');
    
    // Test basic connectivity
    const testKey = `health-check-${Date.now()}`;
    const testData = 'test-connection';
    
    try {
      const uploadResult = await client.uploadFromText(testKey, testData);
      if (uploadResult.ok) {
        console.log('✅ Replit Object Storage connectivity test passed');
        
        // Clean up test file
        await client.delete(testKey);
        return true;
      } else {
        console.log('⚠️  Replit Object Storage upload test failed:', uploadResult.error);
        return false;
      }
    } catch (error) {
      console.log('⚠️  Replit Object Storage connectivity test failed:', error.message);
      return false;
    }
  } catch (error) {
    console.log('⚠️  Replit Object Storage SDK not available');
    console.log('   This is normal for local development');
    return false;
  }
}

async function testEnvironmentSetup() {
  console.log('🔍 Testing Enhanced Litestream + Replit SDK Setup\n');
  
  // Test binary availability
  const binaryAvailable = await testLitestreamBinary();
  
  // Test Replit SDK
  const replitSDKAvailable = await testReplitStorageSDK();
  
  // Test environment variables
  const gcsBucket = process.env.GCS_BUCKET_NAME;
  if (gcsBucket) {
    console.log(`✅ GCS_BUCKET_NAME configured: ${gcsBucket}`);
  } else {
    console.log('⚠️  GCS_BUCKET_NAME not set (will use default: replit-object-storage)');
  }
  
  // Test Google Cloud credentials
  const gcsCredentials = process.env.GOOGLE_APPLICATION_CREDENTIALS;
  if (gcsCredentials) {
    if (fs.existsSync(gcsCredentials)) {
      console.log('✅ Google Cloud credentials file found');
    } else {
      console.log('❌ Google Cloud credentials file not found at specified path');
    }
  } else {
    console.log('⚠️  GOOGLE_APPLICATION_CREDENTIALS not set (may use default auth)');
  }
  
  // Test path to config file
  const configPath = '/workspace/litestream-single-db.yml';
  if (fs.existsSync(configPath)) {
    console.log('✅ Litestream config template found');
  } else {
    console.log('❌ Litestream config template not found at:', configPath);
  }
  
  console.log('\n📋 Setup Summary:');
  console.log(`   Litestream Binary: ${binaryAvailable ? '✅' : '❌'}`);
  console.log(`   Replit SDK: ${replitSDKAvailable ? '✅' : '⚠️  (not available)'}`);
  console.log(`   Config Template: ${fs.existsSync(configPath) ? '✅' : '❌'}`);
  console.log(`   GCS Bucket: ${gcsBucket || 'default'}`);
  console.log(`   GCS Credentials: ${gcsCredentials ? '✅' : '⚠️  (default auth)'}`);
  
  console.log('\n🏗️  Architecture Strategy:');
  if (replitSDKAvailable) {
    console.log('   📁 Replica Management: Replit SDK (simplified operations)');
  } else {
    console.log('   📁 Replica Management: GCS SDK (full control)');
  }
  console.log('   🔒 Atomic Locking: GCS SDK (ifGenerationMatch support)');
  console.log('   🔄 Litestream Replication: Standard GCS (proven reliability)');
  
  if (binaryAvailable && fs.existsSync(configPath)) {
    console.log('\n🎉 Hybrid SQLite/GCS architecture ready!');
    if (replitSDKAvailable) {
      console.log('   Using optimal hybrid SDK approach');
    } else {
      console.log('   Using GCS SDK for all operations (still fully functional)');
    }
  } else {
    console.log('\n⚠️  Setup incomplete - see errors above');
  }
}

// Run if this is the main module
if (import.meta.url === `file://${process.argv[1]}`) {
  testEnvironmentSetup().catch(console.error);
}