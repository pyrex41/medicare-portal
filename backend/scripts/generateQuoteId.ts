import { generateQuoteId } from '../src/utils/quoteId';

// Get command line arguments
const orgId = parseInt(process.argv[2]);
const contactId = parseInt(process.argv[3]);

if (isNaN(orgId) || isNaN(contactId)) {
  console.error('Usage: ts-node generateQuoteId.ts <orgId> <contactId>');
  process.exit(1);
}

// Generate quote ID
const quoteId = generateQuoteId(orgId, contactId);

// Output results
console.log('Generated Quote ID:', quoteId);
console.log('Quote URL:', `http://localhost:5173/quote?id=${quoteId}`); 