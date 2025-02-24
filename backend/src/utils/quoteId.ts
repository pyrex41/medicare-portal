import { createHash } from 'crypto';

// Function to generate a unique quote ID from org ID and contact ID
export function generateQuoteId(orgId: number, contactId: number): string {
  // Create a string combining org ID and contact ID
  const data = `${orgId}-${contactId}-${process.env.QUOTE_ID_SECRET || 'default-secret'}`;
  
  // Generate a hash of the data
  const hash = createHash('sha256').update(data).digest('base64');
  
  // Take first 12 characters and make URL safe
  const urlSafeHash = hash
    .slice(0, 12)
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
  
  // Encode org ID and contact ID in base36 for shorter strings
  const encodedOrgId = orgId.toString(36);
  const encodedContactId = contactId.toString(36);
  
  // Combine everything with a separator
  return `${encodedOrgId}-${encodedContactId}-${urlSafeHash}`;
}

// Function to decode a quote ID back to org ID and contact ID
export function decodeQuoteId(quoteId: string): { orgId: number; contactId: number } | null {
  try {
    // Split the quote ID into its components
    const [encodedOrgId, encodedContactId] = quoteId.split('-');
    
    // Decode the IDs from base36 back to numbers
    const orgId = parseInt(encodedOrgId, 36);
    const contactId = parseInt(encodedContactId, 36);
    
    if (isNaN(orgId) || isNaN(contactId)) {
      return null;
    }
    
    return { orgId, contactId };
  } catch (e) {
    return null;
  }
} 