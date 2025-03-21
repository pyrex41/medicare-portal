import { createHash } from 'crypto';

// Function to generate a unique quote ID from org ID and contact ID
export function generateQuoteId(orgId: number, contactId: number): string {
    // Create a string to hash that includes org, contact, and secret
    const dataToHash = `${orgId}-${contactId}-${process.env.QUOTE_SECRET || 'your-default-secret-key'}`;
    
    // Generate hash using crypto
    const hash = createHash('sha256')
        .update(dataToHash)
        .digest('hex')
        .slice(0, 8); // Take first 8 characters for brevity
    
    // Combine components into quote ID
    return `${orgId}-${contactId}-${hash}`;
}

// Function to decode a quote ID back to org ID and contact ID
export function decodeQuoteId(quoteId: string): { orgId: number; contactId: number } | null {
    try {
        const parts = quoteId.split('-');
        if (parts.length !== 3) {
            return null;
        }

        const [orgId, contactId, providedHash] = parts;
        
        // Recreate hash to validate
        const dataToHash = `${orgId}-${contactId}-${process.env.QUOTE_SECRET || 'your-default-secret-key'}`;
        const expectedHash = createHash('sha256')
            .update(dataToHash)
            .digest('hex')
            .slice(0, 8);

        // Compare hashes
        if (providedHash !== expectedHash) {
            return null;
        }

        return {
            orgId: parseInt(orgId),
            contactId: parseInt(contactId)
        };
    } catch (e) {
        return null;
    }
} 