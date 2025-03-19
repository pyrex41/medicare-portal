import axios, { AxiosError } from 'axios';

// Get command line arguments
const orgId = parseInt(process.argv[2]);
const contactId = parseInt(process.argv[3]);

if (isNaN(orgId) || isNaN(contactId)) {
  console.error('Usage: ts-node generateQuoteUrl.ts <orgId> <contactId>');
  process.exit(1);
}

// Call the API to generate a quote ID
const generateQuoteUrl = async () => {
  try {
    // Use the debug endpoint to generate a quote ID without authentication
    const response = await axios.get(`http://localhost:8000/api/quotes/debug-generate/${orgId}/${contactId}`);

    if (response.data.success) {
      console.log('Quote ID:', response.data.quoteId);
      console.log('Quote URL:', response.data.redirectUrl);
    } else {
      console.error('Failed to generate quote ID:', response.data.error);
      
      if (response.data.availableContacts) {
        console.log('\nAvailable contacts for organization', orgId, ':');
        response.data.availableContacts.forEach((contact: any) => {
          console.log(`- ID: ${contact.id}, Name: ${contact.first_name} ${contact.last_name || ''}, Email: ${contact.email || 'N/A'}`);
        });
      }
    }
  } catch (error) {
    if (error instanceof AxiosError) {
      console.error('Error:', error.response?.data || error.message);
    } else {
      console.error('Error:', error);
    }
  }
};

generateQuoteUrl(); 