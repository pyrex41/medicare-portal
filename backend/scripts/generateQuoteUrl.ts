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
    // First get a session token
    const sessionResponse = await axios.get('http://localhost:8000/api/dev/session/login');
    
    // Then generate the quote ID
    const response = await axios.get(`http://localhost:8000/api/quotes/generate/${contactId}`, {
      headers: {
        Cookie: sessionResponse.headers['set-cookie']?.[0]
      }
    });

    if (response.data.success) {
      console.log('Quote ID:', response.data.quoteId);
      console.log('Quote URL:', response.data.redirectUrl);
    } else {
      console.error('Failed to generate quote ID:', response.data.error);
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