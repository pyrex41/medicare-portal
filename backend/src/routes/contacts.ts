import { createClient } from '@libsql/client';

// Get Turso client for organization
async function getOrgTursoClient(orgId: string) {
  const org = await db.one(`
    SELECT turso_db_url, turso_auth_token 
    FROM organizations 
    WHERE id = $1`,
    [orgId]
  );

  return createClient({
    url: org.turso_db_url,
    authToken: org.turso_auth_token
  });
}

// Update contacts endpoints to use org's Turso DB
app.post('/api/contacts/upload', async (req, res) => {
  const orgId = req.user.organizationId;
  const tursoClient = await getOrgTursoClient(orgId);

  try {
    // Process CSV file
    const contacts = parseCSV(req.file);
    
    // Insert into org's Turso DB
    for (const contact of contacts) {
      await tursoClient.execute(`
        INSERT INTO contacts (
          first_name, last_name, email, /* other fields */
        ) VALUES (?, ?, ?, /* other values */)`,
        [contact.firstName, contact.lastName, contact.email /* other values */]
      );
    }

    res.json({ success: true });
  } catch (err) {
    res.status(500).json({ 
      success: false,
      message: err.message,
      // Include other error details as needed
    });
  }
});

// Similar updates needed for other contact endpoints 