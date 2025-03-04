// Get organization subscription info
router.get('/:slug/subscription', async (req, res) => {
  try {
    const { slug } = req.params;
    
    // Get the organization data
    const organization = await db.get(
      `SELECT subscription_tier as tierId, agent_limit as agentLimit, contact_limit as contactLimit 
       FROM organizations WHERE slug = ?`,
      [slug]
    );
    
    if (!organization) {
      return res.status(404).json({ error: 'Organization not found' });
    }
    
    res.json(organization);
  } catch (error) {
    console.error('Error fetching organization subscription:', error);
    res.status(500).json({ error: 'Internal server error' });
  }
}); 