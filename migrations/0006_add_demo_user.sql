-- Add user entry for the demo agent
INSERT INTO users (email, organization_id, role)
SELECT 
    'reub.brooks@gmail.com',
    o.id,
    'agent'
FROM organizations o 
WHERE o.slug = 'demo-org'; 