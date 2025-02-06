-- Verify user exists with correct data
SELECT u.*, o.slug 
FROM users u
JOIN organizations o ON u.organization_id = o.id
WHERE LOWER(u.email) = LOWER('reub.brooks@gmail.com')
AND o.slug = 'demo-org'; 