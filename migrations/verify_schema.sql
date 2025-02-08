-- Run these queries to verify the schema
SELECT sql FROM sqlite_master WHERE type='table' AND name='organizations';
SELECT sql FROM sqlite_master WHERE type='table' AND name='users';
SELECT sql FROM sqlite_master WHERE type='table' AND name='verification_tokens'; 