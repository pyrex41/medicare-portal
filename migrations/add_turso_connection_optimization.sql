-- Turso/SQLite optimization settings
-- These settings will help improve performance and reduce 429 errors

-- Set synchronous mode to NORMAL for better performance with Turso
PRAGMA synchronous = NORMAL;

-- Set journal mode to WAL for better concurrent operations
PRAGMA journal_mode = WAL;

-- Set temp store to MEMORY to reduce disk I/O
PRAGMA temp_store = MEMORY;

-- Set cache size to 10000 pages (about 40MB)
PRAGMA cache_size = 10000;

-- Set mmap size to 30MB for better read performance 
PRAGMA mmap_size = 30000000;

-- Optimize all indexes
PRAGMA optimize; 