% PostgreSQL Setup Guide for Zotonic Analytics Module

# PostgreSQL Setup Guide

This guide explains how to configure and run the Zotonic Analytics Module with PostgreSQL.

## Overview

The analytics module has been updated to use **PostgreSQL** instead of DuckDB for storing access logs. It uses the **COPY protocol** for efficient batch inserts (10-100x faster than individual INSERTs).

Key features:
- ✅ Connection pooling via `poolboy` (configurable pool size)
- ✅ Batch buffering (up to 500 rows per batch)
- ✅ COPY protocol for fast inserts
- ✅ Auto-schema creation on startup
- ✅ Configurable via `z_config`

## Prerequisites

1. **PostgreSQL 12+** installed and running
2. **Poolboy** in your rebar.config dependencies (automatically included with Zotonic)
3. **epgsql** (Zotonic's PostgreSQL driver)

## Step 1: Create Analytics Database

```bash
# As PostgreSQL superuser
sudo -u postgres createdb analytics
sudo -u postgres createuser analytics_user --password
```

When prompted, enter a secure password for `analytics_user`.

## Step 2: Configure Zotonic

Add these configuration variables to your Zotonic configuration file (typically `~/.config/zotonic/zotonic.config` or environment variables):

```erlang
%% In zotonic.config or equivalent
{analytics_db_host, "localhost"},
{analytics_db_port, 5432},
{analytics_db_name, "analytics"},
{analytics_db_user, "analytics_user"},
{analytics_db_password, "your_secure_password"},
{analytics_db_schema, "public"},
{analytics_db_pool_size, 5}          % Number of DB connections
```

Or set via environment variables:
```bash
export ZOTONIC_ANALYTICS_DB_HOST=localhost
export ZOTONIC_ANALYTICS_DB_PORT=5432
export ZOTONIC_ANALYTICS_DB_NAME=analytics
export ZOTONIC_ANALYTICS_DB_USER=analytics_user
export ZOTONIC_ANALYTICS_DB_PASSWORD=secure_password
export ZOTONIC_ANALYTICS_DB_POOL_SIZE=5
```

## Step 3: Enable the Module

1. Ensure `mod_analytics` is enabled in your Zotonic site configuration
2. Start or restart Zotonic:
   ```bash
   zotonic restart
   ```

The module will automatically:
- Create a connection pool
- Create the `access_log` table if it doesn't exist
- Create required indexes

## Step 4: Verify Setup

### Check logs
```bash
tail -f ~/.config/zotonic/zotonic.log | grep analytics
```

You should see messages like:
```
Analytics logger pool initialized
Analytics schema verified
Analytics batch flushed to PostgreSQL
```

### Query the database
```bash
psql -U analytics_user -d analytics -c "SELECT COUNT(*) FROM access_log;"
```

## Configuration Reference

| Config Key | Default | Description |
|-----------|---------|-------------|
| `analytics_db_host` | `"localhost"` | PostgreSQL server hostname |
| `analytics_db_port` | `5432` | PostgreSQL server port |
| `analytics_db_name` | `"analytics"` | Database name |
| `analytics_db_user` | `"postgres"` | Database user |
| `analytics_db_password` | `""` | Database password |
| `analytics_db_schema` | `"public"` | Database schema |
| `analytics_db_pool_size` | `5` | Connection pool size |

## Database Schema

The module automatically creates the `access_log` table with the following structure:

```sql
CREATE TABLE access_log (
    id BIGSERIAL PRIMARY KEY,
    req_version VARCHAR(10),           -- HTTP version (e.g., "1.1")
    req_method VARCHAR(10),            -- HTTP method (GET, POST, etc)
    req_bytes INTEGER,                 -- Request size in bytes
    resp_category SMALLINT,            -- Response category (1-5 for 1xx-5xx)
    resp_code SMALLINT,                -- HTTP status code
    resp_bytes INTEGER,                -- Response size in bytes
    site VARCHAR(128),                 -- Zotonic site name
    path VARCHAR(512),                 -- Request path
    qs VARCHAR(512),                   -- Query string
    referer VARCHAR(512),              -- HTTP Referer header
    controller VARCHAR(128),           -- Zotonic controller
    dispatch_rule VARCHAR(128),        -- Dispatch rule matched
    rsc_id INTEGER,                    -- Zotonic resource ID (if applicable)
    duration_process INTEGER,          -- Processing time (microseconds)
    duration_total INTEGER,            -- Total request time (microseconds)
    peer_ip INET,                      -- Client IP address
    session_id VARCHAR(50),            -- Session identifier
    user_id INTEGER,                   -- Zotonic user ID (if logged in)
    language VARCHAR(10),              -- Accept-Language
    timezone VARCHAR(64),              -- User timezone
    user_agent TEXT,                   -- HTTP User-Agent header
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### Indexes

The following indexes are automatically created for query performance:

- `idx_access_log_timestamp` - For time-range queries
- `idx_access_log_site_timestamp` - For site-specific time-range queries
- `idx_access_log_rsc_id` - For resource-specific analytics
- `idx_access_log_user_id` - For user activity tracking
- `idx_access_log_session_id` - For session analysis

## Performance Tuning

### Batch Settings

The logger buffers up to 500 rows (configurable in `analytics_logger.erl`):
```erlang
-define(MAX_BUFFERED, 500).       % Flush when this many rows buffered
-define(FLUSH_TIMEOUT_MS, 3000).  % Or flush after this timeout
```

Adjust these based on your traffic:
- **High traffic (>1000 req/s)**: Increase `MAX_BUFFERED` to 1000, decrease `FLUSH_TIMEOUT_MS` to 1000
- **Low traffic (<100 req/s)**: Decrease `MAX_BUFFERED` to 100, keep `FLUSH_TIMEOUT_MS` at 3000

### Connection Pool Size

The pool size defaults to 5 connections. For high concurrency:
```erlang
{analytics_db_pool_size, 10}  % Increase pool size
```

### PostgreSQL Tuning

For high-volume analytics, optimize PostgreSQL:

```sql
-- In postgresql.conf or psql:
ALTER SYSTEM SET shared_buffers = '256MB';
ALTER SYSTEM SET work_mem = '32MB';
ALTER SYSTEM SET maintenance_work_mem = '64MB';
ALTER SYSTEM SET max_parallel_workers_per_gather = 4;
SELECT pg_reload_conf();
```

### Partitioning (Optional)

For very large datasets, partition by month:

```sql
CREATE TABLE access_log_2024_01 PARTITION OF access_log
    FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE access_log_2024_02 PARTITION OF access_log
    FOR VALUES FROM ('2024-02-01') TO ('2024-03-01');
```

## Maintenance

### Archiving Old Data

```bash
# Archive data older than 1 year
psql -U analytics_user -d analytics -c "
    BEGIN;
    CREATE TABLE access_log_archive AS 
        SELECT * FROM access_log 
        WHERE timestamp < NOW() - INTERVAL '1 year';
    DELETE FROM access_log 
        WHERE timestamp < NOW() - INTERVAL '1 year';
    COMMIT;
"
```

### Vacuuming

```bash
# Manual vacuum and analyze
psql -U analytics_user -d analytics -c "VACUUM ANALYZE access_log;"
```

Or configure automatic vacuuming in `postgresql.conf`:
```ini
autovacuum = on
autovacuum_vacuum_scale_factor = 0.01  # Run after 1% changes
autovacuum_analyze_scale_factor = 0.005
```

### Monitoring

Check table size:
```sql
SELECT pg_size_pretty(pg_total_relation_size('access_log'));
```

Check insert rate (per minute):
```sql
SELECT DATE_TRUNC('minute', timestamp), COUNT(*)
FROM access_log
WHERE timestamp > NOW() - INTERVAL '1 hour'
GROUP BY DATE_TRUNC('minute', timestamp)
ORDER BY 1 DESC;
```

## Troubleshooting

### "Connection refused"
```bash
# Check if PostgreSQL is running
sudo systemctl status postgresql
# Or with Docker:
docker ps | grep postgres
```

### "FATAL: Ident authentication failed"
Update `pg_hba.conf` to allow password authentication:
```
# Change this line:
local   all             all                                     peer

# To this:
local   all             all                                     md5
```

Then reload: `sudo systemctl reload postgresql`

### "Database does not exist"
```bash
sudo -u postgres createdb analytics
```

### "Role 'analytics_user' does not exist"
```bash
sudo -u postgres createuser analytics_user --password
```

### "Analytics batch flushed" appearing frequently
This is normal. It means rows are being inserted successfully. Each message shows how many rows were buffered before flushing.

To reduce frequency, increase `FLUSH_TIMEOUT_MS` or `MAX_BUFFERED`.

### No data appearing in the database
1. Check Zotonic logs: `zotonic debug`
2. Verify module is enabled: Check admin interface
3. Test connection manually:
   ```bash
   psql -U analytics_user -d analytics -c "SELECT COUNT(*) FROM access_log;"
   ```

## Migration from DuckDB

If you previously used DuckDB:

1. **Export data** from DuckDB:
   ```bash
   duckdb ducklog.db "SELECT * FROM access_log" > access_log.csv
   ```

2. **Create PostgreSQL table** (see schema above)

3. **Import CSV**:
   ```bash
   psql -U analytics_user -d analytics -c "
   COPY access_log (
       req_version, req_method, req_bytes, resp_category, resp_code, resp_bytes,
       site, path, qs, referer, controller, dispatch_rule, rsc_id,
       duration_process, duration_total, peer_ip, session_id, user_id,
       language, timezone, user_agent, timestamp
   ) FROM STDIN WITH (FORMAT CSV);" < access_log.csv
   ```

4. **Rebuild indexes**:
   ```bash
   psql -U analytics_user -d analytics -c "REINDEX TABLE access_log;"
   ```

## Support

For issues or questions:
1. Check Zotonic logs: `tail -f ~/.config/zotonic/zotonic.log`
2. Enable debug logging in `zotonic.config`: `{loglevel, debug}`
3. Review PostgreSQL logs: `/var/log/postgresql/postgresql.log` (Linux)
