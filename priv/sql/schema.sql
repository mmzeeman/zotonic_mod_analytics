-- PostgreSQL schema for Zotonic Analytics Module
-- This file is automatically run by analytics_logger.erl on startup
-- 
-- If you prefer to create the schema manually, run this script:
-- psql -U postgres -d analytics -f priv/sql/schema.sql

CREATE TABLE IF NOT EXISTS access_log (
    id BIGSERIAL PRIMARY KEY,
    req_version VARCHAR(10),
    req_method VARCHAR(10),
    req_bytes INTEGER,
    resp_category SMALLINT,
    resp_code SMALLINT,
    resp_bytes INTEGER,
    site VARCHAR(128),
    path VARCHAR(512),
    qs VARCHAR(512),
    referer VARCHAR(512),
    controller VARCHAR(128),
    dispatch_rule VARCHAR(128),
    rsc_id INTEGER,
    duration_process INTEGER,
    duration_total INTEGER,
    peer_ip INET,
    session_id VARCHAR(50),
    user_id INTEGER,
    language VARCHAR(10),
    timezone VARCHAR(64),
    user_agent TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for common queries
CREATE INDEX IF NOT EXISTS idx_access_log_timestamp 
    ON access_log(timestamp DESC);

CREATE INDEX IF NOT EXISTS idx_access_log_site_timestamp 
    ON access_log(site, timestamp DESC);

CREATE INDEX IF NOT EXISTS idx_access_log_rsc_id 
    ON access_log(rsc_id);

CREATE INDEX IF NOT EXISTS idx_access_log_user_id 
    ON access_log(user_id);

CREATE INDEX IF NOT EXISTS idx_access_log_session_id 
    ON access_log(session_id);

-- Optional: Create a view for daily summary stats
CREATE OR REPLACE VIEW access_log_daily_summary AS
SELECT
    DATE(timestamp) AS day,
    site,
    COUNT(*) AS total_requests,
    COUNT(DISTINCT session_id) AS unique_sessions,
    COUNT(DISTINCT user_id) AS unique_users,
    COUNT(DISTINCT peer_ip) AS unique_ips,
    ROUND(AVG(CAST(duration_total AS NUMERIC)), 2) AS avg_duration_ms,
    COUNT(*) FILTER (WHERE resp_code >= 200 AND resp_code < 300) AS successful_requests,
    COUNT(*) FILTER (WHERE resp_code >= 400 AND resp_code < 500) AS client_errors,
    COUNT(*) FILTER (WHERE resp_code >= 500) AS server_errors
FROM access_log
GROUP BY DATE(timestamp), site;
