# Database Manager Migration Guide

## Overview

The new `DatabaseManager` provides connection pooling, retry logic, HA failover, and metrics tracking for all database types (MSSQL, PostgreSQL, MongoDB, MS Access).

## Key Features

1. **Connection Pooling**
   - PostgreSQL: 20 connections
   - MSSQL: 15 connections
   - MongoDB: 10 connections
   - MS Access: 1 connection (no pooling)

2. **Retry Logic with Exponential Backoff**
   - Default: 3 retries with 1s, 2s, 4s backoff
   - Smart retry: Skips authentication/permission errors

3. **High-Availability Failover**
   - Support for primary + replica connection strings
   - Writes → primary, Reads → round-robin

4. **Metrics Tracking**
   - Active connections, query times, failure rates
   - Health check endpoints

5. **Backward Compatible**
   - Works with existing `database.py` format
   - Provides legacy wrapper functions

## Migration Steps

### Option 1: New Code (Recommended)

```python
from digitalmodel.core.database_manager import DatabaseManager

# Initialize with config
manager = DatabaseManager(
    db_type='postgresql',
    config={
        'host': 'localhost',
        'database': 'mydb',
        'user': 'myuser',
        'password': 'mypass',
        'port': 5432
    }
)

# Use context manager for safe connections
with manager.get_connection() as conn:
    result = conn.execute("SELECT * FROM users")
    for row in result:
        print(row)

# Get metrics
metrics = manager.get_metrics()
print(f"Active connections: {metrics['active_connections']}")
print(f"Avg query time: {metrics['avg_query_time']:.3f}s")

# Health check
health = manager.health_check()
print(f"Status: {health['status']}")

# Clean up
manager.close()
```

### Option 2: Environment Variables (Production)

```python
import os

# Set environment variables (or use .env file)
os.environ['DB_HOST'] = 'production-server'
os.environ['DB_NAME'] = 'prod_db'
os.environ['DB_USER'] = 'prod_user'
os.environ['DB_PASSWORD'] = 'secure_password'

# Env vars automatically override config
manager = DatabaseManager(db_type='postgresql')
```

### Option 3: High-Availability Setup

```python
# Multiple connection strings for HA
manager = DatabaseManager(
    db_type='postgresql',
    connection_strings=[
        "postgresql://primary:5432/db",
        "postgresql://replica1:5432/db",
        "postgresql://replica2:5432/db"
    ]
)

# Writes always go to primary
with manager.get_connection(read_only=False) as conn:
    conn.execute("INSERT INTO logs VALUES (...)")

# Reads round-robin across all servers
with manager.get_connection(read_only=True) as conn:
    result = conn.execute("SELECT * FROM users")
```

### Option 4: Legacy Compatibility

```python
from digitalmodel.core.database_manager import get_db_connection_pooled

# Old format (from existing database.py)
db_properties = {
    'server_type': 'postgresql',
    'server': 'localhost',
    'database': 'testdb',
    'user': 'testuser',
    'password': 'testpass',
    'port': 5432
}

# Use legacy wrapper (with pooling)
manager, status = get_db_connection_pooled(db_properties)

if status:
    with manager.get_connection() as conn:
        # Use connection
        pass
```

## Configuration Precedence

**Environment variables ALWAYS override config dict:**

1. Environment variables (highest priority)
2. Config dict
3. Default values

## Retry Behavior

### Retryable Errors
- Network failures
- Connection timeouts
- Temporary unavailability

### Non-Retryable Errors
- Authentication failures
- Permission denied
- Invalid database names
- Schema errors

## MongoDB Usage

```python
from digitalmodel.core.database_manager import DatabaseManager

# MongoDB with motor (async)
manager = DatabaseManager(
    db_type='mongodb',
    config={
        'host': 'localhost',
        'port': 27017,
        'database': 'mydb'
    }
)

with manager.get_connection() as db:
    # Use motor API
    collection = db['users']
    # async operations...
```

## MS Access Handling

```python
# Access (development only)
manager = DatabaseManager(
    db_type='access',
    config={
        'database': r'C:\path\to\database.accdb'
    }
)

# Warns in production environment
# No connection pooling (single connection)
```

## Custom Pool Configuration

```python
manager = DatabaseManager(
    db_type='postgresql',
    pool_size=30,          # Custom pool size
    max_overflow=15,       # Max connections beyond pool
    pool_recycle=1800,     # Recycle after 30 minutes
    max_retries=5          # More retry attempts
)
```

## Metrics and Monitoring

```python
# Get detailed metrics
metrics = manager.get_metrics()

print(f"Pool size: {metrics['pool_size']}")
print(f"Active connections: {metrics['active_connections']}")
print(f"Total connections: {metrics['total_connections']}")
print(f"Connection errors: {metrics['connection_errors']}")
print(f"Failure rate: {metrics['failure_rate']:.2%}")
print(f"Avg query time: {metrics['avg_query_time']:.3f}s")
print(f"Total queries: {metrics['total_queries']}")
print(f"Retry attempts: {metrics['retry_attempts']}")

# Health check endpoint
health = manager.health_check()
if health['status'] == 'healthy':
    print(f"Database OK (response: {health['response_time']:.3f}s)")
else:
    print(f"Database UNHEALTHY: {health.get('error')}")
```

## Migrating Existing Code

### Before (using old database.py)

```python
from digitalmodel.asset_integrity.common.database import Database, get_db_connection

db_properties = {...}
dbe, status = get_db_connection(db_properties)

if status:
    dbe.enable_connection_and_cursor()
    # No automatic cleanup
    # No retry logic
    # No pooling
```

### After (using new DatabaseManager)

```python
from digitalmodel.core.database_manager import DatabaseManager

manager = DatabaseManager.from_legacy_properties(db_properties)

with manager.get_connection() as conn:
    # Automatic cleanup
    # Built-in retry logic
    # Connection pooling
    # Metrics tracking
    result = conn.execute(query)
```

## Testing

```python
import pytest
from digitalmodel.core.database_manager import DatabaseManager

def test_database_connection():
    manager = DatabaseManager(
        db_type='postgresql',
        config={'host': 'test-db', 'database': 'test'}
    )

    # Test connection
    with manager.get_connection() as conn:
        result = conn.execute("SELECT 1")
        assert result.scalar() == 1

    # Verify metrics
    metrics = manager.get_metrics()
    assert metrics['total_connections'] >= 1

    # Cleanup
    manager.close()
```

## Best Practices

1. **Always use context managers** for automatic connection cleanup
2. **Set environment variables** for production secrets (never hardcode)
3. **Monitor metrics** to track performance and failures
4. **Use read-only flag** for queries to leverage replicas
5. **Close manager** when application shuts down
6. **Test health check** in monitoring/alerting systems
7. **Configure pool sizes** based on workload (monitor metrics)

## Performance Benefits

- **Connection reuse**: Pooling reduces connection overhead
- **Auto-reconnect**: `pool_pre_ping` handles stale connections
- **Failover**: Automatic replica fallback on primary failure
- **Retry logic**: Transient errors handled automatically
- **Metrics**: Track performance bottlenecks

## Troubleshooting

### Connection Pool Exhausted

```python
# Increase pool size
manager = DatabaseManager(
    db_type='postgresql',
    pool_size=50,
    max_overflow=20
)
```

### Stale Connections

```python
# Reduce pool_recycle time
manager = DatabaseManager(
    db_type='postgresql',
    pool_recycle=900  # 15 minutes
)
```

### High Retry Attempts

```python
# Check metrics
metrics = manager.get_metrics()
if metrics['retry_attempts'] > metrics['total_connections'] * 0.1:
    print("Warning: High retry rate - check network/database")
```

### Authentication Failures

```python
# Use environment variables for credentials
os.environ['DB_USER'] = 'correct_user'
os.environ['DB_PASSWORD'] = 'correct_password'

manager = DatabaseManager(db_type='postgresql')
```

## Migration Checklist

- [ ] Install motor for MongoDB: `uv pip install motor pymongo`
- [ ] Update imports to use `DatabaseManager`
- [ ] Replace direct connection code with context managers
- [ ] Move credentials to environment variables
- [ ] Configure appropriate pool sizes for workload
- [ ] Add health check to monitoring
- [ ] Set up metrics collection
- [ ] Update tests to use new manager
- [ ] Configure HA failover if needed
- [ ] Test retry logic with transient failures
- [ ] Update documentation

## Support

For issues or questions:
- Check metrics: `manager.get_metrics()`
- Run health check: `manager.health_check()`
- Review logs for retry attempts
- Verify environment variables: `manager.get_config()`

## Example: Complete Application

```python
import os
from digitalmodel.core.database_manager import DatabaseManager

# Production configuration
os.environ['DB_HOST'] = 'prod-server'
os.environ['DB_NAME'] = 'production'
os.environ['DB_USER'] = 'app_user'
os.environ['DB_PASSWORD'] = os.environ.get('SECRET_DB_PASSWORD')

def main():
    # Initialize manager
    manager = DatabaseManager(
        db_type='postgresql',
        pool_size=20,
        max_retries=3
    )

    try:
        # Health check
        health = manager.health_check()
        if health['status'] != 'healthy':
            raise RuntimeError(f"Database unhealthy: {health.get('error')}")

        # Application logic
        with manager.get_connection() as conn:
            result = conn.execute("SELECT * FROM users WHERE active = true")
            users = result.fetchall()

            for user in users:
                process_user(user)

        # Log metrics
        metrics = manager.get_metrics()
        print(f"Processed {metrics['total_queries']} queries")
        print(f"Avg query time: {metrics['avg_query_time']:.3f}s")

    finally:
        # Always cleanup
        manager.close()

if __name__ == '__main__':
    main()
```

---

**Migration Status**: Ready for production use
**Backward Compatible**: Yes
**Breaking Changes**: None (legacy wrapper provided)
