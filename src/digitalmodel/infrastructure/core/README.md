# Core Database Manager

## Quick Start

```python
from digitalmodel.core.database_manager import DatabaseManager

# Simple usage
manager = DatabaseManager(
    db_type='postgresql',
    config={
        'host': 'localhost',
        'database': 'mydb',
        'user': 'user',
        'password': 'pass'
    }
)

# Use connection
with manager.get_connection() as conn:
    result = conn.execute("SELECT * FROM users")
    for row in result:
        print(row)

# Cleanup
manager.close()
```

## Features

- **Connection Pooling**: Automatic pooling for PostgreSQL (20), MSSQL (15), MongoDB (10)
- **Retry Logic**: Exponential backoff (1s, 2s, 4s) on transient failures
- **HA Failover**: Primary + replica support with round-robin reads
- **Metrics**: Track connections, query times, failure rates
- **Health Checks**: Built-in health check endpoints
- **Backward Compatible**: Works with existing `database.py` format

## Documentation

- **Migration Guide**: `docs/database_manager_migration_guide.md`
- **Implementation Summary**: `docs/database_manager_implementation_summary.md`
- **Tests**: `tests/core/test_database_manager.py`

## Installation

```bash
uv pip install motor pymongo
```

## Configuration

### Environment Variables (Recommended for Production)

```python
import os

os.environ['DB_HOST'] = 'prod-server'
os.environ['DB_NAME'] = 'production'
os.environ['DB_USER'] = 'app_user'
os.environ['DB_PASSWORD'] = 'secret'

manager = DatabaseManager(db_type='postgresql')
# Environment variables automatically used
```

### Config Dict

```python
manager = DatabaseManager(
    db_type='postgresql',
    config={
        'host': 'localhost',
        'port': 5432,
        'database': 'mydb',
        'user': 'myuser',
        'password': 'mypass'
    }
)
```

### High-Availability

```python
manager = DatabaseManager(
    db_type='postgresql',
    connection_strings=[
        "postgresql://primary:5432/db",
        "postgresql://replica1:5432/db",
        "postgresql://replica2:5432/db"
    ]
)

# Writes → primary
with manager.get_connection(read_only=False) as conn:
    conn.execute("INSERT INTO logs VALUES (...)")

# Reads → round-robin across all
with manager.get_connection(read_only=True) as conn:
    result = conn.execute("SELECT * FROM users")
```

## Supported Databases

- **PostgreSQL**: Full pooling support (default 20 connections)
- **MSSQL**: Full pooling support (default 15 connections)
- **MongoDB**: motor-based pooling (default 10 connections)
- **MS Access**: Single connection, development only

## Metrics

```python
metrics = manager.get_metrics()

print(f"Active: {metrics['active_connections']}")
print(f"Total: {metrics['total_connections']}")
print(f"Errors: {metrics['connection_errors']}")
print(f"Avg query time: {metrics['avg_query_time']:.3f}s")
print(f"Failure rate: {metrics['failure_rate']:.2%}")
```

## Health Check

```python
health = manager.health_check()

if health['status'] == 'healthy':
    print(f"Database OK ({health['response_time']:.3f}s)")
else:
    print(f"Database ERROR: {health.get('error')}")
```

## Legacy Compatibility

```python
# Old database.py format
db_properties = {
    'server_type': 'postgresql',
    'server': 'localhost',
    'database': 'testdb',
    'user': 'testuser',
    'password': 'testpass'
}

# Convert to new manager
manager = DatabaseManager.from_legacy_properties(db_properties)

# Or use legacy wrapper
from digitalmodel.core.database_manager import get_db_connection_pooled
manager, status = get_db_connection_pooled(db_properties)
```

## Best Practices

1. **Always use context managers** for connections
2. **Set environment variables** for production secrets
3. **Monitor metrics** to track performance
4. **Use read-only flag** to leverage replicas
5. **Close manager** on shutdown
6. **Configure pool sizes** based on workload

## Examples

See `docs/database_manager_migration_guide.md` for comprehensive examples.

## Testing

```bash
# Run tests
python -m pytest tests/core/test_database_manager.py -v

# With coverage
python -m pytest tests/core/test_database_manager.py --cov=src/digitalmodel/core
```

## License

Part of digitalmodel project
