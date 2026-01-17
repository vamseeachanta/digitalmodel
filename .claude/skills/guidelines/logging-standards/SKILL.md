# Logging Standards Skill

> Version: 1.0.0
> Category: Standards
> Triggers: Adding logging, debug output, error tracking

## Quick Reference

### Logging Levels

| Level | When to Use |
|-------|------------|
| **DEBUG** | Variable inspection, development troubleshooting |
| **INFO** | Normal operations, milestones, confirmations |
| **WARNING** | Deprecated features, recoverable errors, anomalies |
| **ERROR** | Failed operations, exceptions, data issues |
| **CRITICAL** | System crashes, data loss, security breaches |

### Standard Format

```
%(asctime)s - %(name)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(message)s
```

**Example:**
```
2025-01-14 14:23:45,123 - data_processor - INFO - [processor.py:45] - Processing started
```

## Python Setup

### Basic Configuration

```python
import logging

logger = logging.getLogger(__name__)

# Configure with standard format
formatter = logging.Formatter(
    "%(asctime)s - %(name)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(message)s"
)

handler = logging.StreamHandler()
handler.setFormatter(formatter)
logger.addHandler(handler)
logger.setLevel(logging.INFO)
```

### YAML Configuration

```yaml
version: 1
disable_existing_loggers: false

formatters:
  standard:
    format: "%(asctime)s - %(name)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(message)s"

handlers:
  console:
    class: logging.StreamHandler
    formatter: standard
    level: INFO
  file:
    class: logging.handlers.RotatingFileHandler
    filename: logs/app.log
    maxBytes: 10485760  # 10MB
    backupCount: 5

root:
  level: INFO
  handlers: [console, file]
```

## Usage Examples

### Good Logging

```python
# ✅ DEBUG: Context for troubleshooting
logger.debug("Function called with args: %s", args)
logger.debug("Processing item %d of %d", current, total)

# ✅ INFO: Confirmations and milestones
logger.info("Starting data processing pipeline")
logger.info("Successfully processed %d records", count)

# ✅ WARNING: Potential issues
logger.warning("Deprecated function called: %s", func_name)
logger.warning("Retry attempt %d of %d", attempt, max_retries)

# ✅ ERROR: Failed operations with context
logger.error("Failed to connect to database: %s", error)
logger.error("Invalid input data: %s", validation_error, exc_info=True)
```

### Bad Logging

```python
# ❌ No context
logger.error("Error occurred")

# ❌ Loses stack trace
try:
    result = process_data(data)
except Exception as e:
    logger.error(str(e))  # Missing exc_info=True

# ❌ Too verbose
logger.debug(f"Processing: x={x}, y={y}, z={z}, a={a}, b={b}, c={c}")
```

## Sensitive Data Protection

### NEVER Log
- Passwords or API keys
- Credit card numbers
- Social security numbers
- Session tokens
- Personal health information

### Sanitize Before Logging

```python
def sanitize_for_logging(data: dict) -> dict:
    sensitive_fields = ['password', 'api_key', 'token', 'ssn']
    sanitized = data.copy()
    for field in sensitive_fields:
        if field in sanitized:
            sanitized[field] = '***REDACTED***'
    return sanitized

logger.info("User data: %s", sanitize_for_logging(user_data))
```

## Performance Logging

### Track Execution Time

```python
import time
from functools import wraps

def log_execution_time(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        logger.debug("Starting %s", func.__name__)
        try:
            result = func(*args, **kwargs)
            elapsed = time.time() - start
            logger.info("Function %s completed in %.3f seconds", func.__name__, elapsed)
            return result
        except Exception as e:
            elapsed = time.time() - start
            logger.error("Function %s failed after %.3f seconds: %s", func.__name__, elapsed, str(e))
            raise
    return wrapper
```

## File Management

### Directory Structure

```
repository/
├── logs/
│   ├── app.log          # General logs
│   ├── error.log        # Errors only
│   ├── performance.log  # Performance metrics
│   └── archive/         # Rotated logs
└── logs/.gitignore      # Prevent logs from git
```

### Rotation Configuration

**maxBytes: 10MB** (10485760 bytes)
**backupCount: 5** files

### Log Retention

| Type | Retention | Rotation |
|------|-----------|----------|
| Application | 7 days | Daily |
| Error | 30 days | Weekly |
| Performance | 14 days | Daily |
| Debug | 3 days | Daily |

## Testing Logs

### Capture Logs in Tests

```python
def test_with_logs(caplog):
    caplog.set_level(logging.INFO)

    process_data([1, 2, 3])

    assert "Processing started" in caplog.text
    assert caplog.records[0].levelname == "INFO"
```

## Compliance Checklist

- [ ] All five logging levels supported (DEBUG, INFO, WARNING, ERROR, CRITICAL)
- [ ] Standard format used with timestamps, module name, level, file, line
- [ ] logs/ directory configured with .gitignore
- [ ] Log rotation setup (10MB max, 5 backups)
- [ ] Sensitive information sanitized before logging
- [ ] Module-specific loggers using __name__
- [ ] Performance logging for long operations
- [ ] Exception logging with stack traces (exc_info=True)
- [ ] Configuration loaded from YAML

## Full Reference

See: @docs/modules/standards/LOGGING_STANDARDS.md

---

*Use this when adding logging to modules, debugging issues, or setting up log infrastructure.*
