# Automation Module

**ABOUTME**: Automation utilities for digitalmodel including memory lifecycle management, workflow automation, and agent coordination tools.

## Components

### Memory Lifecycle Management

TTL-based retention, garbage collection, and CRDT conflict resolution for agent coordination memory.

**Quick Start:**

```python
from digitalmodel.workflows.automation.memory_lifecycle import MemoryLifecycleManager

# Initialize
manager = MemoryLifecycleManager()

# Store data
manager.store("coordination/agent1/status", {"task": "running"})

# Retrieve data
status = manager.retrieve("coordination/agent1/status")

# Cleanup expired entries
stats = manager.cleanup()
```

**Features:**
- ✅ TTL-based retention policies (1 hour, 24 hours, 7 days, permanent)
- ✅ Multiple cleanup strategies (lazy, scheduled, manual)
- ✅ CRDT conflict resolution (Last-Write-Wins, Multi-Value, Counter)
- ✅ Automatic compression for large values (gzip, >1KB)
- ✅ SQLite metadata tracking
- ✅ Claude-flow hook integration

**Files:**
- `memory_lifecycle.py` - Main implementation (302 lines, 89.95% coverage)
- `../../scripts/memory_cleanup_hook.py` - Hook integration
- `../../scripts/scheduled_memory_cleanup.py` - Scheduled daemon
- `../../scripts/example_memory_lifecycle.py` - Usage examples

**Documentation:**
- [Complete Guide](../../../docs/domains/automation/MEMORY_LIFECYCLE.md)
- [Retention Policies](../../../.claude/memory-retention-policies.yaml)
- [Tests](../../../tests/test_memory_lifecycle.py)

**Usage Examples:**

```bash
# Run examples
python scripts/example_memory_lifecycle.py

# Run tests
uv run pytest tests/test_memory_lifecycle.py -v

# Hook integration
python scripts/memory_cleanup_hook.py --hook session-end --session-id "abc123"
python scripts/memory_cleanup_hook.py --hook memory-cleanup --force

# Scheduled cleanup (daily at 2 AM)
python scripts/scheduled_memory_cleanup.py --daemon

# Run once
python scripts/scheduled_memory_cleanup.py --once
```

## Architecture

```
automation/
├── memory_lifecycle.py           # Main implementation
│   ├── MemoryLifecycleManager   # Orchestrator
│   ├── RetentionPolicy          # Policy configuration
│   ├── MemoryGarbageCollector   # Cleanup operations
│   ├── CRDTResolver             # Conflict resolution
│   └── MemoryCompressor         # Value optimization
└── README.md                     # This file
```

## Configuration

Retention policies are defined in `.claude/memory-retention-policies.yaml`:

```yaml
namespaces:
  - namespace: "coordination/*"
    ttl_seconds: 3600          # 1 hour
    cleanup_strategy: lazy     # Clean on access
    conflict_strategy: lww     # Last-Write-Wins
    compression_enabled: true
```

## Storage

- **Files**: `.claude-flow/memory/*.json` (one file per key)
- **Metadata**: `.claude-flow/memory.db` (SQLite)
- **Policies**: `.claude/memory-retention-policies.yaml`

## Performance

Typical performance with 10,000 entries:

| Operation | Time |
|-----------|------|
| Store | ~2ms |
| Retrieve (uncompressed) | ~1ms |
| Retrieve (compressed) | ~3ms |
| Cleanup | ~500ms |

Code coverage: **89.95%** (302 statements, 26 missed)

## Testing

```bash
# Run all tests
uv run pytest tests/test_memory_lifecycle.py -v

# Run specific test class
uv run pytest tests/test_memory_lifecycle.py::TestMemoryLifecycleManager -v

# Run with coverage
uv run pytest tests/test_memory_lifecycle.py --cov=digitalmodel.workflows.automation

# 29 tests, all passing ✅
```

## Integration with Claude-Flow

The memory lifecycle system integrates with claude-flow through hooks:

### Hook Points

1. **session-end**: Cleanup session-specific memory
2. **pre-task**: Cleanup stale coordination data before task
3. **memory-cleanup**: Periodic maintenance

### Example Hook Script

```bash
#!/bin/bash
# .claude-flow/hooks/session-end.sh

python scripts/memory_cleanup_hook.py \
  --hook session-end \
  --session-id "$SESSION_ID" \
  --output json
```

## Future Enhancements

- [ ] Distributed coordination with Redis backend
- [ ] Real-time cleanup monitoring dashboard
- [ ] Advanced CRDT types (G-Counter, PN-Counter, OR-Set)
- [ ] Automatic policy learning from usage patterns
- [ ] Memory usage alerts and quotas

## Related Modules

- `digitalmodel.solvers.orcaflex` - OrcaFlex simulation automation
- `digitalmodel.signal_processing.signal_analysis` - Signal processing
- `digitalmodel.infrastructure.cathodic_protection` - CP system design

## Contributing

When adding new automation features:

1. Add comprehensive docstrings and type hints
2. Write tests with >85% coverage
3. Update this README
4. Add usage examples
5. Document configuration options

## License

Part of the digitalmodel project.
