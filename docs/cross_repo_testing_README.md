# Cross-Repository Testing Infrastructure - Quick Reference

## Overview

Automated testing infrastructure for all 26 repositories in workspace-hub with:
- ✅ Parallel execution (4 workers by default)
- ✅ Isolated uv environments per repository
- ✅ Standards compliance checking
- ✅ Interactive HTML dashboards (Plotly)
- ✅ CSV and JSON exports
- ✅ Timeout protection (5 min per repo)

## Quick Start

```bash
# Windows
scripts\run_cross_repo_tests.bat

# Linux/macOS
./scripts/run_cross_repo_tests.sh

# Or directly with uv
uv run python scripts/cross_repo_test_runner.py
```

## Files Created

### Core Implementation
1. **`scripts/cross_repo_test_runner.py`** - Main test runner with parallel execution
2. **`tests/cross_repo/test_standards_compliance.py`** - Compliance test suite
3. **`config/cross-repo-test-config.yaml`** - Configuration file

### Helper Scripts
4. **`scripts/run_cross_repo_tests.bat`** - Windows launcher
5. **`scripts/run_cross_repo_tests.sh`** - Unix/Linux launcher

### Documentation
6. **`docs/CROSS_REPO_TESTING.md`** - Full documentation
7. **`docs/CROSS_REPO_TESTING_EXAMPLES.md`** - Examples and use cases
8. **`docs/cross_repo_testing_README.md`** - This file

### Output Directory
9. **`reports/cross_repo_tests/`** - Test results directory
   - `dashboard-{timestamp}.html` - Interactive Plotly dashboard
   - `results-{timestamp}.csv` - CSV export (one row per repo)
   - `test-results-{timestamp}.json` - Full JSON results

## Key Features

### Automated Repository Discovery
- Scans `D:/workspace-hub/` for repositories
- Requires: `pyproject.toml` + `tests/` directory
- Currently finds: 8 repositories (expandable to 26+)

### Parallel Test Execution
- Default: 4 concurrent workers
- Isolated uv environments per repository
- 5-minute timeout per repository
- Gracefully handles failures

### Standards Compliance Checks

| Check | Description |
|-------|-------------|
| **Module Structure** | Validates `src/*/modules/` pattern |
| **File Organization** | Ensures no work files in root directory |
| **CLAUDE.md** | Checks for required sections |
| **Agent Registry** | Validates agent registry JSON |

### Interactive HTML Dashboard

6 Plotly visualizations:
1. Test results by repository (stacked bar)
2. Test execution time (bar)
3. Coverage distribution (box plot)
4. Compliance scores (bar)
5. Test status summary (pie)
6. Top issues by repository (bar)

## Command Options

```bash
# Sequential execution (debugging)
uv run python scripts/cross_repo_test_runner.py --sequential

# Increase parallelism
uv run python scripts/cross_repo_test_runner.py --workers 8

# Extend timeout
uv run python scripts/cross_repo_test_runner.py --timeout 600
```

## Output Interpretation

### Test Status Values
- `passed` - All tests passed
- `failed` - One or more tests failed
- `skipped` - Missing requirements (no tests/)
- `error` - Execution error (imports, syntax)
- `timeout` - Exceeded time limit
- `no_tests` - No tests collected

### Compliance Score
- **100%** - All checks passed
- **75%** - 3 out of 4 checks passed
- **<75%** - Multiple issues, needs attention

## Common Use Cases

### 1. Daily Health Check
```bash
# Run every morning
scripts/run_cross_repo_tests.bat
# Check dashboard for any regressions
```

### 2. Pre-Release Validation
```bash
# Before major releases
uv run python scripts/cross_repo_test_runner.py --workers 8
# Ensure all repositories pass
```

### 3. Compliance Audit
```bash
# Check specific repository
uv run python tests/cross_repo/test_standards_compliance.py /d/workspace-hub/my-repo
```

### 4. Performance Analysis
```bash
# Identify slow test suites
# Check CSV: sort by duration_seconds column
```

## Expected Performance

| Mode | Workers | Time (8 repos) | Time (26 repos) |
|------|---------|----------------|-----------------|
| Sequential | 1 | ~20 min | ~60 min |
| Parallel | 4 | ~8 min | ~25 min |
| High Parallel | 8 | ~5 min | ~15 min |

*Times vary based on test suite sizes*

## Requirements

### Repository Requirements
- `pyproject.toml` with uv configuration
- `tests/` directory with pytest tests
- Optional: `pytest-cov` for coverage

### System Requirements
- Python 3.10+
- uv package manager
- Dependencies: `pandas`, `plotly`, `pyyaml`

## Troubleshooting

### No repositories found
**Fix:** Ensure repositories have `pyproject.toml` AND `tests/` directory

### Module import errors
**Fix:** Run `uv sync` in failing repository

### Coverage not shown
**Fix:** Install `pytest-cov`: `uv add --dev pytest-cov`

### Timeout errors
**Fix:** Increase timeout with `--timeout 600`

## Configuration

Edit `config/cross-repo-test-config.yaml` to customize:

```yaml
execution:
  mode: "parallel"
  max_workers: 4
  timeout_per_repo: 300

standards:
  module_structure:
    enabled: true
  file_organization:
    enabled: true
  # ... more options
```

## Integration Examples

### GitHub Actions
```yaml
- name: Cross-repo tests
  run: uv run python scripts/cross_repo_test_runner.py
```

### Pre-commit Hook
```bash
# .git/hooks/pre-commit
uv run python tests/cross_repo/test_standards_compliance.py $(git rev-parse --show-toplevel)
```

### Cron Job
```bash
# Daily at 2 AM
0 2 * * * cd /workspace-hub/digitalmodel && uv run python scripts/cross_repo_test_runner.py
```

## Next Steps

1. **Run initial test:** `scripts/run_cross_repo_tests.bat`
2. **Review dashboard:** Open `reports/cross_repo_tests/dashboard-*.html`
3. **Address compliance issues:** Check suggestions in JSON results
4. **Set up automation:** Add to CI/CD or cron
5. **Monitor trends:** Run regularly and track improvements

## Documentation

- **Full Guide:** `docs/CROSS_REPO_TESTING.md`
- **Examples:** `docs/CROSS_REPO_TESTING_EXAMPLES.md`
- **Config Reference:** `config/cross-repo-test-config.yaml`

## Support

For issues or questions:
1. Check troubleshooting section in `CROSS_REPO_TESTING.md`
2. Review examples in `CROSS_REPO_TESTING_EXAMPLES.md`
3. Examine configuration in `cross-repo-test-config.yaml`

---

**Created:** 2026-01-06
**Version:** 1.0.0
**Status:** Ready for production use
