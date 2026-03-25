# Cross-Repository Testing Infrastructure

Automated testing infrastructure for all repositories in workspace-hub with parallel execution, compliance checking, and interactive reporting.

## Features

- **Auto-discovery**: Finds all repositories with `pyproject.toml` and `tests/` directory
- **Parallel execution**: Tests up to 4 repositories concurrently
- **Isolated environments**: Each repository runs in its own uv environment
- **Compliance checking**: Validates module structure, file organization, CLAUDE.md, and agent registries
- **Interactive dashboard**: Plotly-based HTML reports with visualizations
- **Multiple export formats**: JSON, CSV, and HTML outputs
- **Timeout protection**: 5-minute timeout per repository
- **Graceful degradation**: Skips non-compliant repositories with warnings

## Usage

### Basic Usage

```bash
# Run with default settings (4 parallel workers)
uv run python scripts/cross_repo_test_runner.py

# Run sequentially (slower but easier to debug)
uv run python scripts/cross_repo_test_runner.py --sequential

# Increase parallelism
uv run python scripts/cross_repo_test_runner.py --workers 8

# Adjust timeout (in seconds)
uv run python scripts/cross_repo_test_runner.py --timeout 600
```

### Manual Compliance Check

Test a single repository for standards compliance:

```bash
uv run python tests/cross_repo/test_standards_compliance.py /path/to/repository
```

## Configuration

Edit `config/cross-repo-test-config.yaml` to customize:

- **Repository discovery**: Scan paths and exclusions
- **Execution settings**: Workers, timeouts, isolation
- **Compliance checks**: Enable/disable specific checks
- **Reporting options**: Output formats and locations

## Standards Compliance Checks

### 1. Module Structure
- Validates presence of `src/*/modules/` pattern
- Ensures standard package organization

### 2. File Organization
- Checks for disallowed files in repository root
- Only allows: pyproject.toml, README.md, CLAUDE.md, .gitignore, LICENSE, etc.
- Work files must be in subdirectories (src/, tests/, docs/, scripts/)

### 3. CLAUDE.md Validation
- Verifies CLAUDE.md exists
- Checks for required sections (e.g., "Interactive Engagement")

### 4. Agent Registry
- Looks for agent registry in standard locations:
  - `.claude/agents-registry.json`
  - `modules/config/ai-agents-registry.json`
- Validates JSON structure and agent definitions

## Output Files

All outputs saved to `reports/cross_repo_tests/`:

### HTML Dashboard (`dashboard-{timestamp}.html`)
Interactive Plotly visualizations:
- Test results by repository (stacked bar chart)
- Test execution time (bar chart)
- Coverage distribution (box plot)
- Compliance scores (bar chart)
- Test status summary (pie chart)
- Top issues by repository (bar chart)

### CSV Export (`results-{timestamp}.csv`)
One row per repository with columns:
- `repo_name`: Repository name
- `status`: Test status (passed/failed/skipped/error)
- `tests_passed`, `tests_failed`, `tests_skipped`, `tests_total`: Test counts
- `duration_seconds`: Execution time
- `coverage_percent`: Code coverage (if available)
- `compliance_score`: Overall compliance percentage
- `module_structure_ok`, `file_organization_ok`, `claude_md_ok`, `agent_registry_ok`: Individual check results
- `suggestion_count`: Number of improvement suggestions

### JSON Results (`test-results-{timestamp}.json`)
Complete results with metadata:
- `timestamp`: Execution timestamp
- `test_results`: Array of test results per repository
- `compliance_results`: Array of compliance results per repository
- `summary`: Aggregated statistics

## Repository Requirements

For a repository to be tested, it must have:
1. `pyproject.toml` file (uv/poetry configuration)
2. `tests/` directory with pytest tests

Optional but recommended:
- `pytest-cov` for coverage metrics
- `pytest-json-report` for structured output

## Timeout Handling

Each repository has a 5-minute (300 seconds) timeout:
- Tests exceeding this limit are marked as "timeout"
- Prevents hanging tests from blocking the entire run
- Adjust with `--timeout` flag if needed

## Performance

**Expected execution time:** ~30 minutes for 26 repositories
- Parallel (4 workers): ~8-10 minutes
- Sequential: ~20-30 minutes
- Depends on test suite sizes

## Troubleshooting

### Issue: Repository skipped with "No pyproject.toml"
**Solution**: Ensure repository has `pyproject.toml` at root

### Issue: "No tests collected" warning
**Solution**: Add pytest tests to `tests/` directory

### Issue: Timeout errors
**Solution**: Increase timeout with `--timeout 600` or optimize slow tests

### Issue: Module import errors
**Solution**: Ensure `uv sync` has been run in the repository

### Issue: Coverage not shown
**Solution**: Install `pytest-cov` in repository: `uv add --dev pytest-cov`

## Integration with CI/CD

Add to GitHub Actions workflow:

```yaml
name: Cross-Repository Tests

on:
  schedule:
    - cron: '0 2 * * *'  # Daily at 2 AM
  workflow_dispatch:

jobs:
  cross-repo-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v1
      - name: Run cross-repository tests
        run: |
          uv run python scripts/cross_repo_test_runner.py
      - name: Upload results
        uses: actions/upload-artifact@v4
        with:
          name: cross-repo-test-results
          path: reports/cross_repo_tests/
```

## Future Enhancements

- **Historical tracking**: Store and compare results over time
- **Regression detection**: Automatically detect performance/quality regressions
- **Notification system**: Alert on failures via email/Slack
- **Incremental testing**: Only test changed repositories
- **Dependency graph**: Test dependent repositories in order
- **Performance benchmarking**: Track execution time trends

## Related Documentation

- `docs/domains/standards/FILE_ORGANIZATION_STANDARDS.md` - File organization rules
- `docs/domains/standards/TESTING_FRAMEWORK_STANDARDS.md` - Testing standards
- `docs/domains/workflow/DEVELOPMENT_WORKFLOW.md` - Development workflow
- `CLAUDE.md` - Project-specific configuration

---

**See also:** `config/cross-repo-test-config.yaml` for full configuration options
