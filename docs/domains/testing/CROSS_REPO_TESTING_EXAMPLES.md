# Cross-Repository Testing Examples

Practical examples and use cases for the cross-repository testing infrastructure.

## Quick Start

### Example 1: Run All Tests with Default Settings

```bash
# Windows
scripts\run_cross_repo_tests.bat

# Linux/macOS
./scripts/run_cross_repo_tests.sh
```

**Output:**
```
========================================
Cross-Repository Test Runner
========================================

Loading configuration...
Discovering repositories...
Found 8 repositories with tests
Running tests in parallel with 4 workers...
[1/8] Completed digitalmodel - passed
[2/8] Completed assethold - passed
[3/8] Completed coordination - failed
[4/8] Completed saipem - passed
...

Generating reports...
HTML dashboard: D:\workspace-hub\digitalmodel\reports\cross_repo_tests\dashboard-20260106_123456.html
CSV results: D:\workspace-hub\digitalmodel\reports\cross_repo_tests\results-20260106_123456.csv
JSON results: D:\workspace-hub\digitalmodel\reports\cross_repo_tests\test-results-20260106_123456.json

============================================================
SUMMARY
============================================================
Total repositories tested: 8
Passed: 6
Failed: 1
Skipped: 1
Errors: 0
Average compliance score: 82.5%
```

### Example 2: Run Tests Sequentially (Debugging)

```bash
# Easier to follow output, better for debugging
uv run python scripts/cross_repo_test_runner.py --sequential
```

**When to use:**
- Investigating test failures
- First-time setup verification
- Limited system resources

### Example 3: Increase Parallelism

```bash
# Use 8 workers for faster execution
uv run python scripts/cross_repo_test_runner.py --workers 8
```

**Performance comparison:**
- 1 worker (sequential): ~25 minutes
- 4 workers (default): ~8 minutes
- 8 workers: ~5 minutes

### Example 4: Extend Timeout for Slow Tests

```bash
# Increase timeout to 10 minutes per repository
uv run python scripts/cross_repo_test_runner.py --timeout 600
```

**When to use:**
- Repositories with large test suites
- Integration tests requiring external services
- First run with cold caches

## Understanding Results

### CSV Export Structure

**File:** `reports/cross_repo_tests/results-{timestamp}.csv`

```csv
repo_name,status,tests_passed,tests_failed,tests_skipped,tests_total,duration_seconds,coverage_percent,compliance_score,module_structure_ok,file_organization_ok,claude_md_ok,agent_registry_ok,suggestion_count
digitalmodel,passed,145,0,5,150,42.5,87.5,75.0,True,False,True,True,5
assethold,passed,89,0,2,91,28.3,92.1,100.0,True,True,True,True,0
coordination,failed,32,8,1,41,55.2,68.4,87.5,True,True,True,False,3
saipem,passed,67,0,3,70,31.8,85.0,87.5,True,True,True,False,2
```

**Column explanations:**
- `status`: Test outcome (passed/failed/skipped/error/timeout)
- `tests_*`: Test count breakdowns
- `duration_seconds`: Total execution time
- `coverage_percent`: Code coverage (null if unavailable)
- `compliance_score`: Overall compliance percentage (0-100)
- `*_ok`: Boolean for each compliance check
- `suggestion_count`: Number of improvement suggestions

### JSON Results Structure

**File:** `reports/cross_repo_tests/test-results-{timestamp}.json`

```json
{
  "timestamp": "2026-01-06T12:34:56",
  "test_results": [
    {
      "repo_name": "digitalmodel",
      "status": "passed",
      "tests_passed": 145,
      "tests_failed": 0,
      "coverage_percent": 87.5,
      "warnings": []
    }
  ],
  "compliance_results": [
    {
      "repo_name": "digitalmodel",
      "overall_score": 75.0,
      "checks": {
        "module_structure": {"passed": true, "details": "..."},
        "file_organization": {"passed": false, "details": "..."}
      },
      "suggestions": [
        "Move AqwaServerLogFile.txt to appropriate directory",
        "Move claude-flow.bat to scripts/ directory"
      ]
    }
  ],
  "summary": {
    "total_repositories": 8,
    "tests_passed_repos": 6,
    "average_compliance_score": 82.5,
    "total_tests_run": 412
  }
}
```

### HTML Dashboard Visualizations

**File:** `reports/cross_repo_tests/dashboard-{timestamp}.html`

**Chart 1: Test Results by Repository (Stacked Bar)**
- Green bars: Passed tests
- Red bars: Failed tests
- Orange bars: Skipped tests
- **Interpretation:** Taller bars = more comprehensive test suites

**Chart 2: Test Execution Time (Bar)**
- Shows duration in seconds per repository
- **Interpretation:** Identify slow test suites for optimization

**Chart 3: Coverage Distribution (Box Plot)**
- Shows statistical distribution of coverage across repositories
- **Interpretation:** Median line shows typical coverage, outliers indicate repos needing attention

**Chart 4: Compliance Scores (Bar)**
- Score out of 100% for each repository
- **Interpretation:** Scores <80% need compliance improvements

**Chart 5: Test Status Summary (Pie)**
- Overall distribution of test statuses
- **Interpretation:** Large "failed" slice indicates systemic issues

**Chart 6: Top Issues by Repository (Bar)**
- Counts compliance suggestions per repository
- **Interpretation:** High bars indicate repos needing refactoring

## Common Scenarios

### Scenario 1: New Repository Setup

**Problem:** Just added a new repository to workspace-hub, want to verify it meets standards.

**Solution:**
```bash
# Test single repository for compliance
uv run python tests/cross_repo/test_standards_compliance.py /d/workspace-hub/new-repo

# View specific suggestions
cat reports/cross_repo_tests/test-results-*.json | grep -A 20 "new-repo"
```

### Scenario 2: Identify Repositories Needing Attention

**Problem:** Want to find repositories with lowest test coverage.

**Solution:**
```python
import pandas as pd

# Load latest CSV results
df = pd.read_csv('reports/cross_repo_tests/results-20260106_123456.csv')

# Sort by coverage
low_coverage = df[df['coverage_percent'].notna()].nsmallest(5, 'coverage_percent')

print("Repositories needing coverage improvements:")
print(low_coverage[['repo_name', 'coverage_percent']])
```

### Scenario 3: Pre-Commit Hook Integration

**Problem:** Want to ensure compliance before committing changes.

**Solution:**
Create `.git/hooks/pre-commit`:
```bash
#!/bin/bash
# Run compliance check on current repository
REPO_PATH=$(git rev-parse --show-toplevel)
uv run python tests/cross_repo/test_standards_compliance.py "$REPO_PATH"

if [ $? -ne 0 ]; then
    echo "Compliance check failed. Fix issues before committing."
    exit 1
fi
```

### Scenario 4: CI/CD Integration

**Problem:** Automate testing across all repositories nightly.

**Solution:**
Add to `.github/workflows/nightly-tests.yml`:
```yaml
name: Nightly Cross-Repository Tests

on:
  schedule:
    - cron: '0 2 * * *'  # 2 AM daily

jobs:
  cross-repo-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 60

    steps:
      - uses: actions/checkout@v4
        with:
          repository: 'workspace-hub/digitalmodel'

      - uses: astral-sh/setup-uv@v1

      - name: Run cross-repository tests
        run: |
          uv run python scripts/cross_repo_test_runner.py --workers 4

      - name: Upload HTML dashboard
        uses: actions/upload-artifact@v4
        with:
          name: test-dashboard
          path: reports/cross_repo_tests/dashboard-*.html

      - name: Comment on PR if failures
        if: failure()
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '❌ Cross-repository tests failed. Check artifacts for details.'
            })
```

### Scenario 5: Track Improvements Over Time

**Problem:** Want to see if compliance scores are improving.

**Solution:**
```python
import pandas as pd
from pathlib import Path
import plotly.graph_objects as go

# Load all historical CSV files
results_dir = Path('reports/cross_repo_tests')
csv_files = sorted(results_dir.glob('results-*.csv'))

history = []
for csv_file in csv_files:
    df = pd.read_csv(csv_file)
    timestamp = csv_file.stem.split('-', 1)[1]
    avg_compliance = df['compliance_score'].mean()
    history.append({'timestamp': timestamp, 'avg_compliance': avg_compliance})

df_history = pd.DataFrame(history)

# Plot trend
fig = go.Figure()
fig.add_trace(go.Scatter(
    x=df_history['timestamp'],
    y=df_history['avg_compliance'],
    mode='lines+markers',
    name='Average Compliance'
))
fig.update_layout(
    title='Compliance Score Trend',
    xaxis_title='Date',
    yaxis_title='Average Compliance (%)'
)
fig.show()
```

## Interpreting Compliance Suggestions

### Module Structure Issues

**Suggestion:** "Create modules/ directory in: digitalmodel/"

**Action:**
```bash
mkdir -p src/digitalmodel/modules
# Move domain logic into modules
```

### File Organization Issues

**Suggestion:** "Move claude-flow.bat to appropriate directory"

**Action:**
```bash
# Move to scripts directory
git mv claude-flow.bat scripts/
git commit -m "fix: Move claude-flow.bat to scripts directory"
```

### CLAUDE.md Issues

**Suggestion:** "Add '## Interactive Engagement' section to CLAUDE.md"

**Action:**
Add to CLAUDE.md:
```markdown
## Interactive Engagement (MANDATORY)

**YOU MUST ask clarifying questions BEFORE starting any task.**

### Required Question Pattern
[See digitalmodel/CLAUDE.md for full template]
```

### Agent Registry Issues

**Suggestion:** "Create agent registry at one of: .claude/agents-registry.json"

**Action:**
```bash
mkdir -p .claude
cat > .claude/agents-registry.json << 'EOF'
{
  "agents": [
    {
      "name": "coder",
      "type": "development",
      "description": "Primary coding agent"
    }
  ]
}
EOF
```

## Performance Optimization

### Slow Test Suites

**Identify slow repositories:**
```bash
# View CSV sorted by duration
uv run python -c "
import pandas as pd
df = pd.read_csv('reports/cross_repo_tests/results-*.csv')
print(df.nlargest(5, 'duration_seconds')[['repo_name', 'duration_seconds']])
"
```

**Common causes:**
1. **Large integration tests** - Consider splitting into unit tests
2. **External API calls** - Mock external dependencies
3. **Database setup** - Use in-memory databases (SQLite)
4. **File I/O** - Use temporary directories

### Memory Issues

**Symptoms:**
- Tests timing out
- "Out of memory" errors
- System slowdown

**Solutions:**
```bash
# Reduce parallel workers
uv run python scripts/cross_repo_test_runner.py --workers 2

# Run sequentially
uv run python scripts/cross_repo_test_runner.py --sequential
```

## Troubleshooting

### Issue: "No repositories found"

**Diagnosis:**
```bash
# Check scan path exists
ls -la D:/workspace-hub/

# Verify repositories have required files
find D:/workspace-hub/ -name "pyproject.toml"
find D:/workspace-hub/ -name "tests" -type d
```

**Fix:** Ensure repositories have both `pyproject.toml` and `tests/` directory.

### Issue: "Module import errors"

**Example error:** `ModuleNotFoundError: No module named 'digitalmodel'`

**Fix:**
```bash
# Sync uv environment in failing repository
cd /d/workspace-hub/failing-repo
uv sync
```

### Issue: Coverage always null

**Diagnosis:**
```bash
# Check if pytest-cov is installed
cd /d/workspace-hub/repo-name
uv pip list | grep pytest-cov
```

**Fix:**
```bash
# Add pytest-cov to dev dependencies
uv add --dev pytest-cov
```

## Best Practices

### 1. Run Regularly
```bash
# Add to daily routine
crontab -e
0 9 * * * cd /workspace-hub/digitalmodel && uv run python scripts/cross_repo_test_runner.py
```

### 2. Review Compliance Suggestions
- Address suggestions incrementally
- Prioritize high-impact issues (file organization)
- Update CLAUDE.md templates across repositories

### 3. Monitor Trends
- Track average compliance score weekly
- Set targets: >90% compliance, >80% coverage
- Celebrate improvements!

### 4. Integrate with Development Workflow
- Run before major releases
- Include in PR review process
- Share dashboard with team

---

**See also:**
- `docs/CROSS_REPO_TESTING.md` - Full documentation
- `config/cross-repo-test-config.yaml` - Configuration reference
