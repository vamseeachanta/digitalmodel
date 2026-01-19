# Cross-Repository Standards Enforcement Guide

**Version:** 1.0.0
**Last Updated:** 2026-01-06

---

## Overview

This guide explains how to use the cross-repository standards enforcement system to maintain consistency across all 26+ repositories in the workspace-hub ecosystem.

## System Components

### 1. Standards Index
**Location:** `docs/standards/STANDARDS_INDEX.md`

Comprehensive catalog of all mandatory and recommended standards:
- **STD-ORG-001**: Repository Organization Pattern (MANDATORY)
- **STD-BEHAVE-001**: Anti-Sycophancy Protocol (MANDATORY)
- **STD-FILE-001**: File Management Rules (MANDATORY)
- **STD-REPORT-001**: HTML Reporting Requirements (RECOMMENDED)
- **STD-TDD-001**: Test-Driven Development (RECOMMENDED)

### 2. Validation Script
**Location:** `scripts/validate_cross_repo_standards.sh`

Bash script that scans all repositories and validates compliance against standards.

**Features:**
- Automatic repository discovery in workspace-hub
- Validates 5 standards across all repositories
- Generates markdown compliance reports
- Provides detailed violation information
- Calculates overall compliance scores

### 3. Enhanced Sync Script
**Location:** `scripts/repository_sync_enhanced.sh`

Enhanced synchronization script that propagates templates and enforces standards.

**Features:**
- Pre-sync validation option
- Template synchronization (.agent-os, .claude/settings.json)
- Standards documentation propagation
- Automatic git commits
- Dry-run mode for safe testing
- Force mode for overwriting existing files

---

## Quick Start

### Validate All Repositories

```bash
# Run validation on all repositories
bash scripts/validate_cross_repo_standards.sh

# Output will show:
# - Real-time validation progress
# - Compliance scores per repository
# - Detailed violation information
# - Final compliance report location
```

**Example Output:**
```
[INFO] Cross-Repository Standards Validation
======================================

[INFO] Scanning for repositories in: /d/workspace-hub
[PASS] Found 26 repositories

[INFO] Validating 26 repositories...

[INFO] Validating: digitalmodel
[PASS] digitalmodel: 100%

[INFO] Validating: orcaflex-tools
[WARN] orcaflex-tools: 85%
  REPORT-001: No interactive plotting libraries found (plotly, bokeh, altair, d3.js)

[INFO] Validating: aqwa-analysis
[FAIL] aqwa-analysis: 60%
  BEHAVE-001: No anti-sycophancy instructions found in CLAUDE.md files
  FILE-001: Working files in root: analysis.py temp.csv

...

============================================
VALIDATION COMPLETE
============================================
Total Repositories: 26
✅ Fully Compliant: 18
⚠️  Partial Compliance: 6
❌ Non-Compliant: 2
Compliance Rate: 69%

Full report: /d/workspace-hub/digitalmodel/reports/standards_compliance_20260106_120530.md
============================================
```

### Sync Templates Across Repositories

```bash
# Standard sync with validation
bash scripts/repository_sync_enhanced.sh

# Dry run (see what would change)
bash scripts/repository_sync_enhanced.sh --dry-run

# Force overwrite existing files
bash scripts/repository_sync_enhanced.sh --force

# Skip validation (faster)
bash scripts/repository_sync_enhanced.sh --no-validate
```

**What Gets Synced:**
- `.agent-os/` directory structure
- `.claude/settings.json` hooks configuration
- `docs/standards/STANDARDS_INDEX.md`
- `scripts/validate_cross_repo_standards.sh`

---

## Detailed Usage

### Validation Script

#### Command Options

```bash
# Basic validation
bash scripts/validate_cross_repo_standards.sh

# Custom workspace location
WORKSPACE_ROOT=/custom/path bash scripts/validate_cross_repo_standards.sh

# Custom report location
REPORT_DIR=/custom/reports bash scripts/validate_cross_repo_standards.sh
```

#### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `WORKSPACE_ROOT` | `/d/workspace-hub` | Root directory to scan for repositories |
| `REPORT_DIR` | `/d/workspace-hub/digitalmodel/reports` | Where to save compliance reports |

#### Output Files

**Compliance Report:** `reports/standards_compliance_YYYYMMDD_HHMMSS.md`

Contains:
- Summary statistics
- Detailed compliance matrix table
- Violation details per repository
- Next steps for remediation

**Example Report:**
```markdown
# Cross-Repository Standards Compliance Report

**Generated:** 2026-01-06 12:05:30
**Workspace:** /d/workspace-hub
**Total Repositories:** 26

---

## Summary

**Fully Compliant (✅):** 18 repositories
**Partial Compliance (⚠️):** 6 repositories
**Non-Compliant (❌):** 2 repositories

**Compliance Rate:** 69%

---

## Detailed Compliance Matrix

| Repository | ORG-001 | BEHAVE-001 | FILE-001 | REPORT-001 | TDD-001 | Overall | Status |
|-----------|---------|------------|----------|------------|---------|---------|--------|
| digitalmodel | PASS | PASS | PASS | PASS | PASS | 100% | ✅ |
| orcaflex-tools | PASS | PASS | PASS | WARN | PASS | 85% | ⚠️ |
| aqwa-analysis | PASS | FAIL | FAIL | WARN | PASS | 60% | ❌ |
...
```

---

### Enhanced Sync Script

#### Command Options

```bash
# Standard sync with pre-validation
bash scripts/repository_sync_enhanced.sh

# Dry run mode (no changes made)
bash scripts/repository_sync_enhanced.sh --dry-run

# Force overwrite existing files
bash scripts/repository_sync_enhanced.sh --force

# Skip validation step
bash scripts/repository_sync_enhanced.sh --no-validate

# Show help
bash scripts/repository_sync_enhanced.sh --help
```

#### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `WORKSPACE_ROOT` | `/d/workspace-hub` | Root directory to scan for repositories |
| `VALIDATE_FIRST` | `true` | Run validation before sync |
| `DRY_RUN` | `false` | Show what would be synced without making changes |
| `FORCE` | `false` | Overwrite existing files (default: preserve) |

#### Sync Behavior

**Default Mode (Preserve Existing):**
- Creates directories if missing
- Copies files only if they don't exist
- Preserves local customizations
- Safe for production use

**Force Mode (`--force`):**
- Overwrites all files
- Deletes extraneous files in .agent-os/
- Use with caution
- Useful for fixing broken configurations

**Dry Run Mode (`--dry-run`):**
- Shows what would be changed
- No files modified
- No git commits
- Safe for testing

#### Output Files

**Sync Log:** `logs/sync_YYYYMMDD_HHMMSS.log`

Contains:
- Detailed sync operations
- File copy operations
- Git commit messages
- Error messages
- Summary statistics

---

## Standard Remediation Guide

### STD-ORG-001: Repository Organization

**Violation:** Missing `src/` directory

**Fix:**
```bash
cd /path/to/repository
mkdir -p src/marine/modules
mkdir -p tests/marine/modules
```

**Violation:** Working files in root

**Fix:**
```bash
# Move working files to appropriate directories
mv analysis.py src/marine/modules/analysis/
mv test_results.md docs/analysis/
mv data.csv data/raw/
```

---

### STD-BEHAVE-001: Anti-Sycophancy Protocol

**Violation:** No anti-sycophancy instructions

**Fix:**
Add to `CLAUDE.md` or `.claude/CLAUDE.md`:

```markdown
## Interactive Engagement (MANDATORY)

**YOU MUST ask clarifying questions BEFORE starting any task.**

### Required Question Pattern

Before implementing ANY request, you MUST:
1. **Understand the requirement** - Ask about goals, constraints, and context
2. **Clarify ambiguities** - Identify and ask about unclear aspects
3. **Propose approach** - Describe your planned implementation strategy
4. **Wait for confirmation** - Get explicit approval before proceeding
5. **Ask follow-ups** - Continue dialogue as implementation reveals questions

### Never Assume

- **Never guess** at requirements
- **Never implement** without explicit approval
- **Never skip** the question phase
- **Always engage** in dialogue before coding
```

---

### STD-FILE-001: File Management

**Violation:** User files in root without proper naming

**Fix:**
```bash
# Create user input directory
mkdir -p inputs/user_provided

# Rename and move user files
mv client_data.xlsx inputs/user_provided/user_client_data_20260106.xlsx
mv survey.csv inputs/user_provided/user_survey_20260106.csv
```

---

### STD-REPORT-001: HTML Reporting

**Violation:** Using matplotlib static exports

**Fix:**

**Before:**
```python
import matplotlib.pyplot as plt

plt.plot(x, y)
plt.savefig('plot.png')  # ❌ Static export
```

**After:**
```python
import plotly.graph_objects as go

fig = go.Figure(data=go.Scatter(x=x, y=y))
fig.write_html('reports/analysis.html')  # ✅ Interactive HTML
```

---

### STD-TDD-001: Test-Driven Development

**Violation:** Missing test configuration

**Fix for Python:**
```bash
# Create pytest.ini
cat > pytest.ini <<EOF
[pytest]
testpaths = tests
python_files = test_*.py
python_classes = Test*
python_functions = test_*
addopts = --cov=src --cov-report=html --cov-report=term
EOF
```

**Fix for JavaScript:**
```bash
# Create jest.config.js
cat > jest.config.js <<EOF
module.exports = {
  testEnvironment: 'node',
  coverageDirectory: 'coverage',
  collectCoverageFrom: ['src/**/*.js'],
  testMatch: ['**/__tests__/**/*.js', '**/?(*.)+(spec|test).js'],
};
EOF
```

---

## Workflows

### Initial Repository Setup

1. **Validate current state:**
   ```bash
   bash scripts/validate_cross_repo_standards.sh
   ```

2. **Review compliance report:**
   ```bash
   cat reports/standards_compliance_*.md
   ```

3. **Sync templates (dry run first):**
   ```bash
   bash scripts/repository_sync_enhanced.sh --dry-run
   ```

4. **Apply sync:**
   ```bash
   bash scripts/repository_sync_enhanced.sh
   ```

5. **Fix violations manually**

6. **Re-validate:**
   ```bash
   bash scripts/validate_cross_repo_standards.sh
   ```

---

### Continuous Maintenance

**Weekly:**
```bash
# Validate compliance
bash scripts/validate_cross_repo_standards.sh

# Review report for new violations
cat reports/standards_compliance_*.md
```

**Monthly:**
```bash
# Sync updated templates
bash scripts/repository_sync_enhanced.sh --dry-run
bash scripts/repository_sync_enhanced.sh

# Re-validate
bash scripts/validate_cross_repo_standards.sh
```

**After Template Updates:**
```bash
# Force sync to all repositories
bash scripts/repository_sync_enhanced.sh --force

# Validate
bash scripts/validate_cross_repo_standards.sh
```

---

### Fixing Non-Compliant Repositories

**Workflow:**
1. Identify violations from report
2. Fix mandatory violations (ORG, BEHAVE, FILE) first
3. Address recommended violations (REPORT, TDD) second
4. Re-validate after each fix
5. Commit fixes with descriptive messages

**Example:**
```bash
# Fix repository "aqwa-analysis"
cd /d/workspace-hub/aqwa-analysis

# Fix ORG-001: Create proper structure
mkdir -p src/marine/modules/aqwa
mkdir -p tests/marine/modules/aqwa
mv *.py src/marine/modules/aqwa/

# Fix BEHAVE-001: Add anti-sycophancy instructions
cat >> CLAUDE.md <<EOF

## Anti-Sycophancy (MANDATORY)
- MUST ask 3-5 clarifying questions before implementation
- Never assume requirements
- Wait for explicit approval
EOF

# Fix FILE-001: Move working files
mkdir -p inputs/user_provided
mv data.csv inputs/user_provided/user_data_20260106.csv

# Commit fixes
git add .
git commit -m "fix: Enforce standards compliance

- Add proper src/tests directory structure
- Add anti-sycophancy instructions to CLAUDE.md
- Move user files to inputs/user_provided/

Fixes STD-ORG-001, STD-BEHAVE-001, STD-FILE-001"

# Re-validate
cd /d/workspace-hub/digitalmodel
bash scripts/validate_cross_repo_standards.sh
```

---

## Integration with Existing Tools

### With Repository Sync
```bash
# Existing repository_sync tool
bash scripts/repository_sync pull all

# Then run enhanced sync
bash scripts/repository_sync_enhanced.sh
```

### With Pre-Commit Hooks
```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: validate-standards
        name: Validate Repository Standards
        entry: bash scripts/validate_cross_repo_standards.sh
        language: system
        pass_filenames: false
```

### With CI/CD
```yaml
# .github/workflows/standards-check.yml
name: Standards Compliance Check

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Validate Standards
        run: bash scripts/validate_cross_repo_standards.sh
```

---

## Troubleshooting

### Common Issues

**Issue:** Validation script not finding repositories

**Solution:**
```bash
# Check WORKSPACE_ROOT is correct
echo $WORKSPACE_ROOT

# Set custom workspace
WORKSPACE_ROOT=/custom/path bash scripts/validate_cross_repo_standards.sh
```

---

**Issue:** Sync script not committing changes

**Solution:**
```bash
# Check git configuration in target repository
cd /path/to/repository
git config user.name
git config user.email

# Set if missing
git config user.name "Your Name"
git config user.email "your.email@example.com"
```

---

**Issue:** Force mode not overwriting files

**Solution:**
```bash
# Check file permissions
ls -la /path/to/repository/.claude/settings.json

# Make writable
chmod u+w /path/to/repository/.claude/settings.json

# Re-run with force
bash scripts/repository_sync_enhanced.sh --force
```

---

**Issue:** Validation reports too large

**Solution:**
```bash
# Archive old reports
cd reports
mkdir archive
mv standards_compliance_*.md archive/

# Keep only last 10 reports
ls -t archive/standards_compliance_*.md | tail -n +11 | xargs rm
```

---

## FAQ

**Q: What happens if I run sync on a repository with local customizations?**

A: By default, sync preserves existing files. Use `--force` only when you want to overwrite local changes.

---

**Q: Can I exclude specific repositories from validation?**

A: Currently no built-in exclusion. You can modify the script or move repositories outside workspace-hub temporarily.

---

**Q: How do I add a new standard?**

A:
1. Document in `STANDARDS_INDEX.md`
2. Add validation function to `validate_cross_repo_standards.sh`
3. Update table generation to include new standard
4. Test on pilot repository first

---

**Q: Can I sync only specific templates?**

A: Modify `repository_sync_enhanced.sh` and comment out unwanted sync functions in `sync_repository()`.

---

**Q: How long does validation take for 26 repositories?**

A: Approximately 30-60 seconds depending on repository sizes and disk I/O.

---

## Future Enhancements

**Phase 2 (Planned):**
- Auto-fix mode with confirmation
- HTML dashboard reports
- Specific repository filtering
- Custom standard definitions
- Integration with AI agent workflows

**Phase 3 (Planned):**
- Real-time validation (file watchers)
- CI/CD integration templates
- Automated PR creation for fixes
- Standards evolution tracking
- Cross-repository dependency mapping

---

## Support

For issues or questions:
1. Check this guide first
2. Review `STANDARDS_INDEX.md` for standard definitions
3. Examine script logs in `logs/` directory
4. Open issue in digitalmodel repository

---

**Document Owner:** Workspace Standards Team
**Last Updated:** 2026-01-06
**Next Review:** 2026-02-06
