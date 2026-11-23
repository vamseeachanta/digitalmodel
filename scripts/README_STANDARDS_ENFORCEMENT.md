# Standards Enforcement Scripts

Quick reference for cross-repository standards enforcement tools.

## Scripts

### 1. validate_cross_repo_standards.sh
Validates all repositories against mandatory and recommended standards.

```bash
# Basic usage
bash scripts/validate_cross_repo_standards.sh

# Custom workspace
WORKSPACE_ROOT=/custom/path bash scripts/validate_cross_repo_standards.sh
```

**Output:** Compliance report in `reports/standards_compliance_YYYYMMDD_HHMMSS.md`

---

### 2. repository_sync_enhanced.sh
Syncs templates and enforces standards across all repositories.

```bash
# Standard sync with validation
bash scripts/repository_sync_enhanced.sh

# Dry run (safe preview)
bash scripts/repository_sync_enhanced.sh --dry-run

# Force overwrite
bash scripts/repository_sync_enhanced.sh --force

# Skip validation
bash scripts/repository_sync_enhanced.sh --no-validate
```

**Output:** Sync log in `logs/sync_YYYYMMDD_HHMMSS.log`

---

## Quick Workflow

```bash
# 1. Validate current state
bash scripts/validate_cross_repo_standards.sh

# 2. Review report
cat reports/standards_compliance_*.md | tail -n 50

# 3. Preview sync changes
bash scripts/repository_sync_enhanced.sh --dry-run

# 4. Apply sync
bash scripts/repository_sync_enhanced.sh

# 5. Re-validate
bash scripts/validate_cross_repo_standards.sh
```

---

## Standards Checked

- **STD-ORG-001**: Repository Organization (MANDATORY)
- **STD-BEHAVE-001**: Anti-Sycophancy Protocol (MANDATORY)
- **STD-FILE-001**: File Management Rules (MANDATORY)
- **STD-REPORT-001**: HTML Reporting (RECOMMENDED)
- **STD-TDD-001**: Test-Driven Development (RECOMMENDED)

---

## Full Documentation

See `docs/standards/CROSS_REPO_ENFORCEMENT_GUIDE.md` for complete usage guide.

---

**Last Updated:** 2026-01-06
