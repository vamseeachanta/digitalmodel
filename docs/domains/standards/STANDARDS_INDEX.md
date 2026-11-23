# Cross-Repository Standards Index

**Version:** 1.0.0
**Last Updated:** 2026-01-06
**Applies To:** All 26+ repositories in workspace-hub

---

## Overview

This document defines mandatory and recommended standards enforced across all repositories in the workspace-hub ecosystem. These standards ensure consistency, maintainability, and quality across the entire codebase.

## Standard Categories

### 🔴 MANDATORY Standards (Must Pass)

#### 1. Repository Organization Pattern
**ID:** `STD-ORG-001`
**Priority:** CRITICAL
**Description:** Repositories must follow the hierarchical module structure

**Requirements:**
```
<repository-root>/
├── src/<group>/modules/<module>/
├── tests/<group>/modules/<module>/
├── docs/
├── scripts/
├── config/
├── data/
└── reports/
```

**Validation Rules:**
- `src/` directory must exist
- Module directories must follow `<group>/modules/<module>/` pattern
- No working files in repository root (except standard config files)
- Test structure mirrors source structure

**Allowed Root Files:**
- Configuration: `*.toml`, `*.yaml`, `*.json`, `.gitignore`, `.env.example`
- Documentation: `README.md`, `CHANGELOG.md`, `LICENSE`
- Build/Package: `pyproject.toml`, `package.json`, `setup.py`, `Makefile`
- CI/CD: `.github/`, `.gitlab-ci.yml`, `Jenkinsfile`

**Violations Examples:**
- ❌ Working files in root: `test.py`, `analysis.md`, `temp_data.csv`
- ❌ Flat module structure: `src/module1/`, `src/module2/`
- ❌ Missing test mirror: `src/marine/modules/orcaflex/` without `tests/marine/modules/orcaflex/`

---

#### 2. Anti-Sycophancy Protocol
**ID:** `STD-BEHAVE-001`
**Priority:** CRITICAL
**Description:** AI agents must ask 3-5 clarifying questions before implementation

**Requirements:**
- Agent must identify ambiguities in user requests
- Must ask between 3-5 questions before starting work
- Questions must cover: scope, technical choices, edge cases, integration
- Must wait for explicit user approval before proceeding

**Validation Rules:**
- `.claude/CLAUDE.md` or `CLAUDE.md` must contain anti-sycophancy instructions
- Agent configuration must disable auto-implementation
- Documentation must include question requirement

**Detection Patterns:**
```markdown
# Required in CLAUDE.md or .claude/CLAUDE.md
- "ask clarifying questions"
- "MUST ask"
- "never assume"
- "wait for confirmation"
- "explicit approval"
```

**Violations Examples:**
- ❌ No anti-sycophancy instructions in CLAUDE.md
- ❌ Instructions say "proceed automatically"
- ❌ Missing question requirement

---

#### 3. File Management Rules
**ID:** `STD-FILE-001`
**Priority:** CRITICAL
**Description:** Strict rules for file placement and user input handling

**Requirements:**
- **NO working files in repository root** (except allowed config files)
- User-provided files must use naming: `user_<descriptor>_YYYYMMDD.<ext>`
- User inputs saved to: `/inputs/user_provided/` or `/data/user_inputs/`
- All analysis/test/temp files go to appropriate subdirectories

**Validation Rules:**
- Root contains only allowed configuration files
- `/inputs/user_provided/` or `/data/user_inputs/` exists if user files present
- User files follow naming convention
- No temporary files in root (`.tmp`, `.cache`, etc.)

**Violations Examples:**
- ❌ `analysis_output.csv` in root
- ❌ `test_results.md` in root
- ❌ User file: `client_data.xlsx` instead of `user_client_data_20260106.xlsx`

---

### 🟡 RECOMMENDED Standards (Should Pass)

#### 4. HTML Reporting Requirements
**ID:** `STD-REPORT-001`
**Priority:** WARNING
**Description:** Interactive HTML reports for all analysis modules

**Requirements:**
- All visualizations must be interactive (Plotly, Bokeh, Altair, D3.js)
- NO static matplotlib PNG/SVG exports
- CSV data imported with relative paths
- Reports saved to `/reports/` directory

**Validation Rules:**
- Check for HTML report generation in code
- Verify interactive plotting library usage
- Check for matplotlib static exports (anti-pattern)
- Validate CSV relative path usage

**Violations Examples:**
- ❌ `plt.savefig('plot.png')` - static export
- ❌ Absolute paths to CSV: `C:/data/results.csv`
- ❌ No HTML report generation

---

#### 5. Test-Driven Development (TDD)
**ID:** `STD-TDD-001`
**Priority:** WARNING
**Description:** Tests written before implementation

**Requirements:**
- Test coverage >80%
- Tests exist for all modules in `src/`
- Test structure mirrors source structure
- Tests use appropriate framework (pytest, jest, etc.)

**Validation Rules:**
- `tests/` directory exists and follows structure
- Test files exist for source modules
- Coverage reports available
- Test configuration present (`pytest.ini`, `jest.config.js`)

**Violations Examples:**
- ❌ Source module without corresponding test
- ❌ Missing test configuration
- ❌ Coverage <80%

---

## Repository Compliance Matrix

| Repository | ORG-001 | BEHAVE-001 | FILE-001 | REPORT-001 | TDD-001 | Overall | Last Check |
|-----------|---------|------------|----------|------------|---------|---------|------------|
| *To be populated by validation script* ||||||||

**Legend:**
- ✅ **PASS** - Fully compliant
- ⚠️ **WARN** - Minor issues (recommended standards)
- ❌ **FAIL** - Critical violations (mandatory standards)
- ⏭️ **SKIP** - Not applicable

**Overall Score:**
- **100%** - All standards pass
- **75-99%** - Mandatory pass, some recommended fail
- **50-74%** - Some mandatory violations
- **<50%** - Multiple critical violations

---

## Enforcement Phases

### Phase 1: Report Only (Current)
- Run validation script
- Generate compliance reports
- No automated fixes
- Manual remediation

### Phase 2: Auto-Fix with Confirmation (Future)
- Propose fixes for violations
- Wait for user approval
- Apply fixes with git commits
- Re-validate after fixes

### Phase 3: Continuous Enforcement (Future)
- Pre-commit hook validation
- CI/CD pipeline checks
- Block non-compliant commits
- Automated reporting

---

## Validation Commands

```bash
# Validate all repositories
bash scripts/validate_cross_repo_standards.sh

# Validate specific repository
bash scripts/validate_cross_repo_standards.sh --repo /path/to/repo

# Generate HTML report
bash scripts/validate_cross_repo_standards.sh --format html

# Show only failures
bash scripts/validate_cross_repo_standards.sh --only-failures
```

---

## Template Files to Propagate

### `.agent-os/` Directory Structure
```
.agent-os/
├── standards/
│   └── file-management.md
├── product/
│   ├── mission.md
│   ├── tech-stack.md
│   └── roadmap.md
└── instructions/
    ├── create-spec.md
    └── execute-tasks.md
```

### `.claude/settings.json` (Hooks Configuration)
```json
{
  "hooks": {
    "enabled": true,
    "preTask": "npx claude-flow@alpha hooks pre-task",
    "postTask": "npx claude-flow@alpha hooks post-task",
    "postEdit": "npx claude-flow@alpha hooks post-edit"
  },
  "antiSycophancy": {
    "enabled": true,
    "minimumQuestions": 3,
    "maxiumQuestions": 5
  }
}
```

---

## Standard Definitions

### Repository Organization
- **Module**: A logical unit of functionality (e.g., `orcaflex`, `aqwa`)
- **Group**: A domain grouping (e.g., `marine`, `structural`, `utilities`)
- **Component**: A sub-module within a module

### File Categories
- **Source**: Production code in `src/`
- **Tests**: Test code in `tests/`
- **Documentation**: Guides in `docs/`
- **Scripts**: Automation in `scripts/`
- **Configuration**: Settings in `config/` or root
- **Data**: Inputs/outputs in `data/`
- **Reports**: Analysis results in `reports/`

### Compliance Levels
- **CRITICAL**: Must fix immediately, blocks deployments
- **WARNING**: Should fix soon, technical debt
- **INFO**: Nice to have, improvement opportunity

---

## Related Documentation

- [File Organization Standards](../modules/standards/FILE_ORGANIZATION_STANDARDS.md)
- [HTML Reporting Standards](../modules/standards/HTML_REPORTING_STANDARDS.md)
- [Testing Framework Standards](../modules/standards/TESTING_FRAMEWORK_STANDARDS.md)
- [AI Agent Guidelines](../modules/ai/AI_AGENT_GUIDELINES.md)
- [Development Workflow](../modules/workflow/DEVELOPMENT_WORKFLOW.md)

---

## Maintenance

**Document Owner:** Workspace Standards Team
**Review Cycle:** Quarterly
**Last Review:** 2026-01-06
**Next Review:** 2026-04-06

**Change Log:**
- 2026-01-06: Initial version created
- Future updates tracked here

---

## Support

For questions or violations needing clarification:
1. Check related documentation links above
2. Review repository-specific CLAUDE.md
3. Consult workspace standards team
4. Open issue in digitalmodel repository

---

**Remember:** Standards exist to improve code quality and maintainability across all 26+ repositories. Compliance is measured, not mandated (Phase 1).
