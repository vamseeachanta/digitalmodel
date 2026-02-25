# Cross-Repository Change Propagation

**ABOUTME:** Documentation for the cross-repository change propagation system that synchronizes configuration files, skills, and standards across all 26+ repositories in workspace-hub.

## Overview

The cross-repository change propagation tool automates the distribution of shared configuration files, agent skills, and standards across multiple repositories while maintaining safety and allowing repository-specific customization.

## Key Features

- ✅ **Global Propagation**: Automatically sync configuration files to all active repositories
- ✅ **Conditional Propagation**: Smart distribution based on repository tags (marine, engineering, CAD, etc.)
- ✅ **Template Merging**: Preserve repository-specific sections while updating workspace-managed content
- ✅ **Safety Checks**: Create backup commits and propagation branches before changes
- ✅ **Conflict Detection**: Skip files with local modifications and log conflicts
- ✅ **Dry-Run Mode**: Preview changes before applying them
- ✅ **Interactive Mode**: Confirm changes per repository
- ✅ **Detailed Reporting**: Generate comprehensive reports of propagation results

## Architecture

### Components

1. **`cross_repo_change_propagator.py`**: Main propagation tool
2. **`.claude/propagation-rules.yaml`**: Configuration defining propagation rules
3. **Repository Tags**: Categorize repositories for conditional propagation

### Propagation Flow

```
┌─────────────────────────────────────────────────────────┐
│ 1. Load propagation-rules.yaml configuration           │
└────────────────┬────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────┐
│ 2. Discover active repositories in workspace-hub       │
└────────────────┬────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────┐
│ 3. For each repository:                                │
│    - Check if file should propagate (global/conditional)│
│    - Check for conflicts (local modifications)         │
│    - Create backup commit                              │
│    - Create propagation branch                         │
│    - Copy or merge file                                │
│    - Commit changes                                    │
└────────────────┬────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────┐
│ 4. Generate detailed propagation report                │
└─────────────────────────────────────────────────────────┘
```

## Configuration

### Propagation Rules (`propagation-rules.yaml`)

#### Global Propagation

Files that propagate to **all active repositories**:

```yaml
propagation_rules:
  global:
    - .claude/settings.json  # Hook configurations
    - .agent-os/standards/*.md  # All standards files
```

**Note:** `CLAUDE.md` is **not** globally propagated because it contains project-specific configuration.

#### Conditional Propagation

Files that propagate based on repository tags:

```yaml
conditional:
  # Marine engineering skills
  - path: .claude/skills/orcaflex-*
    condition: marine
    description: "OrcaFlex-related skills for marine engineering repos"

  # Engineering analysis skills
  - path: .claude/skills/fatigue-*
    condition: engineering
    description: "Fatigue analysis skills"

  # CAD skills
  - path: .claude/skills/freecad-*
    condition: cad
    description: "FreeCAD automation skills"
```

#### Template Merging (CLAUDE.md)

For files like `CLAUDE.md` that need to merge workspace-managed sections with repository-specific content:

```yaml
template_merge:
  .claude/CLAUDE.md:
    enabled: true
    managed_sections:
      - "## Interactive Engagement (MANDATORY)"
      - "## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT"
    preserve_sections:
      - "## Project Overview"
      - "## Custom Rules"
    merge_strategy: section_markers
    markers:
      managed_start: "# WORKSPACE-MANAGED"
      managed_end: "# END WORKSPACE-MANAGED"
      repo_specific_start: "# REPO-SPECIFIC"
      repo_specific_end: "# END REPO-SPECIFIC"
```

**CLAUDE.md Structure Example:**

```markdown
# Claude Code Configuration - My Project

# WORKSPACE-MANAGED
## Interactive Engagement (MANDATORY)
[Workspace-managed content here...]

## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT
[Workspace-managed content here...]
# END WORKSPACE-MANAGED

# REPO-SPECIFIC
## Project Overview
This project does XYZ...

## Custom Rules
- Project-specific rule 1
- Project-specific rule 2
# END REPO-SPECIFIC
```

#### Repository Tags

Define tags for each repository to enable conditional propagation:

```yaml
repository_discovery:
  repository_tags:
    digitalmodel: [marine, engineering, cad, meshing, signal-processing, active]
    my-marine-project: [marine, active]
    my-cad-tool: [cad, active]
    archived-project: [marine]  # No 'active' tag = skipped
```

**Available Tags:**
- `active`: Repository is active (required for propagation)
- `marine`: Marine engineering projects
- `engineering`: Structural/mechanical engineering
- `cad`: CAD-related projects
- `meshing`: Meshing and FEM
- `signal-processing`: Signal analysis projects

### Safety Configuration

```yaml
safety:
  create_backup: true  # Create backup commit before changes
  backup_branch_prefix: "propagation"

  require_approval: true  # Require approval after dry-run

  create_branch: true  # Create propagation branch
  branch_prefix: "propagation"
  branch_format: "{prefix}/{date}"  # e.g., propagation/20260107

  on_conflict: skip  # skip, overwrite, merge
  log_conflicts: true
```

## Usage

### Installation

The propagation tool is included in the workspace-hub repository. Ensure you have the uv environment activated:

```bash
cd D:/workspace-hub/digitalmodel
.venv/Scripts/activate  # Windows
# or
source .venv/bin/activate  # Linux/Mac
```

### Basic Commands

#### 1. Dry Run (Default)

Preview what would be propagated without making changes:

```bash
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --dry-run
```

**Output:**
```
🔍 DRY RUN MODE - No changes will be made

📋 Propagating: .claude/settings.json

📦 Processing my-marine-project
   Reason: Global pattern: .claude/settings.json
  🔍 DRY RUN: Would propagate to D:/workspace-hub/my-marine-project/.claude/settings.json
  ✅ Successfully propagated to my-marine-project
```

#### 2. Interactive Mode

Show diffs and ask for confirmation per repository:

```bash
python scripts/cross_repo_change_propagator.py \
  --file .claude/skills/orcaflex-modeling/SKILL.md \
  --interactive
```

**Output:**
```
📦 Processing my-marine-project
   Reason: Conditional: OrcaFlex-related skills for marine engineering repos

  📄 Current file exists. Showing diff:

  --- .claude/skills/orcaflex-modeling/SKILL.md
  +++ .claude/skills/orcaflex-modeling/SKILL.md
  @@ -10,6 +10,8 @@
   - Static analysis
   - Dynamic simulations
  +- Mooring analysis
  +- Batch processing

  Apply changes to my-marine-project? [y/N]:
```

#### 3. Auto-Propagate

Automatically apply changes with safety checks:

```bash
python scripts/cross_repo_change_propagator.py \
  --file .agent-os/standards/FILE_ORGANIZATION_STANDARDS.md \
  --auto \
  --create-branch
```

**This will:**
1. Create backup commit (current HEAD)
2. Create `propagation/20260107` branch
3. Copy/merge files
4. Commit changes with descriptive message
5. Generate detailed report

#### 4. Target Specific Repositories

Propagate to specific repositories only:

```bash
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --repos repo1 repo2 repo3 \
  --auto
```

### Advanced Usage

#### Propagate Multiple Files

Use a shell loop to propagate multiple files:

```bash
# Bash/Git Bash
for file in .agent-os/standards/*.md; do
  python scripts/cross_repo_change_propagator.py --file "$file" --auto
done
```

```powershell
# PowerShell
Get-ChildItem .agent-os/standards/*.md | ForEach-Object {
  python scripts/cross_repo_change_propagator.py --file $_.FullName.Replace("D:\workspace-hub\digitalmodel\", "") --auto
}
```

#### Custom Configuration Path

```bash
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --config /path/to/custom-rules.yaml \
  --workspace /path/to/workspace \
  --auto
```

## Propagation Reports

After each propagation, a detailed Markdown report is generated in `reports/propagation/`.

### Report Structure

```markdown
# Propagation Report

**Propagation ID:** 20260107_143022
**Date:** 2026-01-07 14:30:22

## Summary

- **Total repositories processed:** 8
- **Successfully updated:** 6
- **Skipped:** 2
- **Conflicts detected:** 0

## ✅ Successfully Updated (6)

### my-marine-project

- **Files changed:** .claude/settings.json
- **Branch:** `propagation/20260107`
- **Commit:** `a1b2c3d4`
- **Backup commit:** `e5f6g7h8`

## ⏭️  Skipped Repositories (2)

### my-cad-tool

- **Reason:** Tag mismatch: requires 'marine', has ['cad', 'active']
```

## Workflow Integration

### 1. Update a Standard or Configuration

```bash
# Edit the file
vim .agent-os/standards/HTML_REPORTING_STANDARDS.md

# Commit locally
git add .agent-os/standards/HTML_REPORTING_STANDARDS.md
git commit -m "docs(standards): Update HTML reporting requirements"

# Dry-run to see impact
python scripts/cross_repo_change_propagator.py \
  --file .agent-os/standards/HTML_REPORTING_STANDARDS.md \
  --dry-run

# Review output, then propagate
python scripts/cross_repo_change_propagator.py \
  --file .agent-os/standards/HTML_REPORTING_STANDARDS.md \
  --auto \
  --create-branch
```

### 2. Review Propagation Branches

After propagation, review the branches in each repository:

```bash
cd ../my-marine-project
git checkout propagation/20260107
git diff main
git log -1

# If good, merge
git checkout main
git merge propagation/20260107
git push origin main

# Delete propagation branch
git branch -d propagation/20260107
```

### 3. Bulk Merge Script

Create a helper script to merge propagation branches across all repos:

```bash
#!/bin/bash
# scripts/merge_propagation_branches.sh

BRANCH_NAME="propagation/20260107"

for repo in ../*/; do
  cd "$repo"
  if git rev-parse --verify "$BRANCH_NAME" >/dev/null 2>&1; then
    echo "Merging $BRANCH_NAME in $repo"
    git checkout main
    git merge "$BRANCH_NAME"
    git branch -d "$BRANCH_NAME"
  fi
  cd -
done
```

## Safety Features

### 1. Backup Commits

Before any changes, the tool records the current HEAD commit hash:

```
✓ Backup commit: e5f6g7h8
```

To revert a repository:

```bash
git reset --hard e5f6g7h8
```

### 2. Propagation Branches

Changes are isolated in dedicated branches:

```
✓ Created branch: propagation/20260107
```

This allows review before merging into main.

### 3. Conflict Detection

If a file has local modifications, propagation is skipped:

```
⚠️  Conflict: File has local modifications
```

Manually resolve conflicts before re-running propagation.

### 4. Dry-Run Mode

Always preview changes first:

```bash
python scripts/cross_repo_change_propagator.py --file <path> --dry-run
```

## Common Scenarios

### Scenario 1: Update All Repositories with New Hook Configuration

```bash
# Edit hook settings
vim .claude/settings.json

# Propagate to all active repos
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --auto \
  --create-branch

# Review report
cat reports/propagation/propagation_*.md

# Merge branches in each repo (or use bulk script)
```

### Scenario 2: Add New Agent Skill for Marine Projects

```bash
# Create new skill
mkdir -p .claude/skills/my-new-marine-skill
vim .claude/skills/my-new-marine-skill/SKILL.md

# Add to propagation rules
vim .claude/propagation-rules.yaml
# Add:
#   - path: .claude/skills/my-new-marine-skill
#     condition: marine
#     description: "My new marine skill"

# Propagate
python scripts/cross_repo_change_propagator.py \
  --file .claude/skills/my-new-marine-skill/SKILL.md \
  --auto \
  --create-branch
```

### Scenario 3: Update CLAUDE.md with New Workspace Rules

```bash
# Edit CLAUDE.md (in WORKSPACE-MANAGED section)
vim CLAUDE.md

# Propagate (will merge with repo-specific sections)
python scripts/cross_repo_change_propagator.py \
  --file CLAUDE.md \
  --interactive  # Use interactive to review merges

# Review each repository's merged CLAUDE.md
```

### Scenario 4: Rollback Propagation

```bash
# If propagation was a mistake, use backup commits
# From propagation report, get backup commit hash

cd ../my-marine-project
git reset --hard e5f6g7h8  # Backup commit hash

# Or delete propagation branch without merging
git branch -D propagation/20260107
```

## Best Practices

### 1. Always Dry-Run First

```bash
python scripts/cross_repo_change_propagator.py --file <path> --dry-run
```

### 2. Use Interactive Mode for Template Merges

When propagating files with template merge (like `CLAUDE.md`), use interactive mode:

```bash
python scripts/cross_repo_change_propagator.py --file CLAUDE.md --interactive
```

### 3. Tag Repositories Correctly

Ensure repository tags are accurate in `propagation-rules.yaml`:

```yaml
repository_tags:
  my-marine-project: [marine, engineering, active]
  my-archived-project: [marine, engineering]  # No 'active' = won't propagate
```

### 4. Review Reports

Always check propagation reports after completion:

```bash
cat reports/propagation/propagation_*.md
```

### 5. Use Branches for Safety

Always use `--create-branch` for automatic propagation:

```bash
python scripts/cross_repo_change_propagator.py --file <path> --auto --create-branch
```

### 6. Test on One Repository First

Before bulk propagation, test on a single repository:

```bash
python scripts/cross_repo_change_propagator.py \
  --file <path> \
  --repos test-repo \
  --interactive
```

### 7. Document Propagation Rules

When adding new conditional rules, document them clearly:

```yaml
conditional:
  - path: .claude/skills/my-skill
    condition: my-tag
    description: "Clear description of when this propagates"
```

## Troubleshooting

### Issue: "Configuration file not found"

**Solution:** Run from workspace root or specify `--config` path:

```bash
cd D:/workspace-hub/digitalmodel
python scripts/cross_repo_change_propagator.py --file <path>
```

### Issue: "Could not find workspace root"

**Solution:** Specify workspace path explicitly:

```bash
python scripts/cross_repo_change_propagator.py \
  --file <path> \
  --workspace D:/workspace-hub/digitalmodel
```

### Issue: "File has local modifications"

**Solution:** Commit or stash changes in target repository:

```bash
cd ../target-repo
git add .
git commit -m "Save local changes"
# or
git stash
```

### Issue: "Failed to create branch"

**Solution:** Ensure repository is in clean state:

```bash
cd ../target-repo
git status  # Should be clean
git checkout main  # Ensure on main branch
```

### Issue: Template merge not working

**Solution:** Ensure CLAUDE.md has proper section markers:

```markdown
# WORKSPACE-MANAGED
... workspace content ...
# END WORKSPACE-MANAGED

# REPO-SPECIFIC
... repo-specific content ...
# END REPO-SPECIFIC
```

## File Organization

```
workspace-hub/digitalmodel/
├── .claude/
│   ├── propagation-rules.yaml          # Propagation configuration
│   ├── settings.json                   # Hook settings (propagates globally)
│   └── skills/                         # Agent skills (conditional propagation)
├── .agent-os/
│   └── standards/                      # Standards (propagate globally)
├── scripts/
│   └── cross_repo_change_propagator.py # Propagation tool
├── reports/
│   └── propagation/                    # Propagation reports
│       └── propagation_YYYYMMDD_HHMMSS.md
└── docs/
    └── CROSS_REPO_PROPAGATION.md       # This documentation
```

## Future Enhancements

Potential improvements for future versions:

1. **Parallel Propagation**: Process multiple repositories concurrently
2. **Web UI**: Visual interface for managing propagation
3. **Scheduled Propagation**: Automatic periodic synchronization
4. **Conflict Resolution UI**: Interactive merge tool for conflicts
5. **Propagation History**: Track all historical propagations
6. **Rollback Command**: One-command rollback across all repos
7. **Validation Hooks**: Pre/post-propagation validation scripts
8. **Template Variables**: Dynamic content injection based on repo metadata

## Related Documentation

- [File Organization Standards](../.agent-os/standards/FILE_ORGANIZATION_STANDARDS.md)
- [AI Agent Guidelines](../docs/domains/ai/AI_AGENT_GUIDELINES.md)
- [Development Workflow](../docs/domains/workflow/DEVELOPMENT_WORKFLOW.md)
- [HTML Reporting Standards](../docs/domains/standards/HTML_REPORTING_STANDARDS.md)

## Support

For issues or questions:

1. Check this documentation
2. Review propagation reports in `reports/propagation/`
3. Run with `--dry-run` to debug issues
4. Check repository git status for conflicts
5. Verify propagation-rules.yaml configuration

---

**Last Updated:** 2026-01-07
**Version:** 1.0.0
