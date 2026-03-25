# Cross-Repository Propagation Setup Example

**ABOUTME:** Example configuration and workflow for setting up cross-repository propagation for workspace-hub.

## Step 1: Configure Repository Tags

Edit `.claude/propagation-rules.yaml` and add your repositories under `repository_discovery.repository_tags`:

```yaml
repository_discovery:
  base_path: D:/workspace-hub

  repository_tags:
    # Main repository (source)
    digitalmodel: [marine, engineering, cad, meshing, signal-processing, active]

    # Example marine engineering repository
    offshore-analysis: [marine, engineering, active]

    # Example CAD-focused repository
    cad-automation: [cad, active]

    # Example archived repository (no propagation)
    legacy-project: [marine, engineering]  # No 'active' tag

    # Example web application (minimal propagation)
    web-dashboard: [active]  # Only global files propagate
```

### Available Tags

- **`active`** (required): Repository receives propagations
- **`marine`**: Marine engineering (OrcaFlex, AQWA, mooring, etc.)
- **`engineering`**: Structural analysis (fatigue, VIV, stress, etc.)
- **`cad`**: CAD and 3D modeling (FreeCAD, etc.)
- **`meshing`**: Mesh generation (GMSH, etc.)
- **`signal-processing`**: Signal analysis and processing

## Step 2: Verify Configuration

Test the configuration with a dry run:

```bash
cd D:/workspace-hub/digitalmodel

# Test with a global file
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --dry-run

# Expected output:
# Processing offshore-analysis (Reason: Global pattern)
# Processing cad-automation (Reason: Global pattern)
# Processing web-dashboard (Reason: Global pattern)
# Skipping legacy-project (Reason: Tag 'active' not found)
```

## Step 3: Test Conditional Propagation

Test conditional propagation based on tags:

```bash
# Marine skill - should only go to marine-tagged repos
python scripts/cross_repo_change_propagator.py \
  --file .claude/skills/orcaflex-modeling/SKILL.md \
  --dry-run

# Expected output:
# Processing offshore-analysis (Reason: Conditional - marine tag)
# Skipping cad-automation (Reason: Tag mismatch - requires 'marine')
# Skipping web-dashboard (Reason: Tag mismatch - requires 'marine')
```

## Step 4: Propagate a Change

Once verified, propagate a real change:

```bash
# Edit a global file
vim .claude/settings.json

# Commit locally first
git add .claude/settings.json
git commit -m "chore(config): Update hook settings"

# Dry run to preview
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --dry-run

# Review output, then apply
python scripts/cross_repo_change_propagator.py \
  --file .claude/settings.json \
  --auto \
  --create-branch

# Check the report
cat reports/propagation/propagation_*.md
```

## Step 5: Review and Merge Propagation Branches

After propagation, review the changes in each repository:

```bash
# Go to target repository
cd ../offshore-analysis

# Check the propagation branch
git checkout propagation/20260107
git log -1
git diff main

# If good, merge
git checkout main
git merge propagation/20260107

# Push to remote
git push origin main

# Clean up branch
git branch -d propagation/20260107
```

## Example Workflow: Adding a New Skill

### 1. Create the Skill

```bash
cd D:/workspace-hub/digitalmodel

# Create new marine skill
mkdir -p .claude/skills/wave-analysis
cat > .claude/skills/wave-analysis/SKILL.md <<EOF
# Wave Analysis Skill

Expert in wave spectrum analysis and sea state modeling.

## Capabilities
- Wave spectrum generation (JONSWAP, PM, etc.)
- Sea state statistical analysis
- Wave force calculations

## Tools
- Spectrum analyzers
- Statistical processors
EOF
```

### 2. Add Propagation Rule (if needed)

If the skill follows existing patterns (e.g., `orcaflex-*`, `fatigue-*`), no rule change needed.

Otherwise, add to `.claude/propagation-rules.yaml`:

```yaml
conditional:
  - path: .claude/skills/wave-analysis
    condition: marine
    description: "Wave spectrum analysis and modeling"
```

### 3. Propagate the Skill

```bash
# Dry run first
python scripts/cross_repo_change_propagator.py \
  --file .claude/skills/wave-analysis/SKILL.md \
  --dry-run

# Review which repos will receive it
# Then propagate
python scripts/cross_repo_change_propagator.py \
  --file .claude/skills/wave-analysis/SKILL.md \
  --auto \
  --create-branch
```

### 4. Verify Propagation

```bash
# Check report
cat reports/propagation/propagation_*.md

# Verify in target repos
cd ../offshore-analysis
git checkout propagation/20260107
cat .claude/skills/wave-analysis/SKILL.md

# Merge if satisfied
git checkout main
git merge propagation/20260107
```

## Example Workflow: Updating Standards

### 1. Update the Standard

```bash
cd D:/workspace-hub/digitalmodel

# Edit standard
vim .agent-os/standards/HTML_REPORTING_STANDARDS.md

# Commit
git add .agent-os/standards/HTML_REPORTING_STANDARDS.md
git commit -m "docs(standards): Add D3.js visualization requirements"
```

### 2. Propagate to All Active Repos

Standards propagate globally (to all active repos):

```bash
# Preview impact
python scripts/cross_repo_change_propagator.py \
  --file .agent-os/standards/HTML_REPORTING_STANDARDS.md \
  --dry-run

# Apply
python scripts/cross_repo_change_propagator.py \
  --file .agent-os/standards/HTML_REPORTING_STANDARDS.md \
  --auto \
  --create-branch
```

### 3. Bulk Merge Across Repos

Create a helper script for bulk merging:

```bash
# scripts/merge_propagation.sh
#!/bin/bash

BRANCH="propagation/$(date +%Y%m%d)"
BASE_DIR="/d/workspace-hub"

for repo in offshore-analysis cad-automation web-dashboard; do
  echo "=== Merging in $repo ==="
  cd "$BASE_DIR/$repo"

  if git rev-parse --verify "$BRANCH" >/dev/null 2>&1; then
    git checkout main
    git merge "$BRANCH"
    git branch -d "$BRANCH"
    echo "✓ Merged and cleaned up"
  else
    echo "⏭️  No propagation branch found"
  fi

  echo ""
done
```

Make executable and run:

```bash
chmod +x scripts/merge_propagation.sh
./scripts/merge_propagation.sh
```

## Example Workflow: Template Merge (CLAUDE.md)

CLAUDE.md uses template merging to preserve repo-specific content.

### 1. Structure Your CLAUDE.md

```markdown
# Claude Code Configuration - My Project

# WORKSPACE-MANAGED
## Interactive Engagement (MANDATORY)
[Workspace-managed content - will be updated by propagation]

## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT
[Workspace-managed content - will be updated by propagation]
# END WORKSPACE-MANAGED

# REPO-SPECIFIC
## Project Overview
This is my offshore analysis project for subsea systems.

## Custom Rules
- Use meters for all measurements
- Default water depth: 1500m
- Apply DNV-GL standards
# END REPO-SPECIFIC
```

### 2. Update Workspace-Managed Section

```bash
cd D:/workspace-hub/digitalmodel

# Edit WORKSPACE-MANAGED section only
vim CLAUDE.md

# Commit
git add CLAUDE.md
git commit -m "docs(claude): Update concurrent execution guidelines"
```

### 3. Propagate with Interactive Mode

Use interactive mode to review each merge:

```bash
python scripts/cross_repo_change_propagator.py \
  --file CLAUDE.md \
  --interactive
```

You'll see:

```
📦 Processing offshore-analysis
   Reason: Template merge: CLAUDE.md

  📄 Current file exists. Showing diff:

  [Shows diff of WORKSPACE-MANAGED sections only]

  Apply changes to offshore-analysis? [y/N]:
```

### 4. Verify Merge Preserved Repo-Specific Content

```bash
cd ../offshore-analysis
git checkout propagation/20260107
cat CLAUDE.md

# Should show:
# - Updated WORKSPACE-MANAGED section
# - Preserved REPO-SPECIFIC section
```

## Troubleshooting Examples

### Problem: Propagation Skipped Due to Tags

```
⏭️  Skipping cad-automation: Tag mismatch: requires 'marine', has ['cad', 'active']
```

**Solution:** This is expected. The skill requires the `marine` tag, which `cad-automation` doesn't have.

### Problem: File Has Local Modifications

```
⚠️  Conflict: File has local modifications
```

**Solution:** Commit or stash changes in the target repository:

```bash
cd ../target-repo
git status
git add .
git commit -m "Save local changes before propagation"

# Re-run propagation
cd ../digitalmodel
python scripts/cross_repo_change_propagator.py --file <path> --auto
```

### Problem: Branch Already Exists

```
❌ Failed to create branch propagation/20260107 in target-repo
```

**Solution:** Delete or merge the existing branch:

```bash
cd ../target-repo
git branch -D propagation/20260107
# or
git merge propagation/20260107
```

## Best Practices

1. **Always dry-run first**
   ```bash
   python scripts/cross_repo_change_propagator.py --file <path> --dry-run
   ```

2. **Use interactive mode for template merges**
   ```bash
   python scripts/cross_repo_change_propagator.py --file CLAUDE.md --interactive
   ```

3. **Commit source changes before propagation**
   ```bash
   git add <file>
   git commit -m "description"
   python scripts/cross_repo_change_propagator.py --file <file> --auto
   ```

4. **Review propagation reports**
   ```bash
   cat reports/propagation/propagation_*.md
   ```

5. **Test on one repo first**
   ```bash
   python scripts/cross_repo_change_propagator.py \
     --file <path> \
     --repos test-repo \
     --interactive
   ```

6. **Keep repository tags accurate**
   - Review tags quarterly
   - Update when repos change focus
   - Use `active` tag to control propagation

## Advanced: Bulk Propagation

Propagate multiple files at once:

```bash
#!/bin/bash
# scripts/propagate_all_standards.sh

STANDARDS=(
  ".agent-os/standards/FILE_ORGANIZATION_STANDARDS.md"
  ".agent-os/standards/HTML_REPORTING_STANDARDS.md"
  ".agent-os/standards/TESTING_FRAMEWORK_STANDARDS.md"
  ".agent-os/standards/LOGGING_STANDARDS.md"
)

for standard in "${STANDARDS[@]}"; do
  echo "=== Propagating $standard ==="
  python scripts/cross_repo_change_propagator.py \
    --file "$standard" \
    --auto \
    --create-branch
  echo ""
done

echo "✅ All standards propagated"
echo "📊 Check reports in: reports/propagation/"
```

## Summary

1. **Configure** repository tags in `propagation-rules.yaml`
2. **Test** with `--dry-run`
3. **Propagate** with `--auto --create-branch`
4. **Review** propagation reports
5. **Merge** propagation branches in target repos

For full documentation, see [CROSS_REPO_PROPAGATION.md](../CROSS_REPO_PROPAGATION.md).
