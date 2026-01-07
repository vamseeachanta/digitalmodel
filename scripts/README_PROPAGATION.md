# Cross-Repository Change Propagation

**Quick Reference Guide**

## Quick Start

```bash
# Dry run (preview changes)
python scripts/cross_repo_change_propagator.py --file <path> --dry-run

# Interactive mode (confirm per repo)
python scripts/cross_repo_change_propagator.py --file <path> --interactive

# Auto-propagate (with safety)
python scripts/cross_repo_change_propagator.py --file <path> --auto --create-branch
```

## Common Use Cases

### 1. Update Hook Configuration Globally

```bash
# Edit settings
vim .claude/settings.json

# Preview propagation
python scripts/cross_repo_change_propagator.py --file .claude/settings.json --dry-run

# Apply to all repos
python scripts/cross_repo_change_propagator.py --file .claude/settings.json --auto --create-branch
```

### 2. Add Marine Engineering Skill

```bash
# Propagate new skill to marine-tagged repos only
python scripts/cross_repo_change_propagator.py \
  --file .claude/skills/orcaflex-modeling/SKILL.md \
  --auto --create-branch
```

### 3. Update Standards Documentation

```bash
# Propagate standards to all active repos
python scripts/cross_repo_change_propagator.py \
  --file .agent-os/standards/HTML_REPORTING_STANDARDS.md \
  --auto --create-branch
```

### 4. Update CLAUDE.md (Template Merge)

```bash
# Interactive mode recommended for template merges
python scripts/cross_repo_change_propagator.py \
  --file CLAUDE.md \
  --interactive
```

## What Gets Propagated?

### Global (All Active Repos)
- `.claude/settings.json` - Hook configurations
- `.agent-os/standards/*.md` - All standards

### Conditional (Based on Tags)
- `marine` tag: OrcaFlex, AQWA, mooring, catenary skills
- `engineering` tag: Fatigue, structural, VIV skills
- `cad` tag: FreeCAD, CAD engineering skills
- `meshing` tag: GMSH meshing skills

## Safety Features

All propagations include:
1. **Backup commit** - Current HEAD recorded before changes
2. **Propagation branch** - Changes isolated in `propagation/YYYYMMDD`
3. **Conflict detection** - Skips files with local modifications
4. **Detailed reports** - Generated in `reports/propagation/`

## Configuration

Edit `.claude/propagation-rules.yaml` to:
- Add new global patterns
- Define conditional rules for new tags
- Configure template merge sections
- Add repository tags

## Full Documentation

See [CROSS_REPO_PROPAGATION.md](../docs/CROSS_REPO_PROPAGATION.md) for complete documentation.

## Troubleshooting

**"Configuration file not found"**
```bash
cd D:/workspace-hub/digitalmodel
python scripts/cross_repo_change_propagator.py --file <path>
```

**"File has local modifications"**
```bash
cd ../target-repo
git add . && git commit -m "Save changes"
# or
git stash
```

**Need to rollback?**
```bash
cd ../target-repo
git reset --hard <backup-commit-hash>
# or
git branch -D propagation/YYYYMMDD
```

## Report Location

After each run, check: `reports/propagation/propagation_YYYYMMDD_HHMMSS.md`

## Next Steps After Propagation

1. Review propagation report
2. Check propagation branches in each repo
3. Test changes
4. Merge branches: `git merge propagation/YYYYMMDD`
5. Push to remote
6. Delete propagation branches
