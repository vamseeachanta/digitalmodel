---
name: hidden-folder-audit
description: Audit and consolidate hidden folders in a repository. Identifies duplicates, dead directories, and consolidation opportunities for .agent-os/, .ai/, .claude/, and other hidden folders.
version: 1.0.0
updated: 2025-01-20
category: meta
triggers:
- hidden folder audit
- consolidate hidden folders
- .agent-os cleanup
- .ai cleanup
- dot folder review
---

# Hidden Folder Audit Skill

Systematic audit and consolidation of hidden (dot) folders in a repository. Identifies duplicate configurations, dead symlinks, orphaned directories, and consolidation opportunities to establish a clean, maintainable folder structure.

## Version Metadata

```yaml
version: 1.0.0
python_min_version: '3.10'
dependencies: []
compatibility:
  tested_python:
  - '3.10'
  - '3.11'
  - '3.12'
  - '3.13'
  os:
  - Windows
  - Linux
  - macOS
```

## When to Use

- Repository has accumulated multiple hidden folders over time
- Multiple AI/agent configuration directories exist (`.agent-os/`, `.ai/`, `.claude/`)
- Symlinks point to non-existent targets
- Unclear which configuration is authoritative
- Preparing for repository restructure or cleanup
- After inheriting or forking a repository
- Before establishing new folder standards

## Audit Process

### Step 1: Inventory All Hidden Folders

List all hidden directories at the repository root.

```bash
# List all hidden directories (excluding .git)
find . -maxdepth 1 -type d -name ".*" ! -name ".git" | sort

# With sizes
du -sh .*/ 2>/dev/null | grep -v "^0" | sort -hr

# Include symlinks
find . -maxdepth 1 \( -type d -o -type l \) -name ".*" ! -name ".git" | sort
```

### Step 2: Check Git Tracking Status

Determine which hidden folders are tracked, ignored, or untracked.

```bash
# Check if folder is tracked
git ls-files --error-unmatch .folder/ 2>/dev/null && echo "TRACKED" || echo "NOT TRACKED"

# Check if folder is ignored
git check-ignore -v .folder/ 2>/dev/null && echo "IGNORED" || echo "NOT IGNORED"

# List all tracked hidden files
git ls-files | grep "^\." | cut -d'/' -f1 | sort -u

# List all gitignored hidden folders
git status --porcelain --ignored | grep "^!!" | grep "^\./\." | cut -d'/' -f2 | sort -u
```

### Step 3: Analyze Content Overlap

Check for duplicate or overlapping content between hidden folders.

```bash
# Compare agent configurations
diff -rq .agent-os/agents/ .claude/agents/ 2>/dev/null

# Find duplicate files by name
find .agent-os .ai .claude -name "*.md" -type f 2>/dev/null | xargs -I {} basename {} | sort | uniq -d

# Find duplicate files by content (MD5)
find .agent-os .ai .claude -type f 2>/dev/null | xargs md5sum 2>/dev/null | sort | uniq -w32 -d

# Compare directory structures
diff <(find .agent-os -type f | sed 's|.agent-os/||' | sort) \
     <(find .claude -type f | sed 's|.claude/||' | sort) 2>/dev/null
```

### Step 4: Identify Authoritative Source

Determine which folder should be the single source of truth.

**Criteria for Authoritative Source:**
1. **Git tracking** - Tracked folders are more likely authoritative
2. **Recency** - Check last modification dates
3. **Completeness** - More complete configuration wins
4. **Active use** - Referenced in CI/CD, scripts, documentation
5. **Tool requirements** - Some tools require specific folder names

```bash
# Check modification times
stat -c '%Y %n' .*/ 2>/dev/null | sort -rn | head -10

# Check for references in scripts and CI
grep -r "\.agent-os" --include="*.sh" --include="*.yml" --include="*.yaml" .
grep -r "\.claude" --include="*.sh" --include="*.yml" --include="*.yaml" .

# Check symlink targets
find . -maxdepth 2 -type l -exec ls -la {} \;
```

### Step 5: Plan Consolidation

Create a migration plan based on analysis.

**Migration Checklist:**
- [ ] Identify target folder structure
- [ ] List files to migrate
- [ ] Identify files to delete
- [ ] Update references in code/scripts
- [ ] Update .gitignore
- [ ] Test after migration

## Common Hidden Folders Reference

| Folder | Purpose | Typical Action |
|--------|---------|----------------|
| `.claude/` | Claude Code configuration, agents, skills, docs | **KEEP** (authoritative for AI tools) |
| `.claude-flow/` | Runtime data, state, coordination | **KEEP** (gitignore, runtime only) |
| `.agent-os/` | Legacy agent OS configuration | **CONSOLIDATE** to `.claude/` |
| `.ai/` | Legacy AI configuration | **CONSOLIDATE** to `.claude/` |
| `.agent-runtime/` | Dead symlinks, orphaned state | **DELETE** after backup |
| `.common/` | Orphaned utilities | **DELETE** or **RELOCATE** to `src/` |
| `.specify/` | Stale specification templates | **DELETE** (migrate to `specs/templates/`) |
| `.drcode/` | External tool (Dr. Code) config | **KEEP** if actively used |
| `.github/` | GitHub workflows, templates | **KEEP** (required by GitHub) |
| `.vscode/` | VS Code settings | **KEEP** (team settings) |
| `.idea/` | JetBrains IDE settings | **KEEP** or gitignore |
| `.git/` | Git repository data | **NEVER TOUCH** |
| `.gitignore` | Git ignore patterns | **KEEP** (update as needed) |
| `.env` | Environment variables | **KEEP** (gitignore, never commit) |
| `.pytest_cache/` | Pytest cache | **DELETE** (regenerated) |
| `.ruff_cache/` | Ruff linter cache | **DELETE** (regenerated) |
| `.mypy_cache/` | MyPy type checker cache | **DELETE** (regenerated) |
| `.coordination/` | Multi-agent coordination | **MIGRATE** to `.claude-flow/` |
| `.session/` | Session state files | **MIGRATE** to `.claude-flow/` |

## Consolidation Commands

### Migrate .agent-os to .claude

```bash
# Backup first
cp -r .agent-os .agent-os.backup

# Migrate agents
mkdir -p .claude/agents
cp -r .agent-os/agents/* .claude/agents/ 2>/dev/null

# Migrate standards (if applicable)
mkdir -p .claude/standards
cp -r .agent-os/standards/* .claude/standards/ 2>/dev/null

# Update git tracking
git rm -r --cached .agent-os/ 2>/dev/null
git add .claude/

# Remove old folder after verification
rm -rf .agent-os
rm -rf .agent-os.backup  # After confirming migration
```

### Migrate .ai to .claude

```bash
# Backup first
cp -r .ai .ai.backup

# Migrate prompts to skills
mkdir -p .claude/skills/prompts
cp -r .ai/prompts/* .claude/skills/prompts/ 2>/dev/null

# Migrate config
cp .ai/config.* .claude/ 2>/dev/null

# Cleanup
git rm -r --cached .ai/ 2>/dev/null
rm -rf .ai
```

### Clean Dead Symlinks

```bash
# Find broken symlinks
find . -maxdepth 2 -type l ! -exec test -e {} \; -print

# Remove broken symlinks
find . -maxdepth 2 -type l ! -exec test -e {} \; -delete

# Remove specific dead symlink folder
rm -rf .agent-runtime
```

### Consolidate Runtime Directories

```bash
# Create standard runtime directory
mkdir -p .claude-flow/state
mkdir -p .claude-flow/cache
mkdir -p .claude-flow/logs

# Migrate coordination data
mv .coordination/* .claude-flow/state/ 2>/dev/null
mv .session/* .claude-flow/state/ 2>/dev/null

# Remove old directories
rm -rf .coordination .session
```

### Update .gitignore

Add these patterns after consolidation:

```gitignore
# Runtime and state (not tracked)
.claude-flow/
.coordination/
.session/

# Legacy folders (prevent re-creation)
.agent-os/
.ai/
.agent-runtime/

# Cache directories
.pytest_cache/
.ruff_cache/
.mypy_cache/
__pycache__/
```

## Audit Checklist

### Pre-Audit
- [ ] Working directory is clean (`git status`)
- [ ] Create backup branch: `git checkout -b backup/pre-hidden-audit`
- [ ] Document current state: `ls -la .*/ > hidden-folders-before.txt`

### Discovery Phase
- [ ] List all hidden folders at root level
- [ ] Record size of each hidden folder
- [ ] Identify symlinks and their targets
- [ ] Check git tracking status for each

### Analysis Phase
- [ ] Identify duplicate configurations
- [ ] Find broken symlinks
- [ ] Determine authoritative source for each config type
- [ ] Check for active usage in scripts/CI

### Planning Phase
- [ ] Create target folder structure diagram
- [ ] List files to migrate with source/destination
- [ ] List files/folders to delete
- [ ] Identify scripts/references to update

### Execution Phase
- [ ] Backup folders before migration
- [ ] Migrate content to authoritative locations
- [ ] Update git tracking (`git rm --cached`, `git add`)
- [ ] Remove legacy folders
- [ ] Update .gitignore

### Verification Phase
- [ ] Run tests to ensure nothing broke
- [ ] Verify symlinks work (if any remain)
- [ ] Confirm scripts still function
- [ ] Check CI/CD pipelines
- [ ] Commit with descriptive message

### Documentation Phase
- [ ] Update README if folder structure changed
- [ ] Document new standard in CLAUDE.md or equivalent
- [ ] Remove references to legacy folders in docs

## Best Practices

1. **Always backup before deleting** - Create a backup branch or copy
2. **Use git rm for tracked folders** - Preserves history
3. **Check symlink targets before deleting** - May need to update references
4. **Update .gitignore first** - Prevents accidental re-tracking
5. **Test after consolidation** - Ensure nothing broke
6. **Commit in logical chunks** - Separate migration from cleanup
7. **Document the changes** - Future maintainers will thank you

## Related Skills

- [repo-cleanup](../repo-cleanup/SKILL.md) - General repository cleanup
- [module-based-refactor](../module-based-refactor/SKILL.md) - For source code restructuring
- [session-start-routine](../session-start-routine/SKILL.md) - Session initialization

## References

- Git ls-files documentation: https://git-scm.com/docs/git-ls-files
- Git check-ignore documentation: https://git-scm.com/docs/git-check-ignore
- Symbolic links in Git: https://git-scm.com/book/en/v2/Git-Internals-Git-References

---

## Version History

- **1.0.0** (2025-01-20): Initial release based on digitalmodel repository hidden folder audit session
