# Branch Management Policy

## Purpose
This document establishes standards for branch creation, maintenance, and cleanup across all repositories to maintain a clean and efficient development environment.

## Branch Naming Conventions

### Standard Branch Types
- `master` or `main` - Production-ready code
- `develop` - Integration branch for features
- `feature/*` - New features or enhancements
- `bugfix/*` - Bug fixes
- `hotfix/*` - Emergency production fixes
- `release/*` - Release preparation branches
- `experiment/*` - Experimental or proof-of-concept work

### Naming Rules
1. Use lowercase with hyphens: `feature/user-authentication`
2. Include ticket/issue number when applicable: `bugfix/JIRA-1234-login-error`
3. Be descriptive but concise: `feature/pdf-export` not `feature/new-feature-1`
4. Avoid personal names or dates in branch names

## Branch Lifecycle

### Creation
1. Always branch from the appropriate base:
   - Features: from `develop` or `master`
   - Bugfixes: from `develop`
   - Hotfixes: from `master`
2. Ensure branch name follows conventions
3. Push to remote within 24 hours of creation

### Active Development
1. Commit regularly with meaningful messages
2. Push changes at least daily
3. Keep branch up-to-date with base branch
4. Create draft PR early for visibility

### Completion
1. Ensure all tests pass
2. Code review completed
3. Merge via pull request
4. Delete branch immediately after merge

## Stale Branch Management

### Definition of Stale
A branch is considered stale if:
- **Merged**: Already merged to master/main
- **Inactive**: No commits for 60+ days
- **Abandoned**: Author confirms it's no longer needed
- **Obsolete**: Feature/fix implemented differently

### Cleanup Schedule
- **Weekly**: Delete merged branches
- **Monthly**: Review and archive stale branches
- **Quarterly**: Comprehensive cleanup of all repositories

### Archiving Process
Before deleting stale branches with unmerged commits:
1. Create archive tag: `archive/branch-name_YYYYMMDD`
2. Push tag to remote
3. Document reason for archiving
4. Delete branch

## Automated Cleanup

### Using the Cleanup Script
```bash
# Dry run (see what would be cleaned)
python tools/cleanup_stale_branches.py .

# Execute cleanup for single repository
python tools/cleanup_stale_branches.py . --execute

# Clean all repositories
python tools/cleanup_stale_branches.py /path/to/repos --all-repos --execute

# Adjust staleness threshold
python tools/cleanup_stale_branches.py . --stale-days 90 --execute
```

### Protected Branches
The following branches are never automatically deleted:
- `master`
- `main`
- `develop`
- `staging`
- `production`
- Any branch matching `release/*` pattern

## Recovery Procedures

### Restoring Archived Branches
```bash
# List all archive tags
git tag -l "archive/*"

# View archived branch content
git show archive/branch-name_YYYYMMDD

# Restore branch from archive
git checkout -b restored-branch-name archive/branch-name_YYYYMMDD
```

### Recovering Deleted Branches
```bash
# Find the commit hash from reflog
git reflog

# Recreate branch at specific commit
git checkout -b recovered-branch <commit-hash>
```

## Best Practices

### Do's
- ✅ Delete branches immediately after merging
- ✅ Use descriptive branch names
- ✅ Keep branches focused on single features/fixes
- ✅ Regularly sync with base branch
- ✅ Archive important experimental work

### Don'ts
- ❌ Don't reuse old branch names
- ❌ Don't commit directly to master/main
- ❌ Don't leave merged branches undeleted
- ❌ Don't create branches without clear purpose
- ❌ Don't force-push to shared branches

## Responsibilities

### Developers
- Follow naming conventions
- Delete own branches after merge
- Keep branches up-to-date
- Respond to cleanup notifications

### Team Leads
- Enforce branch policies
- Review stale branches monthly
- Approve force deletions
- Maintain protected branch list

### DevOps
- Run automated cleanup scripts
- Maintain cleanup automation
- Generate cleanup reports
- Handle recovery requests

## Metrics and Monitoring

### Key Metrics
- Number of active branches per repository
- Average branch age
- Percentage of stale branches
- Cleanup frequency

### Reporting
Monthly reports should include:
- Branches cleaned per repository
- Storage space recovered
- Stale branch trends
- Policy compliance rate

## Implementation Timeline

1. **Immediate**: Start using naming conventions
2. **Week 1**: Run first cleanup with script
3. **Week 2**: Establish weekly merged branch cleanup
4. **Month 1**: Implement monthly stale branch review
5. **Quarter 1**: Full automation in CI/CD

## Exceptions

Exceptions to this policy require:
1. Written justification
2. Team lead approval
3. Documentation in repository README
4. Quarterly review

## Policy Updates

This policy will be reviewed quarterly and updated based on:
- Team feedback
- Cleanup metrics
- Tool improvements
- Industry best practices

---

*Last Updated: August 31, 2025*  
*Version: 1.0*  
*Owner: Development Team*