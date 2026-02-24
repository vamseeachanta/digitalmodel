# Repository Size Reduction - Prompt Documentation

## Original User Request

**Date:** 2024-12-24
**Request:** Create a plan for repo to reduce size

### Full Request
```
/create-spec create a plan for repo to reduce size. Lean libraries are easier to clone, handle, execute etc. The following are known issues or ideas:
1. Phased approach: >100MB, >50MB, >25MB, >10MB, >5MB
2. Research deep: git history size reduction, while preserving all metadata, fundamentals etc.
```

## Context Analysis

### Repository State
- Total .git directory: 2.5GB
- Working directory: ~500MB  
- Largest file: 71MB (wigleyHull_LTS.tar.gz)
- Files over 10MB: 30 files
- Primary issue: Git history accumulation of binary files

### Key Requirements
1. **Phased Reduction**: Progressive size targets from 100MB down to 5MB
2. **Metadata Preservation**: Maintain all commit history, authors, tags, issues
3. **Git History Optimization**: Deep cleaning while preserving fundamentals
4. **Zero Data Loss**: All essential data must be preserved or migrated
5. **Team Continuity**: Minimize disruption to ongoing development

## Research Findings

### Git History Reduction Techniques

#### 1. BFG Repo-Cleaner
- **Pros**: Fastest method, preserves recent commits, simple commands
- **Cons**: Less granular control, requires Java
- **Best for**: Removing files by size or pattern

#### 2. git filter-branch
- **Pros**: Built into Git, fine-grained control, scriptable
- **Cons**: Slower, more complex, can break references
- **Best for**: Surgical removal of specific paths

#### 3. git-filter-repo
- **Pros**: Modern replacement for filter-branch, Python-based, fast
- **Cons**: Not built into Git, requires installation
- **Best for**: Complex filtering operations

### Large File Storage Options

#### Git LFS (Selected)
- **Pros**: Integrated with Git, GitHub support, transparent to users
- **Cons**: Requires LFS server, bandwidth costs
- **Best for**: Binary files that need version control

#### External Storage Alternatives
- S3/Cloud storage for archives
- CI/CD artifact storage for build outputs
- Package registries for dependencies

### Metadata Preservation Strategies

1. **Commit History Export**
   ```bash
   git log --all --pretty=format:"%H|%an|%ae|%ad|%s|%P" > commits.csv
   ```

2. **GitHub API Export**
   ```bash
   gh api repos/:owner/:repo/issues --paginate > issues.json
   gh api repos/:owner/:repo/pulls --paginate > pulls.json
   ```

3. **Tag and Branch Documentation**
   ```bash
   git for-each-ref --format='%(refname:short)|%(objectname)|%(subject)' refs/tags > tags.csv
   ```

## Design Decisions

### Decision 1: Use BFG for Primary Cleanup
**Rationale**: Fastest, safest for large-scale cleanup
**Alternative Considered**: git filter-branch (too slow for 2.5GB)

### Decision 2: Implement Git LFS for Large Files
**Rationale**: Best integration with existing Git workflow
**Alternative Considered**: External storage (adds complexity)

### Decision 3: Phased Migration Approach
**Rationale**: Reduces risk, allows validation at each step
**Alternative Considered**: Single migration (too risky)

### Decision 4: Preserve Full Metadata
**Rationale**: Maintains project history and attribution
**Alternative Considered**: Fresh repository (loses valuable history)

## Implementation Strategy

### Phase Structure
1. **Phase 1 (&gt;100MB)**: No files currently, check history
2. **Phase 2 (&gt;50MB)**: 1 file (wigleyHull_LTS.tar.gz)
3. **Phase 3 (&gt;25MB)**: 2 files (animations, test results)
4. **Phase 4 (&gt;10MB)**: 27 files (docs, data)
5. **Phase 5 (&gt;5MB)**: Final optimization

### Critical Success Factors
1. **Complete Backup**: Multiple backup strategies before starting
2. **Team Communication**: Clear timeline and training
3. **Gradual Rollout**: Test on clones before production
4. **Validation Gates**: Verify functionality at each phase
5. **Rollback Plan**: Ability to restore if issues arise

## Curated Reuse Prompt

For future repository size reduction projects, use this prompt:

```
Create a comprehensive repository size reduction plan with the following requirements:

1. Current State Analysis:
   - Analyze .git directory size
   - Identify files by size tiers: >100MB, >50MB, >25MB, >10MB, >5MB
   - Document Git history contributors

2. Phased Approach:
   - Phase 1: Remove/migrate files >100MB
   - Phase 2: Handle files >50MB
   - Phase 3: Optimize files >25MB
   - Phase 4: Review files >10MB
   - Phase 5: Final optimization >5MB

3. Technical Requirements:
   - Use BFG Repo-Cleaner for history cleanup
   - Implement Git LFS for large binary files
   - Preserve all metadata (commits, authors, tags, issues)
   - Create comprehensive backup strategy
   - Maintain zero data loss

4. Deliverables:
   - spec.md: Technical specification with implementation details
   - tasks.md: Detailed task breakdown with time estimates
   - validation procedures for each phase
   - rollback plan for risk mitigation
   - team enablement documentation

5. Success Metrics:
   - Target repository size: <500MB
   - Clone time: <2 minutes
   - No functionality loss
   - All tests passing
   - Team successfully migrated

Focus on safety, gradual migration, and comprehensive validation at each step.
```

## Lessons Learned

### From Analysis
1. **Git history is primary contributor**: 2.5GB .git vs 500MB working directory
2. **Binary files accumulate**: Even deleted files remain in history
3. **Phased approach essential**: Reduces risk and allows validation

### Best Practices Identified
1. **Always backup before cleanup**: Multiple backup strategies
2. **Test on clones first**: Never experiment on production
3. **Communicate extensively**: Team awareness critical
4. **Document everything**: Decisions, processes, rollback plans
5. **Automate monitoring**: Prevent future accumulation

### Tools Recommended
1. **BFG Repo-Cleaner**: For fast, safe history cleanup
2. **Git LFS**: For ongoing large file management
3. **git-sizer**: For detailed size analysis
4. **GitHub CLI**: For metadata export
5. **Pre-commit hooks**: For size enforcement

## Next Steps

1. **Immediate Actions**:
   - Get team approval for plan
   - Schedule migration window
   - Set up test environment

2. **Preparation**:
   - Create full repository backup
   - Export all metadata
   - Set up Git LFS infrastructure

3. **Execution**:
   - Follow phased approach in tasks.md
   - Validate at each phase
   - Document any deviations

4. **Post-Migration**:
   - Monitor repository size
   - Gather team feedback
   - Refine procedures

---
*Created: 2024-12-24*
*Version: 1.0*
*Status: Complete*