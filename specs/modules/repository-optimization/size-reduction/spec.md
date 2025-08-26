# Repository Size Reduction Specification

## Overview
This specification outlines a comprehensive strategy to reduce the DigitalModel repository size from 2.5GB to under 500MB through phased file management, Git history optimization, and metadata preservation.

## Current State Analysis

### Repository Metrics
- **Total .git directory size**: 2.5GB
- **Working directory size**: ~500MB
- **Largest single file**: 71MB (wigleyHull_LTS.tar.gz)
- **Files over 10MB**: 30 files
- **Primary contributors**: Large binary files, accumulated Git history, test data

### Identified Issues
1. **Large Binary Files**: Multiple tar.gz, zip, and data files exceeding 10MB
2. **Git History Bloat**: Years of binary file changes stored in Git history
3. **Test Data**: Large test datasets committed directly to repository
4. **Documentation Assets**: PDFs and images inflating repository size

## Technical Approach

### Phase 1: Immediate Large File Management (&gt;100MB)
**Target**: Files exceeding 100MB
**Strategy**: Git LFS migration or external storage

#### Files to Address
Currently no files exceed 100MB in working directory, but Git history may contain larger deleted files.

#### Implementation
1. **Scan Git history** for files &gt;100MB using:
   ```bash
   git rev-list --objects --all | \
     git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' | \
     awk '/^blob/ {print substr($0,6)}' | \
     sort --numeric-sort --key=2 | \
     awk '$2 &gt;= 100*1024*1024'
   ```

2. **Remove from history** using BFG Repo-Cleaner:
   ```bash
   bfg --strip-blobs-bigger-than 100M
   ```

3. **Add to Git LFS** if still needed:
   ```bash
   git lfs track "*.large"
   git add .gitattributes
   ```

### Phase 2: Large File Reduction (&gt;50MB)
**Target**: Files between 50-100MB
**Strategy**: Selective LFS migration

#### Current Files (50-100MB)
- wigleyHull_LTS.tar.gz (71MB)

#### Implementation
1. **Migrate to LFS**:
   ```bash
   git lfs track "*.tar.gz"
   git lfs track "*.zip"
   ```

2. **Rewrite history**:
   ```bash
   git lfs migrate import --include="*.tar.gz,*.zip" --above=50mb
   ```

### Phase 3: Medium File Optimization (&gt;25MB)
**Target**: Files between 25-50MB
**Strategy**: Evaluate necessity, compress, or externalize

#### Current Files (25-50MB)
- RiserVIV_riser_animation.gif (32MB)
- test_results.zip (28MB)

#### Implementation
1. **Compress animations**: Convert GIF to MP4 (90% size reduction)
2. **Move test data** to external storage or generate on-demand
3. **Update references** in documentation

### Phase 4: Standard File Review (&gt;10MB)
**Target**: Files between 10-25MB
**Strategy**: Case-by-case evaluation

#### Current Files (10-25MB)
- 27 files including PDFs, CSVs, and binary data

#### Implementation
1. **Documentation PDFs**: Convert to markdown or link to external docs
2. **CSV data files**: Compress or generate programmatically
3. **Binary files**: Evaluate necessity, remove if obsolete

### Phase 5: Small File Aggregation (&gt;5MB)
**Target**: Files between 5-10MB
**Strategy**: Aggregate optimization

#### Implementation
1. **Identify patterns**: Group similar files
2. **Create generators**: Replace data files with generation scripts
3. **Archive old versions**: Move to separate archive repository

## Git History Optimization

### Strategy 1: BFG Repo-Cleaner
**Advantages**: Fast, preserves recent history
**Process**:
```bash
# Clone fresh copy
git clone --mirror https://github.com/user/digitalmodel.git

# Clean with BFG
bfg --strip-blobs-bigger-than 10M digitalmodel.git
bfg --delete-folders "{test-data,old-results}" digitalmodel.git

# Cleanup
cd digitalmodel.git
git reflog expire --expire=now --all
git gc --prune=now --aggressive
```

### Strategy 2: git filter-branch
**Advantages**: Fine control, built-in Git
**Process**:
```bash
git filter-branch --force --index-filter \
  'git rm -rf --cached --ignore-unmatch path/to/large/files' \
  --prune-empty --tag-name-filter cat -- --all
```

### Strategy 3: Fresh Repository with Preserved Metadata
**Advantages**: Clean start, maximum size reduction
**Process**:
1. Export all metadata (issues, PRs, wiki)
2. Create new repository
3. Copy only essential files
4. Import metadata
5. Archive old repository

## Metadata Preservation

### Essential Metadata to Preserve
1. **Commit Messages**: Export to changelog
2. **Author Information**: Maintain contributor list
3. **Tags and Releases**: Document version history
4. **Issues and PRs**: Export via GitHub API
5. **Branch Structure**: Document branch purposes

### Preservation Methods
```python
# Export commit history
git log --pretty=format:"%h|%an|%ae|%ad|%s" &gt; commit_history.csv

# Export tags
git tag -l --format='%(refname:short)|%(taggerdate)|%(subject)' &gt; tags.csv

# GitHub metadata via API
gh api repos/:owner/:repo/issues --paginate &gt; issues.json
gh api repos/:owner/:repo/pulls --paginate &gt; pulls.json
```

## Implementation Plan

### Pre-Migration Checklist
- [ ] Full repository backup
- [ ] Document current structure
- [ ] Notify all team members
- [ ] Test migration on clone
- [ ] Prepare rollback plan

### Migration Steps
1. **Backup Phase** (Day 1)
   - Create complete backup
   - Export all metadata
   - Document file locations

2. **Testing Phase** (Day 2-3)
   - Test on repository clone
   - Validate all functionality
   - Measure size reduction

3. **Execution Phase** (Day 4)
   - Execute migration plan
   - Verify all systems operational
   - Update documentation

4. **Validation Phase** (Day 5)
   - Run all tests
   - Verify CI/CD pipelines
   - Confirm team access

### Post-Migration Tasks
- [ ] Update README with new clone instructions
- [ ] Configure Git LFS for all developers
- [ ] Update CI/CD for LFS support
- [ ] Document new file management policies
- [ ] Train team on new procedures

## Success Metrics

### Size Targets
- **Phase 1**: Reduce .git to under 2GB
- **Phase 2**: Reduce .git to under 1.5GB
- **Phase 3**: Reduce .git to under 1GB
- **Phase 4**: Reduce .git to under 750MB
- **Phase 5**: Achieve target of under 500MB

### Performance Metrics
- Clone time: &lt;2 minutes on standard connection
- Checkout time: &lt;30 seconds
- CI/CD build time: Reduced by 50%
- Storage costs: Reduced by 80%

## Risk Mitigation

### Identified Risks
1. **Data Loss**: Accidental deletion of important files
   - **Mitigation**: Multiple backups, staged approach
   
2. **Broken References**: Links to removed files
   - **Mitigation**: Comprehensive link checking, documentation update
   
3. **Team Disruption**: Workflow interruption
   - **Mitigation**: Clear communication, training, phased rollout
   
4. **CI/CD Failures**: Pipeline breaks due to missing files
   - **Mitigation**: Test all pipelines, update configurations

### Rollback Plan
1. Maintain original repository as archive
2. Document all changes made
3. Keep backup of original .git directory
4. Test rollback procedure before execution

## Long-term Maintenance

### File Management Policies
1. **Size Limits**: Enforce 10MB file size limit via pre-commit hooks
2. **Binary Files**: Require LFS for all binary files
3. **Test Data**: Generate test data programmatically
4. **Documentation**: Prefer text formats over binary

### Automated Monitoring
```yaml
# .github/workflows/size-check.yml
name: Repository Size Check
on: [push, pull_request]
jobs:
  check-size:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Check file sizes
        run: |
          find . -type f -size +10M -exec ls -lh {} \; | \
          grep -v "^\.git" && exit 1 || exit 0
```

### Regular Maintenance Schedule
- **Weekly**: Review new large files
- **Monthly**: Check repository growth
- **Quarterly**: Run cleanup scripts
- **Annually**: Full repository audit

## Conclusion

This phased approach will reduce the DigitalModel repository from 2.5GB to under 500MB while preserving all essential functionality and metadata. The implementation minimizes risk through careful planning, testing, and rollback procedures.

---
*Created: 2024-12-24*
*Version: 1.0*
*Status: Ready for Review*