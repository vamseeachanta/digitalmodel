# Technical Specification

This is the technical specification for the spec detailed in @.agent-os/specs/2025-07-27-git-repository-optimization/spec.md

> Created: 2025-07-27
> Version: 1.0.0

## Technical Requirements

- Analyze repository size using git commands and third-party tools to identify large objects and size contributors
- Implement interactive user configuration system for history retention policies with validation
- Support multiple history retention modes: last N commits, last N months, specific date cutoffs, and complete history preservation
- Implement safe large file removal from git history while preserving repository integrity and user-specified commit history
- Implement optional commit history truncation based on user-specified retention policies
- Optimize repository structure through garbage collection, repacking, and configuration tuning
- Coordinate origin repository updates with proper team communication and force-push safety procedures
- Establish automated preventive measures including pre-commit hooks and repository maintenance scripts

## Approach Options

**Option A: Git Filter-Repo (Recommended)**
- Pros: Modern, officially recommended by Git team, fast, comprehensive filtering capabilities, good documentation
- Cons: Requires Python installation, newer tool with less historical usage

**Option B: BFG Repo-Cleaner**
- Pros: Very fast, designed specifically for removing large files, simple command interface, widely used
- Cons: Java dependency, limited filtering options compared to filter-repo, less active development

**Option C: Git Filter-Branch (Legacy)**
- Pros: Built into Git, no external dependencies, maximum control over filtering
- Cons: Very slow on large repositories, deprecated by Git team, complex syntax, memory intensive

**Rationale:** Git filter-repo is selected as the primary approach because it's officially recommended by the Git team as the replacement for filter-branch, offers excellent performance, and provides comprehensive filtering capabilities. BFG Repo-Cleaner will be kept as a backup option for specific large file removal scenarios.

## External Dependencies

- **git-filter-repo** - Modern repository filtering tool for removing large files and rewriting history
- **Justification:** Official replacement for git filter-branch, significantly faster performance, designed specifically for repository cleanup operations

## User Interface Specification

### History Retention Configuration Menu

The system will present an interactive menu with the following options:

```
Git Repository Optimization - History Retention Policy
====================================================

Current repository status:
- Total commits: 1,247
- Repository age: 2.3 years (since 2022-04-15)
- Current size: 1.2 GB
- Estimated large files: 450 MB

Choose history retention policy:

1. Keep last N commits
   └─ Enter number of recent commits to preserve (recommended: 100-1000)
   
2. Keep last N months  
   └─ Enter number of months to preserve (recommended: 6-24)
   
3. Keep all commits
   └─ Preserve complete history, only remove large files
   
4. Custom date cutoff
   └─ Enter specific date (YYYY-MM-DD format)
   
5. Preview mode
   └─ Show what would be removed without making changes

Enter your choice (1-5): _
```

### Input Validation and Preview

- **Numeric validation:** Ensure positive integers for commits/months
- **Date validation:** Verify YYYY-MM-DD format and reasonable date ranges
- **Impact preview:** Show estimated size reduction and commits affected
- **Confirmation prompt:** Require explicit confirmation before proceeding

### Example User Interactions

**Option 1 - Last N Commits:**
```
You selected: Keep last N commits
Enter number of commits to preserve: 500

Preview:
- Commits to preserve: 500 (most recent)
- Commits to remove: 747 (older history)  
- Estimated size reduction: ~60%
- Date range preserved: 2023-08-15 to present

Continue? (y/N): _
```

**Option 2 - Last N Months:**
```
You selected: Keep last N months
Enter number of months to preserve: 12

Preview:
- Date cutoff: 2024-07-27 (12 months ago)
- Commits to preserve: 823
- Commits to remove: 424
- Estimated size reduction: ~45%

Continue? (y/N): _
```

**Option 3 - All Commits:**
```
You selected: Keep all commits (remove large files only)

Preview:
- Commits to preserve: 1,247 (all)
- Large files to remove: ~450 MB
- Estimated size reduction: ~35%
- History preserved: Complete

Continue? (y/N): _
```

## Implementation Strategy

### Phase 1: Analysis and Preparation
- Repository size analysis using `git count-objects -vH` and `git rev-list --objects --all`
- Large object identification using `git verify-pack` and `git ls-tree` commands
- Commit history analysis to show total commits, date ranges, and size distribution
- Interactive user configuration for history retention policy with specific options:
  - **Option 1:** Keep last N commits (user enters number, e.g., 100, 500, 1000)
  - **Option 2:** Keep last N months (user enters months, e.g., 6, 12, 24)
  - **Option 3:** Keep all commits (only remove large files, preserve complete history)
  - **Option 4:** Custom date cutoff (user enters specific date)
- Validation of user input and preview of what will be preserved/removed
- Branch and tag inventory to understand repository structure
- Team coordination and backup creation before any destructive operations

### Phase 2: Repository Optimization
- Large file removal using git-filter-repo with appropriate filtering expressions
- Repository structure optimization through garbage collection and repacking
- Verification of repository integrity and functionality after optimization
- Size comparison and performance benchmarking

### Phase 3: Origin Synchronization
- Team communication and coordination for origin repository updates
- Safe force-push execution with proper backup procedures
- Team member repository update coordination and assistance
- Verification of origin repository optimization success

### Phase 4: Preventive Measures
- .gitignore file updates to prevent future large file commits
- Pre-commit hook implementation for large file detection and blocking
- Repository maintenance script creation for ongoing size monitoring
- Team education and documentation on repository size best practices

## Safety Considerations

- Complete repository backup before any destructive operations
- Team coordination to ensure no active development during origin updates
- Force-push protection through proper Git configuration and team communication
- Repository integrity verification at each phase using `git fsck` and functionality testing
- Rollback procedures documented and tested for emergency recovery