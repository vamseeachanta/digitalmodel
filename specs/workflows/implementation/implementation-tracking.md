# Implementation Tracking Workflow

## Overview
Process for AI assistants to document iterative development progress, decisions, and changes during implementation.

## Tracking Document Structure

### File Naming Convention
`implementation-logs/issue-[number]-implementation-log.md`

### Document Template
```markdown
# Implementation Log: Issue #[number] - [Title]

**Start Date**: [YYYY-MM-DD]
**Current Status**: In Progress | Blocked | Under Review | Complete
**Last Updated**: [YYYY-MM-DD HH:MM]

## Progress Summary
- **Completion**: [X]% complete
- **Current Phase**: [Planning | Development | Testing | Documentation]
- **Next Milestone**: [Description and target date]

## Implementation Sessions

### Session [N]: [YYYY-MM-DD HH:MM]
**Duration**: [X hours]
**Objective**: [What was planned for this session]

#### Work Completed
- [x] [Specific task completed]
- [x] [Another completed task]
- [ ] [Task started but not finished]

#### Code Changes
- **Files Modified**: 
  - `path/to/file1.py` - [Brief description of changes]
  - `path/to/file2.py` - [Brief description of changes]
- **Files Created**:
  - `path/to/newfile.py` - [Purpose and description]
- **Files Deleted**:
  - `path/to/oldfile.py` - [Reason for deletion]

#### Decisions Made
- **Decision**: [Description of decision]
  - **Rationale**: [Why this decision was made]
  - **Alternatives Considered**: [Other options and why they were rejected]
  - **Impact**: [How this affects the implementation]

#### Issues Encountered
- **Issue**: [Description of problem]
  - **Impact**: [How it affected progress]
  - **Resolution**: [How it was resolved or current status]
  - **Prevention**: [How to avoid similar issues]

#### Tests Added/Modified
- [ ] Unit tests for [component]
- [ ] Integration tests for [feature]
- [ ] Mock implementations for [external dependency]

#### Next Session Plan
- [ ] [Task to complete next]
- [ ] [Another planned task]
- [ ] [Additional work items]

---
```

## Progress Tracking Methods

### 1. Task Completion Tracking
```markdown
## Task Progress

### Completed Tasks ✅
- [x] Task 1: [Description] - Completed [date]
- [x] Task 2: [Description] - Completed [date]

### In Progress Tasks 🔄
- [ ] Task 3: [Description] - Started [date], [X]% complete
- [ ] Task 4: [Description] - Blocked by [issue]

### Pending Tasks ⏳
- [ ] Task 5: [Description] - Scheduled for [date]
- [ ] Task 6: [Description] - Waiting for [dependency]
```

### 2. Code Change Tracking
```markdown
## Code Changes Summary

### New Components
| Component | Purpose | Status | Tests |
|-----------|---------|---------|-------|
| `module/new_feature.py` | [Purpose] | Complete | ✅ |
| `tests/test_new_feature.py` | [Test coverage] | Complete | ✅ |

### Modified Components
| Component | Changes | Reason | Impact |
|-----------|---------|---------|---------|
| `existing/module.py` | [Description] | [Reason] | [Impact] |

### Configuration Changes
| File | Changes | Reason |
|------|---------|---------|
| `config.yml` | [Description] | [Reason] |
```

### 3. Decision Log
```markdown
## Decision Log

### Decision D001: [Title]
- **Date**: [YYYY-MM-DD]
- **Context**: [Why decision was needed]
- **Decision**: [What was decided]
- **Rationale**: [Why this was chosen]
- **Consequences**: [Expected outcomes]
- **Status**: Active | Superseded | Reversed

### Decision D002: [Title]
[Same format as above]
```

## Iteration Management

### Daily/Session Updates
```markdown
## Daily Update - [YYYY-MM-DD]

### Yesterday's Accomplishments
- [What was completed]
- [Key progress made]

### Today's Goals
- [What will be worked on]
- [Specific objectives]

### Blockers/Issues
- [Current obstacles]
- [Help needed]

### Estimated Completion
- [Updated timeline]
- [Confidence level]
```

### Weekly Progress Reports
```markdown
## Weekly Progress Report - Week of [YYYY-MM-DD]

### Major Accomplishments
- [Key deliverables completed]
- [Significant milestones reached]

### Code Statistics
- **Lines Added**: [number]
- **Lines Modified**: [number]
- **Files Changed**: [number]
- **Tests Added**: [number]

### Challenges Overcome
- [Problem] → [Solution]
- [Challenge] → [Resolution]

### Next Week's Focus
- [Primary objectives]
- [Key deliverables]

### Risk Updates
- [New risks identified]
- [Risk mitigation progress]
```

## Integration with Tools

### GitHub Integration
```markdown
## GitHub Integration

### Commits Related to This Issue
- `[commit-hash]`: [commit message]
- `[commit-hash]`: [commit message]

### Pull Requests
- PR #[number]: [title] - [status]

### Issue Updates
- Updated labels: [new labels]
- Updated milestone: [milestone]
- Updated assignees: [assignees]
```

### Testing Integration
```markdown
## Testing Progress

### Test Coverage
- **Current Coverage**: [X]%
- **Target Coverage**: [Y]%
- **New Tests Added**: [number]

### Test Results
- **Unit Tests**: [passed/total] passing
- **Integration Tests**: [passed/total] passing
- **Performance Tests**: [status]

### Test Failures
- [Test name]: [failure reason and status]
```

## Quality Gates

### Session Completion Checklist
- [ ] Progress clearly documented
- [ ] Code changes tracked
- [ ] Decisions recorded with rationale
- [ ] Issues and resolutions noted
- [ ] Next steps planned
- [ ] Tests updated/added as needed

### Milestone Completion Checklist
- [ ] All tasks for milestone completed
- [ ] Code changes reviewed and tested
- [ ] Documentation updated
- [ ] Specification alignment verified
- [ ] Risk assessment updated

## Communication Guidelines

### Status Communication
- Update GitHub issue with progress summary
- Link to implementation log in issue comments
- Use appropriate labels and milestones
- Notify stakeholders of significant changes or blockers

### Documentation Standards
- Use clear, concise language
- Include sufficient context for future reference
- Link to relevant code, issues, and specifications
- Maintain consistent formatting and structure