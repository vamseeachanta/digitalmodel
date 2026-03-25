# Issue Planning Workflow

## Overview
Structured approach for AI assistants to analyze and plan implementation of GitHub issues.

## Process Steps

### 1. Issue Analysis
```markdown
## Issue Analysis for #[issue-number]

### Issue Summary
- **Title**: [GitHub issue title]
- **Type**: Bug Fix | Feature | Enhancement | Infrastructure
- **Priority**: Critical | High | Medium | Low
- **Labels**: [GitHub labels]

### Requirements Analysis
- **Functional Requirements**: What needs to be implemented
- **Non-Functional Requirements**: Performance, security, etc.
- **Acceptance Criteria**: How to validate completion
- **Dependencies**: Other issues, external systems, etc.

### Technical Analysis
- **Affected Components**: Which modules/files will change
- **Integration Points**: How this affects other parts of system
- **Risk Assessment**: Potential issues or complications
- **Testing Requirements**: How to validate the implementation
```

### 2. Implementation Planning
```markdown
## Implementation Plan

### Approach
- **Strategy**: High-level approach to solving the issue
- **Architecture Changes**: Any structural modifications needed
- **Technology Choices**: Libraries, frameworks, tools to use

### Task Breakdown
- [ ] Task 1: [Description]
- [ ] Task 2: [Description]
- [ ] Task 3: [Description]

### Timeline Estimate
- **Development**: [X days/hours]
- **Testing**: [X days/hours]
- **Documentation**: [X days/hours]
- **Total**: [X days/hours]

### Dependencies
- **Blocking Issues**: Issues that must be completed first
- **External Dependencies**: Third-party libraries, APIs, etc.
- **Team Dependencies**: Reviews, approvals needed
```

### 3. Risk Assessment
```markdown
## Risk Assessment

### Technical Risks
| Risk | Probability | Impact | Mitigation |
|------|-------------|---------|------------|
| [Risk description] | High/Med/Low | High/Med/Low | [Mitigation strategy] |

### Implementation Risks
| Risk | Probability | Impact | Mitigation |
|------|-------------|---------|------------|
| [Risk description] | High/Med/Low | High/Med/Low | [Mitigation strategy] |

### Contingency Plans
- **Plan A**: [Primary approach]
- **Plan B**: [Fallback if Plan A fails]
- **Plan C**: [Minimal viable solution]
```

### 4. Specification Check
```markdown
## Specification Alignment

### Existing Specifications
- [ ] Check `specs/modules/` for related specifications
- [ ] Check `specs/enhancements/` for similar work
- [ ] Check `specs/bugfixes/` for related issues

### New Specification Needed
- [ ] Yes - Create new specification using appropriate template
- [ ] No - Implementation aligns with existing specifications

### Specification Updates Required
- [ ] Update existing specification: [specification name]
- [ ] Add new section to specification: [specification name]
- [ ] No updates needed
```

## Planning Template

### File Naming Convention
Save planning documents as:
`implementation-plans/issue-[number]-[short-description].md`

### Template Structure
```markdown
# Implementation Plan: Issue #[number] - [Title]

**Date**: [YYYY-MM-DD]
**Assigned to**: [AI Assistant or Developer]
**Estimated Effort**: [X hours/days]
**Target Completion**: [YYYY-MM-DD]

## Issue Analysis
[Use Issue Analysis template above]

## Implementation Plan
[Use Implementation Plan template above]

## Risk Assessment
[Use Risk Assessment template above]

## Specification Alignment
[Use Specification Check template above]

## Next Steps
1. [First action item]
2. [Second action item]
3. [Third action item]

## Notes
[Any additional considerations or context]
```

## Quality Gates

Before proceeding to implementation:
- [ ] Issue requirements are clearly understood
- [ ] Implementation approach is defined
- [ ] Risks are identified and mitigated
- [ ] Specifications are aligned
- [ ] Timeline is realistic
- [ ] Dependencies are identified

## Integration Points

### With GitHub
- Link planning document in issue comments
- Update issue labels based on analysis
- Create milestone if needed
- Assign issue to appropriate project

### With Specifications
- Reference relevant specifications
- Create new specifications if needed
- Update existing specifications
- Maintain traceability

### With Development Process
- Follow code standards in `.ai/code-guidance/`
- Use testing approaches from specifications
- Follow commit message conventions
- Plan for code review process