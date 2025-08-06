# Create Spec Instructions

## Overview
This instruction set guides the creation of detailed specification documents for new features and functionality.

## Process

### Step 1: Gather Requirements
- Collect user requirements and business context
- Clarify scope and boundaries
- Identify technical constraints

### Step 2: Create Specification Structure
- Create spec folder with current date prefix
- Generate main spec.md with all required sections
- Create sub-specifications as needed

### Step 3: Document Technical Approach
- Define implementation strategy
- Identify dependencies and integrations
- Plan testing approach

### Step 4: Create Task Breakdown
- Break down work into manageable tasks
- Estimate effort and dependencies
- Create actionable task list

## File Structure
```
.agent-os/specs/YYYY-MM-DD-spec-name/
├── spec.md
├── tasks.md
└── sub-specs/
    ├── technical-spec.md
    ├── api-spec.md (if needed)
    ├── database-schema.md (if needed)
    └── tests.md
```

## Templates

### Main Spec Template
```markdown
# Spec Requirements Document

> Spec: [SPEC_NAME]
> Created: [DATE]
> Status: Planning

## Overview
[Brief description of what this spec accomplishes]

## User Stories
[User-focused descriptions of functionality]

## Spec Scope
[What is included in this specification]

## Expected Deliverable
[Measurable outcomes and success criteria]
```

This creates a comprehensive specification that can guide implementation work.
