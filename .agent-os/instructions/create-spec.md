# Create Spec Instructions

## Overview
This instruction set guides the creation of detailed specification documents for new features and functionality.

## Process

### Step 1: Gather Requirements
- Collect user requirements and business context
- Clarify scope and boundaries
- Identify technical constraints

### Step 2: Create Specification Structure
- Create spec folder in `specs/modules/<module>/` following repository pattern
- Generate main spec.md with all required sections
- **MANDATORY**: Create prompt.md with prompt history and curated reuse prompt
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
specs/modules/<module>/spec-name/
├── spec.md
├── prompt.md              # MANDATORY - prompt history and reuse prompt
├── tasks.md
└── sub-specs/
    ├── technical-spec.md
    ├── api-spec.md (if needed)
    ├── database-schema.md (if needed)
    └── tests.md
```

**Note**: Following repository pattern, all specs MUST be created in `specs/modules/<module>/` structure.

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

### MANDATORY Prompt Template
Every spec MUST include a `prompt.md` file with this structure:
```markdown
# Prompt History and Reuse Template

> Created: [DATE]  
> Spec: [SPEC_NAME]  
> Location: specs/modules/[MODULE]/[SPEC_NAME]/

## Curated Reuse Prompt

### For Similar Specifications
```
[Refined prompt that can be reused for similar spec creation, incorporating lessons learned]
```

### Key Parameters
- **Module**: [TARGET_MODULE]
- **Scope**: [COMPLEXITY_LEVEL] 
- **Integration Points**: [INTEGRATION_REQUIREMENTS]
- **Success Criteria**: [MEASURABLE_OUTCOMES]

### Reuse Guidelines
- [Guidelines for when and how to reuse this prompt]
- [Customization points for different contexts]
- [Common variations and their applications]

## Lessons Learned
- [Key insights from creating this specification]
- [What worked well in the prompt formulation]
- [What could be improved for future similar requests]

## Prompt History (Reverse Chronological)

### Latest Request - [DATE]
```
[Most recent user prompt or modification request]
```

### Original Request - [DATE]
```
[EXACT original user prompt that initiated this spec creation]
```

### Context at Time of Creation
- **Session Context**: [Brief description of session context]
- **Repository State**: [Current state, recent changes, etc.]
- **Related Work**: [Any related specifications or ongoing work]
```

## MANDATORY Requirements

### For Every /create-spec Command
1. **Repository Pattern**: Spec MUST be created in `specs/modules/<module>/`
2. **Prompt Documentation**: MUST create prompt.md with complete history
3. **Template Compliance**: All files must follow established templates
4. **Cross-References**: Update relevant module READMEs and indices

This creates a comprehensive specification with reusable prompt intelligence for future work.
