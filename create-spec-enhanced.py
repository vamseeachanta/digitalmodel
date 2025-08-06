#!/usr/bin/env python3
"""
Enhanced /create-spec command with advanced features.
Includes prompt summaries, executive summaries, and mermaid diagrams.
"""

import sys
import os
from datetime import datetime
from pathlib import Path

def create_enhanced_spec_directory(spec_name, module_name=None):
    """Create enhanced specification directory structure."""
    today = datetime.now().strftime("%Y-%m-%d")
    
    if module_name:
        # Module-based organization
        specs_base = Path(".agent-os/specs/modules")
        module_dir = specs_base / module_name
        module_dir.mkdir(parents=True, exist_ok=True)
        spec_folder_name = f"{today}-{spec_name}"
        spec_path = module_dir / spec_folder_name
    else:
        # Traditional organization
        specs_dir = Path(".agent-os/specs")
        specs_dir.mkdir(parents=True, exist_ok=True)
        spec_folder_name = f"{today}-{spec_name}"
        spec_path = specs_dir / spec_folder_name
    
    spec_path.mkdir(exist_ok=True)
    
    # Create enhanced sub-directories
    (spec_path / "sub-specs").mkdir(exist_ok=True)
    (spec_path / "diagrams").mkdir(exist_ok=True)
    (spec_path / "summaries").mkdir(exist_ok=True)
    
    return spec_path

def create_enhanced_spec_file(spec_path, spec_name, variant="enhanced"):
    """Create enhanced spec.md file with all advanced features."""
    
    if variant == "enhanced":
        spec_content = f"""# Spec Requirements Document

> Spec: {spec_name}
> Created: {datetime.now().strftime("%Y-%m-%d")}
> Status: Planning
> Variant: Enhanced

## Executive Summary

### Business Impact
[High-level business value and strategic alignment]

### Key Deliverables
- [Primary deliverable with business value]
- [Secondary deliverable with user impact]
- [Technical deliverable with system improvement]

### Success Metrics
- [Quantifiable success measure]
- [User adoption target]
- [Performance improvement goal]

## Overview

[Comprehensive description of what this spec accomplishes]

## User Stories

### Primary User Workflow
As a [user type], I want to [specific functionality], so that I can [business value].

**Acceptance Criteria:**
- [ ] [Specific testable condition]
- [ ] [User interface requirement]
- [ ] [Performance requirement]

### Secondary User Workflow
As a [user type], I want to [specific functionality], so that I can [value/benefit].

**Acceptance Criteria:**
- [ ] [Data quality requirement]
- [ ] [Analysis capability requirement]
- [ ] [Export/sharing requirement]

## Technical Architecture

### System Design
[High-level architecture description with component interactions]

### Data Flow
```mermaid
graph TD
    A[Input Source] --> B[Processing Layer]
    B --> C[Business Logic]
    C --> D[Presentation Layer]
    D --> E[User Interface]
```

### Integration Points
- **Existing Systems:** [Integration requirements]
- **External APIs:** [API integration details]
- **Data Sources:** [Data source connections]

## Spec Scope

### Phase 1: Core Implementation
1. **[Feature Name]** - [Implementation details and acceptance criteria]
2. **[Feature Name]** - [Implementation details and acceptance criteria]

### Phase 2: Enhanced Features
1. **[Advanced Feature]** - [Enhanced capability description]
2. **[Integration Feature]** - [Cross-system integration details]

## Out of Scope

- [Explicitly excluded functionality with rationale]
- [Future considerations for next iteration]

## Expected Deliverable

### Technical Deliverables
1. [Specific code module or function with test coverage]
2. [Documentation updates with examples]
3. [Performance benchmarks and optimization]

### User Deliverables
1. [User-facing feature with usage examples]
2. [Updated user documentation and tutorials]
3. [Migration guide if applicable]

## Risk Assessment

### Technical Risks
- **[Risk Category]:** [Description and mitigation strategy]

### Business Risks
- **[Risk Category]:** [Impact and contingency plan]

## Dependencies

### Internal Dependencies
- [Existing components required]
- [Agent OS framework components needed]

### External Dependencies
- [Third-party libraries or APIs required]
- [Data source availability requirements]

## Testing Strategy

### Unit Testing
- [Specific test requirements for core functions]
- [Data validation test requirements]

### Integration Testing
- [End-to-end workflow testing requirements]
- [Performance testing requirements]

### User Acceptance Testing
- [User scenario testing requirements]
- [Documentation and tutorial validation]

## Documentation

### Code Documentation
- [API documentation requirements]
- [Inline documentation standards]

### User Documentation
- [Tutorial creation requirements]
- [Example notebooks and use cases]

## Spec Documentation

- **Tasks:** @{spec_path.name}/tasks.md
- **Technical Specification:** @{spec_path.name}/sub-specs/technical-spec.md
- **API Specification:** @{spec_path.name}/sub-specs/api-spec.md
- **Tests Specification:** @{spec_path.name}/sub-specs/tests.md
- **Executive Summary:** @{spec_path.name}/summaries/executive-summary.md
- **System Architecture:** @{spec_path.name}/diagrams/architecture.mmd
"""
    elif variant == "research":
        spec_content = f"""# Research Specification Document

> Spec: {spec_name}
> Created: {datetime.now().strftime("%Y-%m-%d")}
> Status: Research Planning
> Variant: Research-Focused

## Research Objective

[Clear statement of research question or hypothesis to be addressed]

## Background & Literature Review

### Current State of Knowledge
[Summary of existing research and industry practices]

### Knowledge Gaps
[Specific gaps this research will address]

### Research Questions
1. [Primary research question]
2. [Secondary research questions]

## Methodology

### Data Sources
- [Primary data sources and access methods]
- [Secondary data sources for validation]

### Analysis Approach
- [Statistical methods to be employed]
- [Modeling techniques and rationale]

### Validation Strategy
- [How results will be validated]
- [Peer review and reproducibility measures]

## Expected Outcomes

### Research Deliverables
1. [Research findings and insights]
2. [Methodology documentation]
3. [Reproducible analysis notebooks]

### Applications
- [How findings can be applied in practice]
- [Potential impact on decisions]

## Timeline & Milestones

### Phase 1: Data Collection (Weeks 1-2)
- [ ] [Specific milestone]

### Phase 2: Analysis (Weeks 3-4)
- [ ] [Specific milestone]

### Phase 3: Documentation (Weeks 5-6)
- [ ] [Specific milestone]
"""
    else:  # minimal variant
        spec_content = f"""# {spec_name.replace('-', ' ').title()} Specification

> Created: {datetime.now().strftime("%Y-%m-%d")}

## Goal
[One sentence describing what this spec achieves]

## User Story
As a [user type], I want [functionality] so that [benefit].

## Requirements
- [ ] [Core requirement]
- [ ] [Core requirement]
- [ ] [Core requirement]

## Tasks
- [ ] [Implementation task]
- [ ] [Testing task]
- [ ] [Documentation task]

## Definition of Done
- [ ] Code implemented and tested
- [ ] Documentation updated
- [ ] All tests passing
"""
    
    with open(spec_path / "spec.md", "w") as f:
        f.write(spec_content)

def create_enhanced_tasks_file(spec_path, spec_name):
    """Create enhanced tasks.md with detailed breakdown."""
    tasks_content = f"""# Spec Tasks - {spec_name}

> Created: {datetime.now().strftime("%Y-%m-%d")}
> Status: Ready for Implementation

## Task Summary

**Total Estimated Effort:** [X weeks]
**Priority:** High/Medium/Low
**Dependencies:** [List any dependencies]

## Phase 1: Foundation

- [ ] **1. Project Setup** `M`
  - [ ] 1.1 Create module structure
  - [ ] 1.2 Set up testing infrastructure
  - [ ] 1.3 Configure dependencies
  - [ ] 1.4 Initialize documentation
  - [ ] 1.5 Verify foundation tests pass

- [ ] **2. Core Implementation** `L`
  - [ ] 2.1 Write unit tests for core functions
  - [ ] 2.2 Implement main logic
  - [ ] 2.3 Add error handling
  - [ ] 2.4 Implement logging
  - [ ] 2.5 Verify all core tests pass

## Phase 2: Integration

- [ ] **3. System Integration** `M`
  - [ ] 3.1 Write integration tests
  - [ ] 3.2 Implement integrations
  - [ ] 3.3 Test end-to-end workflows
  - [ ] 3.4 Verify integration tests pass

- [ ] **4. User Interface** `M`
  - [ ] 4.1 Write UI tests
  - [ ] 4.2 Implement user interface
  - [ ] 4.3 Add input validation
  - [ ] 4.4 Verify UI tests pass

## Phase 3: Quality & Documentation

- [ ] **5. Documentation** `S`
  - [ ] 5.1 Create API documentation
  - [ ] 5.2 Write user guides
  - [ ] 5.3 Add code documentation
  - [ ] 5.4 Create examples
  - [ ] 5.5 Verify documentation completeness

- [ ] **6. Quality Assurance** `M`
  - [ ] 6.1 Run complete test suite
  - [ ] 6.2 Code review and refactoring
  - [ ] 6.3 Performance testing
  - [ ] 6.4 Security review
  - [ ] 6.5 Final verification

## Definition of Done

- [ ] All code implemented following standards
- [ ] Test coverage >= 80%
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Code review completed
- [ ] Performance benchmarks met

## Effort Estimates

- **XS:** 1-2 hours
- **S:** 1-2 days  
- **M:** 3-5 days
- **L:** 1-2 weeks
- **XL:** 3-4 weeks
"""
    
    with open(spec_path / "tasks.md", "w") as f:
        f.write(tasks_content)

def create_executive_summary(spec_path, spec_name):
    """Create executive summary for business stakeholders."""
    summary_content = f"""# Executive Summary - {spec_name}

> Created: {datetime.now().strftime("%Y-%m-%d")}
> Audience: Business Stakeholders

## Business Case

### Problem Statement
[Clear description of the business problem]

### Proposed Solution
[High-level solution overview]

### Expected Benefits
- **Cost Savings:** [Quantified cost reduction]
- **Time Savings:** [Quantified time reduction]
- **Capability Enhancement:** [New capabilities]

## Investment & Resources

### Development Effort
- **Timeline:** [X weeks/months]
- **Team Requirements:** [Roles needed]

### Success Metrics
- **User Adoption:** [Target metrics]
- **Performance:** [Performance goals]
- **Business Impact:** [Impact measures]

## Risk Assessment

### Key Risks
- **[Risk]:** [Mitigation strategy]

## Recommendation

[Clear recommendation with next steps]

### Go/No-Go Criteria
- [ ] [Critical success factor]
- [ ] [Resource availability]
- [ ] [Stakeholder alignment]
"""
    
    summary_path = spec_path / "summaries"
    summary_path.mkdir(exist_ok=True)
    
    with open(summary_path / "executive-summary.md", "w") as f:
        f.write(summary_content)

def create_mermaid_diagrams(spec_path, spec_name):
    """Create system architecture and workflow diagrams."""
    
    architecture_diagram = f"""# System Architecture - {spec_name}

```mermaid
graph TB
    subgraph "Input Layer"
        A[Data Sources]
        B[User Input]
        C[External APIs]
    end
    
    subgraph "Processing Layer"
        D[Data Validation]
        E[Business Logic]
        F[Integration Layer]
    end
    
    subgraph "Output Layer"
        G[User Interface]
        H[API Endpoints]
        I[Reports/Exports]
    end
    
    A --> D
    B --> D
    C --> D
    D --> E
    E --> F
    F --> G
    F --> H
    F --> I
```

## Data Flow

```mermaid
sequenceDiagram
    participant User
    participant System
    participant Database
    participant External
    
    User->>System: Request
    System->>Database: Query data
    Database-->>System: Return data
    System->>External: Fetch additional data
    External-->>System: Return data
    System->>System: Process data
    System-->>User: Response
```

## Component Integration

```mermaid
classDiagram
    class InputHandler {
        +validate()
        +sanitize()
        +parse()
    }
    
    class ProcessingEngine {
        +execute()
        +transform()
        +analyze()
    }
    
    class OutputGenerator {
        +format()
        +render()
        +export()
    }
    
    InputHandler --> ProcessingEngine
    ProcessingEngine --> OutputGenerator
```
"""
    
    diagrams_path = spec_path / "diagrams"
    diagrams_path.mkdir(exist_ok=True)
    
    with open(diagrams_path / "architecture.mmd", "w") as f:
        f.write(architecture_diagram)

def create_sub_specs(spec_path):
    """Create sub-specification templates."""
    
    # Technical spec
    tech_spec = f"""# Technical Specification

> Created: {datetime.now().strftime("%Y-%m-%d")}

## Technical Requirements
- [Specific technical requirement]

## Implementation Approach
- [Technical approach details]

## Performance Requirements
- [Performance criteria]

## Security Considerations
- [Security requirements]
"""
    
    # API spec
    api_spec = f"""# API Specification

> Created: {datetime.now().strftime("%Y-%m-%d")}

## Endpoints

### GET /api/[resource]
- **Purpose:** [Description]
- **Parameters:** [List]
- **Response:** [Format]

### POST /api/[resource]
- **Purpose:** [Description]
- **Body:** [Schema]
- **Response:** [Format]

## Authentication
- [Authentication method]

## Error Handling
- [Error response format]
"""
    
    # Tests spec
    tests_spec = f"""# Tests Specification

> Created: {datetime.now().strftime("%Y-%m-%d")}

## Test Coverage

### Unit Tests
- [Component tests]
- [Function tests]

### Integration Tests
- [Workflow tests]
- [API tests]

### Performance Tests
- [Load tests]
- [Stress tests]

## Test Data
- [Test data requirements]

## Mocking Strategy
- [Mock requirements]
"""
    
    sub_specs_path = spec_path / "sub-specs"
    sub_specs_path.mkdir(exist_ok=True)
    
    with open(sub_specs_path / "technical-spec.md", "w") as f:
        f.write(tech_spec)
    
    with open(sub_specs_path / "api-spec.md", "w") as f:
        f.write(api_spec)
    
    with open(sub_specs_path / "tests.md", "w") as f:
        f.write(tests_spec)

def main():
    """Main enhanced create-spec command."""
    if len(sys.argv) < 2:
        print("Usage: python create-spec-enhanced.py <spec-name> [module-name] [variant]")
        print("Variants: enhanced (default), research, minimal")
        print("")
        print("Examples:")
        print("  python create-spec-enhanced.py user-auth security enhanced")
        print("  python create-spec-enhanced.py data-analysis research")
        print("  python create-spec-enhanced.py bug-fix minimal")
        return 1
    
    spec_name = sys.argv[1]
    module_name = sys.argv[2] if len(sys.argv) > 2 else None
    variant = sys.argv[3] if len(sys.argv) > 3 else "enhanced"
    
    if variant not in ["enhanced", "research", "minimal"]:
        print(f"Invalid variant: {variant}. Use: enhanced, research, or minimal")
        return 1
    
    try:
        print(f"🚀 Creating {variant} specification: {spec_name}")
        
        # Create directory structure
        spec_path = create_enhanced_spec_directory(spec_name, module_name)
        print(f"📁 Created: {spec_path}")
        
        # Create files based on variant
        create_enhanced_spec_file(spec_path, spec_name, variant)
        
        if variant in ["enhanced", "research"]:
            create_enhanced_tasks_file(spec_path, spec_name)
            create_executive_summary(spec_path, spec_name)
            create_sub_specs(spec_path)
            
        if variant == "enhanced":
            create_mermaid_diagrams(spec_path, spec_name)
        
        print(f"✅ Enhanced specification '{spec_name}' created successfully!")
        print(f"📍 Location: {spec_path}")
        print(f"📄 Main file: {spec_path}/spec.md")
        
        if variant == "enhanced":
            print(f"📊 Executive Summary: {spec_path}/summaries/executive-summary.md")
            print(f"🎯 System Diagrams: {spec_path}/diagrams/architecture.mmd")
            print(f"📋 Enhanced Tasks: {spec_path}/tasks.md")
            print(f"🔧 Sub-specs: {spec_path}/sub-specs/")
        
        print("")
        print("Next steps:")
        print("1. Review and customize the generated specification")
        print("2. Add specific details for your use case")
        print("3. Run: python execute-tasks.py @" + str(spec_path / "tasks.md"))
        
        return 0
        
    except Exception as e:
        print(f"❌ Error creating specification: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
