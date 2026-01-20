#!/usr/bin/env python3
"""
Self-contained /create-spec slash command entry point.
Works immediately after git clone with no external dependencies.

MANDATORY: All specs MUST be created in specs/modules/[module-name] structure.
This is enforced across all repositories.

🚀 MANDATORY REQUIREMENTS:
- Auto-assign specialized agents based on task domain
- Execute tasks using subagents for each component
- Utilize parallel processing for all independent operations
- Achieve >3x speed improvement through parallelization

AGENT ASSIGNMENT:
- Primary agent: Based on main module (OrcaFlex, AQWA, CAD, etc.)
- Secondary agents: Supporting functions (Documentation, Testing, DevOps)
- Subagents: Component-specific execution (parallel groups)

PARALLEL EXECUTION:
- Group A: Spec writing, diagram generation
- Group B: Code templates, test creation
- Group C: API design, integration setup
"""

import sys
import os
from datetime import datetime
from pathlib import Path

def create_spec_directory(spec_name, module_name):
    """Create specification directory structure.
    
    MANDATORY: Module name is required. All specs must be in specs/modules/[module-name]/
    """
    today = datetime.now().strftime("%Y-%m-%d")
    spec_folder_name = f"{today}-{spec_name}"
    
    # MANDATORY: Module-based organization only
    specs_base = Path("specs/modules")  # Changed from .agent-os/specs
    module_dir = specs_base / module_name
    module_dir.mkdir(parents=True, exist_ok=True)
    
    spec_path = module_dir / spec_folder_name
    spec_path.mkdir(exist_ok=True)
    
    # Create sub-specs directory
    sub_specs_path = spec_path / "sub-specs"
    sub_specs_path.mkdir(exist_ok=True)
    
    return spec_path

def create_spec_file(spec_path, spec_name):
    """Create main spec.md file."""
    spec_content = f"""# Spec Requirements Document

> Spec: {spec_name}
> Created: {datetime.now().strftime("%Y-%m-%d")}
> Status: Planning

## Overview

[Brief description of what this spec accomplishes]

## User Stories

### [Story Title]

As a [user type], I want to [action], so that [benefit].

[Detailed workflow description]

## Spec Scope

1. **[Feature Name]** - [One sentence description]
2. **[Feature Name]** - [One sentence description]

## Out of Scope

- [Excluded functionality]

## Expected Deliverable

1. [Testable outcome]
2. [Testable outcome]

## Spec Documentation

- Tasks: @{spec_path.name}/tasks.md
- Technical Specification: @{spec_path.name}/sub-specs/technical-spec.md
- Tests Specification: @{spec_path.name}/sub-specs/tests.md
"""
    
    with open(spec_path / "spec.md", "w") as f:
        f.write(spec_content)

def create_tasks_file(spec_path, spec_name):
    """Create tasks.md file."""
    tasks_content = f"""# Spec Tasks

These are the tasks to be completed for the spec detailed in @{spec_path.name}/spec.md

> Created: {datetime.now().strftime("%Y-%m-%d")}
> Status: Ready for Implementation

## Tasks

- [ ] 1. **[Major Task]** - [Description]
  - [ ] 1.1 Write tests for [component]
  - [ ] 1.2 [Implementation step]
  - [ ] 1.3 [Implementation step] 
  - [ ] 1.4 Verify all tests pass

- [ ] 2. **[Major Task]** - [Description]
  - [ ] 2.1 Write tests for [component]
  - [ ] 2.2 [Implementation step]
  - [ ] 2.3 [Implementation step]
  - [ ] 2.4 Verify all tests pass
"""
    
    with open(spec_path / "tasks.md", "w") as f:
        f.write(tasks_content)

def create_sub_specs(spec_path):
    """Create sub-specification files."""
    
    # Technical spec
    tech_spec_content = f"""# Technical Specification

This is the technical specification for the spec detailed in @{spec_path.name}/spec.md

> Created: {datetime.now().strftime("%Y-%m-%d")}
> Version: 1.0.0

## Technical Requirements

- [Specific technical requirement]
- [Specific technical requirement]

## Approach Options

**Option A:** [Description] (Selected)
- Pros: [List advantages]
- Cons: [List disadvantages]

**Rationale:** [Explanation of choice]

## External Dependencies

- **[Library Name]** - [Purpose and justification]

## Performance Considerations

- [Performance requirement or optimization]
"""
    
    with open(spec_path / "sub-specs" / "technical-spec.md", "w") as f:
        f.write(tech_spec_content)
    
    # Tests spec
    tests_content = f"""# Tests Specification

This is the tests coverage details for the spec detailed in @{spec_path.name}/spec.md

> Created: {datetime.now().strftime("%Y-%m-%d")}
> Version: 1.0.0

## Test Coverage

### Unit Tests

**[Component Name]**
- [Test description]
- [Test description]

### Integration Tests

**[Feature Name]**
- [Scenario description]
- [Scenario description]

### Mocking Requirements

- **[Service Name]:** [Mock strategy]
"""
    
    with open(spec_path / "sub-specs" / "tests.md", "w") as f:
        f.write(tests_content)

def main():
    """Main create-spec command."""
    if len(sys.argv) < 3:  # Changed from < 2 to < 3 - module is now required
        print("Usage: python create-spec.py <spec-name> <module-name>")
        print("")
        print("MANDATORY: Module name is required!")
        print("All specs must be created in: specs/modules/[module-name]/")
        print("")
        print("Examples:")
        print("  python create-spec.py user-authentication auth")
        print("  python create-spec.py api-integration core")
        print("  python create-spec.py data-validation utils")
        return 1
    
    spec_name = sys.argv[1]
    module_name = sys.argv[2]  # Now required, not optional
    
    try:
        print(f"Creating specification: {spec_name} in module: {module_name}")
        
        # Create directory structure
        spec_path = create_spec_directory(spec_name, module_name)
        print(f"📁 Created: {spec_path}")
        
        # Create files
        create_spec_file(spec_path, spec_name)
        create_tasks_file(spec_path, spec_name)
        create_sub_specs(spec_path)
        
        print(f"✅ Specification '{spec_name}' created successfully!")
        print(f"📍 Location: {spec_path}")
        print(f"📄 Main file: {spec_path}/spec.md")
        
        return 0
        
    except Exception as e:
        print(f"❌ Error creating specification: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
