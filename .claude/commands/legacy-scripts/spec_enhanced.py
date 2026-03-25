#!/usr/bin/env python3
"""
Enhanced Spec Command with AI Template Integration

Integrates Claude Code Templates and AITmpl best practices
for intelligent spec creation with template selection.
"""

import os
import sys
import json
import yaml
import argparse
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from datetime import datetime
import requests
from dataclasses import dataclass

@dataclass
class TemplateRecommendation:
    """Represents a recommended template."""
    source: str  # 'claude-code' or 'aitmpl'
    name: str
    category: str
    relevance_score: float
    description: str
    url: Optional[str] = None

class TemplateManager:
    """Manages AI development templates."""
    
    def __init__(self):
        self.resources_path = Path("/mnt/github/github/.agent-os/resources/ai_templates.yaml")
        self.templates = self._load_templates()
        self.local_cache = Path.home() / ".agent-os" / "template-cache"
        self.local_cache.mkdir(parents=True, exist_ok=True)
        
    def _load_templates(self) -> Dict:
        """Load template configurations."""
        if self.resources_path.exists():
            with open(self.resources_path, 'r') as f:
                return yaml.safe_load(f)
        return {}
    
    def analyze_project_type(self, spec_name: str, description: str = "") -> str:
        """Analyze and determine project type."""
        spec_lower = spec_name.lower()
        desc_lower = description.lower()
        combined = f"{spec_lower} {desc_lower}"
        
        # Detect project type based on keywords
        if any(word in combined for word in ['api', 'rest', 'graphql', 'endpoint']):
            return 'api'
        elif any(word in combined for word in ['react', 'vue', 'angular', 'frontend', 'ui']):
            return 'web'
        elif any(word in combined for word in ['cli', 'command', 'terminal', 'console']):
            return 'cli'
        elif any(word in combined for word in ['data', 'analysis', 'pipeline', 'etl', 'ml']):
            return 'data'
        elif any(word in combined for word in ['test', 'testing', 'qa', 'quality']):
            return 'testing'
        elif any(word in combined for word in ['docker', 'kubernetes', 'deploy', 'ci', 'cd']):
            return 'devops'
        else:
            return 'general'
    
    def get_template_recommendations(self, spec_name: str, 
                                   module_name: str = None,
                                   description: str = "") -> List[TemplateRecommendation]:
        """Get recommended templates for the spec."""
        recommendations = []
        project_type = self.analyze_project_type(spec_name, description)
        
        # Get Claude Code Templates recommendations
        if 'claude_code_templates' in self.templates:
            claude_templates = self.templates['claude_code_templates']
            for category in claude_templates.get('categories', []):
                if self._category_matches_type(category['name'], project_type):
                    for template in category.get('templates', []):
                        recommendations.append(TemplateRecommendation(
                            source='claude-code',
                            name=template,
                            category=category['name'],
                            relevance_score=0.8,
                            description=f"Claude Code {template} template",
                            url=claude_templates['url']
                        ))
        
        # Get AITmpl recommendations
        if 'aitmpl' in self.templates:
            aitmpl = self.templates['aitmpl']
            for feature in aitmpl.get('features', []):
                if self._feature_relevant(feature, project_type):
                    recommendations.append(TemplateRecommendation(
                        source='aitmpl',
                        name=feature['name'],
                        category='Smart Templates',
                        relevance_score=0.9,
                        description=feature['description'],
                        url=aitmpl['url']
                    ))
        
        # Sort by relevance
        recommendations.sort(key=lambda x: x.relevance_score, reverse=True)
        return recommendations[:5]  # Return top 5
    
    def _category_matches_type(self, category: str, project_type: str) -> bool:
        """Check if category matches project type."""
        category_lower = category.lower()
        
        mappings = {
            'api': ['api', 'backend', 'service'],
            'web': ['web', 'frontend', 'react', 'ui'],
            'cli': ['cli', 'tool', 'command'],
            'data': ['data', 'python', 'analysis'],
            'testing': ['test', 'qa'],
            'devops': ['devops', 'docker', 'kubernetes']
        }
        
        for mapping_type, keywords in mappings.items():
            if project_type == mapping_type:
                return any(keyword in category_lower for keyword in keywords)
        
        return False
    
    def _feature_relevant(self, feature: Dict, project_type: str) -> bool:
        """Check if AITmpl feature is relevant."""
        feature_name = feature.get('name', '').lower()
        
        relevance_map = {
            'api': ['api', 'specification', 'endpoint'],
            'web': ['component', 'frontend', 'ui'],
            'data': ['data', 'pipeline', 'analysis'],
            'testing': ['test', 'quality'],
            'devops': ['deploy', 'infrastructure']
        }
        
        keywords = relevance_map.get(project_type, [])
        return any(keyword in feature_name for keyword in keywords)
    
    def fetch_template_content(self, template: TemplateRecommendation) -> Optional[str]:
        """Fetch actual template content (simulated)."""
        # In a real implementation, this would fetch from GitHub or AITmpl
        # For now, return a structured template
        
        if template.source == 'claude-code':
            return self._generate_claude_template(template)
        elif template.source == 'aitmpl':
            return self._generate_aitmpl_template(template)
        
        return None
    
    def _generate_claude_template(self, template: TemplateRecommendation) -> str:
        """Generate a Claude Code template structure."""
        return f"""# {template.name} Template
# Source: Claude Code Templates
# Category: {template.category}

## Overview
This template provides a starting point for {template.name} implementation.

## Structure
```
src/
├── components/
├── services/
├── utils/
└── tests/
```

## Implementation Guidelines
1. Follow the established pattern
2. Customize for project needs
3. Maintain consistency with existing code

## Best Practices
- Use TypeScript/Python type hints
- Write comprehensive tests
- Document all public APIs
- Follow project coding standards

## References
- Template source: {template.url}
- Documentation: See project README
"""
    
    def _generate_aitmpl_template(self, template: TemplateRecommendation) -> str:
        """Generate an AITmpl template structure."""
        return f"""# {template.name}
# Source: AITmpl
# Type: {template.category}

## AI-Optimized Template

### Context Setup
Provide clear context for AI understanding:
- Project scope and constraints
- Technical requirements
- Integration points

### Implementation Pattern
```yaml
structure:
  input_validation:
    - type_checking
    - error_handling
  
  core_logic:
    - business_rules
    - data_processing
  
  output_generation:
    - formatting
    - validation
```

### Quality Checklist
- [ ] Input validation complete
- [ ] Error handling implemented
- [ ] Tests written
- [ ] Documentation updated
- [ ] Code reviewed

### AI Prompting Guide
When implementing this template:
1. Start with the interface/contract
2. Implement core logic incrementally
3. Add error handling
4. Write tests for each component
5. Document as you go

## References
- AITmpl: {template.url}
"""

class EnhancedSpecCommand:
    """Enhanced spec command with template integration."""
    
    def __init__(self):
        self.base_path = Path.cwd()
        self.template_manager = TemplateManager()
        self.specs_base = self.base_path / "specs" / "modules"
        
    def create(self, spec_name: str, module_name: str = None, 
              use_templates: bool = True, description: str = ""):
        """Create a new spec with template recommendations."""
        
        print(f"\n📝 Creating Spec: {spec_name}")
        if module_name:
            print(f"   Module: {module_name}")
        
        # Get template recommendations if enabled
        templates = []
        selected_template = None
        
        if use_templates:
            print("\n🔍 Analyzing project type and finding templates...")
            templates = self.template_manager.get_template_recommendations(
                spec_name, module_name, description
            )
            
            if templates:
                print("\n📚 Recommended Templates:")
                for i, template in enumerate(templates, 1):
                    print(f"  {i}. [{template.source}] {template.name}")
                    print(f"     {template.description}")
                
                # Ask user to select
                print("\n Select a template (1-5) or press Enter to skip: ", end="")
                try:
                    choice = input().strip()
                    if choice and choice.isdigit():
                        idx = int(choice) - 1
                        if 0 <= idx < len(templates):
                            selected_template = templates[idx]
                            print(f"\n✅ Selected: {selected_template.name}")
                except:
                    pass
        
        # Create spec directory
        if module_name:
            spec_dir = self.specs_base / module_name / datetime.now().strftime("%Y-%m-%d") + f"-{spec_name}"
        else:
            spec_dir = self.specs_base / datetime.now().strftime("%Y-%m-%d") + f"-{spec_name}"
        
        spec_dir.mkdir(parents=True, exist_ok=True)
        
        # Create spec.md with template reference
        spec_content = self._generate_spec_content(
            spec_name, module_name, description, selected_template
        )
        
        spec_file = spec_dir / "spec.md"
        spec_file.write_text(spec_content)
        
        # Create tasks.md
        tasks_content = self._generate_tasks_content(spec_name, selected_template)
        tasks_file = spec_dir / "tasks.md"
        tasks_file.write_text(tasks_content)
        
        # If template selected, create template reference
        if selected_template:
            template_content = self.template_manager.fetch_template_content(selected_template)
            if template_content:
                template_file = spec_dir / "template_reference.md"
                template_file.write_text(template_content)
                print(f"   📄 Template reference saved")
        
        # Create sub-specs directory
        sub_specs_dir = spec_dir / "sub-specs"
        sub_specs_dir.mkdir(exist_ok=True)
        
        # Create technical spec
        tech_spec = sub_specs_dir / "technical-spec.md"
        tech_spec_content = self._generate_tech_spec(spec_name, selected_template)
        tech_spec.write_text(tech_spec_content)
        
        print(f"\n✅ Spec created at: {spec_dir}")
        print("\n📋 Next Steps:")
        print("  1. Review and refine the spec")
        print("  2. Add specific requirements")
        print("  3. Use '/task execute' to implement")
        
        if selected_template:
            print(f"\n💡 Template Integration:")
            print(f"  - Template: {selected_template.name}")
            print(f"  - Source: {selected_template.source}")
            print(f"  - Reference: {spec_dir}/template_reference.md")
        
        # Show best practices reminder
        self._show_best_practices()
    
    def _generate_spec_content(self, spec_name: str, module_name: str,
                              description: str, template: Optional[TemplateRecommendation]) -> str:
        """Generate spec.md content."""
        
        template_section = ""
        if template:
            template_section = f"""
## Template Reference

- **Template**: {template.name}
- **Source**: {template.source}
- **Category**: {template.category}
- **URL**: {template.url}

This spec uses the above template as a starting point. The template provides:
- Proven patterns and best practices
- Consistent structure across projects
- AI-optimized implementation guidelines
"""
        
        return f"""# Spec Requirements Document

> Spec: {spec_name}
> Module: {module_name or 'N/A'}
> Created: {datetime.now().isoformat()}
> Status: Planning

## Overview

{description or 'TODO: Add spec overview'}
{template_section}

## User Stories

### Story 1: [Title]

As a [user type], I want to [action], so that [benefit].

**Acceptance Criteria:**
- [ ] Criteria 1
- [ ] Criteria 2
- [ ] Criteria 3

## Spec Scope

1. **[Feature 1]** - [Description]
2. **[Feature 2]** - [Description]
3. **[Feature 3]** - [Description]

## Out of Scope

- [Item 1]
- [Item 2]

## Expected Deliverable

1. [Deliverable 1]
2. [Deliverable 2]
3. [Deliverable 3]

## Success Metrics

- [Metric 1]
- [Metric 2]

## Spec Documentation

- Tasks: @tasks.md
- Technical Specification: @sub-specs/technical-spec.md
{"- Template Reference: @template_reference.md" if template else ""}

## Resources

- Claude Code Templates: https://github.com/davila7/claude-code-templates
- AITmpl: https://www.aitmpl.com/
"""
    
    def _generate_tasks_content(self, spec_name: str, 
                               template: Optional[TemplateRecommendation]) -> str:
        """Generate tasks.md content."""
        
        template_task = ""
        if template:
            template_task = """
- [ ] 1. Review and customize template
  - [ ] 1.1 Analyze template structure
  - [ ] 1.2 Adapt to project conventions
  - [ ] 1.3 Document customizations"""
        
        return f"""# Spec Tasks

These are the tasks to be completed for: {spec_name}

> Created: {datetime.now().isoformat()}
> Status: Ready for Implementation
{template_task}

- [ ] 2. Setup and Configuration
  - [ ] 2.1 Write tests for setup
  - [ ] 2.2 Create project structure
  - [ ] 2.3 Configure dependencies
  - [ ] 2.4 Verify all tests pass

- [ ] 3. Core Implementation
  - [ ] 3.1 Write tests for core functionality
  - [ ] 3.2 Implement main features
  - [ ] 3.3 Add error handling
  - [ ] 3.4 Verify all tests pass

- [ ] 4. Integration
  - [ ] 4.1 Write integration tests
  - [ ] 4.2 Connect components
  - [ ] 4.3 Test end-to-end flow
  - [ ] 4.4 Verify all tests pass

- [ ] 5. Documentation and Polish
  - [ ] 5.1 Write user documentation
  - [ ] 5.2 Add code comments
  - [ ] 5.3 Create examples
  - [ ] 5.4 Final testing

## Template Checklist (if applicable)

- [ ] Template patterns followed
- [ ] Best practices implemented
- [ ] Consistency maintained
- [ ] Quality standards met

## Notes

- Follow TDD approach
- Refer to template_reference.md for patterns
- Use AITmpl prompts for complex sections
"""
    
    def _generate_tech_spec(self, spec_name: str,
                           template: Optional[TemplateRecommendation]) -> str:
        """Generate technical specification."""
        
        template_section = ""
        if template:
            template_section = f"""
## Template Implementation

Following the {template.name} template from {template.source}:

### Pattern Application
- Use template structure as baseline
- Adapt naming conventions to project
- Maintain template's architectural patterns
- Follow template's testing approach

### Customizations
- TODO: Document any deviations from template
- TODO: Explain rationale for changes
"""
        
        return f"""# Technical Specification

This is the technical specification for: {spec_name}

> Created: {datetime.now().isoformat()}
> Version: 1.0.0
{template_section}

## Technical Requirements

### Functional Requirements
- [Requirement 1]
- [Requirement 2]
- [Requirement 3]

### Non-Functional Requirements
- Performance: [Specify targets]
- Security: [Specify requirements]
- Scalability: [Specify needs]

## Architecture

### Components
- [Component 1]: [Description]
- [Component 2]: [Description]

### Data Flow
```
[Input] -> [Processing] -> [Output]
```

## Implementation Approach

### Technology Stack
- Refer to @../../tech-stack.md
- Additional dependencies: [List any]

### Design Patterns
- [Pattern 1]: [Where/why used]
- [Pattern 2]: [Where/why used]

## Testing Strategy

### Unit Tests
- Test individual components
- Mock external dependencies
- Achieve >80% coverage

### Integration Tests
- Test component interactions
- Verify data flow
- Test error scenarios

## Best Practices Applied

### From Claude Code Templates
- Consistent code structure
- Comprehensive error handling
- Clear documentation

### From AITmpl
- AI-optimized code organization
- Clear context boundaries
- Incremental implementation approach

## References
- Template source: {template.url if template else 'N/A'}
- Project standards: @~/.agent-os/standards/
"""
    
    def _show_best_practices(self):
        """Show best practices reminder."""
        print("\n💡 Best Practices Reminder:")
        print("  • Use templates as starting points, not rigid rules")
        print("  • Customize for project-specific needs")
        print("  • Maintain consistency with existing code")
        print("  • Document deviations from templates")
        print("  • Contribute improvements back to template repos")
    
    def list_templates(self):
        """List available templates."""
        print("\n📚 Available AI Development Templates\n")
        
        templates = self.template_manager.templates
        
        if 'claude_code_templates' in templates:
            print("🔷 Claude Code Templates")
            print(f"   URL: {templates['claude_code_templates']['url']}")
            for category in templates['claude_code_templates'].get('categories', []):
                print(f"   • {category['name']}")
                for template in category.get('templates', [])[:3]:
                    print(f"     - {template}")
            print()
        
        if 'aitmpl' in templates:
            print("🔶 AITmpl Templates")
            print(f"   URL: {templates['aitmpl']['url']}")
            for feature in templates['aitmpl'].get('features', []):
                print(f"   • {feature['name']}")
                print(f"     {feature['description']}")
            print()
        
        print("Use '/spec create' with any spec name to get template recommendations")

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        prog='spec-enhanced',
        description='Enhanced spec creation with AI templates'
    )
    
    parser.add_argument('command', nargs='?', default='help',
                       choices=['create', 'templates', 'help'])
    parser.add_argument('spec_name', nargs='?')
    parser.add_argument('module_name', nargs='?')
    parser.add_argument('--description', '-d', help='Spec description')
    parser.add_argument('--no-templates', action='store_true',
                       help='Skip template recommendations')
    
    args = parser.parse_args()
    
    cmd = EnhancedSpecCommand()
    
    if args.command == 'create':
        if not args.spec_name:
            print("❌ Spec name required")
            print("Usage: /spec create SPEC_NAME [MODULE_NAME]")
            sys.exit(1)
        
        cmd.create(
            args.spec_name,
            args.module_name,
            use_templates=not args.no_templates,
            description=args.description or ""
        )
    
    elif args.command == 'templates':
        cmd.list_templates()
    
    else:
        print("""
📝 Enhanced Spec Command with AI Templates

Usage: /spec [subcommand] [options]

Subcommands:
  create SPEC MODULE    Create spec with template recommendations
  templates            List available AI templates
  help                 Show this help

Options:
  --description TEXT   Add spec description for better template matching
  --no-templates      Skip template recommendations

Features:
  • Intelligent template recommendations
  • Claude Code Templates integration
  • AITmpl best practices
  • Automatic project type detection
  • Template customization guidance

Examples:
  /spec create user-auth authentication
  /spec create data-pipeline --description "ETL for sensor data"
  /spec templates
  
Resources:
  • Claude Code Templates: https://github.com/davila7/claude-code-templates
  • AITmpl: https://www.aitmpl.com/
""")

if __name__ == '__main__':
    main()