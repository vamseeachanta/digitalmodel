#!/usr/bin/env python3
"""
Navigation System Generator
AI-Friendly Documentation Organization - Task 4 Implementation

This module generates navigation systems and cross-reference mappings
for the reorganized documentation structure.
"""

import os
import json
import yaml
import re
from pathlib import Path
from typing import Dict, List, Any, Set, Tuple, Optional
from datetime import datetime
import hashlib


class NavigationGenerator:
    """Generates master navigation and taxonomy systems."""
    
    def __init__(self, docs_root: str = "K:\\github\\digitalmodel\\docs"):
        """Initialize navigation generator."""
        self.docs_root = Path(docs_root)
        self.category_descriptions = {
            "software": "Software tools, libraries, and development environments used in offshore engineering",
            "domains": "Engineering domain knowledge including ship design, offshore structures, and marine analysis",
            "modules": "Analysis modules and computational tools for specific engineering calculations",
            "guides": "Tutorials, best practices, and procedural documentation", 
            "references": "Standards, specifications, APIs, and reference materials",
            "legacy": "Legacy documentation preserved for historical reference"
        }
        
    def discover_content_structure(self) -> Dict[str, Any]:
        """Discover and analyze current documentation structure."""
        structure = {
            "categories": {},
            "total_files": 0,
            "markdown_files": 0,
            "last_updated": datetime.now().isoformat()
        }
        
        for category_dir in self.docs_root.iterdir():
            if category_dir.is_dir() and not category_dir.name.startswith('.'):
                category_info = self.analyze_category(category_dir)
                structure["categories"][category_dir.name] = category_info
                structure["total_files"] += category_info["file_count"]
                structure["markdown_files"] += category_info["markdown_count"]
                
        return structure
        
    def analyze_category(self, category_path: Path) -> Dict[str, Any]:
        """Analyze a single category directory."""
        info = {
            "path": str(category_path.relative_to(self.docs_root)),
            "subcategories": {},
            "file_count": 0,
            "markdown_count": 0,
            "description": self.category_descriptions.get(category_path.name, "")
        }
        
        # Count files and analyze subcategories
        for item in category_path.rglob("*"):
            if item.is_file():
                info["file_count"] += 1
                if item.suffix == ".md":
                    info["markdown_count"] += 1
                    
        # Analyze immediate subdirectories
        for subdir in category_path.iterdir():
            if subdir.is_dir():
                subcat_info = {
                    "path": str(subdir.relative_to(self.docs_root)),
                    "file_count": len([f for f in subdir.rglob("*") if f.is_file()]),
                    "markdown_count": len([f for f in subdir.rglob("*.md") if f.is_file()])
                }
                info["subcategories"][subdir.name] = subcat_info
                
        return info
        
    def generate_master_readme(self) -> str:
        """Generate the master docs/README.md file."""
        structure = self.discover_content_structure()
        
        readme_content = f"""# Digital Model Documentation

> Comprehensive documentation for offshore engineering analysis and software tools
> 
> Last Updated: {datetime.now().strftime('%Y-%m-%d')}
> Total Files: {structure['total_files']} | Markdown Files: {structure['markdown_files']}

## Quick Start

This documentation is organized into logical categories for both human users and AI assistants:

- **🔧 [Software Tools](software/)** - Development environments, analysis software, and libraries
- **🏗️ [Engineering Domains](domains/)** - Ship design, offshore structures, marine analysis
- **⚙️ [Analysis Modules](modules/)** - Computational tools and analysis algorithms  
- **📚 [Guides](guides/)** - Tutorials, best practices, and procedures
- **📖 [References](references/)** - Standards, APIs, and reference materials

## Documentation Structure

### Category Overview

"""
        
        # Generate category sections
        for category_name, category_info in structure["categories"].items():
            if category_name in self.category_descriptions:
                readme_content += f"""#### {category_name.replace('-', ' ').title()}
- **Location**: [`{category_info['path']}/`]({category_info['path']}/)
- **Description**: {category_info['description']}
- **Content**: {category_info['markdown_count']} documents, {category_info['file_count']} total files
- **Subcategories**: {len(category_info['subcategories'])} areas

"""

        readme_content += """## Navigation Guide

### For Developers and Engineers

1. **Start with [Guides](guides/)** for tutorials and getting started
2. **Browse [Software](software/)** for tool-specific documentation
3. **Explore [Domains](domains/)** for engineering knowledge
4. **Reference [Modules](modules/)** for technical implementations
5. **Check [References](references/)** for standards and specifications

### Quick Access Patterns

- **By Software Tool**: `software/{tool-name}/`
- **By Engineering Domain**: `domains/{domain-name}/`
- **By Analysis Type**: `modules/{analysis-type}/`
- **By Document Type**: `guides/{guide-type}/`

## For AI Assistants

### Content Discovery Strategy

This documentation uses hierarchical organization with consistent metadata:

```yaml
# All markdown files include YAML frontmatter
title: "Document Title"
category: "primary-category"
subcategory: "specific-area"  
tags: ["keyword1", "keyword2"]
complexity: "beginner|intermediate|advanced"
industry_standards: ["API", "DNV", "ABS"]
related: ["related-topic-1", "related-topic-2"]
```

### Navigation Patterns

1. **Hierarchical Browse**: Start with category index files (`_index.md`)
2. **Tag-Based Discovery**: Use frontmatter tags for content filtering
3. **Cross-Reference Following**: Follow `related` metadata for topic exploration
4. **Standard Compliance**: Use `industry_standards` for compliance-focused searches

### AI-Friendly Features

- **Consistent Metadata**: All documents have structured YAML frontmatter
- **Cross-References**: Explicit linking between related topics
- **Categorization**: Clear hierarchical organization
- **Content Tagging**: Comprehensive tag system for discovery
- **Complexity Indicators**: Beginner/intermediate/advanced classifications

## Content Guidelines

### For Contributors

- Place content in appropriate category directories
- Include complete YAML frontmatter in all markdown files
- Use relative links for internal references
- Follow naming conventions: `kebab-case-filenames.md`
- Update cross-references when adding related content

### Quality Standards

- **Technical Accuracy**: All content verified against industry standards
- **Cross-Platform Compatibility**: Examples work across development environments  
- **Comprehensive Coverage**: From beginner tutorials to advanced implementations
- **Regular Updates**: Documentation maintained alongside software changes

## Getting Help

- **Issues**: Report documentation issues via GitHub issues
- **Contributions**: Follow contribution guidelines in project root
- **Questions**: Use discussion forums for technical questions
- **Updates**: Check git history for recent documentation changes

---

*This documentation structure is optimized for both human navigation and AI assistant comprehension. The hierarchical organization, consistent metadata, and cross-reference system enable efficient content discovery and contextual understanding.*
"""

        return readme_content
        
    def generate_category_index(self, category_name: str) -> str:
        """Generate index file for a specific category."""
        category_path = self.docs_root / category_name
        
        if not category_path.exists():
            return f"# {category_name.replace('-', ' ').title()}\n\nCategory not found."
            
        category_info = self.analyze_category(category_path)
        
        # Create category title and description
        title_mapping = {
            "software": "Software Tools",
            "domains": "Engineering Domains", 
            "modules": "Analysis Modules",
            "guides": "Guides",
            "references": "References",
            "legacy": "Legacy Documentation"
        }
        title = title_mapping.get(category_name, category_name.replace('-', ' ').title())
        description = self.category_descriptions.get(category_name, "Documentation category")
        
        index_content = f"""# {title}

> {description}
>
> Total Files: {category_info['file_count']} | Documents: {category_info['markdown_count']} | Subcategories: {len(category_info['subcategories'])}

## Overview

{self.generate_category_overview(category_name, category_info)}

## Available Content

### Subcategories

"""
        
        # List subcategories with content summaries
        for subcat_name, subcat_info in category_info["subcategories"].items():
            subcat_title = subcat_name.replace('-', ' ').title()
            index_content += f"""#### [{subcat_title}]({subcat_name}/)
- **Files**: {subcat_info['markdown_count']} documents
- **Location**: `{subcat_info['path']}/`
- **Quick Access**: Browse [all {subcat_name} content]({subcat_name}/)

"""

        # Add navigation help
        index_content += f"""## Navigation

### Quick Links
- [📁 Browse all {category_name} content](./)
- [🏠 Return to documentation home](../README.md)
- [🔍 Search across categories](../README.md#navigation-guide)

### For AI Assistants

This category contains {category_info['markdown_count']} documented topics in {len(category_info['subcategories'])} subcategories. All files include structured metadata for content discovery.

**Content Discovery Pattern:**
```
{category_name}/
├── {list(category_info['subcategories'].keys())[0] if category_info['subcategories'] else 'subcategory'}/
│   ├── topic-1.md (with YAML frontmatter)
│   └── topic-2.md (with cross-references)
└── _index.md (this file)
```

**Metadata Schema:**
- `category`: "{category_name}"
- `subcategory`: specific area within {category_name}
- `tags`: searchable keywords
- `complexity`: difficulty level
- `related`: cross-references to other topics

---

*Last updated: {datetime.now().strftime('%Y-%m-%d')} | Part of AI-Friendly Documentation System*
"""

        return index_content
        
    def generate_category_overview(self, category_name: str, category_info: Dict[str, Any]) -> str:
        """Generate category-specific overview content."""
        overviews = {
            "software": """This section contains documentation for software tools, development environments, and libraries used in offshore engineering analysis. Content includes installation guides, tutorials, API references, and integration examples.

**Key Areas:**
- **ANSYS**: Finite element analysis and structural simulation
- **OrcaFlex**: Dynamic analysis of offshore systems
- **Python**: Libraries and modules for analysis and automation
- **Development Tools**: IDEs, version control, and build systems""",

            "domains": """Engineering domain knowledge covering the core disciplines of offshore and marine engineering. Content ranges from fundamental principles to advanced analysis techniques.

**Key Areas:**
- **Ship Design**: Stability, hydrodynamics, and structural analysis
- **Offshore Structures**: Platform design, mooring systems, and installation
- **Marine Analysis**: Environmental loading, response analysis, and optimization
- **Cathodic Protection**: Corrosion prevention and monitoring systems""",

            "modules": """Analysis modules and computational tools for specific engineering calculations. These modules provide reusable components for complex analysis workflows.

**Key Areas:**
- **Analysis**: Core calculation engines and algorithms
- **Visualization**: Plotting, reporting, and data presentation
- **Data Processing**: Import/export, transformation, and validation
- **Integration**: Workflow orchestration and tool connectivity""",

            "guides": """Practical guidance including tutorials, best practices, troubleshooting, and procedural documentation for both new and experienced users.

**Key Areas:**
- **Tutorials**: Step-by-step learning materials
- **Best Practices**: Industry-proven methodologies
- **Troubleshooting**: Problem diagnosis and resolution
- **Workflows**: End-to-end process documentation""",

            "references": """Reference materials including industry standards, API documentation, specifications, and glossaries for quick lookup and compliance verification.

**Key Areas:**
- **Standards**: API, DNV, ABS, and other industry specifications
- **APIs**: Software interface documentation
- **Glossary**: Technical terms and definitions
- **Specifications**: Technical requirements and guidelines"""
        }
        
        return overviews.get(category_name, f"Documentation for {category_name.replace('-', ' ')} related topics.")
        
    def generate_taxonomy(self) -> Dict[str, Any]:
        """Generate complete documentation taxonomy."""
        structure = self.discover_content_structure()
        
        taxonomy = {
            "version": "1.0",
            "generated": datetime.now().isoformat(),
            "categories": structure["categories"],
            "total_files": structure["total_files"],
            "markdown_files": structure["markdown_files"],
            "last_updated": structure["last_updated"],
            "navigation_schema": {
                "category_pattern": "{category}/",
                "subcategory_pattern": "{category}/{subcategory}/",
                "file_pattern": "{category}/{subcategory}/{filename}.md",
                "index_pattern": "{category}/_index.md"
            },
            "metadata_schema": {
                "required_fields": ["title", "category", "tags", "last_updated"],
                "optional_fields": ["subcategory", "complexity", "related", "industry_standards"],
                "tag_conventions": ["category-based", "topic-based", "tool-based"],
                "complexity_levels": ["beginner", "intermediate", "advanced"]
            }
        }
        
        return taxonomy
        
    def generate_ai_assistant_guidance(self) -> str:
        """Generate specific guidance for AI assistants."""
        return """## For AI Assistants

### Documentation Structure Understanding

This documentation follows a hierarchical, metadata-rich structure optimized for AI comprehension:

1. **Predictable Organization**: Content organized by logical categories and subcategories
2. **Structured Metadata**: YAML frontmatter provides context and relationships
3. **Cross-Reference System**: Explicit linking between related topics
4. **Consistent Naming**: Kebab-case filenames and directory names

### Content Discovery Strategies

#### Method 1: Hierarchical Navigation
```
1. Start with category overview (software/, domains/, modules/)
2. Browse subcategory index files (_index.md)
3. Follow to specific topic documents
4. Use cross-references for related topics
```

#### Method 2: Metadata-Driven Search
```yaml
# Use frontmatter for targeted discovery
category: "software"           # Primary classification
subcategory: "ansys"          # Specific area
tags: ["fem", "analysis"]     # Searchable keywords
complexity: "intermediate"     # Skill level required
industry_standards: ["API"]   # Compliance context
related: ["topic-1", "topic-2"] # Cross-references
```

#### Method 3: Cross-Reference Following
```
1. Identify topic of interest
2. Check 'related' metadata for connected topics
3. Follow cross-reference links in content
4. Build understanding through topic relationships
```

### Navigation Patterns for Common Tasks

**Software Tool Documentation:**
- Path: `software/{tool-name}/`
- Look for: installation.md, getting-started.md, api-reference.md
- Cross-refs: Related domain applications, integration guides

**Engineering Domain Knowledge:**
- Path: `domains/{domain-name}/`
- Look for: overview.md, principles.md, analysis-methods.md
- Cross-refs: Related software tools, analysis modules

**Analysis Implementation:**
- Path: `modules/{module-name}/`
- Look for: implementation.md, examples.md, testing.md
- Cross-refs: Domain applications, software dependencies

### Metadata Usage Guidelines

1. **Category Classification**: Use for high-level content organization
2. **Tag Filtering**: Combine tags for precise content discovery
3. **Complexity Routing**: Match content difficulty to user expertise
4. **Standard Compliance**: Use industry_standards for compliance contexts
5. **Relationship Mapping**: Follow related fields for comprehensive coverage

### Examples of Effective AI Navigation

```python
# Example 1: Find ANSYS documentation
category = "software"
subcategory = "ansys"
path = f"docs/{category}/{subcategory}/"

# Example 2: Discover ship design analysis tools
domain = "ship-design"
related_software = get_cross_references(f"domains/{domain}/")
analysis_modules = get_cross_references(f"modules/analysis/")

# Example 3: Build learning path
beginner_topics = filter_by_complexity("beginner")
learning_sequence = order_by_dependencies(beginner_topics)
```

This structure enables both browsing-based discovery and programmatic content analysis for comprehensive AI assistance."""

    def generate_complete_navigation(self) -> Dict[str, Any]:
        """Generate complete navigation system."""
        print("Generating complete navigation system...")
        
        results = {
            "master_readme": self.generate_master_readme(),
            "category_indices": {},
            "taxonomy": self.generate_taxonomy(),
            "ai_guidance": self.generate_ai_assistant_guidance(),
            "generation_timestamp": datetime.now().isoformat()
        }
        
        # Generate category indices for existing categories
        for category_dir in self.docs_root.iterdir():
            if category_dir.is_dir() and not category_dir.name.startswith('.'):
                category_name = category_dir.name
                results["category_indices"][category_name] = self.generate_category_index(category_name)
                
        # Generate cross-reference analysis
        cross_ref_analyzer = CrossReferenceAnalyzer(self.docs_root)
        results["cross_references"] = cross_ref_analyzer.generate_cross_reference_map()
        
        return results


class CrossReferenceAnalyzer:
    """Analyzes and validates cross-references between documents."""
    
    def __init__(self, docs_root: Path):
        """Initialize cross-reference analyzer."""
        self.docs_root = Path(docs_root)
        self.markdown_files = []
        
    def discover_markdown_files(self) -> List[Path]:
        """Discover all markdown files in the documentation."""
        if not self.markdown_files:
            self.markdown_files = list(self.docs_root.rglob("*.md"))
        return self.markdown_files
        
    def extract_links_from_file(self, file_path: Path) -> List[Dict[str, Any]]:
        """Extract all links from a markdown file."""
        links = []
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                
            # Extract markdown links [text](url)
            link_pattern = r'\[([^\]]+)\]\(([^)]+)\)'
            matches = re.findall(link_pattern, content)
            
            for text, url in matches:
                link_info = {
                    "text": text,
                    "url": url,
                    "source_file": str(file_path),
                    "type": self.classify_link(url)
                }
                links.append(link_info)
                
        except Exception as e:
            print(f"Error reading {file_path}: {e}")
            
        return links
        
    def classify_link(self, url: str) -> str:
        """Classify link type (internal, external, anchor)."""
        if url.startswith('http://') or url.startswith('https://'):
            return 'external'
        elif url.startswith('#'):
            return 'anchor'
        elif url.endswith('.md') or '/' in url:
            return 'internal'
        else:
            return 'unknown'
            
    def validate_internal_links(self, file_path: Path) -> Dict[str, Any]:
        """Validate internal links from a specific file."""
        links = self.extract_links_from_file(file_path)
        internal_links = [link for link in links if link['type'] == 'internal']
        
        validation_results = {
            "valid_links": [],
            "broken_links": [],
            "total_links": len(internal_links)
        }
        
        for link in internal_links:
            target_path = self.resolve_link_path(file_path, link['url'])
            
            if target_path and target_path.exists():
                validation_results["valid_links"].append({
                    "text": link['text'],
                    "target": link['url'],
                    "resolved_path": str(target_path)
                })
            else:
                validation_results["broken_links"].append({
                    "text": link['text'],
                    "target": link['url'],
                    "resolved_path": str(target_path) if target_path else "unresolvable"
                })
                
        return validation_results
        
    def resolve_link_path(self, source_file: Path, link_url: str) -> Optional[Path]:
        """Resolve relative link path to absolute path."""
        try:
            if link_url.startswith('/'):
                # Absolute path from docs root
                return self.docs_root / link_url.lstrip('/')
            else:
                # Relative path from source file
                return (source_file.parent / link_url).resolve()
        except Exception:
            return None
            
    def generate_cross_reference_map(self) -> Dict[str, Any]:
        """Generate comprehensive cross-reference mapping."""
        print("Analyzing cross-references...")
        
        cross_ref_map = {}
        markdown_files = self.discover_markdown_files()
        
        # Build outbound links for each file
        for file_path in markdown_files:
            links = self.extract_links_from_file(file_path)
            internal_links = [link for link in links if link['type'] == 'internal']
            
            cross_ref_map[str(file_path)] = {
                "outbound_links": internal_links,
                "inbound_links": [],  # Will be populated in second pass
                "link_count": len(internal_links),
                "validation_status": "pending"
            }
            
        # Build inbound links (second pass)
        for source_file, file_info in cross_ref_map.items():
            for link in file_info["outbound_links"]:
                target_path = self.resolve_link_path(Path(source_file), link['url'])
                
                if target_path and str(target_path) in cross_ref_map:
                    cross_ref_map[str(target_path)]["inbound_links"].append({
                        "source_file": source_file,
                        "link_text": link['text']
                    })
                    
        return cross_ref_map


def main():
    """Main function for navigation generation."""
    generator = NavigationGenerator()
    
    print("Generating AI-Friendly Navigation System")
    print("=" * 50)
    
    # Generate complete navigation
    results = generator.generate_complete_navigation()
    
    print(f"Generated navigation for {len(results['category_indices'])} categories")
    print(f"Master README: {len(results['master_readme'])} characters")
    print(f"Cross-references analyzed: {len(results['cross_references'])} files")
    
    return results


if __name__ == "__main__":
    main()