#!/usr/bin/env python3
"""
Navigation System Tests
Tests for AI-Friendly Documentation Organization - Task 4.1

This module tests navigation system generation and cross-reference functionality
for the reorganized documentation structure.
"""

import unittest
import tempfile
import shutil
from pathlib import Path
import json
import yaml
from typing import Dict, List, Any, Set
import re


class TestNavigationSystemGeneration(unittest.TestCase):
    """Test navigation system creation and maintenance."""
    
    def setUp(self):
        """Set up test environment with temporary directory structure."""
        self.test_dir = Path(tempfile.mkdtemp())
        self.docs_root = self.test_dir / "docs"
        self.create_test_structure()
        
    def tearDown(self):
        """Clean up test environment."""
        if self.test_dir.exists():
            shutil.rmtree(self.test_dir)
            
    def create_test_structure(self):
        """Create test directory structure with sample content."""
        # Create main category directories
        categories = [
            "software/ansys",
            "software/orcaflex", 
            "domains/ship-design",
            "domains/offshore-structures",
            "modules/analysis",
            "modules/visualization",
            "guides/tutorials",
            "references/standards"
        ]
        
        for category in categories:
            (self.docs_root / category).mkdir(parents=True, exist_ok=True)
            
        # Create sample markdown files with frontmatter
        test_files = [
            ("software/ansys/installation.md", self.create_sample_content("ansys", "installation")),
            ("software/orcaflex/basics.md", self.create_sample_content("orcaflex", "basics")),
            ("domains/ship-design/stability.md", self.create_sample_content("ship-design", "stability")),
            ("modules/analysis/rao.md", self.create_sample_content("analysis", "rao")),
            ("guides/tutorials/getting-started.md", self.create_sample_content("tutorials", "getting-started"))
        ]
        
        for file_path, content in test_files:
            full_path = self.docs_root / file_path
            with open(full_path, 'w', encoding='utf-8') as f:
                f.write(content)
                
    def create_sample_content(self, category: str, topic: str) -> str:
        """Create sample markdown content with frontmatter."""
        frontmatter = {
            "title": f"{topic.replace('-', ' ').title()}",
            "category": category,
            "tags": [category, topic],
            "last_updated": "2025-07-26",
            "status": "active",
            "complexity": "intermediate",
            "related": [f"{category}-overview"],
            "industry_standards": ["API", "DNV"]
        }
        
        yaml_content = yaml.dump(frontmatter, default_flow_style=False)
        
        return f"""---
{yaml_content}---

# {topic.replace('-', ' ').title()}

This is sample content for {category} {topic}.

## Overview

Sample overview content with cross-references to related topics.

## Related Topics

- [{category.title()} Overview]({category}-overview.md)
- [Getting Started](../guides/tutorials/getting-started.md)

## External References

- [Industry Standard Documentation](https://example.com/standards)
"""

    def test_readme_generation_structure(self):
        """Test that master README.md has proper structure."""
        from navigation_generator import NavigationGenerator
        
        generator = NavigationGenerator(self.docs_root)
        readme_content = generator.generate_master_readme()
        
        # Test that README contains essential sections
        required_sections = [
            "# Digital Model Documentation",
            "## Quick Start",
            "## Documentation Structure", 
            "## Category Overview",
            "## Navigation Guide",
            "## For AI Assistants"
        ]
        
        for section in required_sections:
            self.assertIn(section, readme_content)
            
        # Test that it includes category links
        self.assertIn("[Software Tools](software/)", readme_content)
        self.assertIn("[Engineering Domains](domains/)", readme_content)
        self.assertIn("[Analysis Modules](modules/)", readme_content)
        
    def test_category_index_generation(self):
        """Test generation of category index files."""
        from navigation_generator import NavigationGenerator
        
        generator = NavigationGenerator(self.docs_root)
        
        # Test software category index
        software_index = generator.generate_category_index("software")
        
        self.assertIn("# Software Tools", software_index)
        self.assertIn("## Available Content", software_index)
        self.assertIn("#### [Ansys]", software_index)
        self.assertIn("#### [Orcaflex]", software_index)
        
        # Test domains category index  
        domains_index = generator.generate_category_index("domains")
        
        self.assertIn("# Engineering Domains", domains_index)
        self.assertIn("## Available Content", domains_index)
        self.assertIn("Ship Design", domains_index)
        
    def test_taxonomy_generation(self):
        """Test generation of complete documentation taxonomy."""
        from navigation_generator import NavigationGenerator
        
        generator = NavigationGenerator(self.docs_root)
        taxonomy = generator.generate_taxonomy()
        
        # Test taxonomy structure
        self.assertIsInstance(taxonomy, dict)
        self.assertIn("categories", taxonomy)
        self.assertIn("total_files", taxonomy)
        self.assertIn("last_updated", taxonomy)
        
        # Test categories present
        categories = taxonomy["categories"]
        expected_categories = ["software", "domains", "modules", "guides", "references"]
        
        for category in expected_categories:
            if any(Path(self.docs_root / category).exists() for category in expected_categories):
                self.assertIn(category, categories)


class TestCrossReferenceSystem(unittest.TestCase):
    """Test cross-reference detection and validation."""
    
    def setUp(self):
        """Set up test environment."""
        self.test_dir = Path(tempfile.mkdtemp())
        self.docs_root = self.test_dir / "docs"
        self.create_test_files()
        
    def tearDown(self):
        """Clean up test environment."""
        if self.test_dir.exists():
            shutil.rmtree(self.test_dir)
            
    def create_test_files(self):
        """Create test files with various link patterns."""
        (self.docs_root / "domains").mkdir(parents=True, exist_ok=True)
        (self.docs_root / "software").mkdir(parents=True, exist_ok=True)
        
        # File with various link types
        content_with_links = """# Test Document

## Internal Links

- [Relative link](../software/ansys.md)
- [Anchor link](#section-one)
- [Same directory](stability.md)

## External Links

- [External site](https://example.com)
- [API documentation](https://api.example.com/docs)

## Cross-references

See also: [Ship Design Overview](ship-design-overview.md)
Reference: [Analysis Module](../modules/analysis.md)

## Section One

Content here.
"""

        with open(self.docs_root / "domains" / "test-doc.md", 'w') as f:
            f.write(content_with_links)
            
        # Target files for link validation
        with open(self.docs_root / "software" / "ansys.md", 'w') as f:
            f.write("# ANSYS Documentation\n\nContent here.")
            
        with open(self.docs_root / "domains" / "stability.md", 'w') as f:
            f.write("# Stability Analysis\n\nContent here.")
            
    def test_link_extraction(self):
        """Test extraction of different link types from markdown."""
        from navigation_generator import CrossReferenceAnalyzer
        
        analyzer = CrossReferenceAnalyzer(self.docs_root)
        test_file = self.docs_root / "domains" / "test-doc.md"
        
        links = analyzer.extract_links_from_file(test_file)
        
        # Test that all link types are detected
        link_texts = [link['text'] for link in links]
        expected_links = [
            "Relative link", "Anchor link", "Same directory", 
            "External site", "API documentation", 
            "Ship Design Overview", "Analysis Module"
        ]
        
        for expected in expected_links:
            self.assertIn(expected, link_texts)
            
        # Test link categorization
        internal_links = [link for link in links if link['type'] == 'internal']
        external_links = [link for link in links if link['type'] == 'external']
        anchor_links = [link for link in links if link['type'] == 'anchor']
        
        self.assertTrue(len(internal_links) > 0)
        self.assertTrue(len(external_links) > 0)
        self.assertTrue(len(anchor_links) > 0)
        
    def test_link_validation(self):
        """Test validation of internal links."""
        from navigation_generator import CrossReferenceAnalyzer
        
        analyzer = CrossReferenceAnalyzer(self.docs_root)
        test_file = self.docs_root / "domains" / "test-doc.md"
        
        validation_results = analyzer.validate_internal_links(test_file)
        
        # Test validation structure
        self.assertIn('valid_links', validation_results)
        self.assertIn('broken_links', validation_results)
        self.assertIn('total_links', validation_results)
        
        # Test that existing files are marked as valid
        valid_targets = [link['target'] for link in validation_results['valid_links']]
        self.assertIn('../software/ansys.md', valid_targets)
        self.assertIn('stability.md', valid_targets)
        
    def test_cross_reference_mapping(self):
        """Test generation of cross-reference mapping."""
        from navigation_generator import CrossReferenceAnalyzer
        
        analyzer = CrossReferenceAnalyzer(self.docs_root)
        cross_refs = analyzer.generate_cross_reference_map()
        
        # Test mapping structure
        self.assertIsInstance(cross_refs, dict)
        
        # Test that files with references are included
        test_file_key = str(self.docs_root / "domains" / "test-doc.md")
        if test_file_key in cross_refs:
            file_refs = cross_refs[test_file_key]
            self.assertIn('outbound_links', file_refs)
            self.assertIn('inbound_links', file_refs)


class TestNavigationIntegration(unittest.TestCase):
    """Test integrated navigation system functionality."""
    
    def setUp(self):
        """Set up comprehensive test environment."""
        self.test_dir = Path(tempfile.mkdtemp())
        self.docs_root = self.test_dir / "docs"
        self.create_comprehensive_structure()
        
    def tearDown(self):
        """Clean up test environment."""
        if self.test_dir.exists():
            shutil.rmtree(self.test_dir)
            
    def create_comprehensive_structure(self):
        """Create comprehensive test structure."""
        # Create realistic documentation structure
        structure = {
            "software": ["ansys", "orcaflex", "python"],
            "domains": ["ship-design", "offshore-structures", "marine-analysis"],
            "modules": ["analysis", "visualization", "data-processing"],
            "guides": ["tutorials", "best-practices", "troubleshooting"],
            "references": ["standards", "apis", "glossary"]
        }
        
        for category, subcategories in structure.items():
            for subcat in subcategories:
                dir_path = self.docs_root / category / subcat
                dir_path.mkdir(parents=True, exist_ok=True)
                
                # Create sample files in each subcategory
                for i in range(2):
                    file_name = f"topic-{i+1}.md"
                    content = self.create_realistic_content(category, subcat, file_name)
                    
                    with open(dir_path / file_name, 'w', encoding='utf-8') as f:
                        f.write(content)
                        
    def create_realistic_content(self, category: str, subcat: str, filename: str) -> str:
        """Create realistic markdown content with cross-references."""
        title = f"{subcat.replace('-', ' ').title()} - {filename.replace('.md', '').replace('-', ' ').title()}"
        
        # Create cross-references to other categories
        cross_refs = []
        if category == "software":
            cross_refs.append("- [Analysis Modules](../../modules/analysis/topic-1.md)")
            cross_refs.append("- [Domain Guide](../../domains/ship-design/topic-1.md)")
        elif category == "domains":
            cross_refs.append("- [Software Tools](../../software/ansys/topic-1.md)")
            cross_refs.append("- [Analysis Methods](../../modules/analysis/topic-1.md)")
        elif category == "modules":
            cross_refs.append("- [Software Integration](../../software/python/topic-1.md)")
            cross_refs.append("- [Domain Applications](../../domains/marine-analysis/topic-1.md)")
            
        cross_ref_section = "\n".join(cross_refs) if cross_refs else "- No cross-references available"
        
        return f"""---
title: "{title}"
category: {category}
subcategory: {subcat}
tags: [{category}, {subcat}]
last_updated: "2025-07-26"
status: active
complexity: intermediate
related: ["{subcat}-overview"]
---

# {title}

This is documentation for {category}/{subcat}.

## Overview

Detailed overview of the topic with technical content.

## Cross-References

{cross_ref_section}

## Implementation

Technical implementation details would go here.

## References

- [Industry Standards](../../references/standards/topic-1.md)
- [Best Practices](../../guides/best-practices/topic-1.md)
"""

    def test_complete_navigation_generation(self):
        """Test complete navigation system generation."""
        from navigation_generator import NavigationGenerator
        
        generator = NavigationGenerator(self.docs_root)
        
        # Generate complete navigation system
        results = generator.generate_complete_navigation()
        
        # Test results structure
        self.assertIn('master_readme', results)
        self.assertIn('category_indices', results)
        self.assertIn('taxonomy', results)
        self.assertIn('cross_references', results)
        
        # Test that all categories have indices
        expected_categories = ["software", "domains", "modules", "guides", "references"]
        for category in expected_categories:
            if (self.docs_root / category).exists():
                self.assertIn(category, results['category_indices'])
                
    def test_navigation_consistency(self):
        """Test consistency across navigation elements."""
        from navigation_generator import NavigationGenerator
        
        generator = NavigationGenerator(self.docs_root)
        results = generator.generate_complete_navigation()
        
        # Extract categories from different sources
        readme_categories = self.extract_categories_from_readme(results['master_readme'])
        taxonomy_categories = set(results['taxonomy']['categories'].keys())
        index_categories = set(results['category_indices'].keys())
        
        # Test consistency - all should have same categories (where directories exist)
        existing_dirs = {d.name for d in self.docs_root.iterdir() if d.is_dir()}
        expected_categories = existing_dirs.intersection({"software", "domains", "modules", "guides", "references"})
        
        for category in expected_categories:
            self.assertIn(category, readme_categories)
            self.assertIn(category, taxonomy_categories)
            self.assertIn(category, index_categories)
            
    def extract_categories_from_readme(self, readme_content: str) -> Set[str]:
        """Extract category references from README content."""
        # Look for category links in the format [Category Name](category/)
        category_pattern = r'\[.*?\]\((\w+)/\)'
        matches = re.findall(category_pattern, readme_content)
        return set(matches)
        
    def test_ai_assistant_guidance(self):
        """Test AI assistant guidance section generation."""
        from navigation_generator import NavigationGenerator
        
        generator = NavigationGenerator(self.docs_root)
        ai_guidance = generator.generate_ai_assistant_guidance()
        
        # Test essential AI guidance elements
        required_elements = [
            "For AI Assistants",
            "Documentation Structure",
            "Content Discovery",
            "Cross-Reference System",
            "Metadata Usage",
            "Navigation Patterns"
        ]
        
        for element in required_elements:
            self.assertIn(element, ai_guidance)
            
        # Test that it includes practical examples
        self.assertIn("examples", ai_guidance.lower())
        self.assertIn("yaml frontmatter", ai_guidance.lower())


if __name__ == "__main__":
    # Run all tests
    unittest.main(verbosity=2)