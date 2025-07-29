#!/usr/bin/env python3
"""
Tests for documentation structure creation and validation.
Part of the AI-Friendly Documentation Organization project.
"""

import os
import tempfile
import shutil
import unittest
from pathlib import Path
from typing import List, Dict, Any, Tuple


class TestDocumentationStructure(unittest.TestCase):
    """Test cases for documentation structure creation and validation."""
    
    def setUp(self):
        """Set up test environment with temporary directory."""
        self.test_dir = tempfile.mkdtemp()
        self.docs_root = Path(self.test_dir) / "docs"
        self.docs_root.mkdir()
        
    def tearDown(self):
        """Clean up test environment."""
        shutil.rmtree(self.test_dir)
        
    def test_create_root_categories(self):
        """Test creation of root-level category directories."""
        expected_categories = [
            "software",
            "domains", 
            "modules",
            "legacy",
            "references",
            "guides"
        ]
        
        # Create the structure
        for category in expected_categories:
            category_path = self.docs_root / category
            category_path.mkdir(exist_ok=True)
            
        # Verify all categories exist
        for category in expected_categories:
            category_path = self.docs_root / category
            self.assertTrue(category_path.exists(), f"Category {category} should exist")
            self.assertTrue(category_path.is_dir(), f"Category {category} should be a directory")
            
    def test_directory_naming_conventions(self):
        """Test that directory names follow kebab-case conventions."""
        test_names = [
            ("valid-kebab-case", True),
            ("another-valid-name", True),
            ("invalid_snake_case", False),
            ("InvalidCamelCase", False),
            ("invalid space", False),
            ("valid", True),
            ("123-numeric-start", True),
            ("", False)
        ]
        
        for name, should_be_valid in test_names:
            with self.subTest(name=name):
                is_valid = self._is_valid_kebab_case(name)
                self.assertEqual(is_valid, should_be_valid, 
                               f"Name '{name}' validation should be {should_be_valid}")
                               
    def test_maximum_depth_validation(self):
        """Test that directory structure doesn't exceed maximum depth of 4 levels."""
        # Create a test structure that's exactly 4 levels deep
        level1 = self.docs_root / "domains"
        level2 = level1 / "ship-design"
        level3 = level2 / "stability"
        level4 = level3 / "calculations"
        
        level1.mkdir()
        level2.mkdir()
        level3.mkdir()
        level4.mkdir()
        
        # This should be valid (4 levels: docs/domains/ship-design/stability/calculations)
        depth = self._calculate_depth_from_docs_root(level4)
        self.assertEqual(depth, 4, "Four levels should be allowed")
        
        # Create a 5th level - this should be flagged as too deep
        level5 = level4 / "examples"
        level5.mkdir()
        depth = self._calculate_depth_from_docs_root(level5)
        self.assertEqual(depth, 5, "Five levels should be detected")
        self.assertFalse(self._is_within_max_depth(level5), "Five levels should exceed maximum")
        
    def test_category_boundary_validation(self):
        """Test that each category has distinct scope and purpose."""
        category_definitions = {
            "software": ["Tools and software-specific documentation"],
            "domains": ["Engineering subject matter expertise"],
            "modules": ["Active development modules"], 
            "legacy": ["Historical and superseded content"],
            "references": ["Standards, papers, and external documentation"],
            "guides": ["Cross-cutting procedural documentation"]
        }
        
        for category, description in category_definitions.items():
            with self.subTest(category=category):
                self.assertIsInstance(description, list)
                self.assertTrue(len(description) > 0, f"Category {category} should have description")
                
    def test_content_categorization_logic(self):
        """Test the logic for categorizing existing content."""
        test_cases = [
            # (original_path, expected_category, expected_subcategory)
            ("pkg_ansys/ansys.md", "software", "ansys"),
            ("sub_ship_design/stability.md", "domains", "ship-design"),
            ("leg_apirp2rd/calculations.py", "legacy", "apirp2rd"),
            ("modules/orcaflex/analysis.md", "modules", "orcaflex"),
            ("docs/API_standard.pdf", "references", "standards"),
        ]
        
        for original_path, expected_category, expected_subcategory in test_cases:
            with self.subTest(path=original_path):
                category, subcategory = self._categorize_content(original_path)
                self.assertEqual(category, expected_category, 
                               f"Path {original_path} should be categorized as {expected_category}")
                self.assertEqual(subcategory, expected_subcategory,
                               f"Path {original_path} should have subcategory {expected_subcategory}")
    
    def _is_valid_kebab_case(self, name: str) -> bool:
        """Check if a name follows kebab-case convention."""
        if not name:
            return False
        # Kebab-case: lowercase letters, numbers, and hyphens only
        # Cannot start or end with hyphen
        import re
        pattern = r'^[a-z0-9]+(-[a-z0-9]+)*$'
        return bool(re.match(pattern, name))
        
    def _calculate_depth_from_docs_root(self, path: Path) -> int:
        """Calculate the depth of a path relative to docs root."""
        try:
            relative_path = path.relative_to(self.docs_root)
            return len(relative_path.parts)
        except ValueError:
            return 0
            
    def _is_within_max_depth(self, path: Path, max_depth: int = 4) -> bool:
        """Check if a path is within the maximum allowed depth."""
        depth = self._calculate_depth_from_docs_root(path)
        return depth <= max_depth
        
    def _categorize_content(self, original_path: str) -> Tuple[str, str]:
        """
        Categorize content based on original path patterns.
        Returns tuple of (category, subcategory).
        """
        path_parts = original_path.split("/")
        first_part = path_parts[0]
        
        # Handle prefix-based categorization
        if first_part.startswith("pkg_"):
            tool_name = first_part[4:]  # Remove "pkg_" prefix
            return ("software", tool_name)
        elif first_part.startswith("sub_"):
            domain_name = first_part[4:].replace("_", "-")  # Remove "sub_" and convert to kebab-case
            return ("domains", domain_name)
        elif first_part.startswith("leg_"):
            legacy_name = first_part[4:]  # Remove "leg_" prefix
            return ("legacy", legacy_name)
        elif first_part == "modules":
            if len(path_parts) > 1:
                module_name = path_parts[1]
                return ("modules", module_name)
            return ("modules", "unknown")
        elif first_part == "docs" and len(path_parts) > 1:
            # Assume references for docs folder content
            return ("references", "standards")
        else:
            # Default categorization
            return ("guides", "general")


class TestContentAnalysis(unittest.TestCase):
    """Test cases for content analysis and mapping."""
    
    def setUp(self):
        """Set up test environment."""
        self.test_dir = tempfile.mkdtemp()
        self.sample_structure = self._create_sample_structure()
        
    def tearDown(self):
        """Clean up test environment."""
        shutil.rmtree(self.test_dir)
        
    def _create_sample_structure(self) -> Dict[str, Any]:
        """Create a sample directory structure for testing."""
        structure = {
            "pkg_ansys": ["ansys.md", "spaceclaim.md"],
            "sub_ship_design": ["stability.md", "strength.md"],
            "leg_apirp2rd": ["calculations.py", "README.md"],
            "modules": {
                "orcaflex": ["analysis.md", "postproc.py"]
            }
        }
        
        # Create the structure in test directory
        base_path = Path(self.test_dir)
        for folder, contents in structure.items():
            if isinstance(contents, dict):
                # Handle nested structure
                folder_path = base_path / folder
                folder_path.mkdir()
                for subfolder, subcontents in contents.items():
                    subfolder_path = folder_path / subfolder
                    subfolder_path.mkdir()
                    for file in subcontents:
                        (subfolder_path / file).touch()
            else:
                # Handle flat structure
                folder_path = base_path / folder
                folder_path.mkdir()
                for file in contents:
                    (folder_path / file).touch()
                    
        return structure
        
    def test_analyze_existing_structure(self):
        """Test analysis of existing directory structure."""
        analyzer = ContentAnalyzer(self.test_dir)
        analysis = analyzer.analyze_structure()
        
        # Check that analysis contains expected categories
        self.assertIn("prefix_counts", analysis)
        self.assertIn("content_distribution", analysis)
        self.assertIn("file_types", analysis)
        
        # Verify prefix detection
        expected_prefixes = ["pkg_", "sub_", "leg_"]
        for prefix in expected_prefixes:
            self.assertIn(prefix, analysis["prefix_counts"])
            
    def test_content_mapping_generation(self):
        """Test generation of content mapping for migration."""
        analyzer = ContentAnalyzer(self.test_dir)
        mapping = analyzer.generate_content_mapping()
        
        self.assertIsInstance(mapping, dict)
        self.assertTrue(len(mapping) > 0, "Mapping should contain entries")
        
        # Verify mapping structure
        for old_path, new_path in mapping.items():
            self.assertIsInstance(old_path, str)
            self.assertIsInstance(new_path, str)
            self.assertTrue(new_path.startswith(("software/", "domains/", "modules/", "legacy/")))


class ContentAnalyzer:
    """Utility class for analyzing existing content structure."""
    
    def __init__(self, root_path: str):
        """Initialize with root path to analyze."""
        self.root_path = Path(root_path)
        
    def analyze_structure(self) -> Dict[str, Any]:
        """Analyze the existing directory structure."""
        analysis = {
            "prefix_counts": {},
            "content_distribution": {},
            "file_types": {},
            "depth_analysis": {}
        }
        
        # Analyze all directories
        for item in self.root_path.iterdir():
            if item.is_dir():
                folder_name = item.name
                
                # Count prefixes
                for prefix in ["pkg_", "sub_", "leg_"]:
                    if folder_name.startswith(prefix):
                        analysis["prefix_counts"][prefix] = analysis["prefix_counts"].get(prefix, 0) + 1
                        
                # Analyze content distribution
                file_count = len(list(item.rglob("*")))
                analysis["content_distribution"][folder_name] = file_count
                
                # Analyze file types
                for file_path in item.rglob("*"):
                    if file_path.is_file():
                        suffix = file_path.suffix.lower()
                        analysis["file_types"][suffix] = analysis["file_types"].get(suffix, 0) + 1
                        
        return analysis
        
    def generate_content_mapping(self) -> Dict[str, str]:
        """Generate mapping from old paths to new paths."""
        mapping = {}
        
        for item in self.root_path.iterdir():
            if item.is_dir():
                folder_name = item.name
                new_category, new_subcategory = self._categorize_folder(folder_name)
                
                # Generate new path
                new_path = f"{new_category}/{new_subcategory}"
                mapping[folder_name] = new_path
                
        return mapping
        
    def _categorize_folder(self, folder_name: str) -> Tuple[str, str]:
        """Categorize a folder based on its name."""
        if folder_name.startswith("pkg_"):
            tool_name = folder_name[4:]
            return ("software", tool_name)
        elif folder_name.startswith("sub_"):
            domain_name = folder_name[4:].replace("_", "-")
            return ("domains", domain_name)
        elif folder_name.startswith("leg_"):
            legacy_name = folder_name[4:]
            return ("legacy", legacy_name)
        elif folder_name == "modules":
            return ("modules", "active")
        else:
            return ("guides", "general")


if __name__ == "__main__":
    unittest.main()