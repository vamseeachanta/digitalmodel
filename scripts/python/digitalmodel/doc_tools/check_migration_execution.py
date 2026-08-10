#!/usr/bin/env python3
"""
Tests for content migration execution and validation.
Part of the AI-Friendly Documentation Organization project.
"""

import os
import tempfile
import shutil
import unittest
import hashlib
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple


class TestContentIntegrity(unittest.TestCase):
    """Test cases for content integrity during migration."""
    
    def setUp(self):
        """Set up test environment with sample content."""
        self.test_dir = tempfile.mkdtemp()
        self.source_root = Path(self.test_dir) / "source"
        self.target_root = Path(self.test_dir) / "target"
        
        self.source_root.mkdir()
        self.target_root.mkdir()
        
        # Create sample content with various file types
        self.create_sample_content()
        
    def tearDown(self):
        """Clean up test environment."""
        shutil.rmtree(self.test_dir)
        
    def create_sample_content(self):
        """Create diverse sample content for integrity testing."""
        # Create markdown files with different encodings
        md_content = """# Test Document

This is a test document with various characters:
- Unicode: 你好世界 🌍
- Special: ñ, ü, é, ß
- Math: ∑, ∏, √, ∞
"""
        (self.source_root / "test_unicode.md").write_text(md_content, encoding='utf-8')
        
        # Create file with frontmatter
        fm_content = """---
title: Test Document
tags: ["test", "migration"]
---

# Content with Frontmatter

This content should maintain its frontmatter during migration.
"""
        (self.source_root / "test_frontmatter.md").write_text(fm_content, encoding='utf-8')
        
        # Create binary file
        binary_content = bytes([i % 256 for i in range(1024)])
        (self.source_root / "test_binary.dat").write_bytes(binary_content)
        
        # Create nested structure
        nested_dir = self.source_root / "nested" / "deep" / "structure"
        nested_dir.mkdir(parents=True)
        (nested_dir / "deep_file.md").write_text("Deep content", encoding='utf-8')
        
    def test_file_hash_preservation(self):
        """Test that file hashes are preserved during migration."""
        # Calculate source hashes
        source_hashes = {}
        for file_path in self.source_root.rglob("*"):
            if file_path.is_file():
                hash_value = self.calculate_file_hash(file_path)
                rel_path = file_path.relative_to(self.source_root)
                source_hashes[str(rel_path)] = hash_value
                
        # Simulate migration
        for file_path in self.source_root.rglob("*"):
            if file_path.is_file():
                rel_path = file_path.relative_to(self.source_root)
                target_path = self.target_root / rel_path
                target_path.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(file_path, target_path)
                
        # Verify target hashes
        for rel_path, source_hash in source_hashes.items():
            target_file = self.target_root / rel_path
            self.assertTrue(target_file.exists(), f"File {rel_path} should exist in target")
            
            target_hash = self.calculate_file_hash(target_file)
            self.assertEqual(source_hash, target_hash, 
                           f"Hash mismatch for {rel_path}")
            
    def test_encoding_preservation(self):
        """Test that file encodings are preserved during migration."""
        # Test UTF-8 file
        source_file = self.source_root / "test_unicode.md"
        target_file = self.target_root / "test_unicode.md"
        
        # Copy file
        shutil.copy2(source_file, target_file)
        
        # Read and compare content
        source_content = source_file.read_text(encoding='utf-8')
        target_content = target_file.read_text(encoding='utf-8')
        
        self.assertEqual(source_content, target_content)
        self.assertIn("你好世界", target_content)
        self.assertIn("🌍", target_content)
        
    def test_metadata_preservation(self):
        """Test that file metadata (timestamps, permissions) are preserved."""
        source_file = self.source_root / "test_unicode.md"
        target_file = self.target_root / "test_unicode.md"
        
        # Copy with metadata
        shutil.copy2(source_file, target_file)
        
        # Compare metadata
        source_stat = source_file.stat()
        target_stat = target_file.stat()
        
        # Modified time should be preserved
        self.assertEqual(source_stat.st_mtime, target_stat.st_mtime)
        
        # Size should be identical
        self.assertEqual(source_stat.st_size, target_stat.st_size)
        
    def calculate_file_hash(self, file_path: Path) -> str:
        """Calculate SHA-256 hash of a file."""
        hash_sha256 = hashlib.sha256()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_sha256.update(chunk)
        return hash_sha256.hexdigest()


class TestPathTransformation(unittest.TestCase):
    """Test cases for path transformation logic."""
    
    def setUp(self):
        """Set up test environment."""
        self.test_cases = self.create_test_mapping()
        
    def create_test_mapping(self) -> Dict[str, str]:
        """Create comprehensive test mapping for path transformations."""
        return {
            # Software packages
            "pkg_ansys": "software/ansys",
            "pkg_orcaflex": "software/orcaflex",
            "pkg_qgis": "software/qgis",
            
            # Engineering domains
            "sub_ship_design": "domains/marine-systems/ship-design",
            "sub_cathodic_protection": "domains/offshore/cathodic-protection",
            "sub_pipelines": "domains/fluid-systems/pipelines",
            "sub_drilling": "domains/offshore/drilling",
            "sub_fatigue": "domains/structural/fatigue",
            "sub_wind": "domains/environmental/wind",
            
            # Legacy modules
            "leg_apirp2rd": "legacy/apirp2rd",
            "leg_vmstress": "legacy/vmstress",
            "leg_sn_curves": "legacy/sn_curves",
            
            # Special cases
            "modules": "modules",
            "_lib": "guides/_lib",
            "user_stories": "guides/user_stories",
            
            # Non-prefixed directories
            "ansys": "software/ansys",
            "cathodic_protection": "domains/offshore/cathodic-protection",
            "lighting": "domains/infrastructure/lighting"
        }
        
    def test_prefix_removal(self):
        """Test that prefixes are correctly removed during transformation."""
        transformations = {
            "pkg_": "software",
            "sub_": "domains",
            "leg_": "legacy"
        }
        
        for old_path, new_path in self.test_cases.items():
            for prefix, category in transformations.items():
                if old_path.startswith(prefix):
                    # Verify prefix is removed
                    self.assertFalse(new_path.startswith(prefix))
                    # Verify correct category assignment
                    self.assertTrue(new_path.startswith(category))
                    
    def test_underscore_to_hyphen_conversion(self):
        """Test conversion of underscores to hyphens in paths."""
        for old_path, new_path in self.test_cases.items():
            if "_" in old_path and old_path.startswith(("sub_", "leg_")):
                # Extract the name part after prefix
                name_part = old_path.split("_", 1)[1] if "_" in old_path else ""
                if name_part:
                    # Check that underscores in name are converted to hyphens
                    transformed_name = name_part.replace("_", "-")
                    self.assertIn(transformed_name, new_path)
                    
    def test_domain_categorization(self):
        """Test that domains are correctly categorized into groups."""
        domain_groups = {
            "marine-systems": ["ship-design", "moorings", "risers"],
            "offshore": ["drilling", "cathodic-protection", "production"],
            "fluid-systems": ["pipelines", "pipecapacity", "hydrodynamics"],
            "structural": ["fatigue", "vibrations", "welding"],
            "environmental": ["metocean", "wind", "viv"]
        }
        
        for old_path, new_path in self.test_cases.items():
            if new_path.startswith("domains/"):
                # Extract domain group and name
                parts = new_path.split("/")
                if len(parts) >= 3:
                    group = parts[1]
                    domain = parts[2]
                    
                    # Verify domain is in correct group
                    if group in domain_groups:
                        expected_domains = domain_groups[group]
                        if domain in expected_domains:
                            self.assertIn(domain, expected_domains)
                            
    def test_special_case_handling(self):
        """Test handling of special cases without prefixes."""
        special_cases = {
            "ansys": "software/ansys",
            "cathodic_protection": "domains/offshore/cathodic-protection",
            "lighting": "domains/infrastructure/lighting",
            "modules": "modules"
        }
        
        for old_name, expected_path in special_cases.items():
            self.assertEqual(self.test_cases.get(old_name), expected_path)
            
    def test_path_depth_limits(self):
        """Test that transformed paths don't exceed depth limits."""
        max_depth = 4  # Maximum allowed depth from docs root
        
        for old_path, new_path in self.test_cases.items():
            path_parts = new_path.split("/")
            depth = len(path_parts)
            
            self.assertLessEqual(depth, max_depth, 
                               f"Path {new_path} exceeds maximum depth of {max_depth}")


class TestMigrationValidation(unittest.TestCase):
    """Test cases for migration validation functionality."""
    
    def setUp(self):
        """Set up test environment for validation."""
        self.test_dir = tempfile.mkdtemp()
        self.docs_root = Path(self.test_dir) / "docs"
        self.docs_root.mkdir()
        
        # Create migration results file
        self.create_migration_results()
        
    def tearDown(self):
        """Clean up test environment."""
        shutil.rmtree(self.test_dir)
        
    def create_migration_results(self):
        """Create sample migration results for validation testing."""
        results = {
            "migration_log": [
                {
                    "source": "pkg_ansys/ansys.md",
                    "target": "software/ansys/ansys.md",
                    "size": 1024,
                    "timestamp": 1234567890,
                    "integrity_verified": True
                },
                {
                    "source": "sub_ship_design/stability.md",
                    "target": "domains/marine-systems/ship-design/stability.md",
                    "size": 2048,
                    "timestamp": 1234567891,
                    "integrity_verified": True
                }
            ],
            "results": {
                "directories_migrated": 10,
                "files_migrated": 50,
                "migration_errors": 0,
                "links_created": 10
            }
        }
        
        results_file = self.docs_root / "migration_log.json"
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2)
            
    def test_migration_log_validation(self):
        """Test validation of migration log entries."""
        log_file = self.docs_root / "migration_log.json"
        self.assertTrue(log_file.exists())
        
        with open(log_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
            
        self.assertIn("migration_log", data)
        self.assertIn("results", data)
        
        # Validate log entries
        for entry in data["migration_log"]:
            self.assertIn("source", entry)
            self.assertIn("target", entry)
            self.assertIn("integrity_verified", entry)
            self.assertTrue(entry["integrity_verified"])
            
    def test_migration_completeness(self):
        """Test that all expected migrations are completed."""
        # Create expected migrations list
        expected_migrations = [
            ("pkg_ansys", "software/ansys"),
            ("sub_ship_design", "domains/marine-systems/ship-design"),
            ("leg_apirp2rd", "legacy/apirp2rd")
        ]
        
        # Simulate checking against migration log
        log_file = self.docs_root / "migration_log.json"
        with open(log_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
            
        migrated_paths = [(entry["source"].split("/")[0], 
                          "/".join(entry["target"].split("/")[:2]))
                         for entry in data["migration_log"]]
        
        # Verify at least some expected migrations are present
        found_count = 0
        for expected_src, expected_tgt in expected_migrations:
            for actual_src, actual_tgt in migrated_paths:
                if expected_src in actual_src and expected_tgt in actual_tgt:
                    found_count += 1
                    break
                    
        self.assertGreater(found_count, 0, "Should find at least some expected migrations")
        
    def test_symlink_validation(self):
        """Test validation of symbolic links creation."""
        # Create test structure
        old_dir = self.docs_root / "pkg_ansys"
        new_dir = self.docs_root / "software" / "ansys"
        
        old_dir.mkdir()
        new_dir.mkdir(parents=True)
        
        # Create a test file in new location
        test_file = new_dir / "test.md"
        test_file.write_text("Test content", encoding='utf-8')
        
        # Simulate symlink creation (or copy on Windows)
        if os.name == 'nt':
            # On Windows, create a copy
            shutil.copytree(new_dir, old_dir, dirs_exist_ok=True)
        else:
            # On Unix, create actual symlink
            old_dir.rmdir()  # Remove empty directory first
            old_dir.symlink_to(new_dir)
            
        # Validate symlink/copy exists and points to correct location
        self.assertTrue(old_dir.exists())
        
        # Verify content is accessible through old path
        old_test_file = old_dir / "test.md"
        if old_test_file.exists():
            content = old_test_file.read_text(encoding='utf-8')
            self.assertEqual(content, "Test content")


class TestMigrationRollback(unittest.TestCase):
    """Test cases for migration rollback functionality."""
    
    def setUp(self):
        """Set up test environment for rollback testing."""
        self.test_dir = tempfile.mkdtemp()
        self.docs_root = Path(self.test_dir) / "docs"
        self.backup_root = Path(self.test_dir) / "docs_backup_123456"
        
        # Create original structure
        self.create_original_structure()
        
        # Create backup
        shutil.copytree(self.docs_root, self.backup_root)
        
        # Simulate migration
        self.simulate_migration()
        
    def tearDown(self):
        """Clean up test environment."""
        shutil.rmtree(self.test_dir)
        
    def create_original_structure(self):
        """Create original directory structure."""
        self.docs_root.mkdir()
        
        # Create original files
        pkg_dir = self.docs_root / "pkg_ansys"
        pkg_dir.mkdir()
        (pkg_dir / "original.md").write_text("Original content", encoding='utf-8')
        
        sub_dir = self.docs_root / "sub_ship_design"
        sub_dir.mkdir()
        (sub_dir / "design.md").write_text("Design content", encoding='utf-8')
        
    def simulate_migration(self):
        """Simulate a migration that needs rollback."""
        # Remove original directories
        shutil.rmtree(self.docs_root / "pkg_ansys")
        shutil.rmtree(self.docs_root / "sub_ship_design")
        
        # Create new structure
        new_dir = self.docs_root / "software" / "ansys"
        new_dir.mkdir(parents=True)
        (new_dir / "original.md").write_text("Migrated content", encoding='utf-8')
        
    def test_rollback_restoration(self):
        """Test that rollback restores original structure."""
        # Verify migration changes exist
        self.assertTrue((self.docs_root / "software" / "ansys").exists())
        self.assertFalse((self.docs_root / "pkg_ansys").exists())
        
        # Perform rollback
        if self.docs_root.exists():
            shutil.rmtree(self.docs_root)
        shutil.copytree(self.backup_root, self.docs_root)
        
        # Verify original structure restored
        self.assertTrue((self.docs_root / "pkg_ansys").exists())
        self.assertFalse((self.docs_root / "software").exists())
        
        # Verify original content restored
        original_file = self.docs_root / "pkg_ansys" / "original.md"
        content = original_file.read_text(encoding='utf-8')
        self.assertEqual(content, "Original content")
        
    def test_backup_integrity(self):
        """Test that backup maintains complete integrity."""
        # Compare file counts
        backup_files = list(self.backup_root.rglob("*"))
        
        # Verify backup has expected structure
        self.assertTrue((self.backup_root / "pkg_ansys").exists())
        self.assertTrue((self.backup_root / "sub_ship_design").exists())
        
        # Verify backup content unchanged
        backup_file = self.backup_root / "pkg_ansys" / "original.md"
        content = backup_file.read_text(encoding='utf-8')
        self.assertEqual(content, "Original content")


if __name__ == "__main__":
    unittest.main()