#!/usr/bin/env python
"""
Test Suite for Universal OrcaFlex Runner
========================================

Tests the universal runner functionality including real .dat file processing.
"""

import sys
import os
import shutil
import time
import json
from pathlib import Path
from typing import List
import unittest
from unittest.mock import patch, MagicMock

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / "src"))

from digitalmodel.orcaflex.universal import (
    UniversalOrcaFlexRunner,
    PathResolver,
    ModelDiscovery,
    BatchProcessor,
    StatusReporter
)


class TestUniversalRunner(unittest.TestCase):
    """Test the Universal OrcaFlex Runner."""
    
    @classmethod
    def setUpClass(cls):
        """Set up test environment."""
        cls.test_dir = Path(__file__).parent / "test_output"
        cls.test_dir.mkdir(exist_ok=True)
        
        # Path to real test files
        cls.real_test_files = Path(__file__).parent.parent / "orcaflex_analysis"
        
    @classmethod
    def tearDownClass(cls):
        """Clean up test environment."""
        if cls.test_dir.exists():
            shutil.rmtree(cls.test_dir)
    
    def setUp(self):
        """Set up for each test."""
        self.runner = UniversalOrcaFlexRunner(
            base_dir=self.test_dir,
            mock_mode=True,
            verbose=False
        )
    
    def test_initialization(self):
        """Test runner initialization."""
        self.assertIsNotNone(self.runner)
        self.assertEqual(self.runner.mock_mode, True)
        self.assertEqual(self.runner.max_workers, 30)
    
    def test_path_resolver(self):
        """Test path resolution functionality."""
        resolver = PathResolver()
        
        # Test resolve_path
        current = resolver.resolve_path(".")
        self.assertTrue(current.is_absolute())
        
        # Test with None
        none_path = resolver.resolve_path(None)
        self.assertEqual(none_path, Path.cwd())
    
    def test_model_discovery(self):
        """Test model discovery functionality."""
        discovery = ModelDiscovery()
        
        # Create test files
        test_models_dir = self.test_dir / "models"
        test_models_dir.mkdir(exist_ok=True)
        
        # Create test model files
        (test_models_dir / "model1.yml").write_text("General:\n  SimTime: 100")
        (test_models_dir / "model2.yml").write_text("Environment:\n  WaterDepth: 100")
        (test_models_dir / "backup_model.yml").write_text("General:\n  Test: true")
        (test_models_dir / "test.txt").write_text("Not a model")
        
        # Test discovery
        models = discovery.find_models(
            directory=test_models_dir,
            pattern="*.yml",
            exclude_patterns=["*backup*"]
        )
        
        self.assertEqual(len(models), 2)
        self.assertIn("model1.yml", [m.name for m in models])
        self.assertIn("model2.yml", [m.name for m in models])
        self.assertNotIn("backup_model.yml", [m.name for m in models])
    
    def test_mock_processing(self):
        """Test mock mode processing."""
        # Create test model
        test_model = self.test_dir / "test_model.yml"
        test_model.write_text("General:\n  SimTime: 100")
        
        # Run in mock mode
        results = self.runner.run(
            models=[test_model],
            output_directory=self.test_dir / "output"
        )
        
        self.assertEqual(results.total, 1)
        self.assertEqual(results.successful, 1)
        self.assertEqual(results.failed, 0)
        self.assertTrue(len(results.sim_files_created) > 0)
    
    def test_pattern_matching(self):
        """Test pattern-based model discovery."""
        # Create test models
        models_dir = self.test_dir / "pattern_test"
        models_dir.mkdir(exist_ok=True)
        
        (models_dir / "fsts_model1.yml").write_text("General: test")
        (models_dir / "fsts_model2.yml").write_text("General: test")
        (models_dir / "other_model.yml").write_text("General: test")
        
        # Test pattern matching
        results = self.runner.run(
            pattern="fsts_*.yml",
            input_directory=models_dir,
            output_directory=self.test_dir / "pattern_output"
        )
        
        self.assertEqual(results.total, 2)
        self.assertEqual(results.successful, 2)
    
    def test_status_reporter(self):
        """Test status reporting functionality."""
        reporter = StatusReporter(enable_colors=False)
        reporter.total = 10
        reporter.start_time = time.time()
        
        # Log some results
        reporter.log_result("model1.yml", True, "model1.sim")
        reporter.log_result("model2.yml", False, None, "Test error")
        
        self.assertEqual(reporter.completed, 2)
        self.assertEqual(reporter.success, 1)
        self.assertEqual(reporter.failed, 1)
        self.assertEqual(len(reporter.failed_list), 1)
        
        # Test report generation
        report = reporter.generate_report()
        self.assertIn("execution_summary", report)
        self.assertEqual(report["execution_summary"]["successful"], 1)
        self.assertEqual(report["execution_summary"]["failed"], 1)
    
    def test_batch_processor(self):
        """Test batch processing functionality."""
        processor = BatchProcessor(max_workers=2, mock_mode=True)
        
        # Create test models
        models = []
        for i in range(5):
            model = self.test_dir / f"batch_model_{i}.yml"
            model.write_text(f"General: test_{i}")
            models.append(model)
        
        # Process batch
        output_dir = self.test_dir / "batch_output"
        output_dir.mkdir(exist_ok=True)
        
        results = processor.process_batch(
            models=models,
            output_directory=output_dir
        )
        
        self.assertEqual(len(results), 5)
        successful = sum(1 for r in results if r.get('success', False))
        self.assertEqual(successful, 5)


class TestRealOrcaFlexFiles(unittest.TestCase):
    """Test with real OrcaFlex .dat files."""
    
    @classmethod
    def setUpClass(cls):
        """Set up test environment."""
        cls.test_dir = Path(__file__).parent / "real_test_output"
        cls.test_dir.mkdir(exist_ok=True)
        
        # Source directory with real .dat files
        cls.source_dir = Path(__file__).parent.parent / "orcaflex_analysis"
        
        # Copy test .dat files
        cls.test_files = []
        if cls.source_dir.exists():
            dat_files = [
                "orcaflex_test2.dat",
                "moorings/pretension/fsts_lngc/01_qa/6dof/fsts_l095_mwl_125km3_l000_pb.dat",
                "moorings/pretension/fsts_lngc/01_qa/dof_none/fsts_l095_mwl_125km3_l000_pb.dat"
            ]
            
            for dat_file in dat_files:
                source = cls.source_dir / dat_file
                if source.exists():
                    dest = cls.test_dir / Path(dat_file).name
                    shutil.copy2(source, dest)
                    cls.test_files.append(dest)
                    print(f"Copied test file: {dest.name}")
    
    @classmethod
    def tearDownClass(cls):
        """Clean up test environment."""
        if cls.test_dir.exists():
            shutil.rmtree(cls.test_dir)
    
    def test_real_dat_files(self):
        """Test processing real .dat files."""
        if not self.test_files:
            self.skipTest("No real .dat files available for testing")
        
        # Initialize runner (mock mode since we might not have license)
        runner = UniversalOrcaFlexRunner(
            base_dir=self.test_dir,
            mock_mode=True,  # Use mock mode for CI/CD
            max_workers=3,
            verbose=True
        )
        
        # Initialize status reporter
        reporter = StatusReporter(enable_colors=True)
        
        # Process real files
        output_dir = self.test_dir / "sim_output"
        results = runner.run(
            models=self.test_files,
            output_directory=output_dir,
            status_reporter=reporter
        )
        
        # Verify results
        self.assertEqual(results.total, len(self.test_files))
        self.assertEqual(results.successful, len(self.test_files))
        self.assertEqual(results.failed, 0)
        
        # Check that .sim files were created
        for test_file in self.test_files:
            sim_file = output_dir / f"{test_file.stem}.sim"
            self.assertTrue(sim_file.exists(), f"Missing: {sim_file}")
        
        # Display summary
        reporter.display_summary()
        
        # Save report
        report_file = self.test_dir / "test_report.json"
        reporter.save_report(report_file)
        self.assertTrue(report_file.exists())
        
        # Verify report content
        with open(report_file) as f:
            report = json.load(f)
        
        self.assertEqual(report["execution_summary"]["total_models"], len(self.test_files))
        self.assertEqual(report["execution_summary"]["successful"], len(self.test_files))
        
        print(f"\n✓ Successfully processed {len(self.test_files)} real .dat files")
        print(f"  Output directory: {output_dir}")
        print(f"  Report saved to: {report_file}")


def run_tests():
    """Run all tests and display results."""
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test cases
    suite.addTests(loader.loadTestsFromTestCase(TestUniversalRunner))
    suite.addTests(loader.loadTestsFromTestCase(TestRealOrcaFlexFiles))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Display summary
    print("\n" + "=" * 80)
    print("TEST SUMMARY")
    print("=" * 80)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Skipped: {len(result.skipped)}")
    
    if result.wasSuccessful():
        print("\n✓ All tests passed!")
        return 0
    else:
        print("\n✗ Some tests failed")
        return 1


if __name__ == '__main__':
    sys.exit(run_tests())