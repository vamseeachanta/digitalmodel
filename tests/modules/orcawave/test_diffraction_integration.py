#!/usr/bin/env python3
"""
Integration tests for OrcaWave Diffraction Module
Tests the complete workflow from geometry validation to OrcaFlex conversion
"""

import sys
import json
import yaml
import shutil
import unittest
import tempfile
import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock
import numpy as np

# Add module to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

class TestOrcaWaveDiffractionIntegration(unittest.TestCase):
    """Integration tests for OrcaWave diffraction analysis workflow"""
    
    @classmethod
    def setUpClass(cls):
        """Set up test environment"""
        cls.repo_root = Path(__file__).parent.parent.parent.parent
        cls.module_dir = cls.repo_root / "src" / "modules" / "orcawave" / "diffraction"
        cls.geometry_dir = cls.repo_root / "specs" / "modules" / "orcawave" / "sea-cypress-diffraction-analysis" / "inputs" / "geometry"
        cls.test_vessel = "sea_cypress"
        
    def test_01_module_structure(self):
        """Test that module structure is correct"""
        # Check main directories exist
        self.assertTrue(self.module_dir.exists(), f"Module directory not found: {self.module_dir}")
        self.assertTrue((self.module_dir / "configs").exists(), "Configs directory not found")
        self.assertTrue((self.module_dir / "scripts").exists(), "Scripts directory not found")
        
        # Check key files exist
        self.assertTrue((self.module_dir / "orchestrator.py").exists(), "Orchestrator not found")
        self.assertTrue((self.module_dir / "configs" / "base_diffraction_config.yml").exists(), "Base config not found")
        self.assertTrue((self.module_dir / "configs" / "vessels" / "sea_cypress.yml").exists(), "Sea Cypress config not found")
        
    def test_02_geometry_files_exist(self):
        """Test that geometry files are in the correct location"""
        self.assertTrue(self.geometry_dir.exists(), f"Geometry directory not found: {self.geometry_dir}")
        
        expected_files = [
            "Sea Cypress_0.25 Mesh_Binary.stl",
            "Sea Cypress_0.25 Mesh_Ascii.stl", 
            "Sea Cypress_0.25 Mesh_Binary.obj"
        ]
        
        for filename in expected_files:
            file_path = self.geometry_dir / filename
            self.assertTrue(file_path.exists(), f"Geometry file not found: {filename}")
            self.assertGreater(file_path.stat().st_size, 0, f"Geometry file is empty: {filename}")
    
    def test_03_vessel_configuration(self):
        """Test vessel configuration loading and validation"""
        vessel_config_path = self.module_dir / "configs" / "vessels" / "sea_cypress.yml"
        
        with open(vessel_config_path, 'r') as f:
            config = yaml.safe_load(f)
        
        # Check required fields
        self.assertIn('vessel', config)
        self.assertIn('geometry', config)
        self.assertIn('name', config['vessel'])
        self.assertEqual(config['vessel']['name'], 'Sea Cypress')
        
        # Check geometry path
        geometry_path = config['geometry']['path']
        self.assertIn('specs', geometry_path)
        self.assertIn('sea-cypress-diffraction-analysis', geometry_path)
        
    def test_04_orchestrator_initialization(self):
        """Test orchestrator can be initialized with vessel config"""
        sys.path.insert(0, str(self.module_dir))
        
        try:
            from orchestrator import OrcaWaveOrchestrator
            
            # Initialize with vessel name
            orchestrator = OrcaWaveOrchestrator(
                vessel_name="sea_cypress",
                dry_run=True
            )
            
            self.assertEqual(orchestrator.vessel_name, "sea_cypress")
            self.assertIsNotNone(orchestrator.config)
            self.assertTrue(orchestrator.dry_run)
            
            # Check configuration was loaded
            self.assertIn('analysis', orchestrator.config)
            
        except ImportError as e:
            self.fail(f"Failed to import orchestrator: {e}")
    
    def test_05_geometry_validation_script(self):
        """Test geometry validation script functionality"""
        validation_script = self.module_dir / "scripts" / "validate_geometry.py"
        self.assertTrue(validation_script.exists(), "Validation script not found")
        
        # Test script can be imported
        sys.path.insert(0, str(self.module_dir / "scripts"))
        
        try:
            from validate_geometry import GeometryValidator, MeshStatistics
            
            # Create validator instance
            validator = GeometryValidator(str(self.geometry_dir))
            self.assertIsNotNone(validator)
            
            # Check that validator can access files
            stl_file = self.geometry_dir / "Sea Cypress_0.25 Mesh_Binary.stl"
            self.assertTrue(stl_file.exists())
            
        except ImportError as e:
            self.fail(f"Failed to import validation script: {e}")
    
    def test_06_orcaflex_converter(self):
        """Test OrcaFlex conversion script"""
        converter_script = self.module_dir / "scripts" / "convert_to_orcaflex.py"
        self.assertTrue(converter_script.exists(), "Converter script not found")
        
        sys.path.insert(0, str(self.module_dir / "scripts"))
        
        try:
            from convert_to_orcaflex import OrcaFlexConverter, VesselData
            
            # Create test vessel data
            vessel_data = VesselData(
                name="Test Vessel",
                frequencies=np.array([0.1, 0.5, 1.0]),
                directions=np.array([0, 45, 90, 135, 180]),
                added_mass=np.zeros((3, 6, 6)),
                damping=np.zeros((3, 6, 6)),
                excitation_force=np.zeros((3, 5, 6), dtype=complex),
                excitation_moment=np.zeros((3, 5, 6), dtype=complex),
                displacement=1000.0,
                centre_of_gravity=[0, 0, 5],
                centre_of_buoyancy=[0, 0, -1],
                waterplane_area=200.0,
                metacentric_height=1.5
            )
            
            # Test data container
            self.assertEqual(vessel_data.name, "Test Vessel")
            self.assertEqual(len(vessel_data.frequencies), 3)
            self.assertEqual(len(vessel_data.directions), 5)
            
        except ImportError as e:
            self.fail(f"Failed to import converter script: {e}")
    
    def test_07_list_vessels_command(self):
        """Test listing available vessels"""
        sys.path.insert(0, str(self.module_dir))
        
        try:
            from orchestrator import OrcaWaveOrchestrator
            
            vessels = OrcaWaveOrchestrator.list_available_vessels()
            self.assertIsInstance(vessels, list)
            self.assertIn('sea_cypress', vessels)
            
        except Exception as e:
            self.fail(f"Failed to list vessels: {e}")
    
    def test_08_dry_run_workflow(self):
        """Test dry run of complete workflow"""
        sys.path.insert(0, str(self.module_dir))
        
        try:
            from orchestrator import OrcaWaveOrchestrator
            
            # Create orchestrator in dry run mode
            orchestrator = OrcaWaveOrchestrator(
                vessel_name="sea_cypress",
                dry_run=True
            )
            
            # Test phase 1: Setup and validation
            # We'll mock the subprocess calls to avoid actual execution
            with patch('subprocess.run') as mock_run:
                mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")
                
                success = orchestrator._phase1_setup_validation()
                self.assertTrue(success, "Phase 1 validation failed")
                self.assertTrue(orchestrator.workflow_state['geometry_validated'])
            
            # Test phase 2: OrcaWave execution (should skip in dry run)
            success = orchestrator._phase2_orcawave_execution()
            self.assertTrue(success, "Phase 2 execution failed in dry run")
            self.assertTrue(orchestrator.workflow_state['orcawave_executed'])
            
        except Exception as e:
            self.fail(f"Dry run workflow failed: {e}")
    
    def test_09_config_template_substitution(self):
        """Test configuration template variable substitution"""
        base_config_path = self.module_dir / "configs" / "base_diffraction_config.yml"
        
        with open(base_config_path, 'r') as f:
            config_text = f.read()
        
        # Check that template variables are present
        self.assertIn('${VESSEL_NAME}', config_text)
        self.assertIn('${GEOMETRY_FILE}', config_text)
        self.assertIn('${GEOMETRY_PATH}', config_text)
        self.assertIn('${PROJECT_NAME}', config_text)
        
        # Test substitution
        from string import Template
        template = Template(config_text)
        
        substituted = template.safe_substitute(
            VESSEL_NAME="TestVessel",
            GEOMETRY_FILE="test.stl",
            GEOMETRY_PATH="/test/path",
            PROJECT_NAME="Test Project",
            DATE="2024-12-24"
        )
        
        self.assertIn("TestVessel", substituted)
        self.assertIn("test.stl", substituted)
        self.assertIn("/test/path", substituted)
        self.assertNotIn("${VESSEL_NAME}", substituted)
    
    def test_10_results_directory_creation(self):
        """Test that results directories are created correctly"""
        sys.path.insert(0, str(self.module_dir))
        
        try:
            from orchestrator import OrcaWaveOrchestrator
            
            orchestrator = OrcaWaveOrchestrator(
                vessel_name="test_vessel",
                dry_run=True
            )
            
            # Create output directories
            orchestrator._create_output_directories()
            
            # Check vessel-specific directory was created
            vessel_results = orchestrator.results_dir
            self.assertTrue(vessel_results.exists())
            self.assertIn("test_vessel", str(vessel_results))
            
            # Check subdirectories
            expected_subdirs = ["csv_outputs", "orcaflex", "validation", "logs", "cache"]
            for subdir in expected_subdirs:
                self.assertTrue((vessel_results / subdir).exists(), f"Missing subdir: {subdir}")
            
            # Clean up test directories
            if "test_vessel" in str(vessel_results):
                shutil.rmtree(vessel_results, ignore_errors=True)
                
        except Exception as e:
            self.fail(f"Results directory creation failed: {e}")

class TestBatchScript(unittest.TestCase):
    """Test batch script for Windows execution"""
    
    def setUp(self):
        self.repo_root = Path(__file__).parent.parent.parent.parent
        self.batch_script = self.repo_root / "src" / "modules" / "orcawave" / "diffraction" / "scripts" / "run_diffraction_analysis.bat"
    
    def test_batch_script_exists(self):
        """Test that batch script exists"""
        self.assertTrue(self.batch_script.exists(), "Batch script not found")
    
    def test_batch_script_content(self):
        """Test batch script has required content"""
        with open(self.batch_script, 'r') as f:
            content = f.read()
        
        # Check for key elements
        self.assertIn("ORCAWAVE_PATH", content)
        self.assertIn("CONFIG_FILE", content)
        self.assertIn("RESULTS_DIR", content)
        self.assertIn("Sea Cypress", content)

def suite():
    """Create test suite"""
    test_suite = unittest.TestSuite()
    
    # Add tests in order
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_01_module_structure'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_02_geometry_files_exist'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_03_vessel_configuration'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_04_orchestrator_initialization'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_05_geometry_validation_script'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_06_orcaflex_converter'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_07_list_vessels_command'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_08_dry_run_workflow'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_09_config_template_substitution'))
    test_suite.addTest(TestOrcaWaveDiffractionIntegration('test_10_results_directory_creation'))
    test_suite.addTest(TestBatchScript('test_batch_script_exists'))
    test_suite.addTest(TestBatchScript('test_batch_script_content'))
    
    return test_suite

if __name__ == '__main__':
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite())
    
    # Print summary
    print("\n" + "=" * 60)
    print("TEST SUMMARY")
    print("=" * 60)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Success rate: {((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100):.1f}%")
    
    if result.failures:
        print("\nFailed tests:")
        for test, trace in result.failures:
            print(f"  - {test}")
    
    if result.errors:
        print("\nTests with errors:")
        for test, trace in result.errors:
            print(f"  - {test}")
    
    sys.exit(0 if result.wasSuccessful() else 1)