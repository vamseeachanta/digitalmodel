#!/usr/bin/env python3
"""
Test suite for OrcaWave COM API connection
Run this to verify OrcaWave is installed and accessible
"""

import sys
import unittest
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

# Import from parent module
from digitalmodel.workflows.mcp_server.orcawave.api.orcawave_com import OrcaWaveAPI, AnalysisType, VesselType, MeshStatistics


class TestOrcaWaveConnection(unittest.TestCase):
    """Test OrcaWave COM connection and basic operations"""
    
    def setUp(self):
        """Setup test environment"""
        self.api = OrcaWaveAPI()
    
    def tearDown(self):
        """Cleanup after tests"""
        if self.api.is_connected:
            self.api.disconnect()
    
    def test_connection(self):
        """Test basic connection to OrcaWave"""
        # This test will only pass if OrcaWave is installed
        # For CI/CD, we'll need to mock this
        try:
            result = self.api.connect()
            if result:
                self.assertTrue(self.api.is_connected)
                print(f"✓ Connected to OrcaWave version: {self.api._get_version()}")
            else:
                print("✗ OrcaWave not installed - skipping live tests")
                self.skipTest("OrcaWave not available")
        except Exception as e:
            print(f"✗ Connection failed: {e}")
            self.skipTest("OrcaWave not available")
    
    @patch('api.orcawave_com.win32com.client')
    def test_mocked_connection(self, mock_com):
        """Test connection with mocked COM object"""
        # Setup mock
        mock_app = MagicMock()
        mock_app.Visible = True
        mock_app.Version = "11.0.0"
        mock_app.ModelCount = 0
        mock_app.CreateModel.return_value = MagicMock()
        
        mock_com.Dispatch.return_value = mock_app
        
        # Test connection
        result = self.api.connect()
        self.assertTrue(result)
        self.assertTrue(self.api.is_connected)
        
        # Verify COM was called correctly
        mock_com.Dispatch.assert_called_with("OrcaWave.Application")
    
    @patch('api.orcawave_com.win32com.client')
    def test_vessel_creation(self, mock_com):
        """Test vessel creation through COM API"""
        # Setup mock
        mock_app = MagicMock()
        mock_model = MagicMock()
        mock_vessel = MagicMock()
        mock_vessel.Handle = "vessel_001"
        
        mock_model.CreateObject.return_value = mock_vessel
        mock_app.Model = [mock_model]
        mock_app.ModelCount = 1
        mock_com.GetObject.return_value = mock_app
        
        self.api.app = mock_app
        self.api.model = mock_model
        self.api.is_connected = True
        
        # Create vessel
        result = self.api.create_vessel(
            name="Test FPSO",
            vessel_type=VesselType.FPSO,
            dimensions={
                "length": 300.0,
                "beam": 60.0,
                "draft": 20.0
            }
        )
        
        # Verify result
        self.assertTrue(result["success"])
        self.assertEqual(result["name"], "Test FPSO")
        self.assertEqual(result["type"], "FPSO")
        self.assertEqual(result["vessel_id"], "vessel_001")
        
        # Verify vessel properties were set
        self.assertEqual(mock_vessel.Name, "Test FPSO")
        self.assertEqual(mock_vessel.Length, 300.0)
        self.assertEqual(mock_vessel.Beam, 60.0)
        self.assertEqual(mock_vessel.Draft, 20.0)
    
    def test_mesh_quality_calculation(self):
        """Test mesh quality score calculation"""
        # Test with ideal values
        score = self.api._calculate_mesh_quality_score(
            aspect_ratio=1.5,
            skewness=0.1,
            panel_count=3000
        )
        self.assertGreater(score, 0.8)
        
        # Test with poor values
        score = self.api._calculate_mesh_quality_score(
            aspect_ratio=4.0,
            skewness=0.4,
            panel_count=200
        )
        self.assertLess(score, 0.5)
        
        # Test with excessive panels
        score = self.api._calculate_mesh_quality_score(
            aspect_ratio=1.5,
            skewness=0.1,
            panel_count=10000
        )
        self.assertLess(score, 0.9)
    
    def test_mesh_statistics_structure(self):
        """Test MeshStatistics dataclass"""
        stats = MeshStatistics(
            panel_count=2500,
            node_count=2600,
            waterline_panels=150,
            max_aspect_ratio=2.8,
            avg_aspect_ratio=1.9,
            max_skewness=0.35,
            avg_skewness=0.18,
            symmetry_detected=True,
            quality_score=0.82
        )
        
        self.assertEqual(stats.panel_count, 2500)
        self.assertEqual(stats.quality_score, 0.82)
        self.assertTrue(stats.symmetry_detected)
    
    def test_analysis_types(self):
        """Test analysis type enumeration"""
        self.assertEqual(AnalysisType.DIFFRACTION.value, "Diffraction")
        self.assertEqual(AnalysisType.RADIATION.value, "Radiation")
        self.assertEqual(AnalysisType.BOTH.value, "DiffractionAndRadiation")
    
    def test_vessel_types(self):
        """Test vessel type enumeration"""
        self.assertEqual(VesselType.SHIP.value, "Ship")
        self.assertEqual(VesselType.FPSO.value, "FPSO")
        self.assertEqual(VesselType.SEMI_SUBMERSIBLE.value, "SemiSubmersible")


class TestMCPServer(unittest.TestCase):
    """Test MCP server functionality"""
    
    @patch('core.mcp_server.OrcaWaveAPI')
    def test_server_initialization(self, mock_api_class):
        """Test MCP server initialization"""
        from core.mcp_server import OrcaWaveMCPServer
        
        # Create server
        server = OrcaWaveMCPServer()
        
        # Verify configuration loaded
        self.assertEqual(server.config.server.name, "orcawave-mcp-server")
        self.assertEqual(server.config.server.port, 3100)
        
        # Verify API initialized
        mock_api_class.assert_called_once()
    
    @patch('core.mcp_server.OrcaWaveAPI')
    def test_server_tools_registered(self, mock_api_class):
        """Test that MCP tools are registered"""
        from core.mcp_server import OrcaWaveMCPServer
        
        server = OrcaWaveMCPServer()
        
        # Check tools are registered
        # Note: FastMCP stores tools internally
        # This is a simplified check
        self.assertIsNotNone(server.mcp)
        self.assertEqual(server.config.server.name, "orcawave-mcp-server")


def run_tests():
    """Run all tests"""
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add tests
    suite.addTests(loader.loadTestsFromTestCase(TestOrcaWaveConnection))
    suite.addTests(loader.loadTestsFromTestCase(TestMCPServer))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)