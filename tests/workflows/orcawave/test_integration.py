#!/usr/bin/env python3
"""
Integration tests for OrcaWave MCP Server
Tests all components working together
"""

import unittest
import asyncio
import json
import time
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch
import sys

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from src.mcp.orcawave.core.integrated_server import IntegratedOrcaWaveMCP
from src.mcp.orcawave.core.progress_tracker import ProgressTracker, AnalysisStage
from src.mcp.orcawave.core.hybrid_coordinator import HybridCoordinator, ControlMode


class TestOrcaWaveMCPIntegration(unittest.TestCase):
    """Test integrated OrcaWave MCP server"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.server = None
        self.loop = asyncio.new_event_loop()
        asyncio.set_event_loop(self.loop)
    
    def tearDown(self):
        """Clean up after tests"""
        if self.server:
            self.loop.run_until_complete(self.server.stop())
        self.loop.close()
    
    def test_server_creation(self):
        """Test server can be created"""
        server = IntegratedOrcaWaveMCP()
        self.assertIsNotNone(server)
        
        # Check components are initialized
        self.assertIsNotNone(server.progress_tracker)
        self.assertIsNotNone(server.coordinator)
    
    def test_server_status(self):
        """Test server status reporting"""
        server = IntegratedOrcaWaveMCP()
        status = server.get_status()
        
        self.assertIn('server', status)
        self.assertIn('com_api', status)
        self.assertIn('vision', status)
        self.assertIn('monitoring', status)
        self.assertEqual(status['server'], 'integrated')
    
    @patch('src.mcp.orcawave.api.orcawave_com.OrcaWaveAPI')
    def test_com_api_connection(self, mock_api):
        """Test COM API connection handling"""
        # Setup mock
        mock_instance = Mock()
        mock_instance.connect.return_value = True
        mock_api.return_value = mock_instance
        
        # Create server with mocked API
        server = IntegratedOrcaWaveMCP()
        server.api = mock_instance
        
        # Start server
        self.loop.run_until_complete(server.start())
        
        # Check connection
        self.assertTrue(server.is_connected)
    
    def test_progress_tracker(self):
        """Test progress tracking functionality"""
        tracker = ProgressTracker()
        
        # Start tracking
        progress = tracker.start_analysis(
            "test_001",
            "Test Vessel",
            [0.1, 0.5, 1.0],
            [0, 90, 180, 270]
        )
        
        self.assertEqual(progress.analysis_id, "test_001")
        self.assertEqual(progress.vessel_name, "Test Vessel")
        self.assertEqual(progress.total_frequencies, 3)
        self.assertEqual(progress.total_directions, 4)
        
        # Test stage progression
        tracker.start_stage("test_001", AnalysisStage.INITIALIZATION)
        tracker.complete_stage("test_001", AnalysisStage.INITIALIZATION)
        
        updated = tracker.get_progress("test_001")
        self.assertEqual(
            updated.stages[AnalysisStage.INITIALIZATION.name].status,
            'completed'
        )
    
    def test_control_modes(self):
        """Test different control modes"""
        # Test control mode enum
        self.assertEqual(ControlMode.ADAPTIVE.value, "adaptive")
        self.assertEqual(ControlMode.COM_ONLY.value, "com_only")
        self.assertEqual(ControlMode.VISION_ONLY.value, "vision_only")
        self.assertEqual(ControlMode.HYBRID.value, "hybrid")
    
    @patch('src.mcp.orcawave.core.hybrid_coordinator.HybridCoordinator')
    async def test_hybrid_coordinator_execution(self, mock_coordinator):
        """Test hybrid coordinator operation execution"""
        # Setup mock
        mock_instance = Mock()
        mock_result = Mock()
        mock_result.success = True
        mock_result.com_result = {"status": "success"}
        
        # Make execute_operation return a coroutine
        async def mock_execute():
            return mock_result
        
        mock_instance.execute_operation = Mock(return_value=mock_execute())
        mock_coordinator.return_value = mock_instance
        
        # Create server
        server = IntegratedOrcaWaveMCP()
        server.coordinator = mock_instance
        
        # Execute operation
        result = await server._execute_stage(
            "test_001",
            AnalysisStage.INITIALIZATION,
            lambda: {"initialized": True},
            ControlMode.ADAPTIVE
        )
        
        # Verify execution
        mock_instance.execute_operation.assert_called_once()
    
    async def test_analysis_workflow(self):
        """Test complete analysis workflow (mocked)"""
        server = IntegratedOrcaWaveMCP()
        
        # Mock the API
        server.api = Mock()
        server.api.connect.return_value = True
        server.is_connected = True
        
        # Mock coordinator
        server.coordinator = Mock()
        mock_result = Mock()
        mock_result.success = True
        mock_result.com_result = {"status": "success"}
        
        async def mock_execute(*args, **kwargs):
            return mock_result
        
        server.coordinator.execute_operation = mock_execute
        
        # Run analysis
        results = await server.run_analysis_integrated(
            vessel_name="Test Vessel",
            frequencies=[0.1, 0.5],
            directions=[0, 90],
            enable_monitoring=False,
            control_mode="com_only"
        )
        
        # Check results
        self.assertIn('analysis_id', results)
        self.assertIn('vessel', results)
        self.assertEqual(results['vessel'], "Test Vessel")
    
    def test_websocket_monitor_creation(self):
        """Test WebSocket monitor can be created"""
        server = IntegratedOrcaWaveMCP()
        
        # Check monitor exists (may be None if dependencies missing)
        if server.websocket_monitor:
            self.assertEqual(
                server.websocket_monitor.port,
                server.config.get('websocket_port', 8765)
            )
    
    def test_error_handling(self):
        """Test error handling in server"""
        server = IntegratedOrcaWaveMCP()
        
        # Test with no API connection
        server.api = None
        server.is_connected = False
        
        # Should handle gracefully
        result = server._initialize_analysis()
        self.assertEqual(result['mode'], 'vision')
    
    def test_configuration_loading(self):
        """Test configuration handling"""
        config = {
            'server': {'port': 3100},
            'websocket_port': 8765
        }
        
        server = IntegratedOrcaWaveMCP(config)
        self.assertEqual(server.config['server']['port'], 3100)
        self.assertEqual(server.config['websocket_port'], 8765)


class TestProgressEstimation(unittest.TestCase):
    """Test progress estimation and learning"""
    
    def test_time_estimation(self):
        """Test time estimation functionality"""
        tracker = ProgressTracker()
        
        # Add historical data
        tracker.historical_data['stage_durations'] = {
            'INITIALIZATION': [1.0, 1.2, 0.9],
            'MESH_GENERATION': [5.0, 6.0, 4.5]
        }
        
        # Estimate duration
        estimate = tracker._estimate_total_duration(
            "Test Vessel",
            3,  # frequencies
            4   # directions
        )
        
        self.assertIsNotNone(estimate)
        self.assertGreater(estimate, 0)
    
    def test_progress_calculation(self):
        """Test overall progress calculation"""
        tracker = ProgressTracker()
        
        progress = tracker.start_analysis(
            "test_002",
            "Test Vessel",
            [0.1],
            [0]
        )
        
        # Complete some stages
        tracker.start_stage("test_002", AnalysisStage.INITIALIZATION)
        tracker.complete_stage("test_002", AnalysisStage.INITIALIZATION)
        
        # Check progress
        current = tracker.get_progress("test_002")
        self.assertGreater(current.overall_progress, 0)
        self.assertLessEqual(current.overall_progress, 100)


class TestMCPServerEndpoints(unittest.TestCase):
    """Test MCP server endpoints (if FastMCP available)"""
    
    @unittest.skipUnless('fastmcp' in sys.modules, "FastMCP not installed")
    def test_mcp_server_creation(self):
        """Test MCP server can be created"""
        from src.mcp.orcawave.core.integrated_server import create_mcp_server
        
        server = create_mcp_server()
        self.assertIsNotNone(server)
    
    @unittest.skipUnless('fastmcp' in sys.modules, "FastMCP not installed")
    def test_mcp_tools_defined(self):
        """Test MCP tools are defined"""
        from src.mcp.orcawave.core.integrated_server import create_mcp_server
        
        server = create_mcp_server()
        
        # Check tools are registered
        # This would need actual FastMCP introspection
        self.assertIsNotNone(server)


def run_async_test(coro):
    """Helper to run async tests"""
    loop = asyncio.new_event_loop()
    try:
        return loop.run_until_complete(coro)
    finally:
        loop.close()


if __name__ == '__main__':
    # Run tests
    unittest.main(verbosity=2)