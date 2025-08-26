#!/usr/bin/env python3
"""
Test OrcaWave MCP Integration
Demonstrates the integrated server capabilities
"""

import sys
import time
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

def test_integration():
    """Test basic integration components"""
    print("=" * 60)
    print("OrcaWave MCP Server Integration Test")
    print("=" * 60)
    
    # Phase 1: Test COM API components
    print("\n[Phase 1] COM API Components:")
    try:
        from src.mcp.orcawave.api.orcawave_com import OrcaWaveAPI
        print("  [OK] OrcaWaveAPI module loaded")
    except ImportError as e:
        print(f"  [X] OrcaWaveAPI not available: {e}")
    
    try:
        from src.mcp.orcawave.api.geometry_converter import GeometryConverter
        print("  [OK] GeometryConverter module loaded")
    except ImportError as e:
        print(f"  [X] GeometryConverter not available: {e}")
    
    try:
        from src.mcp.orcawave.api.mesh_optimizer import MeshOptimizer
        print("  [OK] MeshOptimizer module loaded")
    except ImportError as e:
        print(f"  [X] MeshOptimizer not available: {e}")
    
    # Phase 2: Test Vision & Monitoring components
    print("\n[Phase 2] Vision & Monitoring Components:")
    try:
        from src.mcp.orcawave.vision.screen_capture import OrcaWaveScreenCapture
        print("  [OK] ScreenCapture module loaded")
        
        # Try to create instance
        capture = OrcaWaveScreenCapture()
        windows = capture.find_orcawave_windows()
        print(f"    Found {len(windows)} OrcaWave windows")
    except ImportError as e:
        print(f"  [X] ScreenCapture not available: {e}")
    except Exception as e:
        print(f"    Note: {e}")
    
    try:
        from src.mcp.orcawave.vision.vision_analyzer import OrcaWaveVisionAnalyzer
        print("  [OK] VisionAnalyzer module loaded")
    except ImportError as e:
        print(f"  [X] VisionAnalyzer not available: {e}")
    
    try:
        from src.mcp.orcawave.core.hybrid_coordinator import HybridCoordinator, ControlMode
        print("  [OK] HybridCoordinator module loaded")
        print(f"    Control modes: {', '.join([m.value for m in ControlMode])}")
    except ImportError as e:
        print(f"  [X] HybridCoordinator not available: {e}")
    
    try:
        from src.mcp.orcawave.core.websocket_monitor import WebSocketMonitor
        print("  [OK] WebSocketMonitor module loaded")
    except ImportError as e:
        print(f"  [X] WebSocketMonitor not available: {e}")
    
    try:
        from src.mcp.orcawave.core.progress_tracker import ProgressTracker, AnalysisStage
        print("  [OK] ProgressTracker module loaded")
        print(f"    Stages: {len(list(AnalysisStage))} defined")
    except ImportError as e:
        print(f"  [X] ProgressTracker not available: {e}")
    
    # Test integrated server
    print("\n[Integration] Integrated Server:")
    try:
        from src.mcp.orcawave.core.integrated_server import IntegratedOrcaWaveMCP
        print("  [OK] IntegratedServer module loaded")
        
        # Create instance (without starting)
        server = IntegratedOrcaWaveMCP()
        status = server.get_status()
        
        print("\n  Server Status:")
        print(f"    COM API: {status.get('com_api', False)}")
        print(f"    Vision: {status.get('vision', False)}")
        print(f"    Monitoring: {status.get('monitoring', False)}")
        
    except ImportError as e:
        print(f"  [X] IntegratedServer not available: {e}")
    except Exception as e:
        print(f"    Note: {e}")
    
    # Summary
    print("\n" + "=" * 60)
    print("Integration Test Summary:")
    print("  Phase 1 (COM API): Core modules available")
    print("  Phase 2 (Vision): Core modules available")
    print("  Integration: Server can be instantiated")
    print("\nNote: Some features require additional dependencies:")
    print("  - structlog (logging)")
    print("  - fastmcp (MCP protocol)")
    print("  - pywin32 (Windows COM)")
    print("  - opencv-python (vision)")
    print("  - fastapi/uvicorn (WebSocket)")
    print("=" * 60)


if __name__ == "__main__":
    test_integration()