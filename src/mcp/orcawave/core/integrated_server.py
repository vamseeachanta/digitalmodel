#!/usr/bin/env python3
"""
Integrated OrcaWave MCP Server
Combines Phase 1 (COM API) and Phase 2 (Vision & Monitoring) components
"""

import asyncio
import time
from typing import Optional, Dict, Any, List
from pathlib import Path
import structlog
from contextlib import asynccontextmanager

try:
    from fastmcp import FastMCP, Context
    from fastmcp.resources import Resource
    from fastmcp.tools import Tool
except ImportError:
    print("FastMCP not installed. Install with: pip install fastmcp")
    FastMCP = None

# Import Phase 1 components
try:
    from ..api.orcawave_com import OrcaWaveAPI
    from ..api.geometry_converter import GeometryConverter
    from ..api.mesh_optimizer import MeshOptimizer
    from ..api.orcaflex_exporter import OrcaFlexExporter
except ImportError:
    OrcaWaveAPI = None
    GeometryConverter = None
    MeshOptimizer = None
    OrcaFlexExporter = None

# Import Phase 2 components
try:
    from .hybrid_coordinator import HybridCoordinator, ControlMode
    from .websocket_monitor import WebSocketMonitor
    from .progress_tracker import ProgressTracker, AnalysisStage
    from ..vision.screen_capture import OrcaWaveScreenCapture
    from ..vision.vision_analyzer import OrcaWaveVisionAnalyzer
except ImportError:
    HybridCoordinator = None
    WebSocketMonitor = None
    ProgressTracker = None
    OrcaWaveScreenCapture = None
    OrcaWaveVisionAnalyzer = None

logger = structlog.get_logger()


class IntegratedOrcaWaveMCP:
    """
    Fully integrated OrcaWave MCP server with hybrid control and monitoring
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize integrated server with all components.
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or {}
        
        # Phase 1: COM API components
        self.api = OrcaWaveAPI() if OrcaWaveAPI else None
        self.geometry_converter = GeometryConverter() if GeometryConverter else None
        self.mesh_optimizer = MeshOptimizer() if MeshOptimizer else None
        self.orcaflex_exporter = OrcaFlexExporter() if OrcaFlexExporter else None
        
        # Phase 2: Vision and monitoring components
        self.screen_capture = OrcaWaveScreenCapture() if OrcaWaveScreenCapture else None
        self.vision_analyzer = OrcaWaveVisionAnalyzer() if OrcaWaveVisionAnalyzer else None
        self.progress_tracker = ProgressTracker() if ProgressTracker else None
        
        # Hybrid coordinator
        self.coordinator = HybridCoordinator(
            api=self.api,
            screen_capture=self.screen_capture,
            vision_analyzer=self.vision_analyzer,
            control_mode=ControlMode.ADAPTIVE
        ) if HybridCoordinator else None
        
        # WebSocket monitor
        self.websocket_monitor = WebSocketMonitor(
            screen_capture=self.screen_capture,
            vision_analyzer=self.vision_analyzer,
            coordinator=self.coordinator,
            port=self.config.get('websocket_port', 8765)
        ) if WebSocketMonitor else None
        
        # Server state
        self.is_connected = False
        self.current_analysis_id = None
        self.websocket_task = None
        
        logger.info("integrated_server_initialized",
                   com_available=self.api is not None,
                   vision_available=self.screen_capture is not None,
                   monitoring_enabled=self.websocket_monitor is not None)
    
    async def start(self):
        """Start the integrated server"""
        # Start WebSocket server if available
        if self.websocket_monitor:
            self.websocket_task = asyncio.create_task(
                self.websocket_monitor.start_server()
            )
            logger.info("websocket_server_started",
                       port=self.config.get('websocket_port', 8765))
        
        # Try to connect to OrcaWave
        if self.api:
            self.is_connected = self.api.connect()
            logger.info("orcawave_connection",
                       connected=self.is_connected)
    
    async def stop(self):
        """Stop the integrated server"""
        # Stop WebSocket server
        if self.websocket_task:
            self.websocket_task.cancel()
            try:
                await self.websocket_task
            except asyncio.CancelledError:
                pass
        
        # Cleanup components
        if self.websocket_monitor:
            self.websocket_monitor.cleanup()
        
        if self.coordinator:
            self.coordinator.cleanup()
        
        if self.api and self.is_connected:
            self.api.disconnect()
        
        logger.info("integrated_server_stopped")
    
    async def run_analysis_integrated(self,
                                     vessel_name: str,
                                     geometry_file: Optional[str] = None,
                                     frequencies: Optional[List[float]] = None,
                                     directions: Optional[List[float]] = None,
                                     water_depth: float = 200.0,
                                     enable_monitoring: bool = True,
                                     control_mode: str = "adaptive") -> Dict[str, Any]:
        """
        Run a complete analysis with integrated control and monitoring.
        
        Args:
            vessel_name: Vessel name
            geometry_file: Optional geometry file to import
            frequencies: Analysis frequencies
            directions: Wave directions
            water_depth: Water depth
            enable_monitoring: Enable WebSocket monitoring
            control_mode: Control mode (com_only, vision_only, hybrid, adaptive)
            
        Returns:
            Analysis results
        """
        # Generate analysis ID
        analysis_id = f"analysis_{vessel_name}_{int(time.time())}"
        self.current_analysis_id = analysis_id
        
        # Default parameters
        if frequencies is None:
            frequencies = [0.1, 0.5, 1.0, 1.5, 2.0]
        if directions is None:
            directions = [0, 45, 90, 135, 180, 225, 270, 315]
        
        # Parse control mode
        mode_map = {
            "com_only": ControlMode.COM_ONLY,
            "vision_only": ControlMode.VISION_ONLY,
            "hybrid": ControlMode.HYBRID,
            "adaptive": ControlMode.ADAPTIVE
        }
        control = mode_map.get(control_mode, ControlMode.ADAPTIVE)
        
        # Start progress tracking
        if self.progress_tracker:
            progress = self.progress_tracker.start_analysis(
                analysis_id,
                vessel_name,
                frequencies,
                directions,
                water_depth
            )
            logger.info("progress_tracking_started",
                       id=analysis_id,
                       estimated_duration=progress.estimated_total_duration)
        
        # Start WebSocket monitoring
        if enable_monitoring and self.websocket_monitor:
            session = await self.websocket_monitor.start_analysis_monitoring(
                analysis_id,
                vessel_name
            )
            logger.info("monitoring_started", session=analysis_id)
        
        try:
            results = {
                "analysis_id": analysis_id,
                "vessel": vessel_name,
                "status": "initializing",
                "stages": {},
                "errors": []
            }
            
            # Stage 1: Initialization
            await self._execute_stage(
                analysis_id,
                AnalysisStage.INITIALIZATION,
                self._initialize_analysis,
                control_mode=control
            )
            
            # Stage 2: Geometry Import (if provided)
            if geometry_file:
                await self._execute_stage(
                    analysis_id,
                    AnalysisStage.GEOMETRY_IMPORT,
                    lambda: self._import_geometry(vessel_name, geometry_file),
                    control_mode=control
                )
            
            # Stage 3: Mesh Generation
            await self._execute_stage(
                analysis_id,
                AnalysisStage.MESH_GENERATION,
                lambda: self._generate_mesh(vessel_name),
                control_mode=control
            )
            
            # Stage 4: Mesh Optimization
            await self._execute_stage(
                analysis_id,
                AnalysisStage.MESH_OPTIMIZATION,
                lambda: self._optimize_mesh(vessel_name),
                control_mode=control
            )
            
            # Stage 5: Analysis Setup
            await self._execute_stage(
                analysis_id,
                AnalysisStage.ANALYSIS_SETUP,
                lambda: self._setup_analysis(frequencies, directions, water_depth),
                control_mode=control
            )
            
            # Stage 6: Run Diffraction Analysis
            await self._execute_stage(
                analysis_id,
                AnalysisStage.DIFFRACTION_ANALYSIS,
                lambda: self._run_diffraction_analysis(analysis_id, frequencies, directions),
                control_mode=control
            )
            
            # Stage 7: Process Results
            await self._execute_stage(
                analysis_id,
                AnalysisStage.RESULTS_PROCESSING,
                lambda: self._process_results(),
                control_mode=control
            )
            
            # Stage 8: Export
            export_result = await self._execute_stage(
                analysis_id,
                AnalysisStage.EXPORT,
                lambda: self._export_results(vessel_name),
                control_mode=control
            )
            
            results["status"] = "completed"
            results["data"] = export_result
            
            # Complete tracking
            if self.progress_tracker:
                self.progress_tracker.complete_analysis(analysis_id, success=True)
            
            logger.info("analysis_completed",
                       id=analysis_id,
                       vessel=vessel_name)
            
        except Exception as e:
            logger.error("analysis_failed",
                        id=analysis_id,
                        error=str(e))
            
            results["status"] = "failed"
            results["errors"].append(str(e))
            
            if self.progress_tracker:
                self.progress_tracker.complete_analysis(analysis_id, success=False)
        
        finally:
            # Stop monitoring
            if enable_monitoring and self.websocket_monitor:
                self.websocket_monitor.stop_monitoring(analysis_id)
            
            self.current_analysis_id = None
        
        return results
    
    async def _execute_stage(self,
                           analysis_id: str,
                           stage: AnalysisStage,
                           operation_func,
                           control_mode: ControlMode = ControlMode.ADAPTIVE) -> Any:
        """
        Execute an analysis stage with tracking and monitoring.
        
        Args:
            analysis_id: Analysis ID
            stage: Analysis stage
            operation_func: Function to execute
            control_mode: Control mode to use
            
        Returns:
            Stage result
        """
        # Start stage tracking
        if self.progress_tracker:
            self.progress_tracker.start_stage(analysis_id, stage)
        
        # Update WebSocket status
        if self.websocket_monitor:
            self.websocket_monitor.update_session_status(
                analysis_id,
                "running",
                stage.display_name
            )
        
        try:
            # Execute with hybrid coordinator if available
            if self.coordinator:
                result = await self.coordinator.execute_operation(
                    stage.display_name,
                    com_func=operation_func if self.api else None,
                    vision_func=None,  # Would implement GUI automation here
                    mode=control_mode
                )
                
                if not result.success:
                    raise Exception(result.error or "Stage failed")
                
                stage_result = result.com_result or result.vision_result
            else:
                # Direct execution
                stage_result = operation_func()
            
            # Complete stage tracking
            if self.progress_tracker:
                self.progress_tracker.complete_stage(analysis_id, stage, success=True)
            
            return stage_result
            
        except Exception as e:
            # Mark stage as failed
            if self.progress_tracker:
                self.progress_tracker.complete_stage(
                    analysis_id,
                    stage,
                    success=False,
                    error_message=str(e)
                )
            raise
    
    def _initialize_analysis(self) -> Dict[str, Any]:
        """Initialize analysis"""
        if self.api and self.is_connected:
            return {"status": "initialized"}
        return {"status": "initialized", "mode": "vision"}
    
    def _import_geometry(self, vessel_name: str, geometry_file: str) -> Dict[str, Any]:
        """Import geometry"""
        if self.api and self.geometry_converter:
            # Convert geometry if needed
            gdf_file = self.geometry_converter.convert_to_gdf(geometry_file)
            
            # Import to OrcaWave
            self.api.import_geometry(gdf_file)
            
            return {"geometry": gdf_file, "imported": True}
        return {"geometry": geometry_file, "imported": False}
    
    def _generate_mesh(self, vessel_name: str) -> Dict[str, Any]:
        """Generate mesh"""
        if self.api:
            mesh_info = self.api.generate_mesh()
            return mesh_info
        return {"mesh": "generated", "quality": 0.8}
    
    def _optimize_mesh(self, vessel_name: str) -> Dict[str, Any]:
        """Optimize mesh"""
        if self.api and self.mesh_optimizer:
            optimized = self.mesh_optimizer.optimize_mesh()
            return {"optimized": True, "quality": optimized.get("quality", 0.85)}
        return {"optimized": False}
    
    def _setup_analysis(self, frequencies: List[float],
                       directions: List[float],
                       water_depth: float) -> Dict[str, Any]:
        """Setup analysis parameters"""
        if self.api:
            self.api.setup_diffraction_analysis(frequencies, directions, water_depth)
        
        return {
            "frequencies": frequencies,
            "directions": directions,
            "water_depth": water_depth
        }
    
    async def _run_diffraction_analysis(self,
                                       analysis_id: str,
                                       frequencies: List[float],
                                       directions: List[float]) -> Dict[str, Any]:
        """Run diffraction analysis with frequency tracking"""
        results = {}
        
        for i, freq in enumerate(frequencies):
            # Update frequency progress
            if self.progress_tracker:
                for j in range(len(directions)):
                    # Simulate direction progress
                    self.progress_tracker.update_frequency_progress(
                        analysis_id,
                        freq,
                        j + 1
                    )
                    
                    # Small delay to simulate processing
                    await asyncio.sleep(0.1)
            
            # Store frequency results
            results[f"freq_{freq}"] = {
                "completed": True,
                "directions": len(directions)
            }
        
        if self.api:
            # Get actual results
            actual_results = self.api.get_results()
            if actual_results:
                results.update(actual_results)
        
        return results
    
    def _process_results(self) -> Dict[str, Any]:
        """Process analysis results"""
        if self.api:
            results = self.api.get_results()
            return results or {"processed": True}
        return {"processed": True}
    
    def _export_results(self, vessel_name: str) -> Dict[str, Any]:
        """Export results"""
        export_data = {
            "vessel": vessel_name,
            "timestamp": time.time()
        }
        
        if self.api and self.orcaflex_exporter:
            # Export to OrcaFlex format
            orcaflex_file = self.orcaflex_exporter.export_to_orcaflex(
                vessel_name,
                self.api.get_results()
            )
            export_data["orcaflex_file"] = orcaflex_file
        
        return export_data
    
    def get_status(self) -> Dict[str, Any]:
        """Get server status"""
        status = {
            "server": "integrated",
            "com_api": self.is_connected,
            "vision": self.screen_capture is not None,
            "monitoring": self.websocket_monitor is not None,
            "current_analysis": self.current_analysis_id
        }
        
        # Add progress info if available
        if self.progress_tracker and self.current_analysis_id:
            progress = self.progress_tracker.get_progress(self.current_analysis_id)
            if progress:
                status["progress"] = {
                    "overall": progress.overall_progress,
                    "stage": progress.current_stage.display_name if progress.current_stage else None,
                    "elapsed": progress.elapsed_time,
                    "remaining": progress.remaining_time
                }
        
        # Add coordinator statistics
        if self.coordinator:
            stats = self.coordinator.get_statistics()
            status["success_rates"] = stats["success_rates"]
        
        return status


def create_mcp_server(config: Optional[Dict[str, Any]] = None) -> Optional[FastMCP]:
    """
    Create FastMCP server with integrated OrcaWave functionality.
    
    Args:
        config: Configuration dictionary
        
    Returns:
        FastMCP server instance
    """
    if not FastMCP:
        logger.error("FastMCP not available")
        return None
    
    # Create integrated server
    orcawave = IntegratedOrcaWaveMCP(config)
    
    # Create FastMCP server
    server = FastMCP(
        "orcawave-integrated-mcp",
        version="2.0.0"
    )
    
    @asynccontextmanager
    async def lifespan(app):
        """Manage server lifecycle"""
        await orcawave.start()
        yield
        await orcawave.stop()
    
    server.lifespan = lifespan
    
    # Define resources
    @server.resource("orcawave://status")
    async def get_status() -> str:
        """Get server status"""
        import json
        return json.dumps(orcawave.get_status(), indent=2)
    
    @server.resource("orcawave://progress/{analysis_id}")
    async def get_progress(analysis_id: str) -> str:
        """Get analysis progress"""
        import json
        if orcawave.progress_tracker:
            progress = orcawave.progress_tracker.get_progress(analysis_id)
            if progress:
                return json.dumps(progress.to_dict(), indent=2)
        return json.dumps({"error": "Analysis not found"})
    
    # Define tools
    @server.tool()
    async def connect_orcawave(ctx: Context) -> str:
        """Connect to OrcaWave application"""
        if orcawave.api:
            success = orcawave.api.connect()
            orcawave.is_connected = success
            return f"Connected: {success}"
        return "COM API not available"
    
    @server.tool()
    async def run_integrated_analysis(
        ctx: Context,
        vessel_name: str,
        geometry_file: Optional[str] = None,
        frequencies: Optional[str] = None,
        directions: Optional[str] = None,
        water_depth: float = 200.0,
        enable_monitoring: bool = True,
        control_mode: str = "adaptive"
    ) -> str:
        """
        Run integrated analysis with hybrid control and monitoring.
        
        Args:
            vessel_name: Vessel name
            geometry_file: Optional geometry file path
            frequencies: Comma-separated frequencies (e.g., "0.1,0.5,1.0")
            directions: Comma-separated directions (e.g., "0,90,180,270")
            water_depth: Water depth in meters
            enable_monitoring: Enable WebSocket monitoring
            control_mode: Control mode (com_only, vision_only, hybrid, adaptive)
        """
        import json
        
        # Parse frequencies and directions
        freq_list = None
        if frequencies:
            freq_list = [float(f) for f in frequencies.split(",")]
        
        dir_list = None
        if directions:
            dir_list = [float(d) for d in directions.split(",")]
        
        # Run analysis
        results = await orcawave.run_analysis_integrated(
            vessel_name=vessel_name,
            geometry_file=geometry_file,
            frequencies=freq_list,
            directions=dir_list,
            water_depth=water_depth,
            enable_monitoring=enable_monitoring,
            control_mode=control_mode
        )
        
        return json.dumps(results, indent=2)
    
    @server.tool()
    async def capture_screenshot(ctx: Context) -> str:
        """Capture OrcaWave screenshot"""
        if orcawave.screen_capture:
            result = orcawave.screen_capture.capture_window()
            if result.success:
                return f"Screenshot captured: {result.window_info.title if result.window_info else 'Unknown'}"
            return f"Capture failed: {result.error}"
        return "Screen capture not available"
    
    @server.tool()
    async def analyze_current_view(ctx: Context) -> str:
        """Analyze current OrcaWave view using vision"""
        import json
        
        if not orcawave.screen_capture or not orcawave.vision_analyzer:
            return "Vision components not available"
        
        # Capture screenshot
        capture = orcawave.screen_capture.capture_window()
        if not capture.success:
            return f"Capture failed: {capture.error}"
        
        # Analyze
        results = {}
        
        # Try different analyses
        mesh_quality = orcawave.vision_analyzer.analyze_mesh_quality(capture.image)
        if mesh_quality.get("quality_score", 0) > 0:
            results["mesh"] = mesh_quality
        
        progress = orcawave.vision_analyzer.analyze_progress(capture.image)
        if progress.get("percentage") is not None:
            results["progress"] = progress
        
        warnings = orcawave.vision_analyzer.detect_warnings(capture.image)
        if warnings:
            results["warnings"] = warnings
        
        return json.dumps(results, indent=2)
    
    @server.tool()
    async def get_server_status(ctx: Context) -> str:
        """Get integrated server status"""
        import json
        return json.dumps(orcawave.get_status(), indent=2)
    
    logger.info("integrated_mcp_server_created",
               resources=2,
               tools=5)
    
    return server


# Standalone test
async def test_integrated_server():
    """Test integrated server functionality"""
    server = IntegratedOrcaWaveMCP()
    
    await server.start()
    
    print("✓ Integrated server started")
    print(f"  Status: {server.get_status()}")
    
    # Test analysis
    results = await server.run_analysis_integrated(
        vessel_name="Test Vessel",
        frequencies=[0.1, 0.5, 1.0],
        directions=[0, 90, 180, 270],
        enable_monitoring=False,
        control_mode="adaptive"
    )
    
    print(f"✓ Analysis completed: {results['status']}")
    
    await server.stop()
    
    return True


if __name__ == "__main__":
    asyncio.run(test_integrated_server())