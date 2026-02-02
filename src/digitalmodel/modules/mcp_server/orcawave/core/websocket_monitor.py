#!/usr/bin/env python3
"""
WebSocket Monitoring for OrcaWave MCP
Provides real-time monitoring and streaming updates
"""

import asyncio
import json
import time
from typing import Optional, Dict, Any, List, Set, Callable
from dataclasses import dataclass, asdict
from datetime import datetime
import threading
from queue import Queue, Empty
import structlog

try:
    from fastapi import FastAPI, WebSocket, WebSocketDisconnect
    from fastapi.middleware.cors import CORSMiddleware
    import uvicorn
except ImportError:
    FastAPI = None
    WebSocket = None
    WebSocketDisconnect = None
    uvicorn = None

try:
    from ..vision.screen_capture import OrcaWaveScreenCapture
    from ..vision.vision_analyzer import OrcaWaveVisionAnalyzer
    from .hybrid_coordinator import HybridCoordinator
except ImportError:
    OrcaWaveScreenCapture = None
    OrcaWaveVisionAnalyzer = None
    HybridCoordinator = None

logger = structlog.get_logger()


@dataclass
class MonitoringEvent:
    """Event for monitoring stream"""
    event_type: str  # 'progress', 'screenshot', 'warning', 'error', 'status'
    timestamp: float
    data: Dict[str, Any]
    analysis_id: Optional[str] = None
    
    def to_json(self) -> str:
        """Convert to JSON string"""
        return json.dumps({
            "type": self.event_type,
            "timestamp": self.timestamp,
            "data": self.data,
            "analysis_id": self.analysis_id
        })


@dataclass
class AnalysisSession:
    """Active analysis session"""
    id: str
    vessel_name: str
    start_time: float
    status: str  # 'initializing', 'running', 'completed', 'failed'
    progress: float
    current_stage: str
    monitoring_enabled: bool
    clients: Set[str]  # Connected WebSocket client IDs


class WebSocketMonitor:
    """
    WebSocket-based real-time monitoring for OrcaWave analyses.
    Streams progress, screenshots, and events to connected clients.
    """
    
    def __init__(self,
                 screen_capture: Optional[OrcaWaveScreenCapture] = None,
                 vision_analyzer: Optional[OrcaWaveVisionAnalyzer] = None,
                 coordinator: Optional[HybridCoordinator] = None,
                 port: int = 8765):
        """
        Initialize WebSocket monitor.
        
        Args:
            screen_capture: Screen capture instance
            vision_analyzer: Vision analyzer instance
            coordinator: Hybrid coordinator instance
            port: WebSocket server port
        """
        self.screen_capture = screen_capture or (OrcaWaveScreenCapture() if OrcaWaveScreenCapture else None)
        self.vision_analyzer = vision_analyzer or (OrcaWaveVisionAnalyzer() if OrcaWaveVisionAnalyzer else None)
        self.coordinator = coordinator or (HybridCoordinator() if HybridCoordinator else None)
        self.port = port
        
        # Session management
        self.sessions: Dict[str, AnalysisSession] = {}
        self.active_monitors: Dict[str, asyncio.Task] = {}
        
        # WebSocket connections
        self.connections: Dict[str, WebSocket] = {}
        self.connection_sessions: Dict[str, str] = {}  # client_id -> session_id
        
        # Event queue for buffering
        self.event_queue = Queue(maxsize=1000)
        self.event_thread = None
        
        # FastAPI app
        self.app = None
        self.server = None
        
        # Monitoring configuration
        self.screenshot_interval = 5.0  # seconds
        self.progress_interval = 1.0  # seconds
        self.max_screenshot_size = 1024 * 1024  # 1MB
        
        logger.info("websocket_monitor_initialized", port=port)
    
    def create_app(self) -> FastAPI:
        """
        Create FastAPI application with WebSocket endpoints.
        
        Returns:
            FastAPI application
        """
        if not FastAPI:
            logger.error("FastAPI not available")
            return None
        
        app = FastAPI(title="OrcaWave Monitor")
        
        # CORS middleware for browser clients
        app.add_middleware(
            CORSMiddleware,
            allow_origins=["*"],
            allow_credentials=True,
            allow_methods=["*"],
            allow_headers=["*"]
        )
        
        @app.get("/")
        async def root():
            return {
                "service": "OrcaWave WebSocket Monitor",
                "version": "1.0.0",
                "status": "running",
                "sessions": len(self.sessions),
                "connections": len(self.connections)
            }
        
        @app.get("/sessions")
        async def get_sessions():
            """Get active analysis sessions"""
            return {
                "sessions": [
                    {
                        "id": session.id,
                        "vessel": session.vessel_name,
                        "status": session.status,
                        "progress": session.progress,
                        "clients": len(session.clients)
                    }
                    for session in self.sessions.values()
                ]
            }
        
        @app.websocket("/ws/{client_id}")
        async def websocket_endpoint(websocket: WebSocket, client_id: str):
            """WebSocket connection endpoint"""
            await self.handle_connection(websocket, client_id)
        
        @app.websocket("/ws/{client_id}/{session_id}")
        async def session_websocket(websocket: WebSocket, client_id: str, session_id: str):
            """WebSocket connection for specific session"""
            await self.handle_connection(websocket, client_id, session_id)
        
        self.app = app
        return app
    
    async def handle_connection(self, websocket: WebSocket, 
                               client_id: str, session_id: Optional[str] = None):
        """
        Handle WebSocket connection.
        
        Args:
            websocket: WebSocket connection
            client_id: Client identifier
            session_id: Optional session to subscribe to
        """
        await websocket.accept()
        self.connections[client_id] = websocket
        
        # Subscribe to session if specified
        if session_id and session_id in self.sessions:
            self.connection_sessions[client_id] = session_id
            self.sessions[session_id].clients.add(client_id)
            
            # Send initial session state
            await self.send_session_state(client_id, session_id)
        
        logger.info("websocket_connected", client=client_id, session=session_id)
        
        try:
            while True:
                # Receive and handle client messages
                data = await websocket.receive_text()
                await self.handle_client_message(client_id, data)
                
        except WebSocketDisconnect:
            logger.info("websocket_disconnected", client=client_id)
        except Exception as e:
            logger.error("websocket_error", client=client_id, error=str(e))
        finally:
            # Cleanup connection
            self.connections.pop(client_id, None)
            
            # Remove from session
            if client_id in self.connection_sessions:
                session_id = self.connection_sessions[client_id]
                if session_id in self.sessions:
                    self.sessions[session_id].clients.discard(client_id)
                self.connection_sessions.pop(client_id)
    
    async def handle_client_message(self, client_id: str, message: str):
        """
        Handle message from client.
        
        Args:
            client_id: Client identifier
            message: Message content
        """
        try:
            data = json.loads(message)
            command = data.get("command")
            
            if command == "subscribe":
                # Subscribe to session
                session_id = data.get("session_id")
                if session_id in self.sessions:
                    self.connection_sessions[client_id] = session_id
                    self.sessions[session_id].clients.add(client_id)
                    await self.send_session_state(client_id, session_id)
            
            elif command == "unsubscribe":
                # Unsubscribe from session
                if client_id in self.connection_sessions:
                    session_id = self.connection_sessions[client_id]
                    if session_id in self.sessions:
                        self.sessions[session_id].clients.discard(client_id)
                    self.connection_sessions.pop(client_id)
            
            elif command == "screenshot":
                # Request screenshot
                await self.send_screenshot(client_id)
            
            elif command == "ping":
                # Respond to ping
                await self.send_to_client(client_id, {"type": "pong"})
            
        except json.JSONDecodeError:
            logger.error("invalid_client_message", client=client_id, message=message)
        except Exception as e:
            logger.error("message_handling_error", client=client_id, error=str(e))
    
    async def send_session_state(self, client_id: str, session_id: str):
        """
        Send current session state to client.
        
        Args:
            client_id: Client identifier
            session_id: Session identifier
        """
        if session_id not in self.sessions:
            return
        
        session = self.sessions[session_id]
        state = {
            "type": "session_state",
            "session": {
                "id": session.id,
                "vessel": session.vessel_name,
                "status": session.status,
                "progress": session.progress,
                "stage": session.current_stage,
                "start_time": session.start_time,
                "elapsed": time.time() - session.start_time
            }
        }
        
        await self.send_to_client(client_id, state)
    
    async def send_screenshot(self, client_id: str):
        """
        Send screenshot to client.
        
        Args:
            client_id: Client identifier
        """
        if not self.screen_capture:
            await self.send_to_client(client_id, {
                "type": "error",
                "message": "Screenshot capture not available"
            })
            return
        
        try:
            capture = self.screen_capture.capture_window()
            if capture.success and capture.image_base64:
                # Check size and compress if needed
                if len(capture.image_base64) > self.max_screenshot_size:
                    # Would implement compression here
                    pass
                
                await self.send_to_client(client_id, {
                    "type": "screenshot",
                    "data": capture.image_base64,
                    "timestamp": capture.timestamp
                })
            else:
                await self.send_to_client(client_id, {
                    "type": "error",
                    "message": "Screenshot capture failed"
                })
                
        except Exception as e:
            logger.error("screenshot_send_error", client=client_id, error=str(e))
    
    async def send_to_client(self, client_id: str, data: Dict[str, Any]):
        """
        Send data to specific client.
        
        Args:
            client_id: Client identifier
            data: Data to send
        """
        if client_id in self.connections:
            try:
                await self.connections[client_id].send_text(json.dumps(data))
            except Exception as e:
                logger.error("send_error", client=client_id, error=str(e))
    
    async def broadcast_to_session(self, session_id: str, data: Dict[str, Any]):
        """
        Broadcast data to all clients in session.
        
        Args:
            session_id: Session identifier
            data: Data to broadcast
        """
        if session_id not in self.sessions:
            return
        
        session = self.sessions[session_id]
        disconnected = []
        
        for client_id in session.clients:
            if client_id in self.connections:
                try:
                    await self.connections[client_id].send_text(json.dumps(data))
                except:
                    disconnected.append(client_id)
        
        # Remove disconnected clients
        for client_id in disconnected:
            session.clients.discard(client_id)
            self.connections.pop(client_id, None)
            self.connection_sessions.pop(client_id, None)
    
    async def start_analysis_monitoring(self, session_id: str,
                                       vessel_name: str) -> AnalysisSession:
        """
        Start monitoring an analysis session.
        
        Args:
            session_id: Unique session identifier
            vessel_name: Vessel being analyzed
            
        Returns:
            AnalysisSession object
        """
        # Create session
        session = AnalysisSession(
            id=session_id,
            vessel_name=vessel_name,
            start_time=time.time(),
            status="initializing",
            progress=0.0,
            current_stage="setup",
            monitoring_enabled=True,
            clients=set()
        )
        
        self.sessions[session_id] = session
        
        # Start monitoring task
        monitor_task = asyncio.create_task(
            self._monitor_session(session_id)
        )
        self.active_monitors[session_id] = monitor_task
        
        logger.info("analysis_monitoring_started",
                   session=session_id,
                   vessel=vessel_name)
        
        # Broadcast session start
        await self.broadcast_to_session(session_id, {
            "type": "session_start",
            "session_id": session_id,
            "vessel": vessel_name,
            "timestamp": time.time()
        })
        
        return session
    
    async def _monitor_session(self, session_id: str):
        """
        Monitor an analysis session.
        
        Args:
            session_id: Session to monitor
        """
        session = self.sessions[session_id]
        last_screenshot = 0
        last_progress = 0
        
        try:
            while session.monitoring_enabled and session.status == "running":
                current_time = time.time()
                
                # Progress update
                if current_time - last_progress >= self.progress_interval:
                    await self._send_progress_update(session_id)
                    last_progress = current_time
                
                # Screenshot update
                if current_time - last_screenshot >= self.screenshot_interval:
                    await self._send_screenshot_update(session_id)
                    last_screenshot = current_time
                
                # Check for warnings/errors
                await self._check_for_issues(session_id)
                
                await asyncio.sleep(0.5)
                
        except asyncio.CancelledError:
            logger.info("monitoring_cancelled", session=session_id)
        except Exception as e:
            logger.error("monitoring_error", session=session_id, error=str(e))
    
    async def _send_progress_update(self, session_id: str):
        """
        Send progress update for session.
        
        Args:
            session_id: Session identifier
        """
        if not self.screen_capture or not self.vision_analyzer:
            return
        
        try:
            # Capture and analyze progress
            capture = self.screen_capture.capture_window()
            if capture.success:
                progress_info = self.vision_analyzer.analyze_progress(capture.image)
                
                if progress_info:
                    session = self.sessions[session_id]
                    session.progress = progress_info.get("percentage", session.progress)
                    
                    # Broadcast progress
                    await self.broadcast_to_session(session_id, {
                        "type": "progress",
                        "progress": session.progress,
                        "status": progress_info.get("status"),
                        "current_frequency": progress_info.get("current_frequency"),
                        "estimated_time": progress_info.get("estimated_time"),
                        "timestamp": time.time()
                    })
                    
        except Exception as e:
            logger.error("progress_update_error", session=session_id, error=str(e))
    
    async def _send_screenshot_update(self, session_id: str):
        """
        Send screenshot update for session.
        
        Args:
            session_id: Session identifier
        """
        if not self.screen_capture:
            return
        
        try:
            capture = self.screen_capture.capture_window()
            if capture.success and capture.image_base64:
                # Only send if clients are connected
                session = self.sessions[session_id]
                if session.clients:
                    # Compress if too large
                    data = capture.image_base64
                    if len(data) > self.max_screenshot_size:
                        # Would implement compression
                        return
                    
                    await self.broadcast_to_session(session_id, {
                        "type": "screenshot",
                        "data": data,
                        "timestamp": capture.timestamp
                    })
                    
        except Exception as e:
            logger.error("screenshot_update_error", session=session_id, error=str(e))
    
    async def _check_for_issues(self, session_id: str):
        """
        Check for warnings or errors.
        
        Args:
            session_id: Session identifier
        """
        if not self.screen_capture or not self.vision_analyzer:
            return
        
        try:
            capture = self.screen_capture.capture_window()
            if capture.success:
                warnings = self.vision_analyzer.detect_warnings(capture.image)
                
                if warnings.get("has_warning") or warnings.get("has_error"):
                    event_type = "error" if warnings.get("has_error") else "warning"
                    
                    await self.broadcast_to_session(session_id, {
                        "type": event_type,
                        "message": warnings.get("message", f"{event_type.title()} detected"),
                        "details": warnings,
                        "timestamp": time.time()
                    })
                    
        except Exception as e:
            logger.error("issue_check_error", session=session_id, error=str(e))
    
    def update_session_status(self, session_id: str, status: str,
                            stage: Optional[str] = None):
        """
        Update session status.
        
        Args:
            session_id: Session identifier
            status: New status
            stage: Current stage (optional)
        """
        if session_id in self.sessions:
            session = self.sessions[session_id]
            session.status = status
            if stage:
                session.current_stage = stage
            
            # Send status update
            asyncio.create_task(self.broadcast_to_session(session_id, {
                "type": "status_change",
                "status": status,
                "stage": stage,
                "timestamp": time.time()
            }))
    
    def stop_monitoring(self, session_id: str):
        """
        Stop monitoring a session.
        
        Args:
            session_id: Session to stop monitoring
        """
        if session_id in self.sessions:
            self.sessions[session_id].monitoring_enabled = False
        
        if session_id in self.active_monitors:
            self.active_monitors[session_id].cancel()
            self.active_monitors.pop(session_id)
        
        logger.info("monitoring_stopped", session=session_id)
    
    async def start_server(self):
        """Start WebSocket server"""
        if not uvicorn or not self.app:
            logger.error("Cannot start server - dependencies not available")
            return
        
        if not self.app:
            self.create_app()
        
        config = uvicorn.Config(
            self.app,
            host="0.0.0.0",
            port=self.port,
            log_level="info"
        )
        
        self.server = uvicorn.Server(config)
        
        logger.info("websocket_server_starting", port=self.port)
        await self.server.serve()
    
    def cleanup(self):
        """Cleanup resources"""
        # Stop all monitoring
        for session_id in list(self.active_monitors.keys()):
            self.stop_monitoring(session_id)
        
        # Close all connections
        for client_id in list(self.connections.keys()):
            asyncio.create_task(self.connections[client_id].close())
        
        self.connections.clear()
        self.sessions.clear()
        
        logger.info("websocket_monitor_cleanup")


# Standalone test
async def test_websocket_monitor():
    """Test WebSocket monitoring"""
    monitor = WebSocketMonitor(port=8765)
    
    # Create test app
    app = monitor.create_app()
    if app:
        print(f"✓ WebSocket app created")
        print(f"  Endpoints: /ws/<client_id>, /sessions")
    
    # Start test session
    session = await monitor.start_analysis_monitoring(
        "test_session_001",
        "Test Vessel"
    )
    
    print(f"✓ Monitoring session started: {session.id}")
    
    # Simulate status updates
    monitor.update_session_status(session.id, "running", "mesh_generation")
    await asyncio.sleep(1)
    
    monitor.update_session_status(session.id, "completed", "export")
    
    # Cleanup
    monitor.stop_monitoring(session.id)
    monitor.cleanup()
    
    return True


if __name__ == "__main__":
    asyncio.run(test_websocket_monitor())