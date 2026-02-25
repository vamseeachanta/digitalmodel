#!/usr/bin/env python3
"""
Simple HTTP server for OrcaWave Dashboard
Serves the dashboard and provides API endpoints
"""

import os
import json
import asyncio
from pathlib import Path
from typing import Optional
import argparse

try:
    from aiohttp import web
    import aiohttp_cors
except ImportError:
    print("Installing required packages...")
    import subprocess
    import sys
    subprocess.run([sys.executable, "-m", "pip", "install", "aiohttp", "aiohttp-cors"])
    from aiohttp import web
    import aiohttp_cors


class DashboardServer:
    """Dashboard server with API endpoints"""
    
    def __init__(self, port=8080, mcp_host="localhost", mcp_port=3100):
        self.port = port
        self.mcp_host = mcp_host
        self.mcp_port = mcp_port
        self.app = web.Application()
        self.setup_routes()
        self.setup_cors()
        
        # Path to dashboard HTML
        self.dashboard_path = Path(__file__).parent / "dashboard.html"
        
        # Mock data for testing
        self.mock_analyses = []
        self.analysis_counter = 0
    
    def setup_routes(self):
        """Set up API routes"""
        self.app.router.add_get('/', self.serve_dashboard)
        self.app.router.add_get('/api/status', self.get_status)
        self.app.router.add_get('/api/sessions', self.get_sessions)
        self.app.router.add_post('/api/analysis/start', self.start_analysis)
        self.app.router.add_get('/api/analysis/{id}', self.get_analysis)
        self.app.router.add_static('/static', Path(__file__).parent, name='static')
    
    def setup_cors(self):
        """Set up CORS for API access"""
        cors = aiohttp_cors.setup(self.app, defaults={
            "*": aiohttp_cors.ResourceOptions(
                allow_credentials=True,
                expose_headers="*",
                allow_headers="*",
                allow_methods="*"
            )
        })
        
        for route in list(self.app.router.routes()):
            cors.add(route)
    
    async def serve_dashboard(self, request):
        """Serve the dashboard HTML"""
        if self.dashboard_path.exists():
            with open(self.dashboard_path, 'r') as f:
                content = f.read()
            return web.Response(text=content, content_type='text/html')
        else:
            return web.Response(text="Dashboard not found", status=404)
    
    async def get_status(self, request):
        """Get server status"""
        # Try to connect to actual MCP server
        try:
            async with web.ClientSession() as session:
                url = f"http://{self.mcp_host}:{self.mcp_port}/status"
                async with session.get(url, timeout=2) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        return web.json_response(data)
        except:
            pass
        
        # Return mock status if MCP server not available
        return web.json_response({
            "server": "dashboard_mock",
            "com_api": False,
            "vision": True,
            "monitoring": True,
            "current_analysis": self.mock_analyses[-1]["id"] if self.mock_analyses else None,
            "success_rates": {
                "com": 0.95,
                "vision": 0.85,
                "hybrid": 0.92
            }
        })
    
    async def get_sessions(self, request):
        """Get analysis sessions"""
        # Try actual MCP server
        try:
            async with web.ClientSession() as session:
                url = f"http://{self.mcp_host}:{self.mcp_port}/sessions"
                async with session.get(url, timeout=2) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        return web.json_response(data)
        except:
            pass
        
        # Return mock sessions
        sessions = []
        for analysis in self.mock_analyses:
            sessions.append({
                "id": analysis["id"],
                "vessel": analysis["vessel_name"],
                "status": analysis["status"],
                "progress": analysis["progress"],
                "clients": 1
            })
        
        return web.json_response({"sessions": sessions})
    
    async def start_analysis(self, request):
        """Start new analysis"""
        try:
            data = await request.json()
        except:
            return web.json_response({"error": "Invalid JSON"}, status=400)
        
        # Try actual MCP server
        try:
            async with web.ClientSession() as session:
                url = f"http://{self.mcp_host}:{self.mcp_port}/analysis/start"
                async with session.post(url, json=data, timeout=5) as resp:
                    if resp.status == 200:
                        result = await resp.json()
                        return web.json_response(result)
        except:
            pass
        
        # Create mock analysis
        self.analysis_counter += 1
        analysis_id = f"analysis_{self.analysis_counter:03d}"
        
        analysis = {
            "id": analysis_id,
            "vessel_name": data.get("vessel_name", "Unknown"),
            "frequencies": data.get("frequencies", [0.1, 1.0]),
            "directions": data.get("directions", [0, 180]),
            "water_depth": data.get("water_depth", 200),
            "control_mode": data.get("control_mode", "adaptive"),
            "status": "running",
            "progress": 0,
            "start_time": asyncio.get_event_loop().time()
        }
        
        self.mock_analyses.append(analysis)
        
        # Start mock progress simulation
        asyncio.create_task(self.simulate_progress(analysis))
        
        return web.json_response({
            "analysis_id": analysis_id,
            "status": "started"
        })
    
    async def get_analysis(self, request):
        """Get analysis details"""
        analysis_id = request.match_info['id']
        
        # Try actual MCP server
        try:
            async with web.ClientSession() as session:
                url = f"http://{self.mcp_host}:{self.mcp_port}/analysis/{analysis_id}"
                async with session.get(url, timeout=2) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        return web.json_response(data)
        except:
            pass
        
        # Find mock analysis
        for analysis in self.mock_analyses:
            if analysis["id"] == analysis_id:
                return web.json_response(analysis)
        
        return web.json_response({"error": "Analysis not found"}, status=404)
    
    async def simulate_progress(self, analysis):
        """Simulate analysis progress for testing"""
        stages = [
            ("Initialization", 10),
            ("Geometry Import", 20),
            ("Mesh Generation", 35),
            ("Mesh Optimization", 45),
            ("Analysis Setup", 50),
            ("Diffraction Analysis", 90),
            ("Results Processing", 95),
            ("Export", 100)
        ]
        
        for stage, target_progress in stages:
            await asyncio.sleep(2)  # Simulate processing time
            
            analysis["stage"] = stage
            analysis["progress"] = target_progress
            
            if target_progress >= 100:
                analysis["status"] = "completed"
                break
    
    async def start_server(self):
        """Start the dashboard server"""
        runner = web.AppRunner(self.app)
        await runner.setup()
        site = web.TCPSite(runner, '0.0.0.0', self.port)
        
        print(f"""
╔══════════════════════════════════════════════════════════════╗
║           OrcaWave MCP Dashboard Server                     ║
╚══════════════════════════════════════════════════════════════╝

Dashboard URL: http://localhost:{self.port}
API Endpoints:
  - GET  /api/status         - Server status
  - GET  /api/sessions       - Active analyses
  - POST /api/analysis/start - Start new analysis
  - GET  /api/analysis/<id>  - Get analysis details

MCP Server: {self.mcp_host}:{self.mcp_port}
WebSocket: ws://localhost:8765

Press Ctrl+C to stop
        """)
        
        await site.start()


async def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="OrcaWave Dashboard Server")
    parser.add_argument("--port", type=int, default=8080, help="Dashboard port")
    parser.add_argument("--mcp-host", default="localhost", help="MCP server host")
    parser.add_argument("--mcp-port", type=int, default=3100, help="MCP server port")
    
    args = parser.parse_args()
    
    server = DashboardServer(args.port, args.mcp_host, args.mcp_port)
    
    try:
        await server.start_server()
        # Keep server running
        await asyncio.Event().wait()
    except KeyboardInterrupt:
        print("\nShutting down dashboard server...")


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("\nDashboard server stopped")