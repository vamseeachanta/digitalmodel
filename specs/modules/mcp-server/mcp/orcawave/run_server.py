#!/usr/bin/env python3
"""
Run OrcaWave MCP Server
Entry point for the integrated MCP server with hybrid control and monitoring
"""

import sys
import os
import asyncio
import yaml
from pathlib import Path

# Add src directory to Python path
repo_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(repo_root))

# Import integrated server
from src.mcp.orcawave.core.integrated_server import create_mcp_server, IntegratedOrcaWaveMCP


def load_config(config_path: str) -> dict:
    """Load configuration from YAML file"""
    try:
        with open(config_path, 'r') as f:
            return yaml.safe_load(f)
    except Exception as e:
        print(f"Warning: Could not load config from {config_path}: {e}")
        return {}


async def run_standalone():
    """Run standalone integrated server (no FastMCP)"""
    print("Starting OrcaWave Integrated Server (Standalone Mode)...")
    
    # Load configuration
    config_path = Path(__file__).parent / "config.yml"
    config = load_config(str(config_path))
    
    # Create integrated server
    server = IntegratedOrcaWaveMCP(config)
    
    try:
        # Start server
        await server.start()
        print(f"✓ Server started")
        print(f"  COM API: {'Connected' if server.is_connected else 'Not connected'}")
        print(f"  Vision: {'Available' if server.screen_capture else 'Not available'}")
        print(f"  WebSocket: Port {config.get('server', {}).get('port', 8765)}")
        print(f"\nPress Ctrl+C to stop...")
        
        # Keep running
        while True:
            await asyncio.sleep(1)
            
    except KeyboardInterrupt:
        print("\nShutting down...")
    finally:
        await server.stop()
        print("✓ Server stopped")


def run_mcp_server():
    """Run as FastMCP server"""
    print("Starting OrcaWave MCP Server (FastMCP Mode)...")
    
    # Load configuration
    config_path = Path(__file__).parent / "config.yml"
    if "--config" in sys.argv:
        idx = sys.argv.index("--config")
        if idx + 1 < len(sys.argv):
            config_path = sys.argv[idx + 1]
    
    config = load_config(str(config_path))
    
    # Create MCP server
    server = create_mcp_server(config)
    
    if server:
        print("✓ MCP server created")
        print("  Resources: orcawave://status, orcawave://progress/{id}")
        print("  Tools: connect_orcawave, run_integrated_analysis, capture_screenshot, analyze_current_view, get_server_status")
        print("\nStarting FastMCP server...")
        
        # Run FastMCP server
        import uvicorn
        uvicorn.run(
            server,
            host=config.get('server', {}).get('host', 'localhost'),
            port=config.get('server', {}).get('port', 3100)
        )
    else:
        print("Error: Could not create MCP server")
        print("Install FastMCP with: pip install fastmcp")
        sys.exit(1)


def main():
    """Main entry point"""
    if "--standalone" in sys.argv:
        # Run standalone server (no FastMCP required)
        asyncio.run(run_standalone())
    else:
        # Run as MCP server (requires FastMCP)
        try:
            run_mcp_server()
        except ImportError as e:
            print(f"\nError: {e}")
            print("\nFastMCP not installed. You can either:")
            print("1. Install FastMCP: pip install fastmcp")
            print("2. Run standalone: python run_server.py --standalone")
            sys.exit(1)


if __name__ == "__main__":
    main()