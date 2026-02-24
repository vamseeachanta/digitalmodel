#!/usr/bin/env python
"""
MarkItDown MCP Server Entry Point
Document to Markdown conversion service for LLM processing
"""

import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent.parent))

def run_server():
    """Run the MarkItDown MCP server with appropriate implementation"""
    
    # Try FastMCP implementation first
    try:
        from src.mcp.markitdown.core.fastmcp_server import main
        print("Starting MarkItDown MCP server with FastMCP...")
        main()
    except ImportError as e:
        print(f"FastMCP not available: {e}")
        print("Falling back to basic implementation...")
        
        # Fallback to basic implementation
        try:
            import asyncio
            import logging
            from src.mcp.markitdown.core.server import MarkItDownMCPServer
            
            logging.basicConfig(
                level=logging.INFO,
                format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
            )
            
            server = MarkItDownMCPServer()
            print(f"Starting basic MarkItDown server on port {server.config['server']['port']}...")
            print("Note: For full MCP protocol support, install FastMCP: uv add fastmcp")
            
            # Basic server loop (placeholder - would need full implementation)
            print("\nServer capabilities:", server.get_capabilities())
            print("\nBasic server is running. Press Ctrl+C to stop.")
            
            try:
                # Keep server running
                while True:
                    import time
                    time.sleep(1)
            except KeyboardInterrupt:
                print("\nServer stopped.")
                
        except ImportError as e2:
            print(f"Could not start server: {e2}")
            print("\nPlease install required dependencies:")
            print("  uv add markitdown")
            print("  uv add fastmcp  (for full MCP support)")
            sys.exit(1)


if __name__ == "__main__":
    run_server()