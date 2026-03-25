#!/usr/bin/env python3
"""
Quick Start Script for OrcaWave MCP Server
Cross-platform setup and launch script
"""

import sys
import os
import subprocess
import platform
from pathlib import Path
import argparse


class QuickStart:
    """Quick start helper for OrcaWave MCP Server"""
    
    def __init__(self):
        self.root_dir = Path(__file__).parent.parent.parent.parent
        self.venv_dir = self.root_dir / ".venv"
        self.is_windows = platform.system() == "Windows"
        self.python_cmd = sys.executable
        
    def print_header(self):
        """Print header"""
        print("=" * 60)
        print("OrcaWave MCP Server Quick Start")
        print("=" * 60)
        print()
    
    def check_python(self):
        """Check Python version"""
        version = sys.version_info
        print(f"[1/5] Python {version.major}.{version.minor}.{version.micro} found")
        
        if version.major < 3 or (version.major == 3 and version.minor < 9):
            print("  ⚠ Warning: Python 3.9+ recommended")
            return False
        return True
    
    def setup_venv(self):
        """Set up virtual environment"""
        if self.venv_dir.exists():
            print("[2/5] Virtual environment already exists")
            return True
        
        print("[2/5] Creating virtual environment...")
        try:
            subprocess.run([self.python_cmd, "-m", "venv", str(self.venv_dir)], check=True)
            return True
        except subprocess.CalledProcessError:
            print("  ✗ Failed to create virtual environment")
            return False
    
    def get_pip_cmd(self):
        """Get pip command for virtual environment"""
        if self.is_windows:
            pip_path = self.venv_dir / "Scripts" / "pip.exe"
        else:
            pip_path = self.venv_dir / "bin" / "pip"
        
        if pip_path.exists():
            return str(pip_path)
        return "pip"
    
    def install_dependencies(self):
        """Install dependencies"""
        print("[3/5] Installing dependencies...")
        print()
        
        pip_cmd = self.get_pip_cmd()
        packages = {
            "Core": ["pyyaml"],
            "MCP Protocol": ["fastmcp"],
            "Windows COM": ["pywin32"],
            "Vision": ["opencv-python", "pillow", "numpy"],
            "WebSocket": ["fastapi", "uvicorn", "websockets"],
            "Logging": ["structlog"]
        }
        
        installed = []
        failed = []
        
        for category, pkgs in packages.items():
            try:
                result = subprocess.run(
                    [pip_cmd, "install", "--quiet"] + pkgs,
                    capture_output=True,
                    text=True
                )
                if result.returncode == 0:
                    print(f"  ✓ {category} installed")
                    installed.extend(pkgs)
                else:
                    print(f"  ✗ {category} not available")
                    failed.extend(pkgs)
            except Exception as e:
                print(f"  ✗ {category} failed: {e}")
                failed.extend(pkgs)
        
        print()
        return len(installed) > 0
    
    def test_imports(self):
        """Test critical imports"""
        print("[4/5] Testing components...")
        
        components = {
            "COM API": "src.mcp.orcawave.api.orcawave_com",
            "Vision": "src.mcp.orcawave.vision.screen_capture",
            "Coordinator": "src.mcp.orcawave.core.hybrid_coordinator",
            "Progress": "src.mcp.orcawave.core.progress_tracker"
        }
        
        # Add src to path
        sys.path.insert(0, str(self.root_dir))
        
        available = []
        for name, module in components.items():
            try:
                __import__(module)
                print(f"  ✓ {name} ready")
                available.append(name)
            except ImportError:
                print(f"  ✗ {name} not available")
        
        print()
        return len(available) > 0
    
    def start_server(self, mode="standalone", config=None):
        """Start the server"""
        print("[5/5] Starting server...")
        print()
        
        # Prepare command
        if self.is_windows:
            python_path = self.venv_dir / "Scripts" / "python.exe"
        else:
            python_path = self.venv_dir / "bin" / "python"
        
        if not python_path.exists():
            python_path = self.python_cmd
        
        server_script = self.root_dir / "mcp" / "orcawave" / "run_server.py"
        
        cmd = [str(python_path), str(server_script)]
        
        if mode == "standalone":
            cmd.append("--standalone")
            print("Starting in standalone mode...")
            print()
            print("Server will be available at:")
            print("  - MCP: localhost:3100")
            print("  - WebSocket: ws://localhost:8765/")
        else:
            print("Starting in MCP mode (requires FastMCP)...")
        
        if config:
            cmd.extend(["--config", config])
        
        print()
        print("Press Ctrl+C to stop the server")
        print("=" * 60)
        print()
        
        try:
            subprocess.run(cmd)
        except KeyboardInterrupt:
            print("\nServer stopped")
        except Exception as e:
            print(f"Error starting server: {e}")
            return False
        
        return True
    
    def run(self, args):
        """Run quick start"""
        self.print_header()
        
        # Check Python
        if not self.check_python():
            return 1
        
        # Set up environment
        if not args.skip_setup:
            if not self.setup_venv():
                return 1
            
            if not self.install_dependencies():
                print("Warning: Some dependencies failed to install")
                print("The server may have limited functionality")
                print()
        
        # Test imports
        if not args.skip_test:
            if not self.test_imports():
                print("Warning: Some components are not available")
                print()
        
        # Start server
        if not args.no_start:
            mode = "mcp" if args.mcp else "standalone"
            if not self.start_server(mode, args.config):
                return 1
        else:
            print("Setup complete. Run the following to start:")
            print(f"  python mcp/orcawave/run_server.py {'--standalone' if not args.mcp else ''}")
        
        return 0


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="OrcaWave MCP Server Quick Start")
    parser.add_argument("--mcp", action="store_true", help="Start in MCP mode (default: standalone)")
    parser.add_argument("--config", help="Configuration file path")
    parser.add_argument("--skip-setup", action="store_true", help="Skip environment setup")
    parser.add_argument("--skip-test", action="store_true", help="Skip component testing")
    parser.add_argument("--no-start", action="store_true", help="Setup only, don't start server")
    
    args = parser.parse_args()
    
    quick_start = QuickStart()
    sys.exit(quick_start.run(args))


if __name__ == "__main__":
    main()