#!/usr/bin/env python3
"""
OrcaWave MCP CLI Client
Interactive command-line client for OrcaWave MCP Server
"""

import asyncio
import json
import sys
import argparse
from pathlib import Path
from typing import Optional, Dict, Any
import cmd
import time
from datetime import datetime

try:
    import websockets
    import aiohttp
    from rich.console import Console
    from rich.table import Table
    from rich.progress import Progress, SpinnerColumn, TextColumn
    from rich.live import Live
    from rich.layout import Layout
    from rich.panel import Panel
    from rich.text import Text
except ImportError:
    print("Installing required packages...")
    import subprocess
    subprocess.run([sys.executable, "-m", "pip", "install", "websockets", "aiohttp", "rich"])
    import websockets
    import aiohttp
    from rich.console import Console
    from rich.table import Table
    from rich.progress import Progress, SpinnerColumn, TextColumn
    from rich.live import Live
    from rich.layout import Layout
    from rich.panel import Panel
    from rich.text import Text


class OrcaWaveClient:
    """Client for OrcaWave MCP Server"""
    
    def __init__(self, host="localhost", mcp_port=3100, ws_port=8765):
        self.host = host
        self.mcp_port = mcp_port
        self.ws_port = ws_port
        self.mcp_url = f"http://{host}:{mcp_port}"
        self.ws_url = f"ws://{host}:{ws_port}"
        self.console = Console()
        self.websocket = None
        self.current_analysis_id = None
        self.monitoring = False
        
    async def connect_websocket(self, session_id: Optional[str] = None):
        """Connect to WebSocket server"""
        try:
            client_id = f"cli_{int(time.time())}"
            url = f"{self.ws_url}/ws/{client_id}"
            if session_id:
                url += f"/{session_id}"
            
            self.websocket = await websockets.connect(url)
            self.console.print(f"[green]✓ Connected to WebSocket server[/green]")
            return True
        except Exception as e:
            self.console.print(f"[red]✗ WebSocket connection failed: {e}[/red]")
            return False
    
    async def disconnect_websocket(self):
        """Disconnect from WebSocket server"""
        if self.websocket:
            await self.websocket.close()
            self.websocket = None
    
    asyncムasync def call_mcp(self, endpoint: str, method="GET", data=None) -> Optional[Dict[str, Any]]:
        """Call MCP server endpoint"""
        url = f"{self.mcp_url}/{endpoint}"
        
        try:
            async with aiohttp.ClientSession() as session:
                if method == "GET":
                    async with session.get(url) as response:
                        return await response.json()
                elif method == "POST":
                    async with session.post(url, json=data) as response:
                        return await response.json()
        except Exception as e:
            self.console.print(f"[red]Error calling MCP server: {e}[/red]")
            return None
    
    async def get_server_status(self):
        """Get server status"""
        with self.console.status("[bold green]Checking server status..."):
            # Try MCP endpoint
            status = await self.call_mcp("status")
            
            if status:
                self.display_status(status)
            else:
                # Try standalone endpoint
                try:
                    async with aiohttp.ClientSession() as session:
                        async with session.get(f"{self.mcp_url}/") as response:
                            if response.status == 200:
                                data = await response.json()
                                self.console.print("[green]Server is running in standalone mode[/green]")
                                self.console.print(f"Version: {data.get('version', 'Unknown')}")
                except:
                    self.console.print("[red]Server is not responding[/red]")
    
    def display_status(self, status: Dict[str, Any]):
        """Display server status"""
        table = Table(title="Server Status", show_header=True, header_style="bold magenta")
        table.add_column("Component", style="cyan", no_wrap=True)
        table.add_column("Status", justify="center")
        table.add_column("Details", style="dim")
        
        # Server info
        table.add_row("Server", "[green]Online[/green]", status.get("server", ""))
        
        # COM API
        com_status = "[green]Connected[/green]" if status.get("com_api") else "[yellow]Not Connected[/yellow]"
        table.add_row("COM API", com_status, "OrcaWave COM Interface")
        
        # Vision
        vision_status = "[green]Available[/green]" if status.get("vision") else "[yellow]Not Available[/yellow]"
        table.add_row("Vision", vision_status, "Screenshot & Analysis")
        
        # Monitoring
        monitor_status = "[green]Available[/green]" if status.get("monitoring") else "[yellow]Not Available[/yellow]"
        table.add_row("Monitoring", monitor_status, f"WebSocket Port {self.ws_port}")
        
        # Current analysis
        if status.get("current_analysis"):
            table.add_row("Current Analysis", "[blue]Running[/blue]", status["current_analysis"])
        
        # Success rates
        if status.get("success_rates"):
            rates = status["success_rates"]
            details = f"COM: {rates.get('com', 0):.0%} | Vision: {rates.get('vision', 0):.0%}"
            table.add_row("Success Rates", "[cyan]Statistics[/cyan]", details)
        
        self.console.print(table)
    
    async def start_analysis(self, vessel_name: str, **kwargs):
        """Start a new analysis"""
        self.console.print(f"\n[bold cyan]Starting analysis for {vessel_name}...[/bold cyan]")
        
        # Prepare analysis parameters
        params = {
            "vessel_name": vessel_name,
            "frequencies": kwargs.get("frequencies", [0.1, 0.5, 1.0, 1.5, 2.0]),
            "directions": kwargs.get("directions", [0, 45, 90, 135, 180, 225, 270, 315]),
            "water_depth": kwargs.get("water_depth", 200.0),
            "enable_monitoring": True,
            "control_mode": kwargs.get("control_mode", "adaptive")
        }
        
        # Display parameters
        self.console.print("\n[bold]Analysis Parameters:[/bold]")
        param_table = Table(show_header=False, box=None)
        param_table.add_column("Parameter", style="cyan")
        param_table.add_column("Value")
        
        param_table.add_row("Vessel", vessel_name)
        param_table.add_row("Frequencies", str(params["frequencies"]))
        param_table.add_row("Directions", str(params["directions"]))
        param_table.add_row("Water Depth", f"{params['water_depth']}m")
        param_table.add_row("Control Mode", params["control_mode"])
        
        self.console.print(param_table)
        
        # Start analysis via MCP
        result = await self.call_mcp("analysis/start", "POST", params)
        
        if result and result.get("analysis_id"):
            self.current_analysis_id = result["analysis_id"]
            self.console.print(f"\n[green]✓ Analysis started: {self.current_analysis_id}[/green]")
            
            # Start monitoring
            if await self.connect_websocket(self.current_analysis_id):
                await self.monitor_progress()
        else:
            self.console.print("[red]✗ Failed to start analysis[/red]")
    
    async def monitor_progress(self):
        """Monitor analysis progress"""
        if not self.websocket:
            self.console.print("[red]WebSocket not connected[/red]")
            return
        
        self.monitoring = True
        layout = self.create_monitoring_layout()
        
        with Live(layout, refresh_per_second=2) as live:
            try:
                while self.monitoring:
                    # Receive WebSocket message
                    try:
                        message = await asyncio.wait_for(
                            self.websocket.recv(), 
                            timeout=1.0
                        )
                        data = json.loads(message)
                        
                        # Update layout based on message type
                        if data["type"] == "progress":
                            self.update_progress(layout, data)
                        elif data["type"] == "status_change":
                            self.update_status(layout, data)
                        elif data["type"] == "warning":
                            self.add_log(layout, f"[yellow]⚠ {data['message']}[/yellow]")
                        elif data["type"] == "error":
                            self.add_log(layout, f"[red]✗ {data['message']}[/red]")
                            
                    except asyncio.TimeoutError:
                        continue
                    except websockets.exceptions.ConnectionClosed:
                        self.console.print("[yellow]Connection closed[/yellow]")
                        break
                        
            except KeyboardInterrupt:
                self.monitoring = False
                self.console.print("\n[yellow]Monitoring stopped[/yellow]")
    
    def create_monitoring_layout(self) -> Layout:
        """Create monitoring dashboard layout"""
        layout = Layout()
        
        layout.split(
            Layout(name="header", size=3),
            Layout(name="body"),
            Layout(name="footer", size=3)
        )
        
        layout["body"].split_row(
            Layout(name="main", ratio=2),
            Layout(name="sidebar", ratio=1)
        )
        
        layout["main"].split(
            Layout(name="progress", size=7),
            Layout(name="logs")
        )
        
        # Initialize sections
        layout["header"].update(Panel(
            Text(f"OrcaWave Analysis Monitor - {self.current_analysis_id}", 
                 style="bold white", justify="center"),
            style="blue"
        ))
        
        layout["progress"].update(Panel(
            Text("Initializing...", justify="center"),
            title="Progress"
        ))
        
        layout["logs"].update(Panel(
            Text("", overflow="fold"),
            title="Activity Log"
        ))
        
        layout["sidebar"].update(Panel(
            Text("Waiting for data...", justify="center"),
            title="Statistics"
        ))
        
        layout["footer"].update(Panel(
            Text("Press Ctrl+C to stop monitoring", style="dim", justify="center")
        ))
        
        return layout
    
    def update_progress(self, layout: Layout, data: Dict[str, Any]):
        """Update progress display"""
        progress = data.get("progress", 0)
        status = data.get("status", "Running")
        stage = data.get("stage", "Unknown")
        
        progress_bar = self.create_progress_bar(progress)
        
        content = Text()
        content.append(f"Status: {status}\n", style="cyan")
        content.append(f"Stage: {stage}\n", style="yellow")
        content.append(f"\nProgress: {progress:.1f}%\n", style="bold")
        content.append(progress_bar)
        
        if data.get("estimated_time"):
            content.append(f"\n\nEstimated time: {data['estimated_time']}s", style="dim")
        
        layout["progress"].update(Panel(content, title="Progress"))
    
    def update_status(self, layout: Layout, data: Dict[str, Any]):
        """Update status display"""
        status = data.get("status", "Unknown")
        stage = data.get("stage", "")
        
        self.add_log(layout, f"[cyan]Status: {status} - {stage}[/cyan]")
    
    def add_log(self, layout: Layout, message: str):
        """Add message to activity log"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        
        # Get current log content
        current = layout["logs"].renderable
        if isinstance(current, Panel):
            current_text = current.renderable
            if isinstance(current_text, Text):
                current_content = str(current_text)
            else:
                current_content = str(current_text)
        else:
            current_content = ""
        
        # Add new message
        new_content = f"{current_content}[dim]{timestamp}[/dim] {message}\n"
        
        # Keep only last 10 lines
        lines = new_content.split("\n")
        if len(lines) > 10:
            lines = lines[-10:]
            new_content = "\n".join(lines)
        
        layout["logs"].update(Panel(
            Text.from_markup(new_content, overflow="fold"),
            title="Activity Log"
        ))
    
    def create_progress_bar(self, percentage: float, width: int = 40) -> str:
        """Create ASCII progress bar"""
        filled = int(width * percentage / 100)
        empty = width - filled
        bar = "█" * filled + "░" * empty
        return f"[green]{bar}[/green]"
    
    async def list_analyses(self):
        """List all analyses"""
        sessions = await self.call_mcp("sessions")
        
        if sessions and sessions.get("sessions"):
            table = Table(title="Active Analyses", show_header=True)
            table.add_column("ID", style="cyan")
            table.add_column("Vessel", style="yellow")
            table.add_column("Status")
            table.add_column("Progress", justify="right")
            table.add_column("Clients", justify="center")
            
            for session in sessions["sessions"]:
                status_color = {
                    "running": "blue",
                    "completed": "green",
                    "failed": "red"
                }.get(session["status"], "white")
                
                table.add_row(
                    session["id"],
                    session["vessel"],
                    f"[{status_color}]{session['status']}[/{status_color}]",
                    f"{session['progress']:.1f}%",
                    str(session["clients"])
                )
            
            self.console.print(table)
        else:
            self.console.print("[yellow]No active analyses[/yellow]")


class OrcaWaveCLI(cmd.Cmd):
    """Interactive CLI for OrcaWave MCP"""
    
    intro = """
╔══════════════════════════════════════════════════════════════╗
║             OrcaWave MCP CLI Client v1.0.0                  ║
║                                                              ║
║  Type 'help' for available commands or 'exit' to quit       ║
╚══════════════════════════════════════════════════════════════╝
    """
    prompt = "orcawave> "
    
    def __init__(self, client: OrcaWaveClient):
        super().__init__()
        self.client = client
        self.loop = asyncio.new_event_loop()
        asyncio.set_event_loop(self.loop)
    
    def do_status(self, arg):
        """Check server status"""
        self.loop.run_until_complete(self.client.get_server_status())
    
    def do_analyze(self, arg):
        """Start analysis: analyze <vessel_name> [options]
        Options:
          --frequencies 0.1,0.5,1.0
          --directions 0,90,180,270
          --water-depth 200
          --control-mode adaptive|com_only|vision_only|hybrid
        """
        parts = arg.split()
        if not parts:
            print("Usage: analyze <vessel_name> [options]")
            return
        
        vessel_name = parts[0]
        kwargs = {}
        
        # Parse options
        i = 1
        while i < len(parts):
            if parts[i] == "--frequencies" and i + 1 < len(parts):
                kwargs["frequencies"] = [float(f) for f in parts[i + 1].split(",")]
                i += 2
            elif parts[i] == "--directions" and i + 1 < len(parts):
                kwargs["directions"] = [float(d) for d in parts[i + 1].split(",")]
                i += 2
            elif parts[i] == "--water-depth" and i + 1 < len(parts):
                kwargs["water_depth"] = float(parts[i + 1])
                i += 2
            elif parts[i] == "--control-mode" and i + 1 < len(parts):
                kwargs["control_mode"] = parts[i + 1]
                i += 2
            else:
                i += 1
        
        self.loop.run_until_complete(self.client.start_analysis(vessel_name, **kwargs))
    
    def do_list(self, arg):
        """List active analyses"""
        self.loop.run_until_complete(self.client.list_analyses())
    
    def do_monitor(self, arg):
        """Monitor specific analysis: monitor <analysis_id>"""
        if not arg:
            if self.client.current_analysis_id:
                arg = self.client.current_analysis_id
            else:
                print("Usage: monitor <analysis_id>")
                return
        
        self.client.current_analysis_id = arg
        self.loop.run_until_complete(self.client.connect_websocket(arg))
        self.loop.run_until_complete(self.client.monitor_progress())
    
    def do_exit(self, arg):
        """Exit the CLI"""
        print("Goodbye!")
        self.loop.run_until_complete(self.client.disconnect_websocket())
        return True
    
    def do_quit(self, arg):
        """Exit the CLI"""
        return self.do_exit(arg)
    
    def do_clear(self, arg):
        """Clear the screen"""
        import os
        os.system('cls' if os.name == 'nt' else 'clear')
    
    def do_help(self, arg):
        """Show help for commands"""
        if arg:
            super().do_help(arg)
        else:
            print("""
Available Commands:
═══════════════════════════════════════════════════════════════
  status              - Check server status
  analyze <vessel>    - Start new analysis
  list                - List active analyses
  monitor [id]        - Monitor analysis progress
  clear               - Clear screen
  help [command]      - Show help
  exit/quit           - Exit CLI

Examples:
  analyze "Sea Cypress" --frequencies 0.1,1.0 --directions 0,180
  monitor analysis_001
═══════════════════════════════════════════════════════════════
            """)


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="OrcaWave MCP CLI Client")
    parser.add_argument("--host", default="localhost", help="Server host")
    parser.add_argument("--mcp-port", type=int, default=3100, help="MCP port")
    parser.add_argument("--ws-port", type=int, default=8765, help="WebSocket port")
    parser.add_argument("--command", help="Execute single command")
    
    args = parser.parse_args()
    
    # Create client
    client = OrcaWaveClient(args.host, args.mcp_port, args.ws_port)
    
    if args.command:
        # Execute single command
        cli = OrcaWaveCLI(client)
        cli.onecmd(args.command)
    else:
        # Interactive mode
        cli = OrcaWaveCLI(client)
        try:
            cli.cmdloop()
        except KeyboardInterrupt:
            print("\nExiting...")


if __name__ == "__main__":
    main()