"""
OrcaFlex CLI Dashboard
Interactive terminal dashboard for browsing and analyzing OrcaFlex data
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__)))

from backend.file_processor import FileProcessor, MetadataExtractor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from pathlib import Path
import pandas as pd
from datetime import datetime
from typing import List, Dict, Any, Optional

# Rich imports for beautiful CLI
from rich.console import Console
from rich.layout import Layout
from rich.panel import Panel
from rich.table import Table
from rich.live import Live
from rich.prompt import Prompt, Confirm, IntPrompt
from rich.text import Text
from rich.align import Align
from rich.columns import Columns
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.style import Style
from rich.rule import Rule
from rich import box
from rich.syntax import Syntax
import json
import time


class OrcaFlexDashboard:
    """Interactive CLI Dashboard for OrcaFlex Data Analysis"""
    
    def __init__(self, base_path: str = None):
        self.console = Console()
        self.base_path = None
        self.file_processor = None
        self.analyzer = OrcaFlexAnalyzer()
        self.metadata_extractor = MetadataExtractor()
        
        # Data storage
        self.files = []
        self.dataframes = {}
        self.analysis_results = None
        self.current_filter = {}
        self.current_view = "overview"
        
        # UI state
        self.selected_file_index = 0
        self.selected_strut = None
        
        if base_path:
            self.set_base_path(base_path)
    
    def set_base_path(self, base_path: str):
        """Set the base path for data files"""
        self.base_path = Path(base_path)
        if self.base_path.exists():
            self.file_processor = FileProcessor(str(self.base_path))
            return True
        return False
    
    def create_header(self) -> Panel:
        """Create dashboard header"""
        header_text = Text()
        header_text.append("OrcaFlex Browser Dashboard", style="bold cyan")
        header_text.append(" | ", style="white")
        header_text.append(f"Path: {self.base_path or 'Not Set'}", style="yellow")
        header_text.append(" | ", style="white")
        header_text.append(datetime.now().strftime("%Y-%m-%d %H:%M:%S"), style="green")
        
        return Panel(
            Align.center(header_text),
            box=box.DOUBLE,
            style="bright_blue"
        )
    
    def create_summary_panel(self) -> Panel:
        """Create summary statistics panel"""
        if not self.analysis_results:
            return Panel("No data loaded", title="Summary", box=box.ROUNDED)
        
        summary = self.analysis_results.get('summary', {})
        
        # Create summary table
        table = Table(show_header=False, box=None)
        table.add_column("Metric", style="cyan")
        table.add_column("Value", style="yellow")
        
        table.add_row("Files Loaded", str(len(self.files)))
        table.add_row("Struts Analyzed", str(summary.get('total_struts_found', 0)))
        
        if summary.get('absolute_max_tension') is not None:
            table.add_row("Max Tension", f"{summary['absolute_max_tension']:.2f}")
            table.add_row("Min Tension", f"{summary['absolute_min_tension']:.2f}")
        
        if self.analysis_results.get('absolute_maximum'):
            abs_max = self.analysis_results['absolute_maximum']
            table.add_row("Critical Strut", abs_max['strut'].upper())
            table.add_row("Critical Value", f"{abs_max['value']:.2f}")
        
        return Panel(table, title="📊 Summary Statistics", box=box.ROUNDED)
    
    def create_critical_panel(self) -> Panel:
        """Create critical case panel"""
        if not self.analysis_results or not self.analysis_results.get('absolute_maximum'):
            return Panel("No critical case identified", title="Critical Case", box=box.ROUNDED)
        
        abs_max = self.analysis_results['absolute_maximum']
        
        # Create critical info display
        content = Text()
        content.append("ABSOLUTE MAXIMUM TENSION\n", style="bold red")
        content.append("-" * 30 + "\n", style="dim")
        content.append(f"Value: ", style="white")
        content.append(f"{abs_max['value']:.2f}\n", style="bold yellow")
        content.append(f"Strut: ", style="white")
        content.append(f"{abs_max['strut'].upper()}\n", style="bold cyan")
        
        if 'fe_filename' in abs_max:
            fe_file = Path(abs_max['fe_filename']).name
            content.append(f"FE File: ", style="white")
            content.append(f"{fe_file}\n", style="green")
            
            # Extract metadata
            metadata = self.metadata_extractor.extract_from_fe_filename(abs_max['fe_filename'])
            if metadata:
                content.append("\nConditions:\n", style="bold white")
                if metadata.get('lng_loading'):
                    content.append(f"  • LNG: {metadata['lng_loading']}\n", style="cyan")
                if metadata.get('tide_level'):
                    content.append(f"  • Tide: {metadata['tide_level']}\n", style="blue")
                if metadata.get('environment_type'):
                    content.append(f"  • Env: {metadata['environment_type']}\n", style="green")
                if metadata.get('direction'):
                    content.append(f"  • Dir: {metadata['direction']}\n", style="yellow")
        
        return Panel(content, title="🚨 Critical Case", box=box.ROUNDED, border_style="red")
    
    def create_strut_table(self) -> Panel:
        """Create strut analysis table"""
        if not self.analysis_results or not self.analysis_results.get('strut_analysis'):
            return Panel("No strut data available", title="Strut Analysis", box=box.ROUNDED)
        
        # Create table
        table = Table(box=box.SIMPLE_HEAVY)
        table.add_column("Strut", style="cyan", width=10)
        table.add_column("Min", style="blue", justify="right")
        table.add_column("Max", style="red", justify="right")
        table.add_column("Range", style="yellow", justify="right")
        table.add_column("Status", style="green", width=15)
        
        # Sort struts by max tension
        strut_items = []
        for strut_name, data in self.analysis_results['strut_analysis'].items():
            strut_items.append({
                'name': strut_name,
                'min': data.get('min_tension', 0),
                'max': data.get('max_tension', 0),
                'range': data.get('max_tension', 0) - data.get('min_tension', 0)
            })
        
        strut_items.sort(key=lambda x: x['max'], reverse=True)
        
        # Add rows
        for item in strut_items:
            # Determine status
            if item['max'] > 5000:
                status = "⚠️  Critical"
                row_style = "bold red"
            elif item['max'] > 3000:
                status = "⚡ High"
                row_style = "yellow"
            else:
                status = "✓ Normal"
                row_style = "green"
            
            table.add_row(
                item['name'].upper(),
                f"{item['min']:.2f}",
                f"{item['max']:.2f}",
                f"{item['range']:.2f}",
                status,
                style=row_style if item['max'] > 5000 else None
            )
        
        return Panel(table, title="📈 Strut Analysis", box=box.ROUNDED)
    
    def create_file_list_panel(self) -> Panel:
        """Create file list panel"""
        if not self.files:
            return Panel("No files loaded", title="Files", box=box.ROUNDED)
        
        # Create file list
        content = Text()
        for i, file in enumerate(self.files[:10]):  # Show first 10
            if i == self.selected_file_index:
                content.append(f"▶ ", style="bold cyan")
            else:
                content.append("  ")
            
            filename = file.name
            # Shorten if too long
            if len(filename) > 40:
                filename = filename[:37] + "..."
            
            style = "bold white" if i == self.selected_file_index else "dim white"
            content.append(f"{i+1}. {filename}\n", style=style)
        
        if len(self.files) > 10:
            content.append(f"\n  ... and {len(self.files)-10} more files", style="dim")
        
        return Panel(content, title="📁 Files", box=box.ROUNDED)
    
    def create_metadata_panel(self) -> Panel:
        """Create metadata display panel"""
        content = Text()
        
        if self.selected_file_index < len(self.files):
            file = self.files[self.selected_file_index]
            metadata = self.metadata_extractor.extract_metadata(file.name)
            
            content.append("File Metadata\n", style="bold white")
            content.append("-" * 20 + "\n", style="dim")
            
            if metadata.get('lng_loading'):
                content.append(f"LNG: {metadata['lng_loading']}\n", style="cyan")
            if metadata.get('tide_level'):
                content.append(f"Tide: {metadata['tide_level']}\n", style="blue")
            if metadata.get('loading_condition'):
                content.append(f"Condition: {metadata['loading_condition']}\n", style="yellow")
            
            # File stats
            if file.exists():
                size_kb = file.stat().st_size / 1024
                content.append(f"\nSize: {size_kb:.1f} KB\n", style="green")
                
                # Modified time
                mtime = datetime.fromtimestamp(file.stat().st_mtime)
                content.append(f"Modified: {mtime.strftime('%Y-%m-%d %H:%M')}\n", style="dim")
        else:
            content.append("No file selected", style="dim")
        
        return Panel(content, title="ℹ️ Metadata", box=box.ROUNDED)
    
    def create_main_layout(self) -> Layout:
        """Create the main dashboard layout"""
        layout = Layout()
        
        # Create layout structure
        layout.split_column(
            Layout(name="header", size=3),
            Layout(name="body"),
            Layout(name="footer", size=3)
        )
        
        # Split body into panels
        layout["body"].split_row(
            Layout(name="left", ratio=1),
            Layout(name="center", ratio=2),
            Layout(name="right", ratio=1)
        )
        
        # Left column
        layout["left"].split_column(
            Layout(name="summary"),
            Layout(name="critical")
        )
        
        # Center column
        layout["center"].split_column(
            Layout(name="struts", ratio=2),
            Layout(name="files", ratio=1)
        )
        
        # Right column
        layout["right"].split_column(
            Layout(name="metadata"),
            Layout(name="controls")
        )
        
        return layout
    
    def create_controls_panel(self) -> Panel:
        """Create controls help panel"""
        controls = Text()
        controls.append("Controls\n", style="bold white")
        controls.append("-" * 20 + "\n", style="dim")
        controls.append("[L] Load Data\n", style="cyan")
        controls.append("[R] Refresh\n", style="green")
        controls.append("[F] Filter\n", style="yellow")
        controls.append("[S] Sort\n", style="blue")
        controls.append("[E] Export\n", style="magenta")
        controls.append("[↑↓] Navigate\n", style="white")
        controls.append("[Q] Quit\n", style="red")
        
        return Panel(controls, title="⌨️ Controls", box=box.ROUNDED)
    
    def create_footer(self) -> Panel:
        """Create footer with status"""
        status = Text()
        if self.analysis_results:
            status.append("✓ Data Loaded", style="green")
            status.append(" | ", style="dim")
            status.append(f"View: {self.current_view.title()}", style="cyan")
            if self.current_filter:
                status.append(" | ", style="dim")
                status.append(f"Filtered", style="yellow")
        else:
            status.append("⚡ Ready", style="yellow")
            status.append(" | ", style="dim")
            status.append("Press 'L' to load data", style="dim")
        
        return Panel(Align.center(status), box=box.DOUBLE, style="bright_blue")
    
    def update_display(self, layout: Layout):
        """Update all panels in the layout"""
        layout["header"].update(self.create_header())
        layout["summary"].update(self.create_summary_panel())
        layout["critical"].update(self.create_critical_panel())
        layout["struts"].update(self.create_strut_table())
        layout["files"].update(self.create_file_list_panel())
        layout["metadata"].update(self.create_metadata_panel())
        layout["controls"].update(self.create_controls_panel())
        layout["footer"].update(self.create_footer())
    
    def load_data(self):
        """Load and analyze data"""
        self.console.print("\n[cyan]Loading OrcaFlex data...[/cyan]")
        
        # Search for files
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=self.console
        ) as progress:
            task = progress.add_task("Searching for files...", total=None)
            
            self.files = self.file_processor.search_files("dm_*_strut_dyn.csv")
            if not self.files:
                # Try alternative pattern
                self.files = self.file_processor.search_files("*.csv")
            
            progress.update(task, description=f"Found {len(self.files)} files")
            
            if self.files:
                progress.update(task, description="Loading CSV files...")
                self.dataframes = self.file_processor.read_multiple_csvs_parallel(
                    self.files[:20], max_workers=4  # Limit for performance
                )
                
                progress.update(task, description="Analyzing data...")
                self.analysis_results = self.analyzer.analyze_multiple_dataframes(self.dataframes)
                
                progress.update(task, description="Complete!")
        
        self.console.print(f"[green]✓ Loaded {len(self.files)} files[/green]")
    
    def export_results(self):
        """Export current results to file"""
        if not self.analysis_results:
            self.console.print("[red]No data to export![/red]")
            return
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"orcaflex_analysis_{timestamp}.json"
        
        export_data = {
            'timestamp': datetime.now().isoformat(),
            'base_path': str(self.base_path),
            'files_analyzed': len(self.files),
            'analysis_results': self.analysis_results,
            'critical_case': self.analysis_results.get('absolute_maximum')
        }
        
        with open(filename, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        self.console.print(f"[green]✓ Exported to {filename}[/green]")
    
    def run_interactive(self):
        """Run the interactive dashboard"""
        # Initial setup if no base path
        if not self.base_path:
            self.console.print("[cyan]Welcome to OrcaFlex CLI Dashboard![/cyan]\n")
            
            path = Prompt.ask(
                "Enter data directory path",
                default="D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr"
            )
            
            if not self.set_base_path(path):
                # Try alternative paths
                alternatives = [
                    "D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr",
                    "sample_data"
                ]
                for alt_path in alternatives:
                    if self.set_base_path(alt_path):
                        self.console.print(f"[green]Using: {alt_path}[/green]")
                        break
                else:
                    self.console.print("[red]Invalid path![/red]")
                    return
        
        # Load initial data
        self.load_data()
        
        # Create layout
        layout = self.create_main_layout()
        
        # Run live display
        with Live(layout, refresh_per_second=1, console=self.console) as live:
            while True:
                try:
                    # Update display
                    self.update_display(layout)
                    
                    # Wait for input (non-blocking in real implementation)
                    time.sleep(0.5)
                    
                    # Handle user input (simplified for demo)
                    # In real implementation, use keyboard library or similar
                    
                    # For demo, just update a few times then exit
                    # Remove this in production
                    if datetime.now().second % 10 == 0:
                        break
                        
                except KeyboardInterrupt:
                    break
        
        self.console.print("\n[cyan]Dashboard closed. Goodbye![/cyan]")
    
    def run_static(self):
        """Run a static (non-interactive) version of the dashboard"""
        self.console.clear()
        
        # Print header
        self.console.print(self.create_header())
        
        # Load data if not loaded
        if not self.analysis_results:
            self.load_data()
        
        # Display main sections
        self.console.print("\n")
        
        # Create two-column layout for summary and critical
        columns = Columns([
            self.create_summary_panel(),
            self.create_critical_panel()
        ])
        self.console.print(columns)
        
        # Display strut table
        self.console.print("\n")
        self.console.print(self.create_strut_table())
        
        # Display file list and metadata
        self.console.print("\n")
        columns = Columns([
            self.create_file_list_panel(),
            self.create_metadata_panel()
        ])
        self.console.print(columns)
        
        # Footer
        self.console.print("\n")
        self.console.print(self.create_footer())
        
        # Options
        self.console.print("\n[cyan]Options:[/cyan]")
        self.console.print("  [E] Export results")
        self.console.print("  [R] Refresh data")
        self.console.print("  [Q] Quit")
        
        choice = Prompt.ask("\nSelect option", choices=["e", "r", "q"], default="q").lower()
        
        if choice == "e":
            self.export_results()
        elif choice == "r":
            self.load_data()
            self.run_static()  # Recursive refresh
        # 'q' or default exits


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="OrcaFlex CLI Dashboard")
    parser.add_argument('--path', help='Data directory path')
    parser.add_argument('--mode', choices=['interactive', 'static'], 
                       default='static', help='Display mode')
    
    args = parser.parse_args()
    
    # Create dashboard
    dashboard = OrcaFlexDashboard(args.path)
    
    try:
        if args.mode == 'interactive':
            dashboard.run_interactive()
        else:
            dashboard.run_static()
    except KeyboardInterrupt:
        dashboard.console.print("\n[yellow]Interrupted by user[/yellow]")
    except Exception as e:
        dashboard.console.print(f"\n[red]Error: {e}[/red]")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()