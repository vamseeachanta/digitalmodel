"""
CLI Test Interface for OrcaFlex Browser Backend
Interactive testing with user feedback capture
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from backend.file_processor import FileProcessor, TensionAnalyzer, MetadataExtractor, VerificationLogger
from pathlib import Path
import json
from datetime import datetime
import argparse
from typing import Dict, Any, List
import pandas as pd
from rich.console import Console
from rich.table import Table
from rich.prompt import Prompt, Confirm
from rich.json import JSON
from rich.panel import Panel
from rich import print as rprint


class CLITester:
    """Interactive CLI for testing backend functionality"""
    
    def __init__(self):
        self.console = Console()
        self.file_processor = None
        self.tension_analyzer = TensionAnalyzer()
        self.metadata_extractor = MetadataExtractor()
        self.verification_logger = None
        self.current_results = {}
        
    def setup(self, base_path: str, verification_dir: str = "verification"):
        """Setup processors with base path"""
        self.file_processor = FileProcessor(base_path)
        self.verification_logger = VerificationLogger(verification_dir)
        self.console.print(f"[green]✓ Setup complete with base path: {base_path}[/green]")
        
    def test_file_search(self) -> List[Path]:
        """Test file search functionality"""
        self.console.print("\n[bold cyan]Testing File Search[/bold cyan]")
        pattern = Prompt.ask("Enter file pattern", default="dm_*_strut_dyn.csv")
        
        files = self.file_processor.search_files(pattern)
        
        # Display results in table
        table = Table(title=f"Found {len(files)} Files")
        table.add_column("Index", style="cyan", no_wrap=True)
        table.add_column("Filename", style="magenta")
        table.add_column("Size", style="green")
        
        for idx, file in enumerate(files):
            size = file.stat().st_size if file.exists() else 0
            table.add_row(str(idx), file.name, f"{size:,} bytes")
            
        self.console.print(table)
        return files
        
    def test_parallel_reading(self, files: List[Path]) -> Dict[str, pd.DataFrame]:
        """Test parallel CSV reading"""
        self.console.print("\n[bold cyan]Testing Parallel CSV Reading[/bold cyan]")
        
        max_workers = int(Prompt.ask("Number of parallel workers", default="4"))
        
        with self.console.status("[bold green]Reading CSV files in parallel..."):
            dataframes = self.file_processor.read_multiple_csvs_parallel(files, max_workers)
            
        self.console.print(f"[green]✓ Loaded {len(dataframes)} dataframes[/green]")
        
        # Display summary
        table = Table(title="Loaded DataFrames")
        table.add_column("Strut", style="cyan")
        table.add_column("Rows", style="magenta")
        table.add_column("Columns", style="green")
        
        for name, df in dataframes.items():
            table.add_row(name, str(len(df)), str(len(df.columns)))
            
        self.console.print(table)
        return dataframes
        
    def test_tension_analysis(self, dataframes: Dict[str, pd.DataFrame]) -> Dict[str, Any]:
        """Test tension analysis"""
        self.console.print("\n[bold cyan]Testing Tension Analysis[/bold cyan]")
        
        with self.console.status("[bold green]Analyzing tensions..."):
            results = self.tension_analyzer.analyze_tensions(dataframes)
            
        # Display results
        if results['absolute_maximum']:
            abs_max = results['absolute_maximum']
            self.console.print(Panel(
                f"[bold red]Absolute Maximum Tension[/bold red]\n"
                f"Value: {abs_max['value']:.2f}\n"
                f"Strut: {abs_max['strut']}\n"
                f"FE Filename: {abs_max['fe_filename']}\n"
                f"FE Filename Stem: {abs_max['fe_filename_stem']}",
                title="Critical Finding"
            ))
            
        # Display per-strut analysis
        table = Table(title="Strut-by-Strut Analysis")
        table.add_column("Strut", style="cyan")
        table.add_column("Min Tension", style="blue")
        table.add_column("Max Tension", style="red")
        table.add_column("Source File", style="magenta")
        
        for strut, data in results['strut_analysis'].items():
            table.add_row(
                strut,
                f"{data['min_tension']:.2f}",
                f"{data['max_tension']:.2f}",
                data['source_file']
            )
            
        self.console.print(table)
        return results
        
    def test_metadata_extraction(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Test metadata extraction from filenames"""
        self.console.print("\n[bold cyan]Testing Metadata Extraction[/bold cyan]")
        
        metadata_results = {}
        
        if results.get('absolute_maximum'):
            fe_filename = results['absolute_maximum'].get('fe_filename', '')
            if fe_filename:
                metadata = self.metadata_extractor.extract_from_fe_filename(fe_filename)
                
                # Display metadata
                self.console.print(Panel(
                    f"[bold yellow]Extracted Metadata[/bold yellow]\n"
                    f"Loading Condition: {metadata['loading_condition']}\n"
                    f"LNG Loading: {metadata['lng_loading']}\n"
                    f"Tide Level: {metadata['tide_level']}\n"
                    f"Environment Type: {metadata['environment_type']}\n"
                    f"Direction: {metadata['direction']}",
                    title="Metadata from Critical File"
                ))
                
                metadata_results['critical_file'] = metadata
                
        return metadata_results
        
    def collect_user_feedback(self) -> Dict[str, Any]:
        """Collect user feedback on results"""
        self.console.print("\n[bold cyan]User Feedback Collection[/bold cyan]")
        
        feedback = {
            'timestamp': datetime.now().isoformat(),
            'corrections': {},
            'validation': {},
            'comments': ''
        }
        
        # Ask for validation
        is_correct = Confirm.ask("Are the tension values correct?")
        feedback['validation']['tensions_correct'] = is_correct
        
        if not is_correct:
            correction = Prompt.ask("What should be corrected?")
            feedback['corrections']['tensions'] = correction
            
        is_metadata_correct = Confirm.ask("Is the metadata extraction correct?")
        feedback['validation']['metadata_correct'] = is_metadata_correct
        
        if not is_metadata_correct:
            correction = Prompt.ask("What metadata needs correction?")
            feedback['corrections']['metadata'] = correction
            
        # Additional comments
        add_comments = Confirm.ask("Would you like to add comments?")
        if add_comments:
            feedback['comments'] = Prompt.ask("Enter your comments")
            
        return feedback
        
    def save_verification(self, results: Dict[str, Any], feedback: Dict[str, Any]) -> str:
        """Save verification results"""
        self.console.print("\n[bold cyan]Saving Verification Results[/bold cyan]")
        
        filepath = self.verification_logger.log_verification(results, feedback)
        self.console.print(f"[green]✓ Saved to: {filepath}[/green]")
        
        return filepath
        
    def run_interactive_test(self):
        """Run complete interactive test"""
        self.console.print("[bold magenta]OrcaFlex Browser Backend Test Interface[/bold magenta]\n")
        
        # Get base path
        base_path = Prompt.ask(
            "Enter base path for CSV files",
            default="D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr"
        )
        
        self.setup(base_path)
        
        # Run tests
        try:
            # 1. Search files
            files = self.test_file_search()
            if not files:
                self.console.print("[red]No files found![/red]")
                return
                
            # 2. Read files in parallel
            dataframes = self.test_parallel_reading(files)
            if not dataframes:
                self.console.print("[red]No data loaded![/red]")
                return
                
            # 3. Analyze tensions
            tension_results = self.test_tension_analysis(dataframes)
            
            # 4. Extract metadata
            metadata_results = self.test_metadata_extraction(tension_results)
            
            # Combine results
            self.current_results = {
                'file_search': {'files_found': len(files), 'file_list': [str(f) for f in files]},
                'data_loading': {'dataframes_loaded': len(dataframes)},
                'tension_analysis': tension_results,
                'metadata_extraction': metadata_results
            }
            
            # 5. Collect feedback
            feedback = self.collect_user_feedback()
            
            # 6. Save verification
            self.save_verification(self.current_results, feedback)
            
            # Ask to continue
            if Confirm.ask("\nWould you like to test with different parameters?"):
                self.run_interactive_test()
                
        except Exception as e:
            self.console.print(f"[red]Error: {e}[/red]")
            import traceback
            traceback.print_exc()
            

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="OrcaFlex Browser Backend CLI Tester")
    parser.add_argument('--base-path', help='Base path for CSV files')
    parser.add_argument('--verify-latest', action='store_true', help='Load and display latest verification')
    
    args = parser.parse_args()
    
    tester = CLITester()
    
    if args.verify_latest:
        # Load and display latest verification
        logger = VerificationLogger()
        latest = logger.get_latest_verification()
        if latest:
            tester.console.print(JSON(json.dumps(latest, indent=2)))
        else:
            tester.console.print("[yellow]No verification files found[/yellow]")
    else:
        # Run interactive test
        tester.run_interactive_test()
        

if __name__ == "__main__":
    main()