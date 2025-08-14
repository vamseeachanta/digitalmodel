"""
OrcaFlex CLI Dashboard (Simple Version)
Terminal dashboard for browsing and analyzing OrcaFlex data
Works without external dependencies beyond pandas/numpy
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__)))

from backend.file_processor import FileProcessor, MetadataExtractor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from pathlib import Path
import pandas as pd
from datetime import datetime
import json
from typing import List, Dict, Any, Optional


class SimpleDashboard:
    """Simple CLI Dashboard for OrcaFlex Data Analysis"""
    
    def __init__(self, base_path: str = None):
        self.base_path = None
        self.file_processor = None
        self.analyzer = OrcaFlexAnalyzer()
        self.metadata_extractor = MetadataExtractor()
        
        # Data storage
        self.files = []
        self.dataframes = {}
        self.analysis_results = None
        
        if base_path:
            self.set_base_path(base_path)
    
    def set_base_path(self, base_path: str) -> bool:
        """Set the base path for data files"""
        self.base_path = Path(base_path)
        if self.base_path.exists():
            self.file_processor = FileProcessor(str(self.base_path))
            return True
        return False
    
    def clear_screen(self):
        """Clear the terminal screen"""
        os.system('cls' if os.name == 'nt' else 'clear')
    
    def print_header(self):
        """Print dashboard header"""
        print("=" * 80)
        print(" " * 20 + "ORCAFLEX BROWSER DASHBOARD")
        print("=" * 80)
        print(f"Path: {self.base_path or 'Not Set'}")
        print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("-" * 80)
    
    def print_summary(self):
        """Print summary statistics"""
        print("\n[SUMMARY STATISTICS]")
        print("-" * 40)
        
        if not self.analysis_results:
            print("No data loaded")
            return
        
        summary = self.analysis_results.get('summary', {})
        
        print(f"Files Loaded:      {len(self.files)}")
        print(f"Struts Analyzed:   {summary.get('total_struts_found', 0)}")
        
        if summary.get('absolute_max_tension') is not None:
            print(f"Max Tension:       {summary['absolute_max_tension']:.2f}")
            print(f"Min Tension:       {summary['absolute_min_tension']:.2f}")
            
        if self.analysis_results.get('absolute_maximum'):
            abs_max = self.analysis_results['absolute_maximum']
            print(f"Critical Strut:    {abs_max['strut'].upper()}")
            print(f"Critical Value:    {abs_max['value']:.2f}")
    
    def print_critical_case(self):
        """Print critical case details"""
        print("\n[CRITICAL CASE]")
        print("-" * 40)
        
        if not self.analysis_results or not self.analysis_results.get('absolute_maximum'):
            print("No critical case identified")
            return
        
        abs_max = self.analysis_results['absolute_maximum']
        
        print(f"ABSOLUTE MAXIMUM TENSION")
        print(f"Value:             {abs_max['value']:.2f}")
        print(f"Strut:             {abs_max['strut'].upper()}")
        
        if 'fe_filename' in abs_max:
            fe_file = Path(abs_max['fe_filename']).name
            print(f"FE File:           {fe_file}")
            
            # Extract metadata
            metadata = self.metadata_extractor.extract_from_fe_filename(abs_max['fe_filename'])
            if metadata:
                print("\nConditions:")
                if metadata.get('lng_loading'):
                    print(f"  - LNG:         {metadata['lng_loading']}")
                if metadata.get('tide_level'):
                    print(f"  - Tide:        {metadata['tide_level']}")
                if metadata.get('environment_type'):
                    print(f"  - Environment: {metadata['environment_type']}")
                if metadata.get('direction'):
                    print(f"  - Direction:   {metadata['direction']}")
    
    def print_strut_table(self):
        """Print strut analysis table"""
        print("\n[STRUT ANALYSIS]")
        print("-" * 70)
        
        if not self.analysis_results or not self.analysis_results.get('strut_analysis'):
            print("No strut data available")
            return
        
        # Print header
        print(f"{'Strut':<10} {'Min':>12} {'Max':>12} {'Range':>12} {'Status':<15}")
        print("-" * 70)
        
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
        
        # Print rows
        for item in strut_items:
            # Determine status
            if item['max'] > 5000:
                status = "*** CRITICAL"
            elif item['max'] > 3000:
                status = "** HIGH"
            else:
                status = "* Normal"
            
            print(f"{item['name'].upper():<10} {item['min']:>12.2f} {item['max']:>12.2f} "
                  f"{item['range']:>12.2f} {status:<15}")
    
    def print_file_list(self):
        """Print file list"""
        print("\n[FILES]")
        print("-" * 40)
        
        if not self.files:
            print("No files loaded")
            return
        
        # Show first 10 files
        for i, file in enumerate(self.files[:10], 1):
            filename = file.name
            if len(filename) > 60:
                filename = filename[:57] + "..."
            print(f"  {i:2d}. {filename}")
        
        if len(self.files) > 10:
            print(f"\n  ... and {len(self.files)-10} more files")
    
    def load_data(self):
        """Load and analyze data"""
        print("\nLoading OrcaFlex data...")
        print("-" * 40)
        
        # Search for files
        print("Searching for files...")
        self.files = self.file_processor.search_files("dm_*_strut_dyn.csv")
        
        if not self.files:
            # Try alternative pattern
            self.files = self.file_processor.search_files("*.csv")
        
        print(f"Found {len(self.files)} files")
        
        if self.files:
            print("Loading CSV files...")
            self.dataframes = self.file_processor.read_multiple_csvs_parallel(
                self.files[:20], max_workers=4  # Limit for performance
            )
            
            print("Analyzing data...")
            self.analysis_results = self.analyzer.analyze_multiple_dataframes(self.dataframes)
            
            print(f"[OK] Analysis complete!")
    
    def export_results(self):
        """Export current results to file"""
        if not self.analysis_results:
            print("No data to export!")
            return
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"orcaflex_analysis_{timestamp}.json"
        
        export_data = {
            'timestamp': datetime.now().isoformat(),
            'base_path': str(self.base_path),
            'files_analyzed': len(self.files),
            'summary': self.analysis_results.get('summary'),
            'critical_case': self.analysis_results.get('absolute_maximum'),
            'strut_analysis': self.analysis_results.get('strut_analysis')
        }
        
        # Convert numpy/pandas types to native Python types
        def convert_types(obj):
            if isinstance(obj, dict):
                return {k: convert_types(v) for k, v in obj.items()}
            elif isinstance(obj, list):
                return [convert_types(i) for i in obj]
            elif hasattr(obj, 'item'):  # numpy scalar
                return obj.item()
            elif pd.isna(obj):
                return None
            else:
                return obj
        
        export_data = convert_types(export_data)
        
        with open(filename, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        print(f"\n[OK] Exported to {filename}")
    
    def display_full_dashboard(self):
        """Display the complete dashboard"""
        self.clear_screen()
        self.print_header()
        
        # Two-column layout simulation
        # Left side
        self.print_summary()
        self.print_critical_case()
        
        # Center
        self.print_strut_table()
        
        # Bottom
        self.print_file_list()
        
        # Footer
        print("\n" + "=" * 80)
        print("OPTIONS: [R]efresh  [E]xport  [F]ilter  [S]ort  [Q]uit")
        print("=" * 80)
    
    def run(self):
        """Run the dashboard"""
        # Initial setup if no base path
        if not self.base_path:
            print("Welcome to OrcaFlex CLI Dashboard!")
            print("-" * 40)
            
            path = input("Enter data directory path (or press Enter for default): ").strip()
            
            if not path:
                path = "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr"
            
            if not self.set_base_path(path):
                # Try alternative paths
                alternatives = [
                    "D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr",
                    "sample_data"
                ]
                for alt_path in alternatives:
                    if self.set_base_path(alt_path):
                        print(f"Using: {alt_path}")
                        break
                else:
                    print("Error: Invalid path!")
                    return
        
        # Load initial data
        self.load_data()
        
        # Main loop
        while True:
            self.display_full_dashboard()
            
            # Get user input
            choice = input("\nSelect option: ").strip().lower()
            
            if choice == 'q':
                print("\nGoodbye!")
                break
            elif choice == 'r':
                self.load_data()
            elif choice == 'e':
                self.export_results()
                input("\nPress Enter to continue...")
            elif choice == 'f':
                print("\nFilter functionality coming soon...")
                input("Press Enter to continue...")
            elif choice == 's':
                print("\nSort functionality coming soon...")
                input("Press Enter to continue...")
            else:
                print("Invalid option. Press Enter to continue...")
                input()


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="OrcaFlex CLI Dashboard")
    parser.add_argument('--path', help='Data directory path')
    parser.add_argument('--auto', action='store_true', 
                       help='Run in automatic mode (no interaction)')
    
    args = parser.parse_args()
    
    # Create dashboard
    dashboard = SimpleDashboard(args.path)
    
    try:
        if args.auto:
            # Automatic mode - just display once and exit
            if not dashboard.base_path:
                dashboard.set_base_path("D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr")
                if not dashboard.base_path.exists():
                    dashboard.set_base_path("sample_data")
            
            dashboard.load_data()
            dashboard.display_full_dashboard()
            dashboard.export_results()
        else:
            dashboard.run()
            
    except KeyboardInterrupt:
        print("\n\nInterrupted by user")
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()