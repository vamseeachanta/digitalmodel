"""
Enhanced OrcaFlex CLI Dashboard with Time Trace Display
Includes tabs for Strut Forces and Jacket Data
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__)))

from backend.file_processor import FileProcessor, MetadataExtractor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from time_trace_viewer import TimeTraceViewer
from pathlib import Path
import pandas as pd
from datetime import datetime
import json


class EnhancedDashboard:
    """Enhanced CLI Dashboard with Time Trace Display"""
    
    def __init__(self, base_path: str = None):
        self.base_path = None
        self.file_processor = None
        self.analyzer = OrcaFlexAnalyzer()
        self.metadata_extractor = MetadataExtractor()
        self.time_trace_viewer = None
        
        # Data storage
        self.files = []
        self.dataframes = {}
        self.analysis_results = None
        self.selected_fe_file = None
        
        # Display mode
        self.current_view = "dashboard"  # dashboard, strut_forces, jacket_data
        
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
    
    def load_data(self):
        """Load and analyze data"""
        print("\nLoading OrcaFlex data...")
        print("-" * 40)
        
        # Search for files
        print("Searching for files...")
        self.files = self.file_processor.search_files("dm_*_strut_dyn.csv")
        
        if not self.files:
            self.files = self.file_processor.search_files("*.csv")
        
        print(f"Found {len(self.files)} files")
        
        if self.files:
            print("Loading CSV files...")
            self.dataframes = self.file_processor.read_multiple_csvs_parallel(
                self.files[:20], max_workers=4
            )
            
            print("Analyzing data...")
            self.analysis_results = self.analyzer.analyze_multiple_dataframes(self.dataframes)
            
            # Extract FE filename from critical case
            if self.analysis_results and self.analysis_results.get('absolute_maximum'):
                abs_max = self.analysis_results['absolute_maximum']
                if 'fe_filename' in abs_max:
                    self.selected_fe_file = abs_max['fe_filename']
                    print(f"Critical FE file: {Path(self.selected_fe_file).name}")
            
            print(f"[OK] Analysis complete!")
    
    def display_main_dashboard(self):
        """Display the main dashboard view"""
        self.clear_screen()
        
        # Header
        print("=" * 80)
        print(" " * 20 + "ORCAFLEX BROWSER DASHBOARD - ENHANCED")
        print("=" * 80)
        print(f"Path: {self.base_path or 'Not Set'}")
        print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        if self.selected_fe_file:
            print(f"Selected FE: {Path(self.selected_fe_file).name}")
        
        print("-" * 80)
        
        # Summary Statistics
        self.print_summary()
        
        # Critical Case
        self.print_critical_case()
        
        # Strut Analysis
        self.print_strut_table()
        
        # Navigation
        print("\n" + "=" * 80)
        print("NAVIGATION OPTIONS")
        print("=" * 80)
        
        if self.selected_fe_file:
            print("[S] View Strut Forces Time Traces")
            print("[J] View Jacket Data Time Traces")
        
        print("[R] Refresh Data")
        print("[E] Export Results")
        print("[Q] Quit")
        print("=" * 80)
    
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
        
        # Print rows (limit to top 8)
        for item in strut_items[:8]:
            # Determine status
            if item['max'] > 5000:
                status = "*** CRITICAL"
            elif item['max'] > 3000:
                status = "** HIGH"
            else:
                status = "* Normal"
            
            print(f"{item['name'].upper():<10} {item['min']:>12.2f} {item['max']:>12.2f} "
                  f"{item['range']:>12.2f} {status:<15}")
    
    def display_strut_forces(self):
        """Display strut forces time traces"""
        if not self.selected_fe_file:
            print("No FE file selected")
            return
        
        # Create time trace viewer if not exists
        if not self.time_trace_viewer:
            # Get the FE file directory (parent of CSV directory)
            fe_base_path = self.base_path.parent if self.base_path else Path.cwd()
            self.time_trace_viewer = TimeTraceViewer(
                Path(self.selected_fe_file).name,
                str(fe_base_path)
            )
        
        # Display strut forces
        self.clear_screen()
        print("=" * 80)
        print(" " * 20 + "STRUT FORCES TIME TRACES")
        print("=" * 80)
        print(f"FE File: {Path(self.selected_fe_file).name}")
        print("-" * 80)
        
        # Show critical strut first
        if self.analysis_results and self.analysis_results.get('absolute_maximum'):
            critical_strut = self.analysis_results['absolute_maximum']['strut']
            strut_num = int(critical_strut.replace('strut', ''))
            self.time_trace_viewer.display_strut_forces(strut_num)
        else:
            self.time_trace_viewer.display_strut_forces(7)  # Default to Strut 7
        
        print("\n" + "=" * 80)
        print("OPTIONS: [D] Dashboard  [J] Jacket Data  [1-8] Select Strut  [Q] Quit")
        print("=" * 80)
    
    def display_jacket_data(self):
        """Display jacket data time traces"""
        if not self.selected_fe_file:
            print("No FE file selected")
            return
        
        # Create time trace viewer if not exists
        if not self.time_trace_viewer:
            fe_base_path = self.base_path.parent if self.base_path else Path.cwd()
            self.time_trace_viewer = TimeTraceViewer(
                Path(self.selected_fe_file).name,
                str(fe_base_path)
            )
        
        # Display jacket data
        self.clear_screen()
        print("=" * 80)
        print(" " * 20 + "JACKET DATA TIME TRACES")
        print("=" * 80)
        print(f"FE File: {Path(self.selected_fe_file).name}")
        print("-" * 80)
        
        self.time_trace_viewer.display_jacket_data()
        
        print("\n" + "=" * 80)
        print("OPTIONS: [D] Dashboard  [S] Strut Forces  [Q] Quit")
        print("=" * 80)
    
    def export_results(self):
        """Export current results to file"""
        if not self.analysis_results:
            print("No data to export!")
            return
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"orcaflex_enhanced_{timestamp}.json"
        
        export_data = {
            'timestamp': datetime.now().isoformat(),
            'base_path': str(self.base_path),
            'selected_fe_file': self.selected_fe_file,
            'files_analyzed': len(self.files),
            'summary': self.analysis_results.get('summary'),
            'critical_case': self.analysis_results.get('absolute_maximum'),
            'strut_analysis': self.analysis_results.get('strut_analysis')
        }
        
        # Convert types for JSON
        def convert_types(obj):
            if isinstance(obj, dict):
                return {k: convert_types(v) for k, v in obj.items()}
            elif isinstance(obj, list):
                return [convert_types(i) for i in obj]
            elif hasattr(obj, 'item'):
                return obj.item()
            elif pd.isna(obj):
                return None
            else:
                return obj
        
        export_data = convert_types(export_data)
        
        with open(filename, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        print(f"\n[OK] Exported to {filename}")
    
    def run(self):
        """Run the enhanced dashboard"""
        # Initial setup
        if not self.base_path:
            print("Welcome to Enhanced OrcaFlex CLI Dashboard!")
            print("-" * 40)
            
            path = input("Enter data directory path (or press Enter for default): ").strip()
            
            if not path:
                path = "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr"
            
            if not self.set_base_path(path):
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
            # Display current view
            if self.current_view == "dashboard":
                self.display_main_dashboard()
            elif self.current_view == "strut_forces":
                self.display_strut_forces()
            elif self.current_view == "jacket_data":
                self.display_jacket_data()
            
            # Get user input
            choice = input("\nSelect option: ").strip().lower()
            
            # Handle navigation
            if choice == 'q':
                print("\nGoodbye!")
                break
            elif choice == 'd':
                self.current_view = "dashboard"
            elif choice == 's' and self.selected_fe_file:
                self.current_view = "strut_forces"
            elif choice == 'j' and self.selected_fe_file:
                self.current_view = "jacket_data"
            elif choice == 'r':
                self.load_data()
            elif choice == 'e':
                self.export_results()
                input("\nPress Enter to continue...")
            elif choice.isdigit() and self.current_view == "strut_forces":
                # Select specific strut
                strut_num = int(choice)
                if 1 <= strut_num <= 8 and self.time_trace_viewer:
                    self.clear_screen()
                    self.time_trace_viewer.display_strut_forces(strut_num)
                    input("\nPress Enter to continue...")
            else:
                if choice and choice not in ['d', 's', 'j', 'r', 'e', 'q']:
                    print("Invalid option. Press Enter to continue...")
                    input()


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Enhanced OrcaFlex CLI Dashboard")
    parser.add_argument('--path', help='Data directory path')
    parser.add_argument('--demo', action='store_true', 
                       help='Run in demo mode with all views')
    
    args = parser.parse_args()
    
    # Create dashboard
    dashboard = EnhancedDashboard(args.path)
    
    try:
        if args.demo:
            # Demo mode - show each view once
            if not dashboard.base_path:
                dashboard.set_base_path("D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr")
                if not dashboard.base_path.exists():
                    dashboard.set_base_path("sample_data")
            
            dashboard.load_data()
            
            # Show main dashboard
            dashboard.display_main_dashboard()
            print("\n[Demo Mode - Showing all views]")
            input("Press Enter to see Strut Forces...")
            
            # Show strut forces
            dashboard.current_view = "strut_forces"
            dashboard.display_strut_forces()
            input("Press Enter to see Jacket Data...")
            
            # Show jacket data
            dashboard.current_view = "jacket_data"
            dashboard.display_jacket_data()
            
            print("\n[Demo Complete]")
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