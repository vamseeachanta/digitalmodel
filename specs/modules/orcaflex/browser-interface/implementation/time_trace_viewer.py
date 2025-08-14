"""
Time Trace Viewer for OrcaFlex FE Simulation Data
Displays time series data for selected FE files
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__)))

from pathlib import Path
import pandas as pd
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional, Tuple
import json


class TimeTraceViewer:
    """View time series data from FE simulation files"""
    
    def __init__(self, fe_filename: str, base_path: str = None):
        self.fe_filename = fe_filename
        self.fe_stem = Path(fe_filename).stem
        self.base_path = Path(base_path) if base_path else Path.cwd()
        
        # Data storage
        self.strut_timeseries = None
        self.jacket_timeseries = None
        self.time_vector = None
        
        # Display settings
        self.current_tab = "overview"
        self.plot_width = 60
        self.plot_height = 20
        
        # Try to load associated time series data
        self.load_timeseries_data()
    
    def find_timeseries_files(self) -> Dict[str, Path]:
        """Find time series CSV files associated with FE file"""
        found_files = {}
        
        # Possible patterns for time series files
        patterns = [
            f"{self.fe_stem}_strut_timeseries.csv",
            f"{self.fe_stem}_jacket_timeseries.csv",
            f"{self.fe_stem}_timeseries.csv",
            f"{self.fe_stem}_*.csv"
        ]
        
        # Search in various locations
        search_dirs = [
            self.base_path,
            self.base_path / "timeseries",
            self.base_path / "results",
            self.base_path / self.fe_stem
        ]
        
        for directory in search_dirs:
            if directory.exists():
                for pattern in patterns:
                    matches = list(directory.glob(pattern))
                    for match in matches:
                        if "strut" in match.name.lower():
                            found_files["strut"] = match
                        elif "jacket" in match.name.lower():
                            found_files["jacket"] = match
                        elif "timeseries" in match.name.lower():
                            found_files["general"] = match
        
        return found_files
    
    def load_timeseries_data(self):
        """Load time series data from CSV files"""
        files = self.find_timeseries_files()
        
        if not files:
            # Generate sample data for demonstration
            self.generate_sample_data()
            return
        
        # Load strut data if available
        if "strut" in files:
            try:
                self.strut_timeseries = pd.read_csv(files["strut"])
                if "Time" in self.strut_timeseries.columns:
                    self.time_vector = self.strut_timeseries["Time"].values
            except Exception as e:
                print(f"Error loading strut data: {e}")
        
        # Load jacket data if available
        if "jacket" in files:
            try:
                self.jacket_timeseries = pd.read_csv(files["jacket"])
            except Exception as e:
                print(f"Error loading jacket data: {e}")
    
    def generate_sample_data(self):
        """Generate sample time series data for demonstration"""
        # Create time vector (3 hours at 0.1 sec intervals)
        duration = 3 * 3600  # 3 hours in seconds
        dt = 0.1  # time step
        self.time_vector = np.arange(0, duration, dt)
        n_points = len(self.time_vector)
        
        # Generate strut forces with realistic patterns
        # Base tension + wave response + drift
        np.random.seed(42)  # For reproducibility
        
        strut_data = {"Time": self.time_vector}
        
        for i in range(1, 9):  # 8 struts
            # Base tension
            base = 2000 + i * 500
            
            # Wave response (multiple frequencies)
            wave1 = 1000 * np.sin(2 * np.pi * 0.1 * self.time_vector)  # 10 sec period
            wave2 = 500 * np.sin(2 * np.pi * 0.067 * self.time_vector)  # 15 sec period
            
            # Add some randomness
            noise = np.random.normal(0, 100, n_points)
            
            # Create spike at specific time for strut 7
            spike = np.zeros(n_points)
            if i == 7:
                spike_time = int(2450.3 / dt)  # At t=2450.3s
                spike[spike_time:spike_time+10] = 3000  # Sharp spike
            
            strut_data[f"Strut{i}_Tension"] = base + wave1 + wave2 + noise + spike
        
        self.strut_timeseries = pd.DataFrame(strut_data)
        
        # Generate jacket data
        jacket_data = {
            "Time": self.time_vector,
            "Disp_X": 0.5 * np.sin(2 * np.pi * 0.1 * self.time_vector) + np.random.normal(0, 0.05, n_points),
            "Disp_Y": 0.4 * np.sin(2 * np.pi * 0.1 * self.time_vector + np.pi/4) + np.random.normal(0, 0.05, n_points),
            "Disp_Z": 0.2 * np.sin(2 * np.pi * 0.067 * self.time_vector) + np.random.normal(0, 0.02, n_points),
            "Accel_X": np.gradient(np.gradient(0.5 * np.sin(2 * np.pi * 0.1 * self.time_vector))) / (dt**2),
            "Accel_Y": np.gradient(np.gradient(0.4 * np.sin(2 * np.pi * 0.1 * self.time_vector + np.pi/4))) / (dt**2)
        }
        
        self.jacket_timeseries = pd.DataFrame(jacket_data)
    
    def plot_ascii(self, values: np.ndarray, width: int = 60, height: int = 20, 
                   title: str = "", show_stats: bool = True) -> List[str]:
        """Create ASCII plot of time series"""
        lines = []
        
        # Title
        if title:
            lines.append(title)
            lines.append("-" * width)
        
        # Handle empty data
        if len(values) == 0:
            lines.append("No data available")
            return lines
        
        # Calculate statistics
        min_val = np.min(values)
        max_val = np.max(values)
        mean_val = np.mean(values)
        std_val = np.std(values)
        
        # Normalize values for plotting
        if max_val > min_val:
            normalized = (values - min_val) / (max_val - min_val)
        else:
            normalized = np.zeros_like(values)
        
        # Downsample if needed
        if len(values) > width:
            indices = np.linspace(0, len(values)-1, width, dtype=int)
            plot_values = normalized[indices]
            time_values = self.time_vector[indices] if self.time_vector is not None else indices
        else:
            plot_values = normalized
            time_values = self.time_vector if self.time_vector is not None else np.arange(len(values))
        
        # Create plot
        for h in range(height, 0, -1):
            threshold = h / height
            line = ""
            for v in plot_values:
                if v >= threshold:
                    line += "█"
                elif v >= threshold - 0.05:
                    line += "▄"
                else:
                    line += " "
            
            # Add y-axis label
            y_val = min_val + (max_val - min_val) * threshold
            lines.append(f"{y_val:8.1f} |{line}")
        
        # Add x-axis
        lines.append(" " * 9 + "+" + "-" * width)
        
        # Add time labels
        if self.time_vector is not None:
            time_str = f"         0s" + " " * (width - 20) + f"{self.time_vector[-1]:.1f}s"
            lines.append(time_str)
        
        # Add statistics
        if show_stats:
            lines.append("")
            lines.append("Statistics:")
            lines.append(f"  Min:  {min_val:10.2f}")
            lines.append(f"  Max:  {max_val:10.2f}")
            lines.append(f"  Mean: {mean_val:10.2f}")
            lines.append(f"  Std:  {std_val:10.2f}")
            
            # Find time of maximum
            max_idx = np.argmax(values)
            if self.time_vector is not None and max_idx < len(self.time_vector):
                lines.append(f"  Max at t={self.time_vector[max_idx]:.1f}s")
        
        return lines
    
    def display_overview(self):
        """Display overview of available data"""
        print("\n" + "=" * 80)
        print(f"TIME TRACE VIEWER - {self.fe_filename}")
        print("=" * 80)
        
        print("\nData Summary:")
        print("-" * 40)
        
        if self.strut_timeseries is not None:
            print(f"Strut Forces: {len(self.strut_timeseries)} time points")
            print(f"  Columns: {', '.join([c for c in self.strut_timeseries.columns if 'Strut' in c])}")
        else:
            print("Strut Forces: No data loaded")
        
        if self.jacket_timeseries is not None:
            print(f"Jacket Data: {len(self.jacket_timeseries)} time points")
            print(f"  Parameters: {', '.join(self.jacket_timeseries.columns[1:])}")
        else:
            print("Jacket Data: No data loaded")
        
        if self.time_vector is not None:
            print(f"\nTime Range: 0 - {self.time_vector[-1]:.1f} seconds")
            print(f"Sample Rate: {self.time_vector[1] - self.time_vector[0]:.2f} seconds")
    
    def display_strut_forces(self, strut_num: int = 7):
        """Display strut forces tab"""
        print("\n" + "=" * 80)
        print(f"STRUT FORCES - {self.fe_filename}")
        print("=" * 80)
        
        if self.strut_timeseries is None:
            print("No strut force data available")
            return
        
        col_name = f"Strut{strut_num}_Tension"
        if col_name not in self.strut_timeseries.columns:
            print(f"No data for {col_name}")
            return
        
        values = self.strut_timeseries[col_name].values
        
        # Create plot
        plot_lines = self.plot_ascii(
            values, 
            width=self.plot_width, 
            height=15,
            title=f"Strut {strut_num} Tension (kN)",
            show_stats=True
        )
        
        for line in plot_lines:
            print(line)
        
        # Show all struts summary
        print("\n" + "-" * 40)
        print("All Struts Summary:")
        for i in range(1, 9):
            col = f"Strut{i}_Tension"
            if col in self.strut_timeseries.columns:
                data = self.strut_timeseries[col]
                max_val = data.max()
                min_val = data.min()
                indicator = " ***" if i == strut_num else ""
                print(f"  Strut {i}: Max={max_val:8.2f}, Min={min_val:8.2f}{indicator}")
    
    def display_jacket_data(self):
        """Display jacket data tab"""
        print("\n" + "=" * 80)
        print(f"JACKET DATA - {self.fe_filename}")
        print("=" * 80)
        
        if self.jacket_timeseries is None:
            print("No jacket data available")
            return
        
        # Display displacement plot
        if "Disp_X" in self.jacket_timeseries.columns:
            values = self.jacket_timeseries["Disp_X"].values
            plot_lines = self.plot_ascii(
                values,
                width=self.plot_width,
                height=10,
                title="Displacement X (m)",
                show_stats=True
            )
            
            for line in plot_lines:
                print(line)
        
        # Summary table
        print("\n" + "-" * 40)
        print("Parameters Summary:")
        print(f"{'Parameter':<15} {'Min':>10} {'Max':>10} {'Mean':>10} {'Units':<10}")
        print("-" * 60)
        
        units_map = {
            "Disp": "m",
            "Accel": "m/s²",
            "Force": "kN",
            "Moment": "kN.m"
        }
        
        for col in self.jacket_timeseries.columns:
            if col != "Time":
                data = self.jacket_timeseries[col]
                
                # Determine units
                unit = "N/A"
                for key, val in units_map.items():
                    if key in col:
                        unit = val
                        break
                
                print(f"{col:<15} {data.min():>10.3f} {data.max():>10.3f} {data.mean():>10.3f} {unit:<10}")
    
    def display_interactive(self):
        """Interactive display with tab navigation"""
        current_tab = "overview"
        
        while True:
            # Clear screen (platform dependent)
            os.system('cls' if os.name == 'nt' else 'clear')
            
            # Display current tab
            if current_tab == "overview":
                self.display_overview()
            elif current_tab == "strut":
                self.display_strut_forces()
            elif current_tab == "jacket":
                self.display_jacket_data()
            
            # Navigation menu
            print("\n" + "=" * 80)
            print("NAVIGATION")
            print("=" * 80)
            print("[1] Overview  [2] Strut Forces  [3] Jacket Data  [Q] Quit")
            print(f"Current: {current_tab.upper()}")
            
            # Get user input
            choice = input("\nSelect option: ").strip().lower()
            
            if choice == 'q':
                break
            elif choice == '1':
                current_tab = "overview"
            elif choice == '2':
                current_tab = "strut"
            elif choice == '3':
                current_tab = "jacket"
            else:
                print("Invalid option. Press Enter to continue...")
                input()


def main():
    """Main entry point for standalone testing"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Time Trace Viewer for OrcaFlex Data")
    parser.add_argument('--fe-file', default='fsts_l015_hwl_ncl_240deg.sim',
                       help='FE filename to view')
    parser.add_argument('--base-path', 
                       default='D:\\1522\\ctr7\\orcaflex\\rev_a08\\03c_100yr',
                       help='Base path for searching files')
    parser.add_argument('--interactive', action='store_true',
                       help='Run in interactive mode')
    
    args = parser.parse_args()
    
    # Create viewer
    viewer = TimeTraceViewer(args.fe_file, args.base_path)
    
    if args.interactive:
        viewer.display_interactive()
    else:
        # Show all tabs once
        viewer.display_overview()
        print("\n" + "=" * 80)
        input("Press Enter to see Strut Forces...")
        viewer.display_strut_forces()
        print("\n" + "=" * 80)
        input("Press Enter to see Jacket Data...")
        viewer.display_jacket_data()


if __name__ == "__main__":
    main()