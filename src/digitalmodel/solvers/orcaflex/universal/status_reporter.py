"""
Status Reporting System
=======================

Real-time status reporting in terminal/bash window with progress tracking.
"""

import os
import sys
import time
import json
import logging
from pathlib import Path
from typing import List, Dict, Optional, Union
from datetime import datetime, timedelta

logger = logging.getLogger(__name__)


class StatusReporter:
    """
    Real-time status reporting in terminal with colored output.
    
    Features:
    - Terminal/bash window title updates
    - Progress bar with colors
    - Summary report generation
    - JSON export of results
    - Live status tracking
    """
    
    def __init__(self, enable_colors: bool = True):
        """
        Initialize the status reporter.
        
        Args:
            enable_colors: Enable colored terminal output
        """
        self.total = 0
        self.completed = 0
        self.success = 0
        self.failed = 0
        self.current_model = ""
        self.failed_list = []
        self.sim_files_created = []
        self.start_time = None
        self.enable_colors = enable_colors and self._supports_colors()
        
        # Terminal colors
        if self.enable_colors:
            self.GREEN = '\033[92m'
            self.RED = '\033[91m'
            self.YELLOW = '\033[93m'
            self.BLUE = '\033[94m'
            self.RESET = '\033[0m'
            self.BOLD = '\033[1m'
        else:
            self.GREEN = self.RED = self.YELLOW = self.BLUE = self.RESET = self.BOLD = ''
        
        # Track processing details
        self.model_times = []
        self.last_update_time = 0
        self.update_interval = 0.1  # Update display every 0.1 seconds
        
    def _supports_colors(self) -> bool:
        """Check if terminal supports colors."""
        # Windows terminal color support
        if sys.platform == "win32":
            try:
                import colorama
                colorama.init()
                return True
            except ImportError:
                # Try to enable ANSI colors on Windows 10+
                try:
                    os.system("color")
                    return True
                except:
                    return False
        
        # Unix/Linux/Mac usually support colors
        return hasattr(sys.stdout, 'isatty') and sys.stdout.isatty()
    
    def update_terminal_title(self):
        """Update terminal/bash window title with current status."""
        if not self.start_time:
            return
        
        elapsed = time.time() - self.start_time
        rate = self.completed / elapsed if elapsed > 0 else 0
        
        # Build status string
        if self.total > 0:
            progress_pct = (self.completed / self.total) * 100
            status_str = (
                f"OrcaFlex: [{self.completed}/{self.total}] "
                f"{progress_pct:.0f}% | "
                f"OK:{self.success} FAIL:{self.failed} | "
                f"{rate:.1f}/s | "
                f"{self.current_model[:30]}"
            )
        else:
            status_str = f"OrcaFlex: Initializing..."
        
        # Update terminal title based on platform
        try:
            if sys.platform == "win32":
                # Windows command prompt/PowerShell
                os.system(f"title {status_str}")
            else:
                # Unix/Linux/Mac terminals
                sys.stdout.write(f"\033]0;{status_str}\007")
                sys.stdout.flush()
        except:
            # Silently ignore if terminal doesn't support title updates
            pass
    
    def display_progress(self, force: bool = False):
        """
        Display progress bar with colors.
        
        Args:
            force: Force update even if within update interval
        """
        # Check if we should update (avoid too frequent updates)
        current_time = time.time()
        if not force and (current_time - self.last_update_time) < self.update_interval:
            return
        
        self.last_update_time = current_time
        
        if self.total == 0:
            return
        
        progress = self.completed / self.total
        bar_length = 40  # Shorter bar for better display
        filled = int(bar_length * progress)
        
        # Build progress bar
        bar = '█' * filled + '░' * (bar_length - filled)
        
        # Determine color based on status
        if self.failed == 0:
            color = self.GREEN
        elif self.failed > self.success:
            color = self.RED
        else:
            color = self.YELLOW
        
        # Calculate time estimates
        if self.start_time:
            elapsed = current_time - self.start_time
            if self.completed > 0:
                avg_time = elapsed / self.completed
                remaining = (self.total - self.completed) * avg_time
                eta_str = self._format_time(remaining)
            else:
                eta_str = "calculating..."
        else:
            elapsed = 0
            eta_str = "N/A"
        
        # Build status line
        status_line = (
            f"\r{color}[{bar}] {progress*100:.1f}%{self.RESET} | "
            f"{self.GREEN}OK:{self.success}{self.RESET} "
            f"{self.RED}FAIL:{self.failed}{self.RESET} | "
            f"ETA: {eta_str} | "
            f"{self.current_model[:35]:<35}"
        )
        
        # Display (use \r to overwrite the same line)
        sys.stdout.write(status_line)
        sys.stdout.flush()
    
    def log_result(self, model_name: str, success: bool, 
                   sim_file: Optional[str] = None, 
                   error: Optional[str] = None,
                   duration: float = 0):
        """
        Log individual test result.
        
        Args:
            model_name: Name of the model processed
            success: Whether processing was successful
            sim_file: Path to generated .sim file
            error: Error message if failed
            duration: Processing duration in seconds
        """
        self.completed += 1
        
        if success:
            self.success += 1
            if sim_file:
                self.sim_files_created.append(sim_file)
        else:
            self.failed += 1
            self.failed_list.append({
                "model": model_name,
                "error": error or "Unknown error"
            })
        
        # Track timing
        if duration > 0:
            self.model_times.append(duration)
        
        # Update displays
        self.update_terminal_title()
        self.display_progress()
    
    def display_summary(self):
        """Display formatted summary to console."""
        if not self.start_time:
            return
        
        total_time = time.time() - self.start_time
        
        # Clear the progress line
        sys.stdout.write("\r" + " " * 100 + "\r")
        
        print("\n" + "=" * 80)
        print(f"{self.BOLD}ORCAFLEX PROCESSING SUMMARY{self.RESET}")
        print("=" * 80)
        
        # Overall statistics
        print(f"\n{self.BOLD}Overall Statistics:{self.RESET}")
        print(f"  Total Models Processed: {self.total}")
        print(f"  {self.GREEN}Successful: {self.success} (OK){self.RESET}")
        print(f"  {self.RED}Failed: {self.failed} (FAIL){self.RESET}")
        
        if self.total > 0:
            success_rate = (self.success / self.total) * 100
            if success_rate == 100:
                rate_color = self.GREEN
            elif success_rate >= 80:
                rate_color = self.YELLOW
            else:
                rate_color = self.RED
            print(f"  {rate_color}Success Rate: {success_rate:.1f}%{self.RESET}")
        
        # Timing information
        print(f"\n{self.BOLD}Performance:{self.RESET}")
        print(f"  Total Time: {self._format_time(total_time)}")
        
        if self.model_times:
            avg_time = sum(self.model_times) / len(self.model_times)
            min_time = min(self.model_times)
            max_time = max(self.model_times)
            print(f"  Average Time per Model: {avg_time:.2f}s")
            print(f"  Fastest Model: {min_time:.2f}s")
            print(f"  Slowest Model: {max_time:.2f}s")
        
        if self.completed > 0:
            rate = self.completed / total_time
            print(f"  Processing Rate: {rate:.2f} models/second")
        
        # Output files
        if self.sim_files_created:
            print(f"\n{self.BOLD}Output Files:{self.RESET}")
            print(f"  Simulation Files Created: {len(self.sim_files_created)}")
            for sim_file in self.sim_files_created[:3]:
                print(f"    - {Path(sim_file).name}")
            if len(self.sim_files_created) > 3:
                print(f"    ... and {len(self.sim_files_created) - 3} more")
        
        # Failed models
        if self.failed_list:
            print(f"\n{self.BOLD}{self.RED}Failed Models:{self.RESET}")
            for failed in self.failed_list[:5]:
                error_msg = failed['error']
                if len(error_msg) > 60:
                    error_msg = error_msg[:57] + "..."
                print(f"  FAIL: {failed['model']}: {error_msg}")
            if len(self.failed_list) > 5:
                print(f"  ... and {len(self.failed_list) - 5} more failures")
        
        print("=" * 80)
    
    def generate_report(self) -> Dict:
        """
        Generate summary report dictionary.
        
        Returns:
            Dictionary with complete processing summary
        """
        total_time = time.time() - self.start_time if self.start_time else 0
        
        report = {
            "execution_summary": {
                "total_models": self.total,
                "successful": self.success,
                "failed": self.failed,
                "success_rate": (self.success / self.total * 100) if self.total > 0 else 0,
                "total_time": total_time,
                "start_time": datetime.fromtimestamp(self.start_time).isoformat() if self.start_time else None,
                "end_time": datetime.now().isoformat(),
            },
            "performance": {
                "avg_time_per_model": sum(self.model_times) / len(self.model_times) if self.model_times else 0,
                "min_time": min(self.model_times) if self.model_times else 0,
                "max_time": max(self.model_times) if self.model_times else 0,
                "processing_rate": self.completed / total_time if total_time > 0 else 0,
            },
            "output_files": {
                "sim_files_created": len(self.sim_files_created),
                "files": self.sim_files_created
            },
            "failed_models": self.failed_list,
        }
        
        return report
    
    def save_report(self, output_path: Union[str, Path]):
        """
        Save detailed report to JSON file.
        
        Args:
            output_path: Path for the JSON report file
        """
        report = self.generate_report()
        
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        
        print(f"\n{self.BLUE}Detailed report saved to: {output_path}{self.RESET}")
    
    def _format_time(self, seconds: float) -> str:
        """Format time duration in human-readable format."""
        if seconds < 60:
            return f"{seconds:.1f}s"
        elif seconds < 3600:
            minutes = int(seconds / 60)
            secs = int(seconds % 60)
            return f"{minutes}m {secs}s"
        else:
            hours = int(seconds / 3600)
            minutes = int((seconds % 3600) / 60)
            return f"{hours}h {minutes}m"
    
    def reset(self):
        """Reset the status reporter for a new run."""
        self.total = 0
        self.completed = 0
        self.success = 0
        self.failed = 0
        self.current_model = ""
        self.failed_list = []
        self.sim_files_created = []
        self.start_time = None
        self.model_times = []
        self.last_update_time = 0
    
    def set_final_status(self):
        """Set terminal title to final status."""
        if self.completed > 0:
            final_status = (
                f"OrcaFlex Complete: "
                f"OK:{self.success} FAIL:{self.failed} "
                f"({(self.success/self.total*100):.0f}% success)"
            )
        else:
            final_status = "OrcaFlex: No models processed"
        
        try:
            if sys.platform == "win32":
                os.system(f"title {final_status}")
            else:
                sys.stdout.write(f"\033]0;{final_status}\007")
                sys.stdout.flush()
        except:
            pass