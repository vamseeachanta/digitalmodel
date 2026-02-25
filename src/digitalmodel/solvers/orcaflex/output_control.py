"""
Output control utilities for OrcaFlex post-processing.

Provides verbosity control and progress tracking to reduce output noise.
"""

import sys
import time
from typing import Optional, List, Tuple
from contextlib import contextmanager


class OutputController:
    """
    Controls output verbosity for OrcaFlex processing.
    
    Supports three levels:
    - QUIET: Minimal output (summary only)
    - NORMAL: Balanced output with progress
    - VERBOSE: Detailed output (current behavior)
    """
    
    QUIET = 0
    NORMAL = 1
    VERBOSE = 2
    
    def __init__(self, level: int = NORMAL):
        """
        Initialize output controller.
        
        Args:
            level: Output level (QUIET, NORMAL, or VERBOSE)
        """
        self.level = level
        self._start_time = None
        self._file_count = 0
        self._completed = 0
        self._failed = 0
        self._failed_files = []
        
    @classmethod
    def from_config(cls, cfg: dict) -> 'OutputController':
        """
        Create controller from configuration.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            OutputController instance
        """
        if cfg.get('quiet', False):
            level = cls.QUIET
        elif cfg.get('verbose', False):
            level = cls.VERBOSE
        else:
            level = cls.NORMAL
            
        return cls(level)
    
    def start_processing(self, file_count: int, worker_count: int, optimization_msg: str = None):
        """
        Print processing start message based on verbosity.
        
        Args:
            file_count: Number of files to process
            worker_count: Number of workers
            optimization_msg: Optional optimization message
        """
        self._start_time = time.time()
        self._file_count = file_count
        self._completed = 0
        self._failed = 0
        self._failed_files = []
        
        if self.level == self.QUIET:
            # Minimal output
            print(f"Processing {file_count} files with {worker_count} workers...")
        elif self.level == self.NORMAL:
            # Balanced output
            if optimization_msg:
                print(f"[OPP Auto-Optimization] {optimization_msg}")
            print(f"Processing {file_count} files in parallel with {worker_count} workers")
        else:  # VERBOSE
            # Detailed output
            if optimization_msg:
                print(f"[OPP Auto-Optimization] {optimization_msg}")
            print("=" * 80)
            print(f"PARALLEL PROCESSING - STARTING")
            print(f"Total files: {file_count}")
            print(f"Parallel workers: {worker_count}")
            print("=" * 80)
    
    def file_started(self, filename: str):
        """
        Print file start message if verbose.
        
        Args:
            filename: Name of file being processed
        """
        if self.level == self.VERBOSE:
            print(f"Post-processing file: {filename}")
    
    def file_completed(self, filename: str, error: Optional[str] = None):
        """
        Handle file completion.
        
        Args:
            filename: Name of completed file
            error: Error message if failed
        """
        if error:
            self._failed += 1
            self._failed_files.append((filename, error))
            if self.level == self.VERBOSE:
                print(f"Failed: {filename} - {error}")
        else:
            self._completed += 1
            if self.level == self.VERBOSE:
                print(f"Completed: {filename}")
        
        # Update progress for QUIET mode
        if self.level == self.QUIET:
            self._print_progress_line()
    
    def _print_progress_line(self):
        """Print single-line progress for quiet mode."""
        total = self._completed + self._failed
        percent = (total / self._file_count * 100) if self._file_count > 0 else 0
        bar_length = 40
        filled = int(bar_length * total / self._file_count) if self._file_count > 0 else 0
        bar = '=' * filled + '-' * (bar_length - filled)
        
        # Use carriage return to update same line
        print(f"\r[{bar}] {percent:.0f}% ({total}/{self._file_count}) ", end='', flush=True)
    
    def finish_processing(self):
        """Print final summary based on verbosity."""
        elapsed = time.time() - self._start_time if self._start_time else 0
        elapsed_str = self._format_time(elapsed)
        
        if self.level == self.QUIET:
            # Complete the progress line
            print()  # New line after progress
            print(f"Completed: {self._completed} | Failed: {self._failed} | Time: {elapsed_str}")
            
            # Show failed files even in quiet mode
            if self._failed_files:
                print("\nFailed files:")
                for filename, error in self._failed_files[:5]:  # Show first 5
                    print(f"  - {filename}: {error[:50]}...")
                if len(self._failed_files) > 5:
                    print(f"  ... and {len(self._failed_files) - 5} more")
                    
        elif self.level == self.NORMAL:
            print("\n" + "=" * 60)
            print("Summary:")
            print(f"  Completed: {self._completed} files")
            print(f"  Failed: {self._failed} files")
            print(f"  Total time: {elapsed_str}")
            if self._file_count > 0:
                avg_time = elapsed / self._file_count
                print(f"  Avg per file: {avg_time:.2f}s")
            
            if self._failed_files:
                print("\nFailed files:")
                for filename, error in self._failed_files[:10]:
                    print(f"  - {filename}: {error[:80]}...")
                if len(self._failed_files) > 10:
                    print(f"  ... and {len(self._failed_files) - 10} more (see error log)")
                    
        else:  # VERBOSE
            print("\n" + "=" * 80)
            print("PROCESSING COMPLETE")
            print("=" * 80)
            print(f"Files Processed:    {self._file_count}")
            print(f"Successful:         {self._completed} ({self._completed/self._file_count*100:.1f}%)")
            print(f"Failed:             {self._failed} ({self._failed/self._file_count*100:.1f}%)")
            print(f"Total Time:         {elapsed_str}")
            if self._file_count > 0:
                print(f"Average Time:       {elapsed/self._file_count:.2f}s per file")
            print("=" * 80)
            
            if self._failed_files:
                print("\nFailed Files Details:")
                for filename, error in self._failed_files:
                    print(f"  - {filename}:")
                    print(f"    {error}")
    
    def _format_time(self, seconds: float) -> str:
        """Format seconds into human-readable time."""
        if seconds < 60:
            return f"{seconds:.1f}s"
        elif seconds < 3600:
            minutes = seconds / 60
            secs = seconds % 60
            return f"{int(minutes)}m {int(secs)}s"
        else:
            hours = seconds / 3600
            mins = (seconds % 3600) / 60
            return f"{int(hours)}h {int(mins)}m"
    
    @contextmanager
    def progress_context(self, file_count: int, worker_count: int, optimization_msg: str = None):
        """
        Context manager for processing with automatic start/finish.
        
        Usage:
            with output_controller.progress_context(len(files), workers) as ctx:
                # Process files
                for file in files:
                    ctx.file_started(file)
                    # ... process ...
                    ctx.file_completed(file, error)
        """
        self.start_processing(file_count, worker_count, optimization_msg)
        try:
            yield self
        finally:
            self.finish_processing()


def add_output_arguments(parser):
    """
    Add output control arguments to argument parser.
    
    Args:
        parser: ArgumentParser instance
    """
    output_group = parser.add_mutually_exclusive_group()
    output_group.add_argument(
        '--quiet', '-q',
        action='store_true',
        help='Minimal output - show only summary'
    )
    output_group.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Detailed output - show all messages'
    )


def get_output_level_from_argv() -> int:
    """
    Determine output level from command-line arguments.
    
    Returns:
        Output level (QUIET, NORMAL, or VERBOSE)
    """
    if '--quiet' in sys.argv or '-q' in sys.argv:
        return OutputController.QUIET
    elif '--verbose' in sys.argv or '-v' in sys.argv:
        return OutputController.VERBOSE
    else:
        return OutputController.NORMAL


# Example usage
if __name__ == "__main__":
    # Simulate different verbosity levels
    import random
    
    print("DEMO: Quiet Mode")
    print("-" * 40)
    controller = OutputController(OutputController.QUIET)
    with controller.progress_context(10, 4) as ctx:
        for i in range(10):
            filename = f"test_{i:03d}.sim"
            ctx.file_started(filename)
            time.sleep(0.1)  # Simulate processing
            error = "Random error" if random.random() < 0.2 else None
            ctx.file_completed(filename, error)
    
    print("\n\nDEMO: Normal Mode")
    print("-" * 40)
    controller = OutputController(OutputController.NORMAL)
    with controller.progress_context(10, 4, "Files: 10, Optimal threads: 4") as ctx:
        for i in range(10):
            filename = f"test_{i:03d}.sim"
            ctx.file_started(filename)
            time.sleep(0.1)
            error = "Random error" if random.random() < 0.2 else None
            ctx.file_completed(filename, error)
    
    print("\n\nDEMO: Verbose Mode")
    print("-" * 40)
    controller = OutputController(OutputController.VERBOSE)
    with controller.progress_context(5, 2) as ctx:
        for i in range(5):
            filename = f"test_{i:03d}.sim"
            ctx.file_started(filename)
            time.sleep(0.1)
            error = "Random error" if random.random() < 0.2 else None
            ctx.file_completed(filename, error)