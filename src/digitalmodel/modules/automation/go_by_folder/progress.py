"""
Progress reporting module for Create Go-By Folder Tool
"""

import time
import sys
import threading
from pathlib import Path
from typing import Optional, Dict, Any, Callable
from datetime import datetime, timedelta
from dataclasses import dataclass, field
import logging

logger = logging.getLogger(__name__)


@dataclass
class ProgressMetrics:
    """Track progress metrics."""
    total_files: int = 0
    processed_files: int = 0
    total_size: int = 0
    processed_size: int = 0
    start_time: float = field(default_factory=time.time)
    current_file: str = ""
    current_stage: str = "Initializing"
    errors_count: int = 0
    warnings_count: int = 0
    
    @property
    def percentage(self) -> float:
        """Calculate completion percentage."""
        if self.total_files == 0:
            return 0.0
        return (self.processed_files / self.total_files) * 100
    
    @property
    def elapsed_time(self) -> float:
        """Get elapsed time in seconds."""
        return time.time() - self.start_time
    
    @property
    def eta(self) -> Optional[float]:
        """Estimate time remaining."""
        if self.processed_files == 0 or self.processed_files >= self.total_files:
            return None
        
        rate = self.processed_files / self.elapsed_time
        remaining = self.total_files - self.processed_files
        return remaining / rate if rate > 0 else None
    
    @property
    def processing_rate(self) -> float:
        """Calculate files per second."""
        if self.elapsed_time == 0:
            return 0.0
        return self.processed_files / self.elapsed_time


class ProgressReporter:
    """Handle progress reporting for go-by folder creation."""
    
    def __init__(self, 
                 verbose: bool = True, 
                 show_bar: bool = True,
                 update_interval: float = 0.1):
        """
        Initialize progress reporter.
        
        Args:
            verbose: Show detailed progress
            show_bar: Show progress bar
            update_interval: Update interval in seconds
        """
        self.verbose = verbose
        self.show_bar = show_bar
        self.update_interval = update_interval
        self.metrics = ProgressMetrics()
        self._stop_event = threading.Event()
        self._update_thread = None
        self._last_update = 0
        self._spinner_chars = ['-', '\\', '|', '/']
        self._spinner_index = 0
        self._memory_usage = 0
        self._callbacks = []
    
    def start(self, total_files: int, total_size: int = 0) -> None:
        """
        Start progress tracking.
        
        Args:
            total_files: Total number of files to process
            total_size: Total size in bytes
        """
        self.metrics.total_files = total_files
        self.metrics.total_size = total_size
        self.metrics.start_time = time.time()
        
        if self.verbose:
            print(f"\n>> Starting go-by folder creation")
            print(f"   Total files: {total_files:,}")
            if total_size > 0:
                print(f"   Total size: {self._format_size(total_size)}")
            print("")
        
        if self.show_bar:
            self._start_progress_display()
    
    def update(self, 
              processed_files: Optional[int] = None,
              current_file: Optional[str] = None,
              current_stage: Optional[str] = None,
              increment: bool = False) -> None:
        """
        Update progress.
        
        Args:
            processed_files: Number of processed files
            current_file: Currently processing file
            current_stage: Current processing stage
            increment: If True, increment processed count by 1
        """
        if increment:
            self.metrics.processed_files += 1
        elif processed_files is not None:
            self.metrics.processed_files = processed_files
        
        if current_file is not None:
            self.metrics.current_file = current_file
        
        if current_stage is not None:
            self.metrics.current_stage = current_stage
        
        # Trigger callbacks
        for callback in self._callbacks:
            try:
                callback(self.metrics)
            except Exception as e:
                logger.warning(f"Progress callback error: {e}")
        
        # Log progress at intervals
        if self.verbose and time.time() - self._last_update > 10:
            self._log_progress()
            self._last_update = time.time()
    
    def complete(self, success: bool = True) -> None:
        """
        Mark progress as complete.
        
        Args:
            success: Whether operation completed successfully
        """
        self.metrics.processed_files = self.metrics.total_files
        
        if self.show_bar:
            self._stop_progress_display()
        
        if self.verbose:
            self._print_summary(success)
    
    def error(self, message: str, file: Optional[str] = None) -> None:
        """
        Report an error.
        
        Args:
            message: Error message
            file: File that caused error
        """
        self.metrics.errors_count += 1
        
        if self.verbose:
            error_msg = f"ERROR"
            if file:
                error_msg += f" [{file}]"
            error_msg += f": {message}"
            print(error_msg, file=sys.stderr)
        
        logger.error(f"Error processing {file}: {message}")
    
    def warning(self, message: str, file: Optional[str] = None) -> None:
        """
        Report a warning.
        
        Args:
            message: Warning message
            file: File that caused warning
        """
        self.metrics.warnings_count += 1
        
        if self.verbose:
            warn_msg = f"WARNING"
            if file:
                warn_msg += f" [{file}]"
            warn_msg += f": {message}"
            print(warn_msg)
        
        logger.warning(f"Warning processing {file}: {message}")
    
    def add_callback(self, callback: Callable[[ProgressMetrics], None]) -> None:
        """
        Add a progress callback.
        
        Args:
            callback: Function to call on progress updates
        """
        self._callbacks.append(callback)
    
    def set_memory_usage(self, memory_bytes: int) -> None:
        """
        Update memory usage.
        
        Args:
            memory_bytes: Current memory usage in bytes
        """
        self._memory_usage = memory_bytes
    
    def _start_progress_display(self) -> None:
        """Start the progress display thread."""
        self._stop_event.clear()
        self._update_thread = threading.Thread(target=self._update_display, daemon=True)
        self._update_thread.start()
    
    def _stop_progress_display(self) -> None:
        """Stop the progress display thread."""
        self._stop_event.set()
        if self._update_thread:
            self._update_thread.join(timeout=1)
        
        # Clear the progress line
        print('\r' + ' ' * 100 + '\r', end='')
    
    def _update_display(self) -> None:
        """Update the progress display (runs in thread)."""
        while not self._stop_event.is_set():
            self._render_progress_bar()
            time.sleep(self.update_interval)
            self._spinner_index = (self._spinner_index + 1) % len(self._spinner_chars)
    
    def _render_progress_bar(self) -> None:
        """Render the progress bar."""
        # Calculate progress
        percentage = self.metrics.percentage
        bar_width = 40
        filled = int(bar_width * percentage / 100)
        
        # Create bar
        bar = '=' * filled + '-' * (bar_width - filled)
        
        # Create status line
        spinner = self._spinner_chars[self._spinner_index]
        status = f"\r{spinner} [{bar}] {percentage:.1f}% "
        
        # Add file count
        status += f"({self.metrics.processed_files}/{self.metrics.total_files}) "
        
        # Add current file (truncated)
        if self.metrics.current_file:
            file_display = Path(self.metrics.current_file).name
            if len(file_display) > 30:
                file_display = file_display[:27] + "..."
            status += f"[{file_display}]"
        
        # Add ETA
        eta = self.metrics.eta
        if eta:
            eta_str = str(timedelta(seconds=int(eta)))
            status += f" ETA: {eta_str}"
        
        # Add memory usage if available
        if self._memory_usage > 0:
            status += f" | Mem: {self._format_size(self._memory_usage)}"
        
        # Ensure line doesn't exceed terminal width
        try:
            import shutil
            terminal_width = shutil.get_terminal_size().columns
            if len(status) > terminal_width:
                status = status[:terminal_width - 3] + "..."
        except:
            pass
        
        # Print status
        print(status, end='', flush=True)
    
    def _log_progress(self) -> None:
        """Log progress to logger."""
        logger.info(
            f"Progress: {self.metrics.percentage:.1f}% "
            f"({self.metrics.processed_files}/{self.metrics.total_files} files) "
            f"- Stage: {self.metrics.current_stage}"
        )
    
    def _print_summary(self, success: bool) -> None:
        """Print completion summary."""
        print("\n" + "=" * 60)
        
        if success:
            print(">> Go-by folder creation completed successfully!")
        else:
            print(">> Go-by folder creation completed with issues")
        
        print(f"\nSummary:")
        print(f"   Files processed: {self.metrics.processed_files:,}/{self.metrics.total_files:,}")
        print(f"   Time elapsed: {str(timedelta(seconds=int(self.metrics.elapsed_time)))}")
        print(f"   Processing rate: {self.metrics.processing_rate:.2f} files/sec")
        
        if self.metrics.total_size > 0:
            print(f"   Original size: {self._format_size(self.metrics.total_size)}")
        
        if self.metrics.errors_count > 0:
            print(f"   Errors: {self.metrics.errors_count}")
        
        if self.metrics.warnings_count > 0:
            print(f"   Warnings: {self.metrics.warnings_count}")
        
        if self._memory_usage > 0:
            print(f"   Peak memory: {self._format_size(self._memory_usage)}")
        
        print("=" * 60)
    
    def _format_size(self, size_bytes: int) -> str:
        """Format size in human-readable format."""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size_bytes < 1024.0:
                return f"{size_bytes:.2f}{unit}"
            size_bytes /= 1024.0
        return f"{size_bytes:.2f}TB"


class MemoryMonitor:
    """Monitor memory usage during processing."""
    
    def __init__(self, threshold_mb: int = 1000):
        """
        Initialize memory monitor.
        
        Args:
            threshold_mb: Memory threshold in MB for warnings
        """
        self.threshold_bytes = threshold_mb * 1024 * 1024
        self.peak_usage = 0
        self._monitoring = False
        self._monitor_thread = None
    
    def start(self) -> None:
        """Start memory monitoring."""
        self._monitoring = True
        self._monitor_thread = threading.Thread(target=self._monitor, daemon=True)
        self._monitor_thread.start()
    
    def stop(self) -> None:
        """Stop memory monitoring."""
        self._monitoring = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=1)
    
    def get_current_usage(self) -> int:
        """Get current memory usage in bytes."""
        try:
            import psutil
            process = psutil.Process()
            return process.memory_info().rss
        except ImportError:
            # psutil not available
            return 0
    
    def _monitor(self) -> None:
        """Monitor memory usage (runs in thread)."""
        while self._monitoring:
            current = self.get_current_usage()
            if current > self.peak_usage:
                self.peak_usage = current
            
            if current > self.threshold_bytes:
                logger.warning(
                    f"High memory usage: {current / (1024*1024):.1f}MB "
                    f"(threshold: {self.threshold_bytes / (1024*1024):.1f}MB)"
                )
            
            time.sleep(5)  # Check every 5 seconds


def create_progress_reporter(config: Dict[str, Any]) -> ProgressReporter:
    """
    Create a progress reporter from configuration.
    
    Args:
        config: Configuration dictionary
        
    Returns:
        Configured ProgressReporter instance
    """
    return ProgressReporter(
        verbose=config.get('verbose', True),
        show_bar=config.get('show_progress_bar', True),
        update_interval=config.get('progress_update_interval', 0.1)
    )