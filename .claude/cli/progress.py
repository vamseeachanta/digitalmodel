"""Progress indicators for Agent OS CLI.

Provides visual feedback during long-running operations.
"""

import sys
import time
import threading
from typing import Optional


class ProgressIndicator:
    """Progress indicator for CLI operations."""
    
    def __init__(self):
        """Initialize progress indicator."""
        self.running = False
        self.thread: Optional[threading.Thread] = None
        self.message = ""
        self.spinner_chars = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
        self.current_char = 0
        
    def start(self, message: str = "Working") -> None:
        """Start progress indicator.
        
        Args:
            message: Message to display
        """
        if self.running:
            return
        
        self.message = message
        self.running = True
        self.thread = threading.Thread(target=self._animate, daemon=True)
        self.thread.start()
    
    def stop(self) -> None:
        """Stop progress indicator."""
        if not self.running:
            return
        
        self.running = False
        if self.thread and self.thread.is_alive():
            self.thread.join(timeout=1.0)
        
        # Clear the progress line
        sys.stdout.write('\r' + ' ' * (len(self.message) + 10) + '\r')
        sys.stdout.flush()
    
    def _animate(self) -> None:
        """Animate the progress indicator."""
        while self.running:
            char = self.spinner_chars[self.current_char]
            sys.stdout.write(f'\r{char} {self.message}...')
            sys.stdout.flush()
            
            self.current_char = (self.current_char + 1) % len(self.spinner_chars)
            time.sleep(0.1)
    
    def update_message(self, message: str) -> None:
        """Update progress message.
        
        Args:
            message: New message to display
        """
        self.message = message


class ProgressBar:
    """Progress bar for operations with known duration."""
    
    def __init__(self, total: int, width: int = 50):
        """Initialize progress bar.
        
        Args:
            total: Total number of steps
            width: Width of progress bar in characters
        """
        self.total = total
        self.width = width
        self.current = 0
        self.message = ""
        
    def update(self, current: int, message: str = "") -> None:
        """Update progress bar.
        
        Args:
            current: Current step number
            message: Optional message to display
        """
        self.current = current
        self.message = message
        self._draw()
    
    def increment(self, message: str = "") -> None:
        """Increment progress by one step.
        
        Args:
            message: Optional message to display
        """
        self.current += 1
        self.message = message
        self._draw()
    
    def _draw(self) -> None:
        """Draw the progress bar."""
        # Calculate percentage
        percentage = (self.current / self.total) * 100
        
        # Calculate filled portion
        filled = int((self.current / self.total) * self.width)
        empty = self.width - filled
        
        # Create progress bar
        bar = "█" * filled + "░" * empty
        
        # Format output
        output = f'\r[{bar}] {percentage:5.1f}% ({self.current}/{self.total})'
        if self.message:
            output += f' - {self.message}'
        
        sys.stdout.write(output)
        sys.stdout.flush()
        
        # Add newline when complete
        if self.current >= self.total:
            sys.stdout.write('\n')
            sys.stdout.flush()


class TaskProgress:
    """Progress indicator for multi-step tasks."""
    
    def __init__(self, tasks: list):
        """Initialize task progress.
        
        Args:
            tasks: List of task names
        """
        self.tasks = tasks
        self.current_task = 0
        self.completed_tasks = []
        self.failed_tasks = []
        
    def start_task(self, task_index: int) -> None:
        """Start a specific task.
        
        Args:
            task_index: Index of task to start
        """
        if 0 <= task_index < len(self.tasks):
            self.current_task = task_index
            task_name = self.tasks[task_index]
            print(f"🔄 Starting: {task_name}")
    
    def complete_task(self, task_index: int, success: bool = True) -> None:
        """Mark a task as completed.
        
        Args:
            task_index: Index of completed task
            success: Whether task completed successfully
        """
        if 0 <= task_index < len(self.tasks):
            task_name = self.tasks[task_index]
            
            if success:
                self.completed_tasks.append(task_index)
                print(f"✅ Completed: {task_name}")
            else:
                self.failed_tasks.append(task_index)
                print(f"❌ Failed: {task_name}")
    
    def show_summary(self) -> None:
        """Show task completion summary."""
        total = len(self.tasks)
        completed = len(self.completed_tasks)
        failed = len(self.failed_tasks)
        
        print(f"\n📊 Task Summary:")
        print(f"   Total tasks: {total}")
        print(f"   ✅ Completed: {completed}")
        print(f"   ❌ Failed: {failed}")
        print(f"   ⏳ Pending: {total - completed - failed}")
        
        if failed > 0:
            print(f"\n❌ Failed tasks:")
            for task_index in self.failed_tasks:
                print(f"   - {self.tasks[task_index]}")


def with_progress(message: str = "Working"):
    """Decorator to show progress during function execution.
    
    Args:
        message: Progress message to display
    """
    def decorator(func):
        def wrapper(*args, **kwargs):
            progress = ProgressIndicator()
            progress.start(message)
            try:
                result = func(*args, **kwargs)
                progress.stop()
                return result
            except Exception:
                progress.stop()
                raise
        return wrapper
    return decorator