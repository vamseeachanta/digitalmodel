# Console I/O Optimization - Implementation Examples

## Immediate Quick Fixes

### 1. Universal Error Suppressor for OrcFxAPI

```python
# digitalmodel/core/orcaflex_helper.py
import sys
import io
import logging
from contextlib import contextmanager
from functools import wraps

@contextmanager
def suppress_orcfx_errors():
    """Context manager to suppress OrcFxAPI console errors"""
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    sys.stdout = io.StringIO()
    sys.stderr = io.StringIO()
    
    # Add filter to root logger
    class OrcFxErrorFilter(logging.Filter):
        def filter(self, record):
            msg = record.getMessage()
            # Suppress known non-critical errors
            suppress_patterns = [
                "Error code: 19",
                "Invalid handle",
                "Function not available",
                "OrcFxAPIError"
            ]
            return not any(pattern in msg for pattern in suppress_patterns)
    
    filter_instance = OrcFxErrorFilter()
    root_logger = logging.getLogger()
    root_logger.addFilter(filter_instance)
    
    try:
        yield
    finally:
        sys.stdout = old_stdout
        sys.stderr = old_stderr
        root_logger.removeFilter(filter_instance)

def safe_load_model(filename):
    """Load OrcaFlex model with error suppression"""
    import OrcFxAPI
    with suppress_orcfx_errors():
        return OrcFxAPI.Model(filename)
```

### 2. Optimized Console Handler

```python
# digitalmodel/core/logging/handlers.py
import time
import threading
from queue import Queue, Empty
from typing import List
import logging

class BatchedConsoleHandler(logging.StreamHandler):
    """Console handler that batches messages for better performance"""
    
    def __init__(self, batch_size=10, flush_interval=0.5):
        super().__init__()
        self.batch_size = batch_size
        self.flush_interval = flush_interval
        self.buffer = []
        self.last_flush = time.time()
        self.lock = threading.Lock()
        
        # Start flush timer thread
        self.timer_thread = threading.Thread(target=self._flush_timer, daemon=True)
        self.timer_thread.start()
    
    def emit(self, record):
        """Add record to buffer instead of immediate output"""
        msg = self.format(record)
        
        with self.lock:
            self.buffer.append(msg)
            
            # Flush if buffer is full
            if len(self.buffer) >= self.batch_size:
                self._flush()
    
    def _flush(self):
        """Flush buffered messages to console"""
        if not self.buffer:
            return
            
        # Join all messages with newline
        output = '\n'.join(self.buffer)
        self.stream.write(output + '\n')
        self.stream.flush()
        
        self.buffer.clear()
        self.last_flush = time.time()
    
    def _flush_timer(self):
        """Timer thread to ensure messages are flushed periodically"""
        while True:
            time.sleep(self.flush_interval)
            with self.lock:
                if self.buffer and (time.time() - self.last_flush) > self.flush_interval:
                    self._flush()

    def close(self):
        """Flush remaining messages on close"""
        with self.lock:
            self._flush()
        super().close()
```

### 3. Parallel Worker Logging Queue

```python
# digitalmodel/core/logging/parallel.py
import multiprocessing as mp
import logging
import queue
import threading
from typing import Optional

class WorkerLogHandler(logging.Handler):
    """Handler that sends log records to a queue for central processing"""
    
    def __init__(self, log_queue: mp.Queue):
        super().__init__()
        self.log_queue = log_queue
    
    def emit(self, record):
        """Send record to queue instead of processing directly"""
        try:
            # Don't send the full record object (not picklable)
            self.log_queue.put({
                'name': record.name,
                'level': record.levelno,
                'msg': record.getMessage(),
                'time': record.created,
                'worker': mp.current_process().name
            })
        except Exception:
            self.handleError(record)

class LogAggregator:
    """Collects logs from worker processes and handles them centrally"""
    
    def __init__(self, console_level=logging.WARNING, file_level=logging.DEBUG):
        self.log_queue = mp.Queue()
        self.console_handler = BatchedConsoleHandler()
        self.console_handler.setLevel(console_level)
        
        # File handler for detailed logs
        self.file_handler = logging.FileHandler('parallel_processing.log')
        self.file_handler.setLevel(file_level)
        
        self.listener_thread = None
        self.running = False
    
    def start(self):
        """Start listening for log messages"""
        self.running = True
        self.listener_thread = threading.Thread(target=self._listen, daemon=True)
        self.listener_thread.start()
    
    def stop(self):
        """Stop listening and flush remaining messages"""
        self.running = False
        self.log_queue.put(None)  # Sentinel to stop listener
        if self.listener_thread:
            self.listener_thread.join(timeout=2)
        
        # Flush handlers
        self.console_handler.close()
        self.file_handler.close()
    
    def _listen(self):
        """Listen for log messages from queue"""
        while self.running:
            try:
                record = self.log_queue.get(timeout=0.1)
                if record is None:  # Sentinel
                    break
                    
                # Recreate log record
                log_record = logging.LogRecord(
                    name=record['name'],
                    level=record['level'],
                    pathname='',
                    lineno=0,
                    msg=record['msg'],
                    args=(),
                    exc_info=None
                )
                log_record.created = record['time']
                log_record.processName = record['worker']
                
                # Send to appropriate handlers
                if log_record.levelno >= self.console_handler.level:
                    self.console_handler.emit(log_record)
                if log_record.levelno >= self.file_handler.level:
                    self.file_handler.emit(log_record)
                    
            except queue.Empty:
                continue
            except Exception as e:
                print(f"Error in log aggregator: {e}")
    
    def get_worker_handler(self):
        """Get a handler for worker processes"""
        return WorkerLogHandler(self.log_queue)
```

### 4. Smart Progress Tracker

```python
# digitalmodel/core/progress.py
import time
from typing import Optional, Dict, Any
from dataclasses import dataclass
import sys

@dataclass
class ProgressStats:
    total: int
    completed: int = 0
    failed: int = 0
    start_time: float = None
    last_update: float = None
    
    def __post_init__(self):
        if self.start_time is None:
            self.start_time = time.time()
        if self.last_update is None:
            self.last_update = time.time()
    
    @property
    def elapsed(self) -> float:
        return time.time() - self.start_time
    
    @property
    def rate(self) -> float:
        if self.elapsed == 0:
            return 0
        return self.completed / self.elapsed
    
    @property
    def eta(self) -> float:
        if self.rate == 0:
            return 0
        remaining = self.total - self.completed
        return remaining / self.rate

class SmartProgressTracker:
    """Intelligent progress tracking with minimal console output"""
    
    def __init__(self, total: int, min_update_interval: float = 1.0,
                 verbose: bool = False):
        self.stats = ProgressStats(total=total)
        self.min_update_interval = min_update_interval
        self.verbose = verbose
        self._last_display = ""
    
    def update(self, success: bool = True, force: bool = False):
        """Update progress and display if appropriate"""
        if success:
            self.stats.completed += 1
        else:
            self.stats.failed += 1
            self.stats.completed += 1
        
        # Check if we should display
        should_display = (
            force or
            self.stats.completed == self.stats.total or
            (time.time() - self.stats.last_update) > self.min_update_interval
        )
        
        if should_display:
            self._display()
            self.stats.last_update = time.time()
    
    def _display(self):
        """Display progress with smart formatting"""
        # Clear previous line
        if self._last_display:
            sys.stdout.write('\r' + ' ' * len(self._last_display) + '\r')
        
        # Format progress
        percent = (self.stats.completed / self.stats.total) * 100
        rate_str = f"{self.stats.rate:.1f} files/s" if self.stats.rate > 0 else ""
        eta_str = f"ETA: {self.stats.eta:.0f}s" if self.stats.eta > 0 else ""
        
        if self.verbose:
            display = (
                f"Progress: {self.stats.completed}/{self.stats.total} "
                f"({percent:.1f}%) | "
                f"Failed: {self.stats.failed} | "
                f"{rate_str} | {eta_str}"
            )
        else:
            # Minimal display
            display = f"[{self.stats.completed}/{self.stats.total}] {percent:.0f}%"
        
        self._last_display = display
        sys.stdout.write(display)
        sys.stdout.flush()
    
    def finish(self):
        """Display final summary"""
        self._display()
        sys.stdout.write('\n')
        
        if self.verbose:
            print(f"\nCompleted: {self.stats.completed}")
            print(f"Failed: {self.stats.failed}")
            print(f"Total time: {self.stats.elapsed:.1f}s")
            print(f"Average rate: {self.stats.rate:.1f} files/s")
```

### 5. Module-Specific Configuration

```python
# digitalmodel/core/logging/config.py
import logging
from typing import Dict, Any, Optional
import yaml
from pathlib import Path

class LoggingConfigurator:
    """Centralized logging configuration management"""
    
    _instance = None
    _config = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance
    
    def __init__(self):
        if self._config is None:
            self.load_default_config()
    
    def load_default_config(self):
        """Load default configuration"""
        self._config = {
            'global': {
                'console_level': 'WARNING',
                'file_level': 'DEBUG',
                'batch_console': True,
                'batch_size': 10,
                'flush_interval': 0.5
            },
            'modules': {
                'orcaflex': {
                    'console_level': 'WARNING',
                    'suppress_errors': [
                        'Error code: 19',
                        'Invalid handle'
                    ]
                },
                'parallel': {
                    'use_queue': True,
                    'aggregate_interval': 1.0
                }
            }
        }
    
    def load_from_file(self, config_path: Path):
        """Load configuration from YAML file"""
        with open(config_path, 'r') as f:
            user_config = yaml.safe_load(f)
        
        # Merge with default config
        self._merge_config(user_config)
    
    def _merge_config(self, user_config: Dict):
        """Merge user config with defaults"""
        from assetutilities.common.update_deep import update_deep_dictionary
        self._config = update_deep_dictionary(self._config, user_config)
    
    def configure_module(self, module_name: str) -> logging.Logger:
        """Configure and return logger for specific module"""
        logger = logging.getLogger(module_name)
        
        # Get module config
        module_config = self._config['modules'].get(module_name, {})
        global_config = self._config['global']
        
        # Clear existing handlers
        logger.handlers.clear()
        
        # Console handler
        console_level = getattr(logging, 
                               module_config.get('console_level', 
                                               global_config['console_level']))
        
        if global_config.get('batch_console'):
            console_handler = BatchedConsoleHandler(
                batch_size=global_config.get('batch_size', 10),
                flush_interval=global_config.get('flush_interval', 0.5)
            )
        else:
            console_handler = logging.StreamHandler()
        
        console_handler.setLevel(console_level)
        console_handler.setFormatter(
            logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )
        logger.addHandler(console_handler)
        
        # File handler
        file_level = getattr(logging,
                            module_config.get('file_level',
                                            global_config['file_level']))
        
        file_handler = logging.FileHandler(f'logs/{module_name}.log')
        file_handler.setLevel(file_level)
        file_handler.setFormatter(
            logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )
        logger.addHandler(file_handler)
        
        # Add error suppression if configured
        if 'suppress_errors' in module_config:
            class ErrorSuppressionFilter(logging.Filter):
                def filter(self, record):
                    msg = record.getMessage()
                    for pattern in module_config['suppress_errors']:
                        if pattern in msg:
                            return False
                    return True
            
            logger.addFilter(ErrorSuppressionFilter())
        
        logger.setLevel(logging.DEBUG)  # Set to lowest to let handlers control
        
        return logger

# Singleton instance
logging_config = LoggingConfigurator()

def get_optimized_logger(name: str) -> logging.Logger:
    """Get an optimized logger for the given module"""
    return logging_config.configure_module(name)
```

## Usage Examples

### Example 1: Using in OrcaFlex Module

```python
# digitalmodel/modules/orcaflex/opp.py
from digitalmodel.core.logging import get_optimized_logger
from digitalmodel.core.orcaflex_helper import safe_load_model
from digitalmodel.core.progress import SmartProgressTracker

logger = get_optimized_logger('orcaflex.opp')

def process_files(file_list):
    """Process files with optimized logging and progress"""
    
    # Create progress tracker
    progress = SmartProgressTracker(
        total=len(file_list),
        min_update_interval=2.0,  # Update every 2 seconds
        verbose=False  # Minimal output
    )
    
    for file_path in file_list:
        try:
            # Load model with error suppression
            model = safe_load_model(file_path)
            
            # Process model
            logger.debug(f"Processing {file_path}")
            # ... processing logic ...
            
            progress.update(success=True)
            
        except Exception as e:
            logger.error(f"Failed to process {file_path}: {e}")
            progress.update(success=False)
    
    progress.finish()
```

### Example 2: Parallel Processing with Optimized Logging

```python
# digitalmodel/modules/parallel/optimized_executor.py
from concurrent.futures import ProcessPoolExecutor
from digitalmodel.core.logging.parallel import LogAggregator
from digitalmodel.core.progress import SmartProgressTracker
import logging

def process_file_worker(args):
    """Worker function with optimized logging"""
    file_path, config, log_handler = args
    
    # Configure worker logging
    logger = logging.getLogger('worker')
    logger.addHandler(log_handler)
    logger.setLevel(logging.DEBUG)
    
    try:
        # Suppress console errors
        from digitalmodel.core.orcaflex_helper import safe_load_model
        model = safe_load_model(file_path)
        
        # Process...
        logger.debug(f"Processing {file_path}")
        result = process_model(model, config)
        
        return {'file': file_path, 'result': result, 'success': True}
        
    except Exception as e:
        logger.error(f"Error processing {file_path}: {e}")
        return {'file': file_path, 'error': str(e), 'success': False}

def process_files_parallel(file_list, config, num_workers=None):
    """Process files in parallel with optimized logging"""
    
    # Set up centralized logging
    log_aggregator = LogAggregator(
        console_level=logging.WARNING,
        file_level=logging.DEBUG
    )
    log_aggregator.start()
    
    # Set up progress tracking
    progress = SmartProgressTracker(
        total=len(file_list),
        verbose=True
    )
    
    # Prepare worker arguments
    worker_handler = log_aggregator.get_worker_handler()
    args_list = [(f, config, worker_handler) for f in file_list]
    
    # Process in parallel
    with ProcessPoolExecutor(max_workers=num_workers) as executor:
        futures = [executor.submit(process_file_worker, args) 
                  for args in args_list]
        
        for future in as_completed(futures):
            result = future.result()
            progress.update(success=result['success'])
    
    # Clean up
    progress.finish()
    log_aggregator.stop()
```

### Example 3: Configuration File

```yaml
# config/logging.yml
logging:
  global:
    console_level: WARNING
    file_level: DEBUG
    batch_console: true
    batch_size: 20
    flush_interval: 1.0
  
  modules:
    orcaflex:
      console_level: ERROR
      file_level: DEBUG
      suppress_errors:
        - "Error code: 19"
        - "Invalid handle"
        - "Function not available for this type of object"
    
    parallel:
      use_queue: true
      aggregate_interval: 2.0
      console_level: WARNING
    
    analysis:
      console_level: INFO
      batch_console: true
      batch_size: 50
```

## Performance Comparison

### Before Optimization
```python
# Old approach - each worker writes directly to console
for file in files:
    print(f"Processing {file}")
    model = OrcFxAPI.Model(file)  # Prints errors to console
    print(f"Loaded {file}")
    # ... process ...
    print(f"Completed {file}")
# Result: 100+ console writes, severe parallel contention
```

### After Optimization
```python
# New approach - batched, filtered, and aggregated
progress = SmartProgressTracker(len(files))
with suppress_orcfx_errors():
    for file in files:
        logger.debug(f"Processing {file}")  # Goes to file, not console
        model = safe_load_model(file)  # Errors suppressed
        # ... process ...
        progress.update()  # Batched updates
# Result: <10 console writes, no parallel contention
```

## Testing the Implementation

```python
# tests/test_console_optimization.py
import time
import io
import sys
from digitalmodel.core.logging import get_optimized_logger
from digitalmodel.core.logging.handlers import BatchedConsoleHandler

def test_batched_console_handler():
    """Test that messages are batched correctly"""
    
    # Capture output
    output = io.StringIO()
    handler = BatchedConsoleHandler(batch_size=5, flush_interval=10)
    handler.stream = output
    
    # Send 4 messages (less than batch size)
    for i in range(4):
        handler.emit(logging.LogRecord(
            name='test', level=logging.INFO, 
            pathname='', lineno=0,
            msg=f"Message {i}", args=(), exc_info=None
        ))
    
    # Should not have flushed yet
    assert output.getvalue() == ""
    
    # Send 5th message to trigger flush
    handler.emit(logging.LogRecord(
        name='test', level=logging.INFO,
        pathname='', lineno=0,
        msg="Message 4", args=(), exc_info=None
    ))
    
    # Should have flushed
    assert "Message 0" in output.getvalue()
    assert "Message 4" in output.getvalue()

def benchmark_console_output():
    """Compare performance with and without optimization"""
    
    files = [f"file_{i}.sim" for i in range(100)]
    
    # Without optimization
    start = time.time()
    for file in files:
        print(f"Processing {file}")
        print(f"Loading {file}")
        print(f"Completed {file}")
    without_opt = time.time() - start
    
    # With optimization
    handler = BatchedConsoleHandler(batch_size=20)
    logger = logging.getLogger('bench')
    logger.addHandler(handler)
    
    start = time.time()
    for file in files:
        logger.info(f"Processing {file}")
        logger.info(f"Loading {file}")
        logger.info(f"Completed {file}")
    with_opt = time.time() - start
    
    print(f"Without optimization: {without_opt:.3f}s")
    print(f"With optimization: {with_opt:.3f}s")
    print(f"Speedup: {without_opt/with_opt:.1f}x")
```

---

These examples provide ready-to-use implementations that can be immediately integrated into the digitalmodel repository to achieve significant performance improvements.