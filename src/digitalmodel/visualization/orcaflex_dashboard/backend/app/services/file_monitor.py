"""
File System Monitor

Monitors OrcaFlex CSV output directory for new files and changes.
Provides real-time file system events with <1s detection latency.
"""

import asyncio
import logging
import time
from typing import Dict, List, Optional, Callable, Set
from pathlib import Path
from dataclasses import dataclass
from enum import Enum
from datetime import datetime, timedelta
import threading
from concurrent.futures import ThreadPoolExecutor
import hashlib
import os
import fnmatch

try:
    from watchdog.observers import Observer
    from watchdog.events import FileSystemEventHandler, FileCreatedEvent, FileModifiedEvent, FileDeletedEvent
    WATCHDOG_AVAILABLE = True
except ImportError:
    WATCHDOG_AVAILABLE = False
    Observer = None
    FileSystemEventHandler = object
    FileCreatedEvent = FileModifiedEvent = FileDeletedEvent = None

logger = logging.getLogger(__name__)


class EventType(Enum):
    """File system event types"""
    CREATED = "created"
    MODIFIED = "modified"
    DELETED = "deleted"
    ERROR = "error"


class FileStatus(Enum):
    """File processing status"""
    NEW = "new"
    PROCESSING = "processing"
    PROCESSED = "processed"
    ERROR = "error"
    IGNORED = "ignored"


@dataclass
class FileEvent:
    """File system event information"""
    event_type: EventType
    file_path: Path
    timestamp: datetime
    file_size: Optional[int] = None
    checksum: Optional[str] = None
    metadata: Dict = None
    
    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}


@dataclass
class MonitoredFile:
    """Information about a monitored file"""
    path: Path
    status: FileStatus
    first_seen: datetime
    last_modified: datetime
    size: Optional[int] = None
    checksum: Optional[str] = None
    processing_attempts: int = 0
    error_message: Optional[str] = None


class FileMonitorConfig:
    """Configuration for file monitoring"""
    
    def __init__(self,
                 watch_patterns: Optional[List[str]] = None,
                 ignore_patterns: Optional[List[str]] = None,
                 min_file_size: int = 1024,  # 1KB minimum
                 max_file_age_hours: int = 24,
                 stable_time_seconds: float = 2.0,  # Wait for file to stabilize
                 polling_interval_seconds: float = 1.0,
                 enable_checksums: bool = True,
                 max_files_tracked: int = 10000):
        
        # File patterns to watch
        self.watch_patterns = watch_patterns or ['*.csv', '*.txt', '*.dat']
        
        # Patterns to ignore
        self.ignore_patterns = ignore_patterns or [
            '*.tmp', '*.temp', '*~', '.DS_Store', 'Thumbs.db',
            '*.log', '*.bak'
        ]
        
        self.min_file_size = min_file_size
        self.max_file_age_hours = max_file_age_hours
        self.stable_time_seconds = stable_time_seconds
        self.polling_interval_seconds = polling_interval_seconds
        self.enable_checksums = enable_checksums
        self.max_files_tracked = max_files_tracked


class OrcaFlexFileHandler(FileSystemEventHandler):
    """Custom file system event handler for OrcaFlex files"""
    
    def __init__(self, monitor: 'FileMonitor'):
        super().__init__()
        self.monitor = monitor
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
    
    def on_created(self, event):
        """Handle file creation events"""
        if not event.is_directory:
            self.monitor._handle_file_event(
                EventType.CREATED, 
                Path(event.src_path)
            )
    
    def on_modified(self, event):
        """Handle file modification events"""
        if not event.is_directory:
            self.monitor._handle_file_event(
                EventType.MODIFIED, 
                Path(event.src_path)
            )
    
    def on_deleted(self, event):
        """Handle file deletion events"""
        if not event.is_directory:
            self.monitor._handle_file_event(
                EventType.DELETED, 
                Path(event.src_path)
            )


class FileMonitor:
    """
    High-performance file system monitor for OrcaFlex output files.
    
    Features:
    - Real-time file system monitoring with <1s latency
    - File stability checking (waits for writes to complete)
    - Checksum-based change detection
    - Pattern-based filtering
    - Async callback system
    - Fallback polling mode when watchdog unavailable
    - Memory-efficient tracking with automatic cleanup
    """
    
    def __init__(self, 
                 watch_directory: Path,
                 config: Optional[FileMonitorConfig] = None,
                 event_callback: Optional[Callable] = None):
        """
        Initialize file monitor.
        
        Args:
            watch_directory: Directory to monitor
            config: Monitor configuration
            event_callback: Async callback function for file events
        """
        self.watch_directory = Path(watch_directory)
        self.config = config or FileMonitorConfig()
        self.event_callback = event_callback
        
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        
        # Monitoring state
        self.is_running = False
        self.observer = None
        self.monitored_files: Dict[Path, MonitoredFile] = {}
        self.event_queue = asyncio.Queue()
        
        # Performance tracking
        self.stats = {
            'events_processed': 0,
            'files_tracked': 0,
            'errors': 0,
            'start_time': None,
            'last_activity': None
        }
        
        # Thread pool for I/O operations
        self.executor = ThreadPoolExecutor(max_workers=2, thread_name_prefix="FileMonitor")
        
        # Lock for thread safety
        self._lock = threading.Lock()
        
        # Event processing task
        self._event_processor_task = None
        
        self.logger.info(f"Initialized file monitor for {watch_directory}")
        self.logger.info(f"Watch patterns: {self.config.watch_patterns}")
        self.logger.info(f"Ignore patterns: {self.config.ignore_patterns}")
    
    async def start(self) -> bool:
        """
        Start file monitoring.
        
        Returns:
            True if started successfully, False otherwise
        """
        if self.is_running:
            self.logger.warning("File monitor is already running")
            return True
        
        if not self.watch_directory.exists():
            self.logger.error(f"Watch directory does not exist: {self.watch_directory}")
            return False
        
        self.logger.info(f"Starting file monitor for {self.watch_directory}")
        
        try:
            # Start event processor
            self._event_processor_task = asyncio.create_task(self._process_events())
            
            # Initialize file tracking with existing files
            await self._initialize_existing_files()
            
            # Start watchdog observer if available
            if WATCHDOG_AVAILABLE:
                self.observer = Observer()
                event_handler = OrcaFlexFileHandler(self)
                self.observer.schedule(
                    event_handler, 
                    str(self.watch_directory), 
                    recursive=True
                )
                self.observer.start()
                self.logger.info("Started watchdog file system observer")
            else:
                # Start polling mode
                asyncio.create_task(self._polling_monitor())
                self.logger.info("Started polling mode (watchdog not available)")
            
            self.is_running = True
            self.stats['start_time'] = datetime.now()
            
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to start file monitor: {e}")
            await self.stop()
            return False
    
    async def stop(self):
        """Stop file monitoring"""
        if not self.is_running:
            return
        
        self.logger.info("Stopping file monitor")
        self.is_running = False
        
        # Stop watchdog observer
        if self.observer:
            self.observer.stop()
            self.observer.join(timeout=5)
            self.observer = None
        
        # Cancel event processor
        if self._event_processor_task and not self._event_processor_task.done():
            self._event_processor_task.cancel()
            try:
                await self._event_processor_task
            except asyncio.CancelledError:
                pass
        
        # Shutdown thread pool
        self.executor.shutdown(wait=True)
        
        self.logger.info("File monitor stopped")
    
    def _handle_file_event(self, event_type: EventType, file_path: Path):
        """Handle file system events (called from watchdog)"""
        if not self._should_monitor_file(file_path):
            return
        
        try:
            # Create file event
            file_event = FileEvent(
                event_type=event_type,
                file_path=file_path,
                timestamp=datetime.now()
            )
            
            # Add to queue for async processing
            if not self.event_queue.full():
                asyncio.create_task(self._queue_event(file_event))
            else:
                self.logger.warning("Event queue full, dropping event")
                
        except Exception as e:
            self.logger.error(f"Error handling file event: {e}")
    
    async def _queue_event(self, event: FileEvent):
        """Queue event for processing"""
        try:
            await self.event_queue.put(event)
        except Exception as e:
            self.logger.error(f"Error queuing event: {e}")
    
    async def _process_events(self):
        """Process events from the queue"""
        while self.is_running:
            try:
                # Wait for events with timeout
                event = await asyncio.wait_for(
                    self.event_queue.get(), 
                    timeout=1.0
                )
                
                await self._process_single_event(event)
                self.stats['events_processed'] += 1
                self.stats['last_activity'] = datetime.now()
                
            except asyncio.TimeoutError:
                # Periodic cleanup and maintenance
                await self._cleanup_old_files()
                continue
            except asyncio.CancelledError:
                break
            except Exception as e:
                self.logger.error(f"Error processing event: {e}")
                self.stats['errors'] += 1
    
    async def _process_single_event(self, event: FileEvent):
        """Process a single file event"""
        try:
            file_path = event.file_path
            
            if event.event_type == EventType.DELETED:
                # Remove from tracking
                with self._lock:
                    if file_path in self.monitored_files:
                        del self.monitored_files[file_path]
                        self.stats['files_tracked'] = len(self.monitored_files)
                
                # Notify callback
                if self.event_callback:
                    await self.event_callback(event)
                return
            
            # Check if file exists and get info
            if not file_path.exists():
                return
            
            file_stat = file_path.stat()
            file_size = file_stat.st_size
            
            # Skip if too small
            if file_size < self.config.min_file_size:
                return
            
            # Update file tracking
            with self._lock:
                if file_path not in self.monitored_files:
                    # New file
                    monitored_file = MonitoredFile(
                        path=file_path,
                        status=FileStatus.NEW,
                        first_seen=datetime.now(),
                        last_modified=datetime.fromtimestamp(file_stat.st_mtime),
                        size=file_size
                    )
                    self.monitored_files[file_path] = monitored_file
                    self.stats['files_tracked'] = len(self.monitored_files)
                else:
                    # Update existing
                    monitored_file = self.monitored_files[file_path]
                    monitored_file.last_modified = datetime.fromtimestamp(file_stat.st_mtime)
                    monitored_file.size = file_size
            
            # Check if file is stable (not being written to)
            if not await self._is_file_stable(file_path):
                # File is still being written, wait
                self.logger.debug(f"File not stable, waiting: {file_path.name}")
                return
            
            # Calculate checksum if enabled and file changed
            if self.config.enable_checksums:
                checksum = await self._calculate_checksum(file_path)
                if monitored_file.checksum == checksum:
                    # File unchanged, skip
                    return
                monitored_file.checksum = checksum
                event.checksum = checksum
            
            # Update event with file info
            event.file_size = file_size
            
            # Update file status
            with self._lock:
                if monitored_file.status == FileStatus.NEW:
                    monitored_file.status = FileStatus.PROCESSING
            
            # Notify callback
            if self.event_callback:
                try:
                    await self.event_callback(event)
                    # Mark as processed if callback succeeded
                    with self._lock:
                        monitored_file.status = FileStatus.PROCESSED
                except Exception as e:
                    self.logger.error(f"Callback failed for {file_path}: {e}")
                    with self._lock:
                        monitored_file.status = FileStatus.ERROR
                        monitored_file.error_message = str(e)
                        monitored_file.processing_attempts += 1
            
        except Exception as e:
            self.logger.error(f"Error processing event {event.file_path}: {e}")
            self.stats['errors'] += 1
    
    def _should_monitor_file(self, file_path: Path) -> bool:
        """Check if file should be monitored based on patterns"""
        filename = file_path.name
        
        # Check ignore patterns first
        for pattern in self.config.ignore_patterns:
            if fnmatch.fnmatch(filename.lower(), pattern.lower()):
                return False
        
        # Check watch patterns
        for pattern in self.config.watch_patterns:
            if fnmatch.fnmatch(filename.lower(), pattern.lower()):
                return True
        
        return False
    
    async def _is_file_stable(self, file_path: Path) -> bool:
        """Check if file is stable (not being written to)"""
        try:
            # Get current size
            size1 = file_path.stat().st_size
            
            # Wait a bit
            await asyncio.sleep(self.config.stable_time_seconds)
            
            # Check size again
            if not file_path.exists():
                return False
            
            size2 = file_path.stat().st_size
            
            # File is stable if size hasn't changed
            return size1 == size2
            
        except OSError:
            return False
    
    async def _calculate_checksum(self, file_path: Path) -> str:
        """Calculate MD5 checksum of file"""
        def _calc_checksum():
            hash_md5 = hashlib.md5()
            try:
                with open(file_path, "rb") as f:
                    # Read in chunks for memory efficiency
                    for chunk in iter(lambda: f.read(8192), b""):
                        hash_md5.update(chunk)
                return hash_md5.hexdigest()
            except OSError:
                return ""
        
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(self.executor, _calc_checksum)
    
    async def _initialize_existing_files(self):
        """Initialize tracking for existing files in directory"""
        try:
            def _scan_directory():
                existing_files = []
                for file_path in self.watch_directory.rglob("*"):
                    if file_path.is_file() and self._should_monitor_file(file_path):
                        existing_files.append(file_path)
                return existing_files
            
            loop = asyncio.get_event_loop()
            existing_files = await loop.run_in_executor(self.executor, _scan_directory)
            
            self.logger.info(f"Found {len(existing_files)} existing files")
            
            # Add to tracking
            for file_path in existing_files:
                try:
                    file_stat = file_path.stat()
                    file_age = datetime.now() - datetime.fromtimestamp(file_stat.st_mtime)
                    
                    # Skip very old files
                    if file_age.total_seconds() > self.config.max_file_age_hours * 3600:
                        continue
                    
                    monitored_file = MonitoredFile(
                        path=file_path,
                        status=FileStatus.NEW,
                        first_seen=datetime.now(),
                        last_modified=datetime.fromtimestamp(file_stat.st_mtime),
                        size=file_stat.st_size
                    )
                    
                    with self._lock:
                        self.monitored_files[file_path] = monitored_file
                
                except OSError as e:
                    self.logger.debug(f"Could not stat file {file_path}: {e}")
            
            self.stats['files_tracked'] = len(self.monitored_files)
            self.logger.info(f"Tracking {len(self.monitored_files)} files")
            
        except Exception as e:
            self.logger.error(f"Error initializing existing files: {e}")
    
    async def _polling_monitor(self):
        """Fallback polling monitor when watchdog is unavailable"""
        self.logger.info("Starting polling monitor")
        
        last_scan = {}
        
        while self.is_running:
            try:
                # Scan directory
                current_files = {}
                for file_path in self.watch_directory.rglob("*"):
                    if file_path.is_file() and self._should_monitor_file(file_path):
                        try:
                            stat = file_path.stat()
                            current_files[file_path] = {
                                'mtime': stat.st_mtime,
                                'size': stat.st_size
                            }
                        except OSError:
                            continue
                
                # Compare with last scan
                for file_path, file_info in current_files.items():
                    if file_path not in last_scan:
                        # New file
                        self._handle_file_event(EventType.CREATED, file_path)
                    elif (last_scan[file_path]['mtime'] != file_info['mtime'] or
                          last_scan[file_path]['size'] != file_info['size']):
                        # Modified file
                        self._handle_file_event(EventType.MODIFIED, file_path)
                
                # Check for deleted files
                for file_path in last_scan.keys():
                    if file_path not in current_files:
                        self._handle_file_event(EventType.DELETED, file_path)
                
                last_scan = current_files
                
                await asyncio.sleep(self.config.polling_interval_seconds)
                
            except Exception as e:
                self.logger.error(f"Error in polling monitor: {e}")
                await asyncio.sleep(5)  # Back off on error
    
    async def _cleanup_old_files(self):
        """Clean up old files from tracking"""
        if len(self.monitored_files) < self.config.max_files_tracked:
            return
        
        cutoff_time = datetime.now() - timedelta(hours=self.config.max_file_age_hours)
        
        with self._lock:
            old_files = [
                path for path, file_info in self.monitored_files.items()
                if file_info.first_seen < cutoff_time
            ]
            
            for file_path in old_files:
                del self.monitored_files[file_path]
            
            if old_files:
                self.logger.info(f"Cleaned up {len(old_files)} old file records")
                self.stats['files_tracked'] = len(self.monitored_files)
    
    def get_monitoring_stats(self) -> Dict:
        """Get current monitoring statistics"""
        with self._lock:
            stats = self.stats.copy()
            stats['files_tracked'] = len(self.monitored_files)
            
            if self.stats['start_time']:
                uptime = datetime.now() - self.stats['start_time']
                stats['uptime_seconds'] = uptime.total_seconds()
                
                if stats['events_processed'] > 0:
                    stats['events_per_second'] = stats['events_processed'] / uptime.total_seconds()
            
            # File status breakdown
            status_counts = {}
            for file_info in self.monitored_files.values():
                status = file_info.status.value
                status_counts[status] = status_counts.get(status, 0) + 1
            
            stats['file_status_breakdown'] = status_counts
            
        return stats
    
    def get_monitored_files(self, status_filter: Optional[FileStatus] = None) -> List[MonitoredFile]:
        """Get list of monitored files, optionally filtered by status"""
        with self._lock:
            if status_filter:
                return [f for f in self.monitored_files.values() if f.status == status_filter]
            else:
                return list(self.monitored_files.values())
    
    def force_rescan(self):
        """Force a rescan of the directory"""
        if self.is_running:
            asyncio.create_task(self._initialize_existing_files())
    
    def __del__(self):
        """Cleanup on deletion"""
        if self.is_running:
            asyncio.create_task(self.stop())


# Factory function for easy setup
def create_orcaflex_monitor(watch_directory: str, 
                           event_callback: Optional[Callable] = None,
                           **config_kwargs) -> FileMonitor:
    """
    Create a file monitor configured for OrcaFlex output files.
    
    Args:
        watch_directory: Directory to monitor (e.g., "D:/1522/ctr7/orcaflex/rev_a08/output/csv")
        event_callback: Async callback function for file events
        **config_kwargs: Additional configuration options
        
    Returns:
        Configured FileMonitor instance
    """
    config = FileMonitorConfig(
        watch_patterns=['*.csv', 'dm_*.csv', '*_inputs.csv'],
        ignore_patterns=['*.tmp', '*.temp', '*~', '*.log'],
        **config_kwargs
    )
    
    return FileMonitor(
        watch_directory=Path(watch_directory),
        config=config,
        event_callback=event_callback
    )