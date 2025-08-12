"""
File monitoring service for watching OrcaFlex output files
"""

import asyncio
from pathlib import Path
from typing import AsyncGenerator, Dict, Any

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, FileSystemEvent

from utils.config import settings
from utils.logger import setup_logger

logger = setup_logger(__name__)


class OrcaFlexFileHandler(FileSystemEventHandler):
    """Handler for OrcaFlex file system events"""
    
    def __init__(self, queue: asyncio.Queue):
        self.queue = queue
        self.patterns = settings.watch_patterns
    
    def on_created(self, event: FileSystemEvent):
        """Handle file creation"""
        if not event.is_directory:
            if any(Path(event.src_path).match(pattern) for pattern in self.patterns):
                asyncio.create_task(self.queue.put({
                    "type": "file_created",
                    "path": event.src_path,
                }))
    
    def on_modified(self, event: FileSystemEvent):
        """Handle file modification"""
        if not event.is_directory:
            if any(Path(event.src_path).match(pattern) for pattern in self.patterns):
                asyncio.create_task(self.queue.put({
                    "type": "file_modified",
                    "path": event.src_path,
                }))


class FileMonitor:
    """File monitoring service"""
    
    def __init__(self):
        self.observer = None
        self.queue = asyncio.Queue()
        self.is_running = False
    
    async def start(self):
        """Start file monitoring"""
        if self.is_running:
            return
        
        logger.info(f"Starting file monitor for {settings.watch_directory}")
        
        handler = OrcaFlexFileHandler(self.queue)
        self.observer = Observer()
        self.observer.schedule(
            handler,
            str(settings.watch_directory),
            recursive=True
        )
        
        self.observer.start()
        self.is_running = True
    
    async def stop(self):
        """Stop file monitoring"""
        if self.observer:
            self.observer.stop()
            self.observer.join()
            self.is_running = False
            logger.info("File monitor stopped")
    
    async def get_updates(self) -> AsyncGenerator[Dict[str, Any], None]:
        """Get file update events"""
        while self.is_running:
            try:
                update = await asyncio.wait_for(self.queue.get(), timeout=1.0)
                yield update
            except asyncio.TimeoutError:
                continue