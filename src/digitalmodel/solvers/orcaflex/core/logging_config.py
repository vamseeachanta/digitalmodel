"""
Logging Framework Configuration for OrcaFlex Module

This module provides centralized logging configuration and utilities for the OrcaFlex system.
It includes structured logging, performance tracking, and audit trails.
"""

import logging
import logging.handlers
import json
import sys
from pathlib import Path
from datetime import datetime
from typing import Any, Dict, Optional, Union
from enum import Enum


class LogLevel(Enum):
    """Enumeration of log levels."""
    DEBUG = logging.DEBUG
    INFO = logging.INFO
    WARNING = logging.WARNING
    ERROR = logging.ERROR
    CRITICAL = logging.CRITICAL


class StructuredFormatter(logging.Formatter):
    """
    Custom formatter that outputs structured JSON logs.
    
    This formatter creates machine-readable logs that can be easily parsed
    and analyzed by log aggregation systems.
    """
    
    def format(self, record: logging.LogRecord) -> str:
        """Format log record as JSON."""
        log_obj = {
            'timestamp': datetime.utcnow().isoformat(),
            'level': record.levelname,
            'logger': record.name,
            'message': record.getMessage(),
            'module': record.module,
            'function': record.funcName,
            'line': record.lineno
        }
        
        # Add exception information if present
        if record.exc_info:
            log_obj['exception'] = self.formatException(record.exc_info)
        
        # Add extra fields if present
        if hasattr(record, 'extra_fields'):
            log_obj.update(record.extra_fields)
        
        return json.dumps(log_obj)


class HumanReadableFormatter(logging.Formatter):
    """
    Custom formatter for human-readable console output.
    
    This formatter uses color coding and clear formatting for console output.
    """
    
    # ANSI color codes
    COLORS = {
        'DEBUG': '\033[36m',     # Cyan
        'INFO': '\033[32m',      # Green
        'WARNING': '\033[33m',   # Yellow
        'ERROR': '\033[31m',     # Red
        'CRITICAL': '\033[35m',  # Magenta
        'RESET': '\033[0m'       # Reset
    }
    
    def __init__(self, use_colors: bool = True):
        """Initialize formatter."""
        super().__init__()
        self.use_colors = use_colors and sys.stdout.isatty()
    
    def format(self, record: logging.LogRecord) -> str:
        """Format log record with colors."""
        # Base format
        fmt = f"[%(asctime)s] %(levelname)-8s | %(name)-20s | %(message)s"
        
        if self.use_colors:
            levelname = record.levelname
            if levelname in self.COLORS:
                fmt = fmt.replace(
                    '%(levelname)-8s',
                    f"{self.COLORS[levelname]}%(levelname)-8s{self.COLORS['RESET']}"
                )
        
        # Add location information for errors
        if record.levelname in ['ERROR', 'CRITICAL']:
            fmt += f" | {record.pathname}:{record.lineno}"
        
        formatter = logging.Formatter(fmt, datefmt='%Y-%m-%d %H:%M:%S')
        return formatter.format(record)


class OrcaFlexLogger:
    """
    Centralized logger configuration for OrcaFlex module.
    
    This class provides:
    - Structured logging setup
    - Multiple output handlers
    - Performance tracking
    - Audit trails
    """
    
    _initialized = False
    _loggers = {}
    
    @classmethod
    def setup(cls,
              log_level: Union[str, LogLevel] = LogLevel.INFO,
              log_dir: Optional[Path] = None,
              enable_console: bool = True,
              enable_file: bool = True,
              enable_structured: bool = False,
              max_bytes: int = 10485760,  # 10MB
              backup_count: int = 5) -> None:
        """
        Set up the logging system.
        
        Args:
            log_level: Logging level
            log_dir: Directory for log files
            enable_console: Enable console output
            enable_file: Enable file output
            enable_structured: Use structured JSON logging
            max_bytes: Maximum size of log file before rotation
            backup_count: Number of backup files to keep
        """
        if cls._initialized:
            return
        
        # Convert string log level to enum
        if isinstance(log_level, str):
            log_level = LogLevel[log_level.upper()]
        
        # Set up root logger for OrcaFlex module
        root_logger = logging.getLogger('digitalmodel.orcaflex')
        root_logger.setLevel(log_level.value)
        root_logger.handlers.clear()
        
        # Console handler
        if enable_console:
            console_handler = logging.StreamHandler(sys.stdout)
            console_handler.setLevel(log_level.value)
            
            if enable_structured:
                console_handler.setFormatter(StructuredFormatter())
            else:
                console_handler.setFormatter(HumanReadableFormatter())
            
            root_logger.addHandler(console_handler)
        
        # File handler
        if enable_file:
            if log_dir is None:
                log_dir = Path.cwd() / 'logs' / 'orcaflex'
            
            log_dir = Path(log_dir)
            log_dir.mkdir(parents=True, exist_ok=True)
            
            # Main log file
            log_file = log_dir / f"orcaflex_{datetime.now().strftime('%Y%m%d')}.log"
            file_handler = logging.handlers.RotatingFileHandler(
                log_file,
                maxBytes=max_bytes,
                backupCount=backup_count
            )
            file_handler.setLevel(log_level.value)
            
            if enable_structured:
                file_handler.setFormatter(StructuredFormatter())
            else:
                file_handler.setFormatter(
                    logging.Formatter(
                        '[%(asctime)s] %(levelname)-8s | %(name)s | %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S'
                    )
                )
            
            root_logger.addHandler(file_handler)
            
            # Error log file
            error_file = log_dir / f"orcaflex_errors_{datetime.now().strftime('%Y%m%d')}.log"
            error_handler = logging.handlers.RotatingFileHandler(
                error_file,
                maxBytes=max_bytes,
                backupCount=backup_count
            )
            error_handler.setLevel(logging.ERROR)
            error_handler.setFormatter(
                logging.Formatter(
                    '[%(asctime)s] %(levelname)s | %(name)s | %(message)s | %(pathname)s:%(lineno)d',
                    datefmt='%Y-%m-%d %H:%M:%S'
                )
            )
            root_logger.addHandler(error_handler)
        
        cls._initialized = True
        root_logger.info("OrcaFlex logging system initialized")
    
    @classmethod
    def get_logger(cls, name: str) -> logging.Logger:
        """
        Get a logger instance.
        
        Args:
            name: Logger name
            
        Returns:
            Logger instance
        """
        if not cls._initialized:
            cls.setup()
        
        if name not in cls._loggers:
            # Ensure it's under the OrcaFlex namespace
            if not name.startswith('digitalmodel.orcaflex'):
                name = f'digitalmodel.orcaflex.{name}'
            
            cls._loggers[name] = logging.getLogger(name)
        
        return cls._loggers[name]
    
    @classmethod
    def log_performance(cls,
                       operation: str,
                       duration: float,
                       metadata: Optional[Dict[str, Any]] = None) -> None:
        """
        Log performance metrics.
        
        Args:
            operation: Operation name
            duration: Duration in seconds
            metadata: Additional metadata
        """
        logger = cls.get_logger('performance')
        
        perf_data = {
            'operation': operation,
            'duration_seconds': duration,
            'timestamp': datetime.utcnow().isoformat()
        }
        
        if metadata:
            perf_data.update(metadata)
        
        logger.info(f"Performance: {operation} took {duration:.3f}s", 
                   extra={'extra_fields': perf_data})
    
    @classmethod
    def log_audit(cls,
                 action: str,
                 user: Optional[str] = None,
                 details: Optional[Dict[str, Any]] = None) -> None:
        """
        Log audit trail.
        
        Args:
            action: Action performed
            user: User performing action
            details: Additional details
        """
        logger = cls.get_logger('audit')
        
        audit_data = {
            'action': action,
            'user': user or 'system',
            'timestamp': datetime.utcnow().isoformat()
        }
        
        if details:
            audit_data.update(details)
        
        logger.info(f"Audit: {action} by {user or 'system'}", 
                   extra={'extra_fields': audit_data})


class LoggerMixin:
    """
    Mixin class to provide logging capabilities to any class.
    
    Usage:
        class MyClass(LoggerMixin):
            def __init__(self):
                super().__init__()
                self.logger.info("MyClass initialized")
    """
    
    @property
    def logger(self) -> logging.Logger:
        """Get logger for this class."""
        if not hasattr(self, '_logger'):
            self._logger = OrcaFlexLogger.get_logger(
                f"{self.__class__.__module__}.{self.__class__.__name__}"
            )
        return self._logger
    
    def log_debug(self, message: str, **kwargs) -> None:
        """Log debug message."""
        self.logger.debug(message, extra={'extra_fields': kwargs} if kwargs else None)
    
    def log_info(self, message: str, **kwargs) -> None:
        """Log info message."""
        self.logger.info(message, extra={'extra_fields': kwargs} if kwargs else None)
    
    def log_warning(self, message: str, **kwargs) -> None:
        """Log warning message."""
        self.logger.warning(message, extra={'extra_fields': kwargs} if kwargs else None)
    
    def log_error(self, message: str, exc_info: bool = False, **kwargs) -> None:
        """Log error message."""
        self.logger.error(message, exc_info=exc_info, 
                         extra={'extra_fields': kwargs} if kwargs else None)
    
    def log_critical(self, message: str, exc_info: bool = True, **kwargs) -> None:
        """Log critical message."""
        self.logger.critical(message, exc_info=exc_info, 
                           extra={'extra_fields': kwargs} if kwargs else None)


# Initialize logging on module import with defaults
OrcaFlexLogger.setup()