"""
Logging and error handling configuration for FreeCAD Agent
"""

import os
import sys
import traceback
from pathlib import Path
from typing import Any, Optional
from datetime import datetime
from loguru import logger


class FreeCADAgentError(Exception):
    """Base exception for FreeCAD Agent errors"""
    pass


class APIError(FreeCADAgentError):
    """FreeCAD API related errors"""
    pass


class ValidationError(FreeCADAgentError):
    """Input validation errors"""
    pass


class ProcessingError(FreeCADAgentError):
    """Processing and execution errors"""
    pass


class ConfigurationError(FreeCADAgentError):
    """Configuration related errors"""
    pass


class ErrorHandler:
    """Centralized error handling with recovery mechanisms"""
    
    def __init__(self, config: dict):
        self.config = config
        self.retry_attempts = config.get('settings', {}).get('retry_attempts', 3)
        self.retry_delay = config.get('settings', {}).get('retry_delay', 1.0)
        self.error_recovery = config.get('settings', {}).get('error_recovery', True)
        
    def handle_error(self, error: Exception, context: Optional[dict] = None) -> Optional[Any]:
        """
        Handle errors with appropriate recovery strategies
        
        Args:
            error: The exception that occurred
            context: Additional context about the error
            
        Returns:
            Recovery result if possible, None otherwise
        """
        error_type = type(error).__name__
        error_msg = str(error)
        
        logger.error(f"{error_type}: {error_msg}")
        if context:
            logger.error(f"Context: {context}")
        
        if self.error_recovery:
            return self._attempt_recovery(error, context)
        
        return None
    
    def _attempt_recovery(self, error: Exception, context: Optional[dict] = None) -> Optional[Any]:
        """Attempt to recover from known error types"""
        
        if isinstance(error, ValidationError):
            logger.info("Attempting to fix validation error...")
            return self._recover_from_validation(error, context)
        
        elif isinstance(error, APIError):
            logger.info("Attempting to recover from API error...")
            return self._recover_from_api_error(error, context)
        
        elif isinstance(error, ProcessingError):
            logger.info("Attempting to recover from processing error...")
            return self._recover_from_processing(error, context)
        
        return None
    
    def _recover_from_validation(self, error: Exception, context: Optional[dict]) -> Optional[Any]:
        """Recover from validation errors by fixing common issues"""
        if context and 'input' in context:
            # Attempt to fix common validation issues
            input_data = context['input']
            logger.debug(f"Attempting to fix input: {input_data}")
            # Add specific validation recovery logic here
        return None
    
    def _recover_from_api_error(self, error: Exception, context: Optional[dict]) -> Optional[Any]:
        """Recover from API errors with retry logic"""
        if context and 'operation' in context:
            logger.debug(f"Retrying operation: {context['operation']}")
            # Add retry logic here
        return None
    
    def _recover_from_processing(self, error: Exception, context: Optional[dict]) -> Optional[Any]:
        """Recover from processing errors"""
        if context and 'fallback' in context:
            logger.debug(f"Using fallback: {context['fallback']}")
            return context['fallback']
        return None


def setup_logging(config: dict) -> None:
    """
    Configure logging for the FreeCAD Agent
    
    Args:
        config: Agent configuration dictionary
    """
    log_level = config.get('settings', {}).get('log_level', 'INFO')
    log_file = config.get('settings', {}).get('log_file', 'logs/freecad_agent.log')
    
    # Ensure log directory exists
    log_dir = Path(log_file).parent
    log_dir.mkdir(parents=True, exist_ok=True)
    
    # Remove default logger
    logger.remove()
    
    # Add console handler with color
    logger.add(
        sys.stdout,
        format="<green>{time:YYYY-MM-DD HH:mm:ss}</green> | <level>{level: <8}</level> | <cyan>{name}</cyan>:<cyan>{function}</cyan>:<cyan>{line}</cyan> - <level>{message}</level>",
        level=log_level,
        colorize=True
    )
    
    # Add file handler with rotation
    logger.add(
        log_file,
        format="{time:YYYY-MM-DD HH:mm:ss} | {level: <8} | {name}:{function}:{line} - {message}",
        level=log_level,
        rotation="10 MB",
        retention="7 days",
        compression="zip"
    )
    
    # Add error file handler
    error_log = log_file.replace('.log', '_errors.log')
    logger.add(
        error_log,
        format="{time:YYYY-MM-DD HH:mm:ss} | {level: <8} | {name}:{function}:{line} - {message}\n{extra}",
        level="ERROR",
        rotation="5 MB",
        retention="30 days",
        compression="zip",
        backtrace=True,
        diagnose=True
    )
    
    logger.info(f"Logging configured - Level: {log_level}, File: {log_file}")


def log_performance(operation: str, duration: float, details: Optional[dict] = None) -> None:
    """
    Log performance metrics
    
    Args:
        operation: Name of the operation
        duration: Duration in seconds
        details: Additional details about the operation
    """
    metrics = {
        "operation": operation,
        "duration_seconds": duration,
        "timestamp": datetime.now().isoformat()
    }
    
    if details:
        metrics.update(details)
    
    logger.info(f"Performance: {operation} took {duration:.2f}s", extra=metrics)


def log_batch_progress(current: int, total: int, operation: str, file: Optional[str] = None) -> None:
    """
    Log batch processing progress
    
    Args:
        current: Current item number
        total: Total number of items
        operation: Current operation
        file: Current file being processed
    """
    percentage = (current / total) * 100 if total > 0 else 0
    
    progress_msg = f"Progress: {current}/{total} ({percentage:.1f}%) - {operation}"
    if file:
        progress_msg += f" - File: {file}"
    
    logger.info(progress_msg)