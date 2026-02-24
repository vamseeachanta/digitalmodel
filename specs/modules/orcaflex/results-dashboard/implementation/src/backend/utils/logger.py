"""
Logging configuration for the application
"""

import logging
import sys
from pathlib import Path
from typing import Optional

from utils.config import settings


def setup_logger(
    name: str,
    level: Optional[int] = None,
    log_file: Optional[Path] = None,
) -> logging.Logger:
    """
    Set up a logger with consistent formatting
    
    Args:
        name: Logger name
        level: Logging level (defaults to DEBUG in development, INFO in production)
        log_file: Optional log file path
    
    Returns:
        Configured logger instance
    """
    logger = logging.getLogger(name)
    
    # Set level based on environment
    if level is None:
        level = logging.DEBUG if settings.debug else logging.INFO
    logger.setLevel(level)
    
    # Remove existing handlers
    logger.handlers.clear()
    
    # Create formatter
    formatter = logging.Formatter(
        fmt="%(asctime)s | %(levelname)-8s | %(name)s | %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )
    
    # Console handler
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)
    
    # File handler (if specified)
    if log_file:
        log_file.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(log_file)
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)
    
    return logger


# Create default application logger
app_logger = setup_logger("orcaflex_dashboard")