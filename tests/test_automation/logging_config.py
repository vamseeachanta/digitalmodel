"""
Logging configuration for test automation system.
"""

import logging
import logging.config
import os
from pathlib import Path
from datetime import datetime

def setup_logging(log_level: str = "INFO", log_dir: str = "logs") -> logging.Logger:
    """
    Set up comprehensive logging for the test automation system.
    
    Args:
        log_level: Logging level (DEBUG, INFO, WARNING, ERROR)
        log_dir: Directory to store log files
        
    Returns:
        Configured logger instance
    """
    
    # Create logs directory if it doesn't exist
    log_path = Path(log_dir)
    log_path.mkdir(exist_ok=True)
    
    # Generate timestamp for log files
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    # Logging configuration
    logging_config = {
        'version': 1,
        'disable_existing_loggers': False,
        'formatters': {
            'detailed': {
                'format': '%(asctime)s - %(name)s - %(levelname)s - %(module)s:%(lineno)d - %(message)s',
                'datefmt': '%Y-%m-%d %H:%M:%S'
            },
            'simple': {
                'format': '%(levelname)s - %(message)s'
            },
            'json': {
                'format': '{"timestamp": "%(asctime)s", "logger": "%(name)s", "level": "%(levelname)s", "module": "%(module)s", "line": %(lineno)d, "message": "%(message)s"}',
                'datefmt': '%Y-%m-%dT%H:%M:%S'
            }
        },
        'handlers': {
            'console': {
                'class': 'logging.StreamHandler',
                'level': log_level,
                'formatter': 'simple',
                'stream': 'ext://sys.stdout'
            },
            'file_detailed': {
                'class': 'logging.handlers.RotatingFileHandler',
                'level': 'DEBUG',
                'formatter': 'detailed',
                'filename': str(log_path / f'test_automation_detailed_{timestamp}.log'),
                'maxBytes': 10485760,  # 10MB
                'backupCount': 5
            },
            'file_errors': {
                'class': 'logging.handlers.RotatingFileHandler',
                'level': 'ERROR',
                'formatter': 'detailed',
                'filename': str(log_path / f'test_automation_errors_{timestamp}.log'),
                'maxBytes': 5242880,  # 5MB
                'backupCount': 3
            },
            'file_json': {
                'class': 'logging.handlers.RotatingFileHandler',
                'level': 'INFO',
                'formatter': 'json',
                'filename': str(log_path / f'test_automation_structured_{timestamp}.log'),
                'maxBytes': 10485760,  # 10MB
                'backupCount': 5
            }
        },
        'loggers': {
            'test_automation': {
                'level': 'DEBUG',
                'handlers': ['console', 'file_detailed', 'file_errors', 'file_json'],
                'propagate': False
            },
            'test_automation.discovery': {
                'level': 'DEBUG',
                'handlers': ['console', 'file_detailed'],
                'propagate': False
            },
            'test_automation.runner': {
                'level': 'DEBUG',
                'handlers': ['console', 'file_detailed'],
                'propagate': False
            },
            'test_automation.analysis': {
                'level': 'DEBUG',
                'handlers': ['console', 'file_detailed'],
                'propagate': False
            },
            'test_automation.autofix': {
                'level': 'DEBUG',
                'handlers': ['console', 'file_detailed', 'file_errors'],
                'propagate': False
            },
            'test_automation.reporting': {
                'level': 'INFO',
                'handlers': ['console', 'file_detailed'],
                'propagate': False
            }
        },
        'root': {
            'level': 'WARNING',
            'handlers': ['console']
        }
    }
    
    # Apply logging configuration
    logging.config.dictConfig(logging_config)
    
    # Get main logger
    logger = logging.getLogger('test_automation')
    
    # Log initialization
    logger.info(f"Test automation logging initialized - Level: {log_level}")
    logger.info(f"Log directory: {log_path.absolute()}")
    
    return logger

def get_logger(name: str) -> logging.Logger:
    """
    Get a logger instance for a specific component.
    
    Args:
        name: Logger name (e.g., 'test_automation.discovery')
        
    Returns:
        Logger instance
    """
    return logging.getLogger(name)

# Performance logging decorator
def log_performance(func):
    """Decorator to log function execution time."""
    import time
    import functools
    
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        logger = get_logger(f'test_automation.performance.{func.__module__}')
        start_time = time.time()
        
        try:
            result = func(*args, **kwargs)
            execution_time = time.time() - start_time
            logger.info(f"{func.__name__} executed in {execution_time:.3f} seconds")
            return result
        except Exception as e:
            execution_time = time.time() - start_time
            logger.error(f"{func.__name__} failed after {execution_time:.3f} seconds: {str(e)}")
            raise
    
    return wrapper

# Initialize logging on import
logger = setup_logging(
    log_level=os.getenv('TEST_AUTOMATION_LOG_LEVEL', 'INFO'),
    log_dir=os.getenv('TEST_AUTOMATION_LOG_DIR', 'logs')
)