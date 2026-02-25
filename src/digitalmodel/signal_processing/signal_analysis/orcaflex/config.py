"""
Configuration Management for Time Series Analysis

Handles YAML configuration loading, validation, and environment variables.
"""

import os
import yaml
import json
from pathlib import Path
from typing import Dict, Any, Optional, Union
import logging
try:
    from jsonschema import validate, ValidationError
    HAS_JSONSCHEMA = True
except ImportError:
    HAS_JSONSCHEMA = False
    ValidationError = Exception

logger = logging.getLogger(__name__)


class ConfigurationManager:
    """Manages configuration for time series analysis"""
    
    # Default configuration
    DEFAULT_CONFIG = {
        'metadata': {
            'name': 'Time Series Analysis',
            'version': '1.0.0'
        },
        'input': {
            'mode': 'single',
            'encoding': 'utf-8',
            'delimiter': ',',
            'decimal': '.'
        },
        'column_mapping': {
            'strategy': 'auto',
            'auto_detect': {
                'time_patterns': ['time', 'Time', 't', 'Time (s)'],
                'data_patterns': ['*'],
                'exclude_patterns': ['*_flag', '*_quality']
            }
        },
        'analysis': {
            'sampling': {
                'auto_detect': True,
                'default_rate': 10.0
            },
            'rainflow': {
                'enable': True,
                'method': 'astm',
                'bin_count': 50,
                'extract_info': True
            },
            'fft': {
                'enable': True,
                'window_size': 4096,
                'overlap': 0.5,
                'window_function': 'hanning',
                'frequency_limit': 10.0,
                'peak_detection': {
                    'enable': True,
                    'n_peaks': 10
                }
            }
        },
        'output': {
            'directory': 'output/timeseries_analysis',
            'formats': {
                'csv': True,
                'json': True,
                'png': True
            },
            'plots': {
                'enable': True
            },
            'summary': {
                'create': True,
                'format': 'html'
            }
        },
        'batch': {
            'parallel': {
                'enable': True,
                'max_workers': 4
            },
            'continue_on_error': True
        },
        'logging': {
            'level': 'INFO',
            'file': 'analysis.log',
            'console': True
        }
    }
    
    # Configuration schema for validation
    CONFIG_SCHEMA = {
        "type": "object",
        "properties": {
            "metadata": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "version": {"type": "string"}
                }
            },
            "input": {
                "type": "object",
                "properties": {
                    "mode": {"type": "string", "enum": ["single", "pattern", "directory", "list"]},
                    "file_path": {"type": "string"},
                    "pattern": {"type": "string"},
                    "directory": {"type": "string"}
                }
            },
            "analysis": {
                "type": "object",
                "properties": {
                    "rainflow": {
                        "type": "object",
                        "properties": {
                            "enable": {"type": "boolean"},
                            "method": {"type": "string"},
                            "bin_count": {"type": "integer", "minimum": 1}
                        }
                    },
                    "fft": {
                        "type": "object",
                        "properties": {
                            "enable": {"type": "boolean"},
                            "window_size": {"type": "integer", "minimum": 64},
                            "overlap": {"type": "number", "minimum": 0, "maximum": 1}
                        }
                    }
                }
            }
        }
    }
    
    def __init__(self, config_path: Optional[Union[str, Path]] = None):
        """
        Initialize configuration manager
        
        Args:
            config_path: Path to configuration file
        """
        self.config_path = config_path
        self.config = self.DEFAULT_CONFIG.copy()
        self.env_prefix = 'TIMESERIES_'
        
        # Load configuration if path provided
        if config_path:
            self.load_config(config_path)
    
    def load_config(self, config_path: Union[str, Path]) -> Dict:
        """
        Load configuration from file
        
        Args:
            config_path: Path to configuration file
            
        Returns:
            Loaded configuration dictionary
        """
        config_path = Path(config_path)
        
        if not config_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_path}")
        
        # Determine file type
        if config_path.suffix.lower() in ['.yml', '.yaml']:
            config = self._load_yaml(config_path)
        elif config_path.suffix.lower() == '.json':
            config = self._load_json(config_path)
        else:
            raise ValueError(f"Unsupported configuration format: {config_path.suffix}")
        
        # Merge with defaults
        self.config = self._merge_configs(self.DEFAULT_CONFIG, config)
        
        # Apply environment variables
        self._apply_env_vars()
        
        # Validate configuration
        self.validate_config()
        
        logger.info(f"Loaded configuration from {config_path}")
        return self.config
    
    def _load_yaml(self, path: Path) -> Dict:
        """Load YAML configuration"""
        with open(path, 'r') as f:
            return yaml.safe_load(f)
    
    def _load_json(self, path: Path) -> Dict:
        """Load JSON configuration"""
        with open(path, 'r') as f:
            return json.load(f)
    
    def _merge_configs(self, base: Dict, override: Dict) -> Dict:
        """
        Recursively merge configuration dictionaries
        
        Args:
            base: Base configuration
            override: Override configuration
            
        Returns:
            Merged configuration
        """
        result = base.copy()
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._merge_configs(result[key], value)
            else:
                result[key] = value
        
        return result
    
    def _apply_env_vars(self):
        """Apply environment variable overrides"""
        # Check for common environment variables
        env_mappings = {
            f'{self.env_prefix}OUTPUT_DIR': ['output', 'directory'],
            f'{self.env_prefix}INPUT_MODE': ['input', 'mode'],
            f'{self.env_prefix}INPUT_PATH': ['input', 'file_path'],
            f'{self.env_prefix}INPUT_PATTERN': ['input', 'pattern'],
            f'{self.env_prefix}LOG_LEVEL': ['logging', 'level'],
            f'{self.env_prefix}PARALLEL': ['batch', 'parallel', 'enable'],
            f'{self.env_prefix}MAX_WORKERS': ['batch', 'parallel', 'max_workers']
        }
        
        for env_var, config_path in env_mappings.items():
            value = os.environ.get(env_var)
            if value:
                self._set_nested_value(self.config, config_path, value)
                logger.info(f"Applied environment variable: {env_var}")
    
    def _set_nested_value(self, config: Dict, path: list, value: Any):
        """Set a value in nested dictionary using path"""
        current = config
        for key in path[:-1]:
            if key not in current:
                current[key] = {}
            current = current[key]
        
        # Convert value types
        if isinstance(value, str):
            if value.lower() in ['true', 'false']:
                value = value.lower() == 'true'
            elif value.isdigit():
                value = int(value)
            elif '.' in value and value.replace('.', '').isdigit():
                value = float(value)
        
        current[path[-1]] = value
    
    def validate_config(self) -> bool:
        """
        Validate configuration against schema
        
        Returns:
            True if valid
            
        Raises:
            ValidationError if invalid
        """
        if not HAS_JSONSCHEMA:
            logger.warning("jsonschema not available, skipping validation")
            return True
            
        try:
            validate(instance=self.config, schema=self.CONFIG_SCHEMA)
            logger.info("Configuration validation successful")
            return True
        except ValidationError as e:
            logger.error(f"Configuration validation failed: {e}")
            raise
    
    def get(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value using dot notation
        
        Args:
            key: Configuration key (e.g., 'analysis.rainflow.method')
            default: Default value if key not found
            
        Returns:
            Configuration value
        """
        keys = key.split('.')
        value = self.config
        
        for k in keys:
            if isinstance(value, dict) and k in value:
                value = value[k]
            else:
                return default
        
        return value
    
    def set(self, key: str, value: Any):
        """
        Set configuration value using dot notation
        
        Args:
            key: Configuration key
            value: Value to set
        """
        keys = key.split('.')
        self._set_nested_value(self.config, keys, value)
    
    def save_config(self, path: Optional[Union[str, Path]] = None):
        """
        Save current configuration to file
        
        Args:
            path: Output path (uses original path if not specified)
        """
        if path is None:
            path = self.config_path
        
        if path is None:
            raise ValueError("No path specified for saving configuration")
        
        path = Path(path)
        
        # Determine format
        if path.suffix.lower() in ['.yml', '.yaml']:
            with open(path, 'w') as f:
                yaml.dump(self.config, f, default_flow_style=False, sort_keys=False)
        elif path.suffix.lower() == '.json':
            with open(path, 'w') as f:
                json.dump(self.config, f, indent=2)
        else:
            raise ValueError(f"Unsupported format: {path.suffix}")
        
        logger.info(f"Saved configuration to {path}")
    
    def get_profile(self, profile_name: str) -> Optional[Dict]:
        """
        Get a column mapping profile
        
        Args:
            profile_name: Name of profile
            
        Returns:
            Profile configuration or None
        """
        profiles = self.get('column_mapping.profiles', {})
        return profiles.get(profile_name)
    
    def list_profiles(self) -> list:
        """
        List available column mapping profiles
        
        Returns:
            List of profile names
        """
        profiles = self.get('column_mapping.profiles', {})
        return list(profiles.keys())
    
    def setup_logging(self):
        """Setup logging based on configuration"""
        log_config = self.config.get('logging', {})
        
        level = getattr(logging, log_config.get('level', 'INFO'))
        
        # Create formatter
        formatter = logging.Formatter(
            log_config.get('format', '%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )
        
        # Setup root logger
        root_logger = logging.getLogger()
        root_logger.setLevel(level)
        
        # Console handler
        if log_config.get('console', True):
            console_handler = logging.StreamHandler()
            console_handler.setFormatter(formatter)
            root_logger.addHandler(console_handler)
        
        # File handler
        if 'file' in log_config:
            file_handler = logging.FileHandler(log_config['file'])
            file_handler.setFormatter(formatter)
            root_logger.addHandler(file_handler)
    
    def to_dict(self) -> Dict:
        """
        Get configuration as dictionary
        
        Returns:
            Configuration dictionary
        """
        return self.config.copy()
    
    def __getitem__(self, key: str) -> Any:
        """Allow dictionary-style access"""
        return self.get(key)
    
    def __setitem__(self, key: str, value: Any):
        """Allow dictionary-style setting"""
        self.set(key, value)