"""
Configuration management for test automation system.
"""

import os
import yaml
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field

@dataclass
class TestPaths:
    """Test path configuration."""
    base_dir: str = "tests"
    modules_dir: str = "tests/domains"
    in_progress_dir: str = "tests/in_progress" 
    no_license_dir: str = "tests/no_license"
    local_temp_dir: str = "tests/local_temp"

@dataclass
class ExecutionConfig:
    """Test execution configuration."""
    parallel: bool = True
    max_workers: int = 4
    timeout_seconds: int = 300
    licensed_software_limit: int = 2
    retry_count: int = 1

@dataclass
class ReportingConfig:
    """Reporting configuration."""
    output_dir: str = "test_automation_reports"
    formats: List[str] = field(default_factory=lambda: ["html", "json"])
    include_trends: bool = True
    archive_days: int = 30

@dataclass
class AutoFixConfig:
    """Auto-fix configuration."""
    enabled: bool = True
    backup_enabled: bool = True
    backup_dir: str = "test_automation_backups"
    confidence_threshold: float = 0.8
    max_fixes_per_run: int = 50

@dataclass
class TestAutomationConfig:
    """Main configuration class."""
    paths: TestPaths = field(default_factory=TestPaths)
    execution: ExecutionConfig = field(default_factory=ExecutionConfig)
    reporting: ReportingConfig = field(default_factory=ReportingConfig)
    autofix: AutoFixConfig = field(default_factory=AutoFixConfig)
    
    @classmethod
    def load_from_file(cls, config_path: Optional[str] = None) -> "TestAutomationConfig":
        """Load configuration from YAML file."""
        if config_path is None:
            config_path = os.getenv("TEST_AUTOMATION_CONFIG", "test_automation_settings.yml")
        
        config_file = Path(config_path)
        if not config_file.exists():
            # Return default configuration if file doesn't exist
            return cls()
        
        with open(config_file, 'r', encoding='utf-8') as f:
            data = yaml.safe_load(f) or {}
        
        # Create configuration with defaults, then update from file
        config = cls()
        if 'paths' in data:
            for key, value in data['paths'].items():
                if hasattr(config.paths, key):
                    setattr(config.paths, key, value)
        
        if 'execution' in data:
            for key, value in data['execution'].items():
                if hasattr(config.execution, key):
                    setattr(config.execution, key, value)
        
        if 'reporting' in data:
            for key, value in data['reporting'].items():
                if hasattr(config.reporting, key):
                    setattr(config.reporting, key, value)
        
        if 'autofix' in data:
            for key, value in data['autofix'].items():
                if hasattr(config.autofix, key):
                    setattr(config.autofix, key, value)
        
        return config
    
    def save_to_file(self, config_path: Optional[str] = None) -> None:
        """Save configuration to YAML file."""
        if config_path is None:
            config_path = "test_automation_settings.yml"
        
        data = {
            'paths': {
                'base_dir': self.paths.base_dir,
                'modules_dir': self.paths.modules_dir,
                'in_progress_dir': self.paths.in_progress_dir,
                'no_license_dir': self.paths.no_license_dir,
                'local_temp_dir': self.paths.local_temp_dir
            },
            'execution': {
                'parallel': self.execution.parallel,
                'max_workers': self.execution.max_workers,
                'timeout_seconds': self.execution.timeout_seconds,
                'licensed_software_limit': self.execution.licensed_software_limit,
                'retry_count': self.execution.retry_count
            },
            'reporting': {
                'output_dir': self.reporting.output_dir,
                'formats': self.reporting.formats,
                'include_trends': self.reporting.include_trends,
                'archive_days': self.reporting.archive_days
            },
            'autofix': {
                'enabled': self.autofix.enabled,
                'backup_enabled': self.autofix.backup_enabled,
                'backup_dir': self.autofix.backup_dir,
                'confidence_threshold': self.autofix.confidence_threshold,
                'max_fixes_per_run': self.autofix.max_fixes_per_run
            }
        }
        
        with open(config_path, 'w', encoding='utf-8') as f:
            yaml.dump(data, f, default_flow_style=False, indent=2)

# Global configuration instance
config = TestAutomationConfig.load_from_file()