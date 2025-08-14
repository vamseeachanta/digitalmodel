"""
Configuration Validation and Error Handling Service
Provides comprehensive validation and error recovery for OrcaFlex browser configurations
"""

import re
import json
from typing import Dict, List, Optional, Tuple, Any, Union
from dataclasses import dataclass, field
from enum import Enum
import logging
from pathlib import Path
from datetime import datetime

logger = logging.getLogger(__name__)


class ValidationLevel(Enum):
    """Validation severity levels"""
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


class ErrorCategory(Enum):
    """Categories of validation errors"""
    MISSING_REQUIRED = "missing_required"
    INVALID_FORMAT = "invalid_format"
    INVALID_VALUE = "invalid_value"
    FILE_NOT_FOUND = "file_not_found"
    INCOMPATIBLE = "incompatible"
    RANGE_ERROR = "range_error"
    DEPENDENCY = "dependency"


@dataclass
class ValidationResult:
    """Result of validation check"""
    is_valid: bool
    level: ValidationLevel
    category: ErrorCategory
    field: str
    message: str
    suggestion: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ValidationReport:
    """Complete validation report"""
    overall_valid: bool
    results: List[ValidationResult]
    timestamp: datetime
    configuration: Dict[str, Any]
    
    def get_errors(self) -> List[ValidationResult]:
        """Get only error-level results"""
        return [r for r in self.results if r.level == ValidationLevel.ERROR]
    
    def get_warnings(self) -> List[ValidationResult]:
        """Get only warning-level results"""
        return [r for r in self.results if r.level == ValidationLevel.WARNING]
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization"""
        return {
            'overall_valid': self.overall_valid,
            'timestamp': self.timestamp.isoformat(),
            'configuration': self.configuration,
            'errors': [
                {
                    'field': r.field,
                    'message': r.message,
                    'suggestion': r.suggestion,
                    'category': r.category.value
                }
                for r in self.get_errors()
            ],
            'warnings': [
                {
                    'field': r.field,
                    'message': r.message,
                    'suggestion': r.suggestion
                }
                for r in self.get_warnings()
            ]
        }


class ConfigurationValidator:
    """Comprehensive configuration validation service"""
    
    # Valid parameter values
    VALID_PARAMETERS = {
        'vessel_type': ['fsts', 'flng', 'lngc'],
        'loading_condition': ['l015', 'l050', 'l095'],
        'tide_level': ['hwl', 'mwl', 'lwl'],
        'return_period': ['0001yr', '0010yr', '0100yr'],
        'analysis_type': ['03c', '04a', '05b', '06d']
    }
    
    # Parameter dependencies
    PARAMETER_DEPENDENCIES = {
        'l095': ['hwl'],  # 95% loading typically with high water
        'l015': ['lwl'],  # 15% loading typically with low water
    }
    
    # Regex patterns for validation
    PATTERNS = {
        'wave_direction': r'^\d{3}deg$',
        'analysis_type': r'^\d{2}[a-z]$',
        'return_period': r'^\d{4}yr$',
        'loading_condition': r'^l\d{3}$',
        'custom_basename': r'^[a-zA-Z0-9_\-]+$'
    }
    
    def __init__(self, base_path: Optional[Path] = None):
        """
        Initialize validator
        
        Args:
            base_path: Base directory path for file validation
        """
        self.base_path = base_path or Path("D:/1522/ctr7/orcaflex/rev_a08")
        self.validation_cache: Dict[str, ValidationReport] = {}
        
    def validate_configuration(
        self,
        config: Dict[str, Any],
        check_files: bool = False
    ) -> ValidationReport:
        """
        Validate a complete configuration
        
        Args:
            config: Configuration dictionary to validate
            check_files: Whether to check for file existence
            
        Returns:
            ValidationReport with all validation results
        """
        results = []
        
        # Check auto-max mode
        if config.get('auto_max', False):
            # In auto-max mode, minimal validation needed
            results.append(ValidationResult(
                is_valid=True,
                level=ValidationLevel.INFO,
                category=ErrorCategory.INCOMPATIBLE,
                field='auto_max',
                message='Auto-max mode enabled, using automatic configuration'
            ))
        else:
            # Manual mode - full validation
            
            # Validate required fields
            results.extend(self._validate_required_fields(config))
            
            # Validate field formats
            results.extend(self._validate_field_formats(config))
            
            # Validate field values
            results.extend(self._validate_field_values(config))
            
            # Check parameter dependencies
            results.extend(self._validate_dependencies(config))
            
            # Check for incompatible combinations
            results.extend(self._validate_compatibility(config))
            
            # Validate custom basename if present
            if 'custom_basename' in config:
                results.extend(self._validate_custom_basename(config['custom_basename']))
        
        # Check for file existence if requested
        if check_files:
            results.extend(self._validate_file_existence(config))
        
        # Determine overall validity
        errors = [r for r in results if r.level == ValidationLevel.ERROR]
        overall_valid = len(errors) == 0
        
        report = ValidationReport(
            overall_valid=overall_valid,
            results=results,
            timestamp=datetime.now(),
            configuration=config
        )
        
        # Cache the report
        cache_key = json.dumps(config, sort_keys=True)
        self.validation_cache[cache_key] = report
        
        return report
    
    def _validate_required_fields(self, config: Dict[str, Any]) -> List[ValidationResult]:
        """Validate required fields are present"""
        results = []
        required_fields = ['vessel_type']
        
        if not config.get('custom_basename'):
            required_fields.extend(['loading_condition', 'tide_level'])
        
        for field in required_fields:
            if field not in config or not config[field]:
                results.append(ValidationResult(
                    is_valid=False,
                    level=ValidationLevel.ERROR,
                    category=ErrorCategory.MISSING_REQUIRED,
                    field=field,
                    message=f'{field.replace("_", " ").title()} is required',
                    suggestion=f'Please provide a value for {field}'
                ))
        
        return results
    
    def _validate_field_formats(self, config: Dict[str, Any]) -> List[ValidationResult]:
        """Validate field formats using regex patterns"""
        results = []
        
        for field, pattern in self.PATTERNS.items():
            if field in config and config[field]:
                if not re.match(pattern, str(config[field])):
                    results.append(ValidationResult(
                        is_valid=False,
                        level=ValidationLevel.ERROR,
                        category=ErrorCategory.INVALID_FORMAT,
                        field=field,
                        message=f'Invalid format for {field}: {config[field]}',
                        suggestion=f'{field} must match pattern: {pattern}'
                    ))
        
        return results
    
    def _validate_field_values(self, config: Dict[str, Any]) -> List[ValidationResult]:
        """Validate field values are in allowed ranges"""
        results = []
        
        for field, valid_values in self.VALID_PARAMETERS.items():
            if field in config and config[field]:
                if config[field] not in valid_values:
                    results.append(ValidationResult(
                        is_valid=False,
                        level=ValidationLevel.ERROR,
                        category=ErrorCategory.INVALID_VALUE,
                        field=field,
                        message=f'Invalid value for {field}: {config[field]}',
                        suggestion=f'Valid values are: {", ".join(valid_values)}'
                    ))
        
        # Validate wave direction range
        if 'wave_direction' in config and config['wave_direction']:
            match = re.match(r'^(\d{3})deg$', config['wave_direction'])
            if match:
                degrees = int(match.group(1))
                if degrees > 360:
                    results.append(ValidationResult(
                        is_valid=False,
                        level=ValidationLevel.ERROR,
                        category=ErrorCategory.RANGE_ERROR,
                        field='wave_direction',
                        message=f'Wave direction {degrees}° exceeds 360°',
                        suggestion='Wave direction must be between 0° and 360°'
                    ))
        
        return results
    
    def _validate_dependencies(self, config: Dict[str, Any]) -> List[ValidationResult]:
        """Validate parameter dependencies"""
        results = []
        
        loading = config.get('loading_condition')
        tide = config.get('tide_level')
        
        if loading in self.PARAMETER_DEPENDENCIES:
            recommended_tides = self.PARAMETER_DEPENDENCIES[loading]
            if tide and tide not in recommended_tides:
                results.append(ValidationResult(
                    is_valid=True,
                    level=ValidationLevel.WARNING,
                    category=ErrorCategory.DEPENDENCY,
                    field='tide_level',
                    message=f'{loading} typically uses {", ".join(recommended_tides)} tide level',
                    suggestion=f'Consider using {recommended_tides[0]} for optimal results'
                ))
        
        return results
    
    def _validate_compatibility(self, config: Dict[str, Any]) -> List[ValidationResult]:
        """Check for incompatible parameter combinations"""
        results = []
        
        # Check vessel-specific constraints
        vessel = config.get('vessel_type')
        analysis = config.get('analysis_type')
        
        if vessel == 'lngc' and analysis == '03c':
            results.append(ValidationResult(
                is_valid=True,
                level=ValidationLevel.WARNING,
                category=ErrorCategory.INCOMPATIBLE,
                field='analysis_type',
                message='LNGC vessel type rarely uses 03c analysis configuration',
                suggestion='Consider using 04a or 05b for LNGC vessels'
            ))
        
        return results
    
    def _validate_custom_basename(self, basename: str) -> List[ValidationResult]:
        """Validate custom basename"""
        results = []
        
        if len(basename) < 3:
            results.append(ValidationResult(
                is_valid=False,
                level=ValidationLevel.ERROR,
                category=ErrorCategory.INVALID_FORMAT,
                field='custom_basename',
                message='Custom basename must be at least 3 characters',
                suggestion='Provide a more descriptive basename'
            ))
        
        if len(basename) > 100:
            results.append(ValidationResult(
                is_valid=False,
                level=ValidationLevel.ERROR,
                category=ErrorCategory.RANGE_ERROR,
                field='custom_basename',
                message='Custom basename exceeds maximum length of 100 characters',
                suggestion='Use a shorter basename'
            ))
        
        # Check for invalid characters
        if not re.match(self.PATTERNS['custom_basename'], basename):
            results.append(ValidationResult(
                is_valid=False,
                level=ValidationLevel.ERROR,
                category=ErrorCategory.INVALID_FORMAT,
                field='custom_basename',
                message='Custom basename contains invalid characters',
                suggestion='Use only letters, numbers, underscore and hyphen'
            ))
        
        return results
    
    def _validate_file_existence(self, config: Dict[str, Any]) -> List[ValidationResult]:
        """Check if files matching configuration exist"""
        results = []
        
        # Build expected file pattern
        pattern_parts = []
        if config.get('custom_basename'):
            pattern_parts.append(config['custom_basename'])
        else:
            for field in ['vessel_type', 'analysis_type', 'return_period', 
                         'loading_condition', 'tide_level']:
                if field in config and config[field]:
                    pattern_parts.append(config[field])
        
        if pattern_parts:
            pattern = '_'.join(pattern_parts)
            search_path = self.base_path / 'output' / 'csv'
            
            if search_path.exists():
                matching_files = list(search_path.glob(f"{pattern}*.csv"))
                
                if not matching_files:
                    results.append(ValidationResult(
                        is_valid=True,
                        level=ValidationLevel.WARNING,
                        category=ErrorCategory.FILE_NOT_FOUND,
                        field='file_pattern',
                        message=f'No files found matching pattern: {pattern}',
                        suggestion='Check if files exist or adjust configuration',
                        metadata={'pattern': pattern, 'search_path': str(search_path)}
                    ))
                else:
                    results.append(ValidationResult(
                        is_valid=True,
                        level=ValidationLevel.INFO,
                        category=ErrorCategory.FILE_NOT_FOUND,
                        field='file_pattern',
                        message=f'Found {len(matching_files)} files matching configuration',
                        metadata={'count': len(matching_files), 'pattern': pattern}
                    ))
        
        return results


class ErrorHandler:
    """Handles errors and provides recovery suggestions"""
    
    ERROR_RECOVERY_STRATEGIES = {
        ErrorCategory.MISSING_REQUIRED: {
            'strategy': 'provide_defaults',
            'message': 'Using default values for missing fields'
        },
        ErrorCategory.INVALID_FORMAT: {
            'strategy': 'format_correction',
            'message': 'Attempting to correct format'
        },
        ErrorCategory.FILE_NOT_FOUND: {
            'strategy': 'suggest_alternatives',
            'message': 'Suggesting alternative configurations'
        },
        ErrorCategory.INCOMPATIBLE: {
            'strategy': 'adjust_parameters',
            'message': 'Adjusting incompatible parameters'
        }
    }
    
    DEFAULT_VALUES = {
        'vessel_type': 'fsts',
        'loading_condition': 'l015',
        'tide_level': 'hwl',
        'return_period': '0100yr',
        'wave_direction': '000deg',
        'analysis_type': '03c'
    }
    
    def __init__(self):
        """Initialize error handler"""
        self.error_log: List[Dict[str, Any]] = []
        
    def handle_validation_errors(
        self,
        report: ValidationReport
    ) -> Tuple[bool, Dict[str, Any]]:
        """
        Handle validation errors and attempt recovery
        
        Args:
            report: ValidationReport with errors
            
        Returns:
            Tuple of (recovery_successful, corrected_configuration)
        """
        if report.overall_valid:
            return True, report.configuration
        
        corrected_config = report.configuration.copy()
        errors = report.get_errors()
        
        for error in errors:
            recovery = self._attempt_recovery(error, corrected_config)
            if recovery:
                corrected_config = recovery
        
        # Log error handling
        self.error_log.append({
            'timestamp': datetime.now().isoformat(),
            'original_config': report.configuration,
            'corrected_config': corrected_config,
            'errors': [e.message for e in errors]
        })
        
        return len(errors) == 0, corrected_config
    
    def _attempt_recovery(
        self,
        error: ValidationResult,
        config: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Attempt to recover from a specific error
        
        Args:
            error: ValidationResult with error details
            config: Current configuration
            
        Returns:
            Corrected configuration or None
        """
        strategy = self.ERROR_RECOVERY_STRATEGIES.get(error.category)
        
        if not strategy:
            return None
        
        if strategy['strategy'] == 'provide_defaults':
            if error.field in self.DEFAULT_VALUES:
                config[error.field] = self.DEFAULT_VALUES[error.field]
                logger.info(f"Applied default value for {error.field}: {config[error.field]}")
                return config
        
        elif strategy['strategy'] == 'format_correction':
            # Attempt to correct format issues
            if error.field == 'wave_direction':
                # Try to extract numeric value and format correctly
                match = re.search(r'\d+', str(config.get(error.field, '')))
                if match:
                    degrees = int(match.group()) % 360
                    config[error.field] = f"{degrees:03d}deg"
                    logger.info(f"Corrected wave_direction format: {config[error.field]}")
                    return config
        
        elif strategy['strategy'] == 'suggest_alternatives':
            # For file not found, suggest using auto-max
            config['auto_max'] = True
            logger.info("No files found, switching to auto-max mode")
            return config
        
        return None
    
    def get_error_summary(self) -> Dict[str, Any]:
        """Get summary of error handling"""
        return {
            'total_errors_handled': len(self.error_log),
            'recent_errors': self.error_log[-10:],  # Last 10 errors
            'recovery_strategies_used': list(self.ERROR_RECOVERY_STRATEGIES.keys())
        }


# Example usage
if __name__ == "__main__":
    # Configure logging
    logging.basicConfig(level=logging.INFO)
    
    # Create validator and error handler
    validator = ConfigurationValidator()
    error_handler = ErrorHandler()
    
    # Test configuration
    test_config = {
        'vessel_type': 'fsts',
        'loading_condition': 'l095',
        'tide_level': 'mwl',
        'return_period': '0100yr',
        'wave_direction': '045deg',
        'analysis_type': '03c',
        'auto_max': False
    }
    
    # Validate configuration
    report = validator.validate_configuration(test_config, check_files=True)
    
    print(f"Configuration valid: {report.overall_valid}")
    print(f"Errors: {len(report.get_errors())}")
    print(f"Warnings: {len(report.get_warnings())}")
    
    # Handle errors if any
    if not report.overall_valid:
        success, corrected = error_handler.handle_validation_errors(report)
        print(f"Recovery successful: {success}")
        print(f"Corrected configuration: {corrected}")
    
    # Test invalid configuration
    invalid_config = {
        'vessel_type': 'invalid',
        'loading_condition': 'xyz',
        'auto_max': False
    }
    
    report2 = validator.validate_configuration(invalid_config)
    print(f"\nInvalid config errors: {[e.message for e in report2.get_errors()]}")
    
    # Attempt recovery
    success, corrected = error_handler.handle_validation_errors(report2)
    print(f"Recovery result: {corrected}")