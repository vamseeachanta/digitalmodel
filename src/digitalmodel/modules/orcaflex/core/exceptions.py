"""
Custom Exception Hierarchy for OrcaFlex Module

This module defines a comprehensive exception hierarchy for consistent error handling
across the OrcaFlex analysis system. All exceptions include detailed context and
suggestions for resolution.
"""

from typing import Any, Dict, Optional, List


class OrcaFlexError(Exception):
    """
    Base exception class for all OrcaFlex-related errors.
    
    This provides common functionality for all OrcaFlex exceptions including:
    - Error codes for programmatic handling
    - Context information for debugging
    - Suggestions for resolution
    """
    
    def __init__(self,
                 message: str,
                 error_code: Optional[str] = None,
                 context: Optional[Dict[str, Any]] = None,
                 suggestions: Optional[List[str]] = None):
        """
        Initialize OrcaFlex exception.
        
        Args:
            message: Error message
            error_code: Optional error code for programmatic handling
            context: Optional context information
            suggestions: Optional list of suggestions for resolution
        """
        super().__init__(message)
        self.message = message
        self.error_code = error_code or "ORCAFLEX_ERROR"
        self.context = context or {}
        self.suggestions = suggestions or []
    
    def __str__(self) -> str:
        """String representation of the exception."""
        result = f"[{self.error_code}] {self.message}"
        
        if self.context:
            result += f"\nContext: {self.context}"
        
        if self.suggestions:
            result += "\nSuggestions:"
            for suggestion in self.suggestions:
                result += f"\n  - {suggestion}"
        
        return result
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert exception to dictionary for serialization."""
        return {
            'error_code': self.error_code,
            'message': self.message,
            'context': self.context,
            'suggestions': self.suggestions,
            'type': self.__class__.__name__
        }


class ConfigurationError(OrcaFlexError):
    """Exception raised for configuration-related errors."""
    
    def __init__(self, message: str, field: Optional[str] = None, **kwargs):
        """
        Initialize configuration error.
        
        Args:
            message: Error message
            field: Optional configuration field that caused the error
            **kwargs: Additional arguments for base class
        """
        if field:
            kwargs.setdefault('context', {})['field'] = field
        
        kwargs.setdefault('error_code', 'CONFIG_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Check configuration file syntax",
            "Verify all required fields are present",
            "Ensure configuration values are valid types",
            "Review configuration schema documentation"
        ])
        
        super().__init__(message, **kwargs)


class ValidationError(OrcaFlexError):
    """Exception raised for validation failures."""
    
    def __init__(self, message: str, validation_errors: Optional[List[str]] = None, **kwargs):
        """
        Initialize validation error.
        
        Args:
            message: Error message
            validation_errors: Optional list of specific validation errors
            **kwargs: Additional arguments for base class
        """
        if validation_errors:
            kwargs.setdefault('context', {})['validation_errors'] = validation_errors
        
        kwargs.setdefault('error_code', 'VALIDATION_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Review input data format",
            "Check data types and ranges",
            "Ensure all required fields are present",
            "Validate against schema requirements"
        ])
        
        super().__init__(message, **kwargs)


class AnalysisError(OrcaFlexError):
    """Exception raised during analysis execution."""
    
    def __init__(self, message: str, step: Optional[str] = None, **kwargs):
        """
        Initialize analysis error.
        
        Args:
            message: Error message
            step: Optional analysis step where error occurred
            **kwargs: Additional arguments for base class
        """
        if step:
            kwargs.setdefault('context', {})['step'] = step
        
        kwargs.setdefault('error_code', 'ANALYSIS_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Check model validity",
            "Verify analysis parameters",
            "Review convergence settings",
            "Check for numerical instabilities"
        ])
        
        super().__init__(message, **kwargs)


class LicenseError(OrcaFlexError):
    """Exception raised for OrcaFlex license issues."""
    
    def __init__(self, message: str, license_type: Optional[str] = None, **kwargs):
        """
        Initialize license error.
        
        Args:
            message: Error message
            license_type: Optional type of license issue
            **kwargs: Additional arguments for base class
        """
        if license_type:
            kwargs.setdefault('context', {})['license_type'] = license_type
        
        kwargs.setdefault('error_code', 'LICENSE_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Check OrcaFlex license server connection",
            "Verify license is valid and not expired",
            "Ensure sufficient licenses are available",
            "Contact IT or license administrator"
        ])
        
        super().__init__(message, **kwargs)


class ModelError(OrcaFlexError):
    """Exception raised for model-related errors."""
    
    def __init__(self, message: str, model_file: Optional[str] = None, **kwargs):
        """
        Initialize model error.
        
        Args:
            message: Error message
            model_file: Optional model file path
            **kwargs: Additional arguments for base class
        """
        if model_file:
            kwargs.setdefault('context', {})['model_file'] = model_file
        
        kwargs.setdefault('error_code', 'MODEL_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Verify model file exists and is accessible",
            "Check model file format and version",
            "Ensure model is not corrupted",
            "Validate model against OrcaFlex requirements"
        ])
        
        super().__init__(message, **kwargs)


class FileError(OrcaFlexError):
    """Exception raised for file I/O errors."""
    
    def __init__(self, message: str, file_path: Optional[str] = None, operation: Optional[str] = None, **kwargs):
        """
        Initialize file error.
        
        Args:
            message: Error message
            file_path: Optional file path
            operation: Optional operation (read/write/delete)
            **kwargs: Additional arguments for base class
        """
        context = kwargs.setdefault('context', {})
        if file_path:
            context['file_path'] = file_path
        if operation:
            context['operation'] = operation
        
        kwargs.setdefault('error_code', 'FILE_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Check file path and permissions",
            "Ensure directory exists",
            "Verify sufficient disk space",
            "Check file is not locked by another process"
        ])
        
        super().__init__(message, **kwargs)


class ComponentNotFoundError(OrcaFlexError):
    """Exception raised when a component is not found in registry."""
    
    def __init__(self, message: str, component_name: Optional[str] = None, **kwargs):
        """
        Initialize component not found error.
        
        Args:
            message: Error message
            component_name: Optional name of missing component
            **kwargs: Additional arguments for base class
        """
        if component_name:
            kwargs.setdefault('context', {})['component_name'] = component_name
        
        kwargs.setdefault('error_code', 'COMPONENT_NOT_FOUND')
        kwargs.setdefault('suggestions', []).extend([
            "Check component name spelling",
            "Ensure component is registered",
            "List available components with registry",
            "Register missing component if needed"
        ])
        
        super().__init__(message, **kwargs)


class RegistrationError(OrcaFlexError):
    """Exception raised during component registration."""
    
    def __init__(self, message: str, component_type: Optional[str] = None, **kwargs):
        """
        Initialize registration error.
        
        Args:
            message: Error message
            component_type: Optional type of component
            **kwargs: Additional arguments for base class
        """
        if component_type:
            kwargs.setdefault('context', {})['component_type'] = component_type
        
        kwargs.setdefault('error_code', 'REGISTRATION_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Verify component implements required interface",
            "Check for naming conflicts",
            "Ensure component class is valid",
            "Review registration requirements"
        ])
        
        super().__init__(message, **kwargs)


class WorkflowError(OrcaFlexError):
    """Exception raised during workflow execution."""
    
    def __init__(self, message: str, workflow_step: Optional[str] = None, **kwargs):
        """
        Initialize workflow error.
        
        Args:
            message: Error message
            workflow_step: Optional workflow step where error occurred
            **kwargs: Additional arguments for base class
        """
        if workflow_step:
            kwargs.setdefault('context', {})['workflow_step'] = workflow_step
        
        kwargs.setdefault('error_code', 'WORKFLOW_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Check workflow step dependencies",
            "Verify step configuration",
            "Review workflow execution order",
            "Check for missing required steps"
        ])
        
        super().__init__(message, **kwargs)


class DataExtractionError(OrcaFlexError):
    """Exception raised during data extraction."""
    
    def __init__(self, message: str, source: Optional[str] = None, query: Optional[str] = None, **kwargs):
        """
        Initialize data extraction error.
        
        Args:
            message: Error message
            source: Optional data source
            query: Optional extraction query
            **kwargs: Additional arguments for base class
        """
        context = kwargs.setdefault('context', {})
        if source:
            context['source'] = source
        if query:
            context['query'] = query
        
        kwargs.setdefault('error_code', 'EXTRACTION_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Verify data source is valid",
            "Check extraction query syntax",
            "Ensure requested fields exist",
            "Review data format requirements"
        ])
        
        super().__init__(message, **kwargs)


class ProcessingError(OrcaFlexError):
    """Exception raised during data processing."""
    
    def __init__(self, message: str, processor: Optional[str] = None, **kwargs):
        """
        Initialize processing error.
        
        Args:
            message: Error message
            processor: Optional processor name
            **kwargs: Additional arguments for base class
        """
        if processor:
            kwargs.setdefault('context', {})['processor'] = processor
        
        kwargs.setdefault('error_code', 'PROCESSING_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Check input data format",
            "Verify processor configuration",
            "Review processing parameters",
            "Check for data compatibility"
        ])
        
        super().__init__(message, **kwargs)


class TimeoutError(OrcaFlexError):
    """Exception raised when operation times out."""
    
    def __init__(self, message: str, timeout_seconds: Optional[float] = None, **kwargs):
        """
        Initialize timeout error.
        
        Args:
            message: Error message
            timeout_seconds: Optional timeout duration
            **kwargs: Additional arguments for base class
        """
        if timeout_seconds:
            kwargs.setdefault('context', {})['timeout_seconds'] = timeout_seconds
        
        kwargs.setdefault('error_code', 'TIMEOUT_ERROR')
        kwargs.setdefault('suggestions', []).extend([
            "Increase timeout duration",
            "Check for infinite loops",
            "Optimize analysis parameters",
            "Consider parallel processing"
        ])
        
        super().__init__(message, **kwargs)