"""Custom exceptions for comprehensive mooring analysis."""


class MooringAnalysisError(Exception):
    """Base exception for mooring analysis errors."""
    pass


class ConfigurationError(MooringAnalysisError):
    """Raised when configuration is invalid or missing."""
    pass


class DataParsingError(MooringAnalysisError):
    """Raised when CSV data cannot be parsed correctly."""
    
    def __init__(self, filename: str, message: str):
        self.filename = filename
        super().__init__(f"Error parsing {filename}: {message}")


class ConvergenceError(MooringAnalysisError):
    """Raised when pretension convergence fails."""
    
    def __init__(self, line_name: str, deviation: float, tolerance: float):
        self.line_name = line_name
        self.deviation = deviation
        self.tolerance = tolerance
        super().__init__(
            f"Line {line_name} failed to converge: "
            f"deviation={deviation:.2%}, tolerance={tolerance:.2%}"
        )


class StiffnessCalculationError(MooringAnalysisError):
    """Raised when stiffness matrix calculation fails."""
    
    def __init__(self, message: str, matrix_condition: float = None):
        self.matrix_condition = matrix_condition
        if matrix_condition:
            message += f" (condition number: {matrix_condition:.2e})"
        super().__init__(message)


class FenderAnalysisError(MooringAnalysisError):
    """Raised when fender force analysis encounters issues."""
    
    def __init__(self, fender_id: str, message: str):
        self.fender_id = fender_id
        super().__init__(f"Fender {fender_id}: {message}")


class GroupComparisonError(MooringAnalysisError):
    """Raised when group comparison fails."""
    
    def __init__(self, message: str, groups: list = None):
        self.groups = groups
        if groups:
            message += f" (groups: {', '.join(groups)})"
        super().__init__(message)


class ContextExtractionError(MooringAnalysisError):
    """Raised when context extraction from filename fails."""
    
    def __init__(self, filename: str, reason: str):
        self.filename = filename
        super().__init__(f"Cannot extract context from {filename}: {reason}")


class ReportGenerationError(MooringAnalysisError):
    """Raised when report generation fails."""
    
    def __init__(self, format: str, message: str):
        self.format = format
        super().__init__(f"Failed to generate {format} report: {message}")


class ValidationError(MooringAnalysisError):
    """Raised when data validation fails."""
    
    def __init__(self, field: str, value, message: str):
        self.field = field
        self.value = value
        super().__init__(f"Validation error for {field}={value}: {message}")


class InsufficientDataError(MooringAnalysisError):
    """Raised when insufficient data is available for analysis."""
    
    def __init__(self, data_type: str, required: int, found: int):
        self.data_type = data_type
        self.required = required
        self.found = found
        super().__init__(
            f"Insufficient {data_type} data: "
            f"required={required}, found={found}"
        )


class StandardsComplianceError(MooringAnalysisError):
    """Raised when results don't meet industry standards."""
    
    def __init__(self, standard: str, violation: str, value: float = None):
        self.standard = standard
        self.violation = violation
        self.value = value
        message = f"{standard} compliance violation: {violation}"
        if value is not None:
            message += f" (value: {value:.2f})"
        super().__init__(message)


class FileAccessError(MooringAnalysisError):
    """Raised when file access operations fail."""
    
    def __init__(self, filepath: str, operation: str, original_error: Exception = None):
        self.filepath = filepath
        self.operation = operation
        self.original_error = original_error
        message = f"Failed to {operation} file: {filepath}"
        if original_error:
            message += f" ({str(original_error)})"
        super().__init__(message)


class ProcessingError(MooringAnalysisError):
    """Raised when batch processing encounters errors."""
    
    def __init__(self, message: str, failed_files: list = None):
        self.failed_files = failed_files or []
        if failed_files:
            message += f" (failed files: {len(failed_files)})"
        super().__init__(message)


class LLMContextError(MooringAnalysisError):
    """Raised when LLM context extraction fails."""
    
    def __init__(self, message: str, fallback_used: bool = False):
        self.fallback_used = fallback_used
        if fallback_used:
            message += " (using fallback pattern matching)"
        super().__init__(message)