"""
Custom exceptions for Create Go-By Folder Tool
"""


class GoByError(Exception):
    """Base exception for go-by folder operations."""
    pass


class ValidationError(GoByError):
    """Raised when validation fails."""
    pass


class ScanError(GoByError):
    """Raised when file scanning encounters an error."""
    pass


class PreservationError(GoByError):
    """Raised when file preservation fails."""
    pass


class MinimizationError(GoByError):
    """Raised when file minimization fails."""
    pass


class MetadataError(GoByError):
    """Raised when metadata generation fails."""
    pass


class CheckpointError(GoByError):
    """Raised when checkpoint operations fail."""
    pass


class UserCancelledError(GoByError):
    """Raised when user cancels operation."""
    pass


class DiskSpaceError(GoByError):
    """Raised when insufficient disk space."""
    pass


class PermissionError(GoByError):
    """Raised when file permissions prevent operation."""
    pass