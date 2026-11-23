"""
Core Interfaces and Protocols for OrcaFlex Module

This module defines the abstract interfaces that all OrcaFlex components must implement.
These interfaces ensure consistency, modularity, and extensibility across the system.
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional, Protocol, Union
from pathlib import Path
import pandas as pd


class ConfigurationInterface(Protocol):
    """Protocol for configuration objects."""
    
    @abstractmethod
    def validate(self) -> bool:
        """Validate the configuration."""
        ...
    
    @abstractmethod
    def to_dict(self) -> Dict[str, Any]:
        """Convert configuration to dictionary."""
        ...
    
    @abstractmethod
    def from_dict(self, data: Dict[str, Any]) -> 'ConfigurationInterface':
        """Create configuration from dictionary."""
        ...
    
    @abstractmethod
    def merge(self, other: 'ConfigurationInterface') -> 'ConfigurationInterface':
        """Merge with another configuration."""
        ...


class ModelInterface(Protocol):
    """Protocol for OrcaFlex model operations."""
    
    @abstractmethod
    def load(self, file_path: Union[str, Path]) -> bool:
        """Load model from file."""
        ...
    
    @abstractmethod
    def save(self, file_path: Union[str, Path]) -> bool:
        """Save model to file."""
        ...
    
    @abstractmethod
    def validate(self) -> bool:
        """Validate model integrity."""
        ...
    
    @abstractmethod
    def get_metadata(self) -> Dict[str, Any]:
        """Get model metadata."""
        ...


class AnalyzerInterface(ABC):
    """Abstract interface for all analyzers."""
    
    @abstractmethod
    def initialize(self, config: ConfigurationInterface) -> None:
        """Initialize the analyzer with configuration."""
        pass
    
    @abstractmethod
    def analyze(self, model: ModelInterface) -> Dict[str, Any]:
        """Perform analysis on the model."""
        pass
    
    @abstractmethod
    def validate_inputs(self) -> bool:
        """Validate analyzer inputs."""
        pass
    
    @abstractmethod
    def get_results(self) -> Dict[str, Any]:
        """Get analysis results."""
        pass
    
    @abstractmethod
    def cleanup(self) -> None:
        """Clean up resources."""
        pass
    
    @property
    @abstractmethod
    def name(self) -> str:
        """Get analyzer name."""
        pass
    
    @property
    @abstractmethod
    def version(self) -> str:
        """Get analyzer version."""
        pass


class ProcessorInterface(ABC):
    """Abstract interface for all processors."""
    
    @abstractmethod
    def process(self, data: Any) -> Any:
        """Process the input data."""
        pass
    
    @abstractmethod
    def validate(self, data: Any) -> bool:
        """Validate input data."""
        pass
    
    @abstractmethod
    def preprocess(self, data: Any) -> Any:
        """Preprocess data before main processing."""
        pass
    
    @abstractmethod
    def postprocess(self, data: Any) -> Any:
        """Postprocess data after main processing."""
        pass
    
    @property
    @abstractmethod
    def supported_types(self) -> List[str]:
        """Get list of supported data types."""
        pass


class ExtractorInterface(ABC):
    """Abstract interface for all data extractors."""
    
    @abstractmethod
    def extract(self, source: Any, query: Optional[str] = None) -> pd.DataFrame:
        """Extract data from source."""
        pass
    
    @abstractmethod
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform extracted data."""
        pass
    
    @abstractmethod
    def validate_source(self, source: Any) -> bool:
        """Validate data source."""
        pass
    
    @abstractmethod
    def get_available_fields(self) -> List[str]:
        """Get list of available fields to extract."""
        pass
    
    @property
    @abstractmethod
    def extraction_type(self) -> str:
        """Get extraction type identifier."""
        pass


class WorkflowInterface(ABC):
    """Abstract interface for workflow orchestration."""
    
    @abstractmethod
    def add_step(self, step: Union[AnalyzerInterface, ProcessorInterface]) -> None:
        """Add a step to the workflow."""
        pass
    
    @abstractmethod
    def remove_step(self, step_id: str) -> bool:
        """Remove a step from the workflow."""
        pass
    
    @abstractmethod
    def execute(self) -> Dict[str, Any]:
        """Execute the complete workflow."""
        pass
    
    @abstractmethod
    def pause(self) -> bool:
        """Pause workflow execution."""
        pass
    
    @abstractmethod
    def resume(self) -> bool:
        """Resume workflow execution."""
        pass
    
    @abstractmethod
    def get_status(self) -> Dict[str, Any]:
        """Get workflow execution status."""
        pass
    
    @property
    @abstractmethod
    def steps(self) -> List[str]:
        """Get list of workflow steps."""
        pass


class ResultInterface(Protocol):
    """Protocol for analysis results."""
    
    @abstractmethod
    def to_dataframe(self) -> pd.DataFrame:
        """Convert results to DataFrame."""
        ...
    
    @abstractmethod
    def to_dict(self) -> Dict[str, Any]:
        """Convert results to dictionary."""
        ...
    
    @abstractmethod
    def save(self, file_path: Union[str, Path], format: str = 'json') -> bool:
        """Save results to file."""
        ...
    
    @abstractmethod
    def summary(self) -> str:
        """Get results summary."""
        ...


class ValidatorInterface(ABC):
    """Abstract interface for validators."""
    
    @abstractmethod
    def validate(self, data: Any) -> bool:
        """Validate data."""
        pass
    
    @abstractmethod
    def get_errors(self) -> List[str]:
        """Get validation errors."""
        pass
    
    @abstractmethod
    def get_warnings(self) -> List[str]:
        """Get validation warnings."""
        pass
    
    @property
    @abstractmethod
    def rules(self) -> Dict[str, Any]:
        """Get validation rules."""
        pass