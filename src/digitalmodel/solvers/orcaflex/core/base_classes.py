"""
Abstract Base Classes for OrcaFlex Components

This module provides base implementations of the core interfaces,
offering common functionality that can be extended by specific implementations.
"""

import logging
from abc import ABC
from typing import Any, Dict, List, Optional, Union
from pathlib import Path
from datetime import datetime
import pandas as pd

from .interfaces import (
    AnalyzerInterface,
    ProcessorInterface,
    ExtractorInterface,
    WorkflowInterface,
    ConfigurationInterface,
    ModelInterface
)
from .exceptions import ValidationError, AnalysisError


class BaseComponent(ABC):
    """Base class for all OrcaFlex components."""
    
    def __init__(self, name: str = None):
        """Initialize base component."""
        self._name = name or self.__class__.__name__
        self._logger = logging.getLogger(self._name)
        self._metadata = {
            'created_at': datetime.now(),
            'version': '2.0.0',
            'component_type': self.__class__.__name__
        }
        self._errors = []
        self._warnings = []
    
    def log_info(self, message: str) -> None:
        """Log information message."""
        self._logger.info(message)
    
    def log_error(self, message: str) -> None:
        """Log error message."""
        self._logger.error(message)
        self._errors.append(message)
    
    def log_warning(self, message: str) -> None:
        """Log warning message."""
        self._logger.warning(message)
        self._warnings.append(message)
    
    def get_errors(self) -> List[str]:
        """Get list of errors."""
        return self._errors.copy()
    
    def get_warnings(self) -> List[str]:
        """Get list of warnings."""
        return self._warnings.copy()
    
    def clear_errors(self) -> None:
        """Clear error list."""
        self._errors.clear()
    
    def clear_warnings(self) -> None:
        """Clear warning list."""
        self._warnings.clear()


class BaseAnalyzer(BaseComponent, AnalyzerInterface):
    """Base implementation of analyzer interface."""
    
    def __init__(self, name: str = None):
        """Initialize base analyzer."""
        super().__init__(name)
        self._config = None
        self._model = None
        self._results = {}
        self._is_initialized = False
    
    def initialize(self, config: ConfigurationInterface) -> None:
        """Initialize the analyzer with configuration."""
        self.log_info(f"Initializing {self._name}")
        
        if not config.validate():
            raise ValidationError("Invalid configuration provided")
        
        self._config = config
        self._is_initialized = True
        self.log_info(f"{self._name} initialized successfully")
    
    def validate_inputs(self) -> bool:
        """Validate analyzer inputs."""
        if not self._is_initialized:
            self.log_error("Analyzer not initialized")
            return False
        
        if self._config is None:
            self.log_error("No configuration available")
            return False
        
        return True
    
    def get_results(self) -> Dict[str, Any]:
        """Get analysis results."""
        return self._results.copy()
    
    def cleanup(self) -> None:
        """Clean up resources."""
        self.log_info(f"Cleaning up {self._name}")
        self._results.clear()
        self._model = None
        self.clear_errors()
        self.clear_warnings()
    
    @property
    def name(self) -> str:
        """Get analyzer name."""
        return self._name
    
    @property
    def version(self) -> str:
        """Get analyzer version."""
        return self._metadata.get('version', 'unknown')


class BaseProcessor(BaseComponent, ProcessorInterface):
    """Base implementation of processor interface."""
    
    def __init__(self, name: str = None):
        """Initialize base processor."""
        super().__init__(name)
        self._supported_types = []
    
    def validate(self, data: Any) -> bool:
        """Validate input data."""
        if data is None:
            self.log_error("No data provided")
            return False
        
        return True
    
    def preprocess(self, data: Any) -> Any:
        """Default preprocessing - returns data unchanged."""
        self.log_info("Preprocessing data")
        return data
    
    def postprocess(self, data: Any) -> Any:
        """Default postprocessing - returns data unchanged."""
        self.log_info("Postprocessing data")
        return data
    
    @property
    def supported_types(self) -> List[str]:
        """Get list of supported data types."""
        return self._supported_types


class BaseExtractor(BaseComponent, ExtractorInterface):
    """Base implementation of extractor interface."""
    
    def __init__(self, name: str = None):
        """Initialize base extractor."""
        super().__init__(name)
        self._available_fields = []
        self._extraction_type = "base"
    
    def validate_source(self, source: Any) -> bool:
        """Validate data source."""
        if source is None:
            self.log_error("No source provided")
            return False
        
        return True
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Default transformation - returns data unchanged."""
        self.log_info("Transforming data")
        return data
    
    def get_available_fields(self) -> List[str]:
        """Get list of available fields to extract."""
        return self._available_fields.copy()
    
    @property
    def extraction_type(self) -> str:
        """Get extraction type identifier."""
        return self._extraction_type


class BaseWorkflow(BaseComponent, WorkflowInterface):
    """Base implementation of workflow interface."""
    
    def __init__(self, name: str = None):
        """Initialize base workflow."""
        super().__init__(name)
        self._steps = []
        self._step_registry = {}
        self._status = {
            'state': 'idle',
            'current_step': None,
            'completed_steps': [],
            'failed_steps': [],
            'total_steps': 0
        }
        self._is_paused = False
    
    def add_step(self, step: Union[AnalyzerInterface, ProcessorInterface]) -> None:
        """Add a step to the workflow."""
        step_id = f"{step.name}_{len(self._steps)}"
        self._steps.append(step_id)
        self._step_registry[step_id] = step
        self._status['total_steps'] = len(self._steps)
        self.log_info(f"Added step: {step_id}")
    
    def remove_step(self, step_id: str) -> bool:
        """Remove a step from the workflow."""
        if step_id not in self._step_registry:
            self.log_warning(f"Step not found: {step_id}")
            return False
        
        self._steps.remove(step_id)
        del self._step_registry[step_id]
        self._status['total_steps'] = len(self._steps)
        self.log_info(f"Removed step: {step_id}")
        return True
    
    def execute(self) -> Dict[str, Any]:
        """Execute the complete workflow."""
        self.log_info(f"Executing workflow: {self._name}")
        self._status['state'] = 'running'
        results = {}
        
        for step_id in self._steps:
            if self._is_paused:
                self._status['state'] = 'paused'
                break
            
            self._status['current_step'] = step_id
            step = self._step_registry[step_id]
            
            try:
                self.log_info(f"Executing step: {step_id}")
                
                if isinstance(step, AnalyzerInterface):
                    step_result = step.analyze(None)  # Model would be passed here
                elif isinstance(step, ProcessorInterface):
                    step_result = step.process(None)  # Data would be passed here
                else:
                    raise AnalysisError(f"Unknown step type: {type(step)}")
                
                results[step_id] = step_result
                self._status['completed_steps'].append(step_id)
                
            except Exception as e:
                self.log_error(f"Step {step_id} failed: {str(e)}")
                self._status['failed_steps'].append(step_id)
                self._status['state'] = 'failed'
                raise AnalysisError(f"Workflow failed at step {step_id}: {str(e)}")
        
        if not self._is_paused:
            self._status['state'] = 'completed'
        
        self.log_info(f"Workflow {self._name} completed")
        return results
    
    def pause(self) -> bool:
        """Pause workflow execution."""
        if self._status['state'] != 'running':
            self.log_warning("Cannot pause - workflow not running")
            return False
        
        self._is_paused = True
        self.log_info("Workflow paused")
        return True
    
    def resume(self) -> bool:
        """Resume workflow execution."""
        if not self._is_paused:
            self.log_warning("Cannot resume - workflow not paused")
            return False
        
        self._is_paused = False
        self._status['state'] = 'running'
        self.log_info("Workflow resumed")
        return True
    
    def get_status(self) -> Dict[str, Any]:
        """Get workflow execution status."""
        return self._status.copy()
    
    @property
    def steps(self) -> List[str]:
        """Get list of workflow steps."""
        return self._steps.copy()