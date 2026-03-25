"""
OrcFxAPI Abstraction Layer

This module provides a comprehensive abstraction layer over the OrcFxAPI,
offering consistent error handling, progress tracking, and mock support for testing.
"""

import os
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Union, Callable, Iterator
from dataclasses import dataclass, field
from datetime import datetime
from contextlib import contextmanager
from enum import Enum
import threading
from concurrent.futures import ThreadPoolExecutor, Future

from .interfaces import ModelInterface, ResultInterface
from .exceptions import (
    OrcaFlexError,
    ModelError,
    LicenseError,
    AnalysisError,
    FileError,
    TimeoutError
)
from .logging_config import LoggerMixin, OrcaFlexLogger
from .configuration import OrcaFlexConfig, AnalysisType


# Try to import OrcFxAPI, but provide mock if not available
try:
    import OrcFxAPI
    ORCFXAPI_AVAILABLE = True
except ImportError:
    OrcFxAPI = None
    ORCFXAPI_AVAILABLE = False


class ModelState(Enum):
    """Enumeration of model states."""
    UNINITIALIZED = "uninitialized"
    LOADED = "loaded"
    STATIC_COMPLETE = "static_complete"
    DYNAMIC_COMPLETE = "dynamic_complete"
    FAILED = "failed"


class ProgressState(Enum):
    """Progress tracking states."""
    NOT_STARTED = "not_started"
    INITIALIZING = "initializing"
    RUNNING = "running"
    FINALIZING = "finalizing"
    COMPLETE = "complete"
    CANCELLED = "cancelled"
    ERROR = "error"


@dataclass
class AnalysisProgress:
    """Progress tracking for analysis operations."""
    state: ProgressState = ProgressState.NOT_STARTED
    current_step: int = 0
    total_steps: int = 0
    percent_complete: float = 0.0
    message: str = ""
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    errors: List[str] = field(default_factory=list)
    
    def update(self, 
               step: Optional[int] = None,
               message: Optional[str] = None,
               state: Optional[ProgressState] = None) -> None:
        """Update progress information."""
        if step is not None:
            self.current_step = step
            if self.total_steps > 0:
                self.percent_complete = (step / self.total_steps) * 100
        
        if message is not None:
            self.message = message
        
        if state is not None:
            self.state = state
            if state == ProgressState.RUNNING and self.start_time is None:
                self.start_time = datetime.now()
            elif state in [ProgressState.COMPLETE, ProgressState.ERROR, ProgressState.CANCELLED]:
                self.end_time = datetime.now()
    
    @property
    def elapsed_time(self) -> Optional[float]:
        """Get elapsed time in seconds."""
        if self.start_time is None:
            return None
        
        end = self.end_time or datetime.now()
        return (end - self.start_time).total_seconds()
    
    @property
    def is_complete(self) -> bool:
        """Check if analysis is complete."""
        return self.state in [ProgressState.COMPLETE, ProgressState.ERROR, ProgressState.CANCELLED]


@dataclass
class ModelResult:
    """Container for model analysis results."""
    state: ModelState
    static_results: Optional[Dict[str, Any]] = None
    dynamic_results: Optional[Dict[str, Any]] = None
    warnings: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation."""
        return {
            'state': self.state.value,
            'static_results': self.static_results,
            'dynamic_results': self.dynamic_results,
            'warnings': self.warnings,
            'errors': self.errors,
            'metadata': self.metadata
        }


class OrcaFlexModelWrapper(LoggerMixin):
    """
    Wrapper for OrcFxAPI.Model providing consistent interface and error handling.
    
    This class abstracts all OrcFxAPI interactions, providing:
    - Consistent error handling with detailed context
    - Progress tracking for long-running operations
    - License checking and validation
    - Mock support for testing without OrcaFlex
    """
    
    def __init__(self, 
                 config: Optional[OrcaFlexConfig] = None,
                 use_mock: bool = False,
                 progress_callback: Optional[Callable[[AnalysisProgress], None]] = None):
        """
        Initialize the model wrapper.
        
        Args:
            config: Configuration object
            use_mock: Force use of mock implementation
            progress_callback: Callback for progress updates
        """
        self.config = config or OrcaFlexConfig()
        
        # Check environment variables for test control
        force_mock = os.environ.get('ORCAFLEX_FORCE_MOCK', '').lower() in ('1', 'true', 'yes')
        skip_real = os.environ.get('ORCAFLEX_SKIP_REAL', '').lower() in ('1', 'true', 'yes')
        
        # Determine mock usage with environment variable override
        if force_mock or skip_real:
            self.use_mock = True
        else:
            self.use_mock = use_mock or not ORCFXAPI_AVAILABLE
        
        self.progress_callback = progress_callback
        
        self._model = None
        self._state = ModelState.UNINITIALIZED
        self._progress = AnalysisProgress()
        self._file_path: Optional[Path] = None
        
        # Initialize logger
        self.log_info(f"Initialized OrcaFlexModelWrapper (mock={self.use_mock})")
        
        # Check license if using real OrcFxAPI
        if not self.use_mock:
            self._check_license()
    
    def _check_license(self) -> None:
        """Check OrcaFlex license availability."""
        try:
            if not ORCFXAPI_AVAILABLE:
                raise LicenseError(
                    "OrcFxAPI module not available",
                    error_code="NO_MODULE",
                    suggestions=["Install OrcaFlex", "Use mock mode for testing"]
                )
            
            # Try to create a model to check license
            test_model = OrcFxAPI.Model()
            del test_model
            self.log_info("OrcaFlex license check passed")
            
        except Exception as e:
            if "license" in str(e).lower() or "OrcFxAPI" not in str(e):
                raise LicenseError(
                    f"OrcaFlex license not available: {e}",
                    error_code="NO_LICENSE",
                    suggestions=[
                        "Check license server connection",
                        "Verify license is available",
                        "Use mock mode for testing"
                    ]
                )
            raise
    
    @contextmanager
    def _error_handler(self, operation: str):
        """Context manager for consistent error handling."""
        try:
            yield
        except LicenseError:
            raise
        except FileError:
            raise
        except Exception as e:
            self.log_error(f"Error during {operation}: {e}", exc_info=True)
            self._state = ModelState.FAILED
            raise ModelError(
                f"Model operation failed: {operation}",
                error_code="MODEL_ERROR",
                context={
                    'operation': operation,
                    'error': str(e),
                    'state': self._state.value
                }
            )
    
    def _update_progress(self, 
                        step: Optional[int] = None,
                        message: Optional[str] = None,
                        state: Optional[ProgressState] = None) -> None:
        """Update and notify progress."""
        self._progress.update(step, message, state)
        
        if self.progress_callback:
            self.progress_callback(self._progress)
        
        if message:
            self.log_info(f"Progress: {message}")
    
    def load_file(self, file_path: Union[str, Path]) -> None:
        """
        Load an OrcaFlex model file.
        
        Args:
            file_path: Path to .dat, .sim, or .yml file
            
        Raises:
            FileError: If file doesn't exist or is invalid
            ModelError: If loading fails
        """
        file_path = Path(file_path)
        
        if not file_path.exists():
            raise FileError(
                f"Model file not found: {file_path}",
                error_code="FILE_NOT_FOUND",
                suggestions=["Check file path", "Verify file exists"]
            )
        
        valid_extensions = {'.dat', '.sim', '.yml', '.yaml'}
        if file_path.suffix.lower() not in valid_extensions:
            raise FileError(
                f"Invalid file extension: {file_path.suffix}",
                error_code="INVALID_EXTENSION",
                context={'valid_extensions': list(valid_extensions)}
            )
        
        with self._error_handler(f"loading file {file_path.name}"):
            self._update_progress(
                state=ProgressState.INITIALIZING,
                message=f"Loading {file_path.name}"
            )
            
            if self.use_mock:
                self._model = MockOrcaFlexModel()
                self._model.LoadData(str(file_path))
            else:
                self._model = OrcFxAPI.Model()
                self._model.LoadData(str(file_path))
            
            self._file_path = file_path
            self._state = ModelState.LOADED
            
            self._update_progress(
                state=ProgressState.COMPLETE,
                message=f"Successfully loaded {file_path.name}"
            )
            
            self.log_info(f"Model loaded from {file_path}")
    
    def validate_model(self) -> List[str]:
        """
        Validate the loaded model.
        
        Returns:
            List of validation warnings
            
        Raises:
            ModelError: If model not loaded
        """
        if self._state == ModelState.UNINITIALIZED:
            raise ModelError(
                "No model loaded for validation",
                error_code="NO_MODEL",
                suggestions=["Load a model file first"]
            )
        
        warnings = []
        
        with self._error_handler("model validation"):
            if not self.use_mock:
                # Check for common issues
                try:
                    # Check general data
                    general = self._model.general
                    if general.StageDuration <= 0:
                        warnings.append("Stage duration is not positive")
                    
                    # Check environment
                    env = self._model.environment
                    if env.WaterDepth <= 0:
                        warnings.append("Water depth is not positive")
                    
                except Exception as e:
                    warnings.append(f"Validation warning: {e}")
        
        self.log_info(f"Model validation complete: {len(warnings)} warnings")
        return warnings
    
    def run_static_analysis(self, 
                           tolerance: Optional[float] = None,
                           max_iterations: Optional[int] = None) -> Dict[str, Any]:
        """
        Run static analysis on the model.
        
        Args:
            tolerance: Convergence tolerance (uses config if not specified)
            max_iterations: Maximum iterations (uses config if not specified)
            
        Returns:
            Static analysis results
            
        Raises:
            AnalysisError: If analysis fails
        """
        if self._state == ModelState.UNINITIALIZED:
            raise ModelError("No model loaded for analysis")
        
        tolerance = tolerance or self.config.analysis.static.tolerance
        max_iterations = max_iterations or self.config.analysis.static.max_iterations
        
        with self._error_handler("static analysis"):
            self._update_progress(
                state=ProgressState.RUNNING,
                message="Starting static analysis",
                step=0
            )
            
            start_time = time.time()
            
            if self.use_mock:
                # Mock static analysis
                time.sleep(0.1)  # Simulate processing
                converged = True
                iterations = 5
            else:
                # Real static analysis
                self._model.CalculateStatics()
                
                # Get convergence info
                converged = True  # OrcaFlex throws if not converged
                iterations = getattr(self._model.state, 'StaticsIterationCount', 0)
            
            elapsed = time.time() - start_time
            
            self._state = ModelState.STATIC_COMPLETE
            
            results = {
                'converged': converged,
                'iterations': iterations,
                'tolerance': tolerance,
                'elapsed_time': elapsed,
                'timestamp': datetime.now().isoformat()
            }
            
            self._update_progress(
                state=ProgressState.COMPLETE,
                message=f"Static analysis complete in {elapsed:.2f}s",
                step=100
            )
            
            OrcaFlexLogger.log_performance("static_analysis", elapsed, {
                'iterations': iterations,
                'file': str(self._file_path)
            })
            
            return results
    
    def run_dynamic_simulation(self,
                              duration: Optional[float] = None,
                              time_step: Optional[float] = None,
                              progress_interval: float = 1.0) -> Dict[str, Any]:
        """
        Run dynamic simulation on the model.
        
        Args:
            duration: Simulation duration (uses config if not specified)
            time_step: Time step (uses config if not specified)
            progress_interval: Progress update interval in seconds
            
        Returns:
            Dynamic simulation results
            
        Raises:
            AnalysisError: If simulation fails
        """
        if self._state not in [ModelState.LOADED, ModelState.STATIC_COMPLETE]:
            raise ModelError(
                f"Model must be loaded or have completed statics. Current state: {self._state}",
                suggestions=["Load a model", "Run static analysis first"]
            )
        
        duration = duration or self.config.analysis.dynamic.duration
        time_step = time_step or self.config.analysis.dynamic.time_step
        
        with self._error_handler("dynamic simulation"):
            self._progress = AnalysisProgress(
                total_steps=int(duration / progress_interval),
                state=ProgressState.RUNNING,
                message="Starting dynamic simulation"
            )
            
            start_time = time.time()
            
            if self.use_mock:
                # Mock dynamic simulation with progress
                for i in range(self._progress.total_steps):
                    time.sleep(0.01)  # Simulate processing
                    self._update_progress(
                        step=i + 1,
                        message=f"Simulating: {((i+1)/self._progress.total_steps)*100:.1f}%"
                    )
            else:
                # Real dynamic simulation
                # Set simulation parameters
                general = self._model.general
                general.StageDuration = duration
                general.InnerTimeStep = time_step
                
                # Run simulation with progress tracking
                def progress_handler(progress_info):
                    if hasattr(progress_info, 'SimulationTime'):
                        percent = (progress_info.SimulationTime / duration) * 100
                        self._update_progress(
                            step=int(percent),
                            message=f"Simulation time: {progress_info.SimulationTime:.1f}s"
                        )
                
                # Run simulation
                self._model.RunSimulation()
            
            elapsed = time.time() - start_time
            
            self._state = ModelState.DYNAMIC_COMPLETE
            
            results = {
                'duration': duration,
                'time_step': time_step,
                'elapsed_time': elapsed,
                'timestamp': datetime.now().isoformat(),
                'simulation_speed': duration / elapsed if elapsed > 0 else 0
            }
            
            self._update_progress(
                state=ProgressState.COMPLETE,
                message=f"Dynamic simulation complete in {elapsed:.2f}s",
                step=self._progress.total_steps
            )
            
            OrcaFlexLogger.log_performance("dynamic_simulation", elapsed, {
                'duration': duration,
                'file': str(self._file_path)
            })
            
            return results
    
    def save_simulation(self, output_path: Union[str, Path]) -> None:
        """
        Save simulation results.
        
        Args:
            output_path: Path for output file
            
        Raises:
            ModelError: If no results to save
            FileError: If save fails
        """
        if self._state == ModelState.UNINITIALIZED:
            raise ModelError("No model to save")
        
        output_path = Path(output_path)
        
        # Create directory if needed
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with self._error_handler(f"saving to {output_path.name}"):
            if self.use_mock:
                # Mock save
                output_path.touch()
            else:
                # Determine save method based on extension
                if output_path.suffix.lower() == '.sim':
                    self._model.SaveSimulation(str(output_path))
                elif output_path.suffix.lower() in ['.dat', '.yml', '.yaml']:
                    self._model.SaveData(str(output_path))
                else:
                    raise FileError(f"Unknown output format: {output_path.suffix}")
            
            self.log_info(f"Model saved to {output_path}")
    
    def get_model_info(self) -> Dict[str, Any]:
        """
        Get information about the loaded model.
        
        Returns:
            Dictionary of model information
        """
        if self._state == ModelState.UNINITIALIZED:
            return {'state': 'uninitialized'}
        
        info = {
            'state': self._state.value,
            'file_path': str(self._file_path) if self._file_path else None,
            'is_mock': self.use_mock
        }
        
        if not self.use_mock and self._model:
            try:
                info.update({
                    'water_depth': self._model.environment.WaterDepth,
                    'stage_duration': self._model.general.StageDuration,
                    'num_objects': len(self._model.objects)
                })
            except:
                pass
        
        return info
    
    def extract_results(self, 
                       object_name: str,
                       variable_name: str,
                       period: Optional[int] = None) -> Any:
        """
        Extract results from the simulation.
        
        Args:
            object_name: Name of object to extract from
            variable_name: Variable to extract
            period: Time period (None for all)
            
        Returns:
            Extracted results
        """
        if self._state not in [ModelState.STATIC_COMPLETE, ModelState.DYNAMIC_COMPLETE]:
            raise ModelError("No results available for extraction")
        
        with self._error_handler(f"extracting {variable_name} from {object_name}"):
            if self.use_mock:
                # Return mock data
                import numpy as np
                return np.random.randn(100)
            else:
                obj = self._model[object_name]
                if period is None:
                    return obj.TimeHistory(variable_name)
                else:
                    return obj.TimeHistory(variable_name, period)
    
    @property
    def state(self) -> ModelState:
        """Get current model state."""
        return self._state
    
    @property
    def progress(self) -> AnalysisProgress:
        """Get current progress."""
        return self._progress
    
    @property
    def model(self):
        """Get underlying model (for advanced usage)."""
        return self._model
    
    def __enter__(self):
        """Context manager entry."""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - cleanup resources."""
        if self._model and not self.use_mock:
            try:
                del self._model
            except:
                pass
        self._model = None
        self._state = ModelState.UNINITIALIZED


class MockOrcaFlexModel:
    """Mock implementation of OrcaFlex model for testing."""
    
    def __init__(self):
        """Initialize mock model."""
        self.general = type('obj', (object,), {
            'StageDuration': 3600.0,
            'InnerTimeStep': 0.1
        })()
        self.environment = type('obj', (object,), {
            'WaterDepth': 100.0
        })()
        self.state = type('obj', (object,), {
            'StaticsIterationCount': 5
        })()
        self.objects = []
    
    def LoadData(self, filename: str) -> None:
        """Mock load data."""
        pass
    
    def CalculateStatics(self) -> None:
        """Mock static calculation."""
        time.sleep(0.1)
    
    def RunSimulation(self) -> None:
        """Mock dynamic simulation."""
        time.sleep(0.2)
    
    def SaveSimulation(self, filename: str) -> None:
        """Mock save simulation."""
        Path(filename).touch()
    
    def SaveData(self, filename: str) -> None:
        """Mock save data."""
        Path(filename).touch()
    
    def __getitem__(self, name: str):
        """Mock object access."""
        return type('obj', (object,), {
            'TimeHistory': lambda var, period=None: [0.0] * 100
        })()


# Convenience function for checking OrcaFlex availability
def check_orcaflex_available() -> Dict[str, bool]:
    """
    Check OrcaFlex availability.
    
    Returns:
        Dictionary with 'has_module' and 'has_license' flags
    """
    status = {'has_module': False, 'has_license': False}
    
    try:
        import OrcFxAPI
        status['has_module'] = True
        
        try:
            model = OrcFxAPI.Model()
            del model
            status['has_license'] = True
        except:
            pass
    except ImportError:
        pass
    
    return status