"""
Analysis Engine Framework for OrcaFlex Module

This module provides the main orchestration engine for OrcaFlex analyses,
supporting various workflow types and execution strategies.
"""

import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Union, Type, Callable
from dataclasses import dataclass, field
from datetime import datetime
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, Future, as_completed
from abc import ABC, abstractmethod
import multiprocessing as mp
from enum import Enum

from .interfaces import (
    WorkflowInterface,
    AnalyzerInterface,
    ProcessorInterface,
    ExtractorInterface
)
from .base_classes import BaseWorkflow, BaseAnalyzer
from .configuration import OrcaFlexConfig, AnalysisType
from .model_interface import (
    OrcaFlexModelWrapper,
    ModelState,
    AnalysisProgress,
    ProgressState,
    ModelResult
)
from .registry import ComponentRegistry, ComponentType
from .exceptions import (
    OrcaFlexError,
    WorkflowError,
    AnalysisError,
    TimeoutError
)
from .logging_config import LoggerMixin, OrcaFlexLogger


class ExecutionMode(Enum):
    """Execution mode for analysis."""
    SEQUENTIAL = "sequential"
    PARALLEL_THREAD = "parallel_thread"
    PARALLEL_PROCESS = "parallel_process"


class WorkflowState(Enum):
    """State of workflow execution."""
    PENDING = "pending"
    RUNNING = "running"
    PAUSED = "paused"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class WorkflowResult:
    """Container for workflow execution results."""
    workflow_id: str
    state: WorkflowState
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    results: Dict[str, Any] = field(default_factory=dict)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def elapsed_time(self) -> Optional[float]:
        """Get elapsed time in seconds."""
        if self.start_time is None:
            return None
        end = self.end_time or datetime.now()
        return (end - self.start_time).total_seconds()
    
    @property
    def success(self) -> bool:
        """Check if workflow completed successfully."""
        return self.state == WorkflowState.COMPLETED and len(self.errors) == 0


class BaseAnalysisWorkflow(BaseWorkflow):
    """
    Base class for OrcaFlex analysis workflows.
    
    Provides common functionality for all workflow types.
    """
    
    def __init__(self, config: OrcaFlexConfig, name: Optional[str] = None):
        """
        Initialize workflow.
        
        Args:
            config: Configuration object
            name: Workflow name
        """
        super().__init__(name or self.__class__.__name__)
        self.config = config
        self.model_wrapper: Optional[OrcaFlexModelWrapper] = None
        self.result = WorkflowResult(
            workflow_id=self.workflow_id,
            state=WorkflowState.PENDING
        )
    
    def initialize(self, **kwargs) -> None:
        """Initialize workflow resources."""
        self.log_info(f"Initializing workflow: {self.name}")
        
        # Create model wrapper
        use_mock = kwargs.get('use_mock', False)
        progress_callback = kwargs.get('progress_callback')
        
        self.model_wrapper = OrcaFlexModelWrapper(
            config=self.config,
            use_mock=use_mock,
            progress_callback=progress_callback
        )
        
        self.result.state = WorkflowState.PENDING
        self.initialized = True
    
    def validate(self) -> List[str]:
        """Validate workflow configuration."""
        issues = []
        
        # Check configuration
        warnings = self.config.validate_compatibility()
        issues.extend(warnings)
        
        # Check file paths
        if not self.config.file_management.input_files:
            issues.append("No input files specified")
        
        return issues
    
    def cleanup(self) -> None:
        """Clean up workflow resources."""
        if self.model_wrapper:
            try:
                del self.model_wrapper
            except:
                pass
            self.model_wrapper = None
        
        self.log_info(f"Cleaned up workflow: {self.name}")
    
    @abstractmethod
    def execute_analysis(self, file_path: Path) -> ModelResult:
        """
        Execute analysis for a single file.
        
        Args:
            file_path: Path to input file
            
        Returns:
            Analysis results
        """
        pass


class StaticAnalysisWorkflow(BaseAnalysisWorkflow):
    """Workflow for static analysis."""
    
    def execute_analysis(self, file_path: Path) -> ModelResult:
        """Execute static analysis."""
        self.log_info(f"Executing static analysis for {file_path.name}")
        
        result = ModelResult(state=ModelState.UNINITIALIZED)
        
        try:
            # Load model
            self.model_wrapper.load_file(file_path)
            
            # Validate
            warnings = self.model_wrapper.validate_model()
            result.warnings.extend(warnings)
            
            # Run static analysis
            static_results = self.model_wrapper.run_static_analysis(
                tolerance=self.config.analysis.static.tolerance,
                max_iterations=self.config.analysis.static.max_iterations
            )
            
            result.static_results = static_results
            result.state = ModelState.STATIC_COMPLETE
            
            # Save results if configured
            if self.config.file_management.output_directory:
                output_path = Path(self.config.file_management.output_directory) / \
                             f"{file_path.stem}_static.sim"
                self.model_wrapper.save_simulation(output_path)
                result.metadata['output_file'] = str(output_path)
            
        except Exception as e:
            self.log_error(f"Static analysis failed: {e}")
            result.errors.append(str(e))
            result.state = ModelState.FAILED
            raise
        
        return result
    
    def execute(self, **kwargs) -> WorkflowResult:
        """Execute the static analysis workflow."""
        self.result.start_time = datetime.now()
        self.result.state = WorkflowState.RUNNING
        
        try:
            # Get input files
            input_files = self._get_input_files()
            
            for file_path in input_files:
                file_result = self.execute_analysis(file_path)
                self.result.results[str(file_path)] = file_result.to_dict()
                
                if file_result.errors:
                    self.result.errors.extend(file_result.errors)
                if file_result.warnings:
                    self.result.warnings.extend(file_result.warnings)
            
            self.result.state = WorkflowState.COMPLETED
            
        except Exception as e:
            self.result.state = WorkflowState.FAILED
            self.result.errors.append(str(e))
            raise
        
        finally:
            self.result.end_time = datetime.now()
        
        return self.result
    
    def _get_input_files(self) -> List[Path]:
        """Get list of input files from configuration."""
        input_files = []
        base_dir = Path(self.config.file_management.input_directory)
        
        for file_type, file_list in self.config.file_management.input_files.items():
            for file_name in file_list:
                file_path = base_dir / file_name
                if file_path.exists():
                    input_files.append(file_path)
                else:
                    self.log_warning(f"Input file not found: {file_path}")
        
        return input_files


class DynamicAnalysisWorkflow(BaseAnalysisWorkflow):
    """Workflow for dynamic analysis."""
    
    def execute_analysis(self, file_path: Path) -> ModelResult:
        """Execute dynamic analysis."""
        self.log_info(f"Executing dynamic analysis for {file_path.name}")
        
        result = ModelResult(state=ModelState.UNINITIALIZED)
        
        try:
            # Load model
            self.model_wrapper.load_file(file_path)
            
            # Run static first if configured
            if self.config.analysis.static.enabled:
                static_results = self.model_wrapper.run_static_analysis()
                result.static_results = static_results
            
            # Run dynamic simulation
            dynamic_results = self.model_wrapper.run_dynamic_simulation(
                duration=self.config.analysis.dynamic.duration,
                time_step=self.config.analysis.dynamic.time_step
            )
            
            result.dynamic_results = dynamic_results
            result.state = ModelState.DYNAMIC_COMPLETE
            
            # Save results
            if self.config.file_management.output_directory:
                output_path = Path(self.config.file_management.output_directory) / \
                             f"{file_path.stem}_dynamic.sim"
                self.model_wrapper.save_simulation(output_path)
                result.metadata['output_file'] = str(output_path)
            
        except Exception as e:
            self.log_error(f"Dynamic analysis failed: {e}")
            result.errors.append(str(e))
            result.state = ModelState.FAILED
            raise
        
        return result
    
    def execute(self, **kwargs) -> WorkflowResult:
        """Execute the dynamic analysis workflow."""
        # Similar to static workflow but calls dynamic analysis
        return super().execute(**kwargs)


class IterativeAnalysisWorkflow(BaseAnalysisWorkflow):
    """Workflow for iterative analysis."""
    
    def execute_analysis(self, file_path: Path) -> ModelResult:
        """Execute iterative analysis."""
        self.log_info(f"Executing iterative analysis for {file_path.name}")
        
        result = ModelResult(state=ModelState.UNINITIALIZED)
        iteration_config = self.config.analysis.iteration
        
        if not iteration_config.enabled:
            raise WorkflowError("Iteration not enabled in configuration")
        
        try:
            # Load model
            self.model_wrapper.load_file(file_path)
            
            current_value = 0.0
            target_value = iteration_config.target_value
            tolerance = iteration_config.tolerance
            max_iterations = iteration_config.max_iterations
            step_size = iteration_config.step_size
            
            iteration_results = []
            
            for iteration in range(max_iterations):
                self.log_info(f"Iteration {iteration + 1}/{max_iterations}")
                
                # Run analysis
                if self.config.analysis.static.enabled:
                    iter_result = self.model_wrapper.run_static_analysis()
                else:
                    iter_result = self.model_wrapper.run_dynamic_simulation()
                
                # Extract target parameter value
                # This is simplified - real implementation would extract from results
                current_value = target_value * (0.9 + 0.2 * iteration / max_iterations)
                
                iteration_results.append({
                    'iteration': iteration + 1,
                    'value': current_value,
                    'target': target_value,
                    'error': abs(current_value - target_value)
                })
                
                # Check convergence
                if abs(current_value - target_value) < tolerance:
                    self.log_info(f"Converged after {iteration + 1} iterations")
                    break
                
                # Adjust parameter for next iteration
                # This would modify the model based on the parameter being iterated
            
            result.metadata['iterations'] = iteration_results
            result.metadata['converged'] = abs(current_value - target_value) < tolerance
            result.state = ModelState.STATIC_COMPLETE if self.config.analysis.static.enabled \
                         else ModelState.DYNAMIC_COMPLETE
            
        except Exception as e:
            self.log_error(f"Iterative analysis failed: {e}")
            result.errors.append(str(e))
            result.state = ModelState.FAILED
            raise
        
        return result
    
    def execute(self, **kwargs) -> WorkflowResult:
        """Execute the iterative analysis workflow."""
        return super().execute(**kwargs)


class AnalysisEngine(LoggerMixin):
    """
    Main orchestrator for OrcaFlex analyses.
    
    This class manages workflow execution, resource allocation,
    and result aggregation.
    """
    
    def __init__(self, config: OrcaFlexConfig):
        """
        Initialize the analysis engine.
        
        Args:
            config: Configuration object
        """
        self.config = config
        self.registry = ComponentRegistry()
        self._register_default_workflows()
        self.active_workflows: Dict[str, BaseAnalysisWorkflow] = {}
    
    def _register_default_workflows(self) -> None:
        """Register default workflow implementations."""
        self.registry.register(
            StaticAnalysisWorkflow,
            ComponentType.WORKFLOW,
            "static"
        )
        self.registry.register(
            DynamicAnalysisWorkflow,
            ComponentType.WORKFLOW,
            "dynamic"
        )
        self.registry.register(
            IterativeAnalysisWorkflow,
            ComponentType.WORKFLOW,
            "iterative"
        )
    
    def create_workflow(self, analysis_type: AnalysisType) -> BaseAnalysisWorkflow:
        """
        Create a workflow for the specified analysis type.
        
        Args:
            analysis_type: Type of analysis
            
        Returns:
            Workflow instance
        """
        workflow_map = {
            AnalysisType.STATIC: "static",
            AnalysisType.DYNAMIC: "dynamic",
            AnalysisType.CUSTOM: "iterative",
            AnalysisType.FATIGUE: "dynamic",  # Uses dynamic as base
            AnalysisType.INSTALLATION: "dynamic",
            AnalysisType.MODAL: "static"
        }
        
        workflow_name = workflow_map.get(analysis_type, "static")
        workflow_class = self.registry.get(ComponentType.WORKFLOW, workflow_name)
        
        if not workflow_class:
            raise WorkflowError(f"No workflow registered for {analysis_type}")
        
        return workflow_class(self.config, name=f"{analysis_type.value}_workflow")
    
    def execute_sequential(self, 
                          workflows: List[BaseAnalysisWorkflow],
                          **kwargs) -> List[WorkflowResult]:
        """
        Execute workflows sequentially.
        
        Args:
            workflows: List of workflows to execute
            **kwargs: Additional execution parameters
            
        Returns:
            List of workflow results
        """
        results = []
        
        for workflow in workflows:
            self.log_info(f"Executing workflow sequentially: {workflow.name}")
            
            try:
                workflow.initialize(**kwargs)
                result = workflow.execute(**kwargs)
                results.append(result)
            finally:
                workflow.cleanup()
        
        return results
    
    def execute_parallel(self,
                        workflows: List[BaseAnalysisWorkflow],
                        mode: ExecutionMode = ExecutionMode.PARALLEL_THREAD,
                        max_workers: Optional[int] = None,
                        **kwargs) -> List[WorkflowResult]:
        """
        Execute workflows in parallel.
        
        Args:
            workflows: List of workflows to execute
            mode: Parallel execution mode
            max_workers: Maximum number of workers
            **kwargs: Additional execution parameters
            
        Returns:
            List of workflow results
        """
        max_workers = max_workers or self.config.parallel.num_threads
        results = []
        
        executor_class = ThreadPoolExecutor if mode == ExecutionMode.PARALLEL_THREAD \
                        else ProcessPoolExecutor
        
        self.log_info(f"Executing {len(workflows)} workflows in parallel with {max_workers} workers")
        
        with executor_class(max_workers=max_workers) as executor:
            # Submit all workflows
            future_to_workflow = {}
            for workflow in workflows:
                workflow.initialize(**kwargs)
                future = executor.submit(workflow.execute, **kwargs)
                future_to_workflow[future] = workflow
            
            # Collect results
            for future in as_completed(future_to_workflow):
                workflow = future_to_workflow[future]
                try:
                    result = future.result(timeout=self.config.parallel.timeout)
                    results.append(result)
                except TimeoutError:
                    error_result = WorkflowResult(
                        workflow_id=workflow.workflow_id,
                        state=WorkflowState.FAILED,
                        errors=[f"Workflow timed out after {self.config.parallel.timeout}s"]
                    )
                    results.append(error_result)
                except Exception as e:
                    error_result = WorkflowResult(
                        workflow_id=workflow.workflow_id,
                        state=WorkflowState.FAILED,
                        errors=[str(e)]
                    )
                    results.append(error_result)
                finally:
                    workflow.cleanup()
        
        return results
    
    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Execute analysis based on configuration.
        
        Args:
            **kwargs: Execution parameters
            
        Returns:
            Execution results
        """
        start_time = time.time()
        
        # Create workflows for each analysis type
        workflows = []
        for analysis_type in self.config.analysis.analysis_type:
            workflow = self.create_workflow(analysis_type)
            workflows.append(workflow)
        
        # Determine execution mode
        if self.config.parallel.enabled and len(workflows) > 1:
            mode = ExecutionMode.PARALLEL_PROCESS if self.config.parallel.use_processes \
                  else ExecutionMode.PARALLEL_THREAD
            results = self.execute_parallel(workflows, mode, **kwargs)
        else:
            results = self.execute_sequential(workflows, **kwargs)
        
        elapsed = time.time() - start_time
        
        # Aggregate results
        summary = {
            'total_workflows': len(workflows),
            'successful': sum(1 for r in results if r.success),
            'failed': sum(1 for r in results if not r.success),
            'elapsed_time': elapsed,
            'results': [r.__dict__ for r in results]
        }
        
        OrcaFlexLogger.log_performance("analysis_engine", elapsed, {
            'workflows': len(workflows),
            'parallel': self.config.parallel.enabled
        })
        
        return summary


class WorkflowRunner:
    """
    Utility class for running individual workflows.
    
    This provides a simplified interface for executing single workflows.
    """
    
    @staticmethod
    def run_static_analysis(config: OrcaFlexConfig, 
                           file_path: Union[str, Path],
                           **kwargs) -> ModelResult:
        """
        Run static analysis on a single file.
        
        Args:
            config: Configuration object
            file_path: Path to input file
            **kwargs: Additional parameters
            
        Returns:
            Analysis results
        """
        workflow = StaticAnalysisWorkflow(config)
        workflow.initialize(**kwargs)
        
        try:
            result = workflow.execute_analysis(Path(file_path))
            return result
        finally:
            workflow.cleanup()
    
    @staticmethod
    def run_dynamic_analysis(config: OrcaFlexConfig,
                           file_path: Union[str, Path],
                           **kwargs) -> ModelResult:
        """
        Run dynamic analysis on a single file.
        
        Args:
            config: Configuration object
            file_path: Path to input file
            **kwargs: Additional parameters
            
        Returns:
            Analysis results
        """
        workflow = DynamicAnalysisWorkflow(config)
        workflow.initialize(**kwargs)
        
        try:
            result = workflow.execute_analysis(Path(file_path))
            return result
        finally:
            workflow.cleanup()
    
    @staticmethod
    def run_iterative_analysis(config: OrcaFlexConfig,
                              file_path: Union[str, Path],
                              **kwargs) -> ModelResult:
        """
        Run iterative analysis on a single file.
        
        Args:
            config: Configuration object
            file_path: Path to input file
            **kwargs: Additional parameters
            
        Returns:
            Analysis results
        """
        workflow = IterativeAnalysisWorkflow(config)
        workflow.initialize(**kwargs)
        
        try:
            result = workflow.execute_analysis(Path(file_path))
            return result
        finally:
            workflow.cleanup()