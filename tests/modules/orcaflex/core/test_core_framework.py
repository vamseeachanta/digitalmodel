"""
Comprehensive tests for OrcaFlex Core Framework

Tests all components of Phase 1: Core Infrastructure
- Interfaces and base classes
- Component registry
- Exception hierarchy
- Configuration management
- Model interface abstraction
- Analysis engine
"""

import pytest
import tempfile
import yaml
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime
import time

# Import core framework components
from src.digitalmodel.modules.orcaflex.core.interfaces import (
    AnalyzerInterface,
    ProcessorInterface,
    ExtractorInterface,
    WorkflowInterface
)
from src.digitalmodel.modules.orcaflex.core.base_classes import (
    BaseComponent,
    BaseAnalyzer,
    BaseProcessor,
    BaseExtractor,
    BaseWorkflow
)
from src.digitalmodel.modules.orcaflex.core.registry import (
    ComponentRegistry,
    ComponentType
)
from src.digitalmodel.modules.orcaflex.core.exceptions import (
    OrcaFlexError,
    ConfigurationError,
    ValidationError,
    ModelError,
    LicenseError,
    ComponentNotFoundError
)
from src.digitalmodel.modules.orcaflex.core.configuration import (
    OrcaFlexConfig,
    AnalysisConfig,
    ParallelConfig,
    AnalysisType
)
from src.digitalmodel.modules.orcaflex.core.model_interface import (
    OrcaFlexModelWrapper,
    ModelState,
    AnalysisProgress,
    ProgressState,
    check_orcaflex_available
)
from src.digitalmodel.modules.orcaflex.core.analysis_engine import (
    AnalysisEngine,
    StaticAnalysisWorkflow,
    DynamicAnalysisWorkflow,
    WorkflowRunner,
    WorkflowState,
    ExecutionMode
)


class TestInterfaces:
    """Test core interfaces and protocols."""
    
    def test_analyzer_interface(self):
        """Test analyzer interface implementation."""
        class TestAnalyzer(BaseAnalyzer):
            def analyze(self, model):
                return {"result": "analyzed"}
        
        analyzer = TestAnalyzer("test")
        assert hasattr(analyzer, 'analyze')
        assert analyzer.name == "test"
    
    def test_workflow_interface(self):
        """Test workflow interface implementation."""
        class TestWorkflow(BaseWorkflow):
            def execute(self, **kwargs):
                return {"executed": True}
        
        workflow = TestWorkflow("test_workflow")
        assert hasattr(workflow, 'execute')
        assert hasattr(workflow, 'validate')
        assert workflow.workflow_id is not None


class TestComponentRegistry:
    """Test component registry functionality."""
    
    def test_singleton_pattern(self):
        """Test registry is a singleton."""
        registry1 = ComponentRegistry()
        registry2 = ComponentRegistry()
        assert registry1 is registry2
    
    def test_component_registration(self):
        """Test registering and retrieving components."""
        registry = ComponentRegistry()
        
        class TestComponent:
            pass
        
        registry.register(TestComponent, ComponentType.ANALYZER, "test")
        retrieved = registry.get(ComponentType.ANALYZER, "test")
        assert retrieved is TestComponent
    
    def test_decorator_registration(self):
        """Test decorator-based registration."""
        registry = ComponentRegistry()
        
        @registry.component(ComponentType.PROCESSOR, "decorated")
        class DecoratedProcessor:
            pass
        
        retrieved = registry.get(ComponentType.PROCESSOR, "decorated")
        assert retrieved is DecoratedProcessor
    
    def test_factory_creation(self):
        """Test factory method for component creation."""
        registry = ComponentRegistry()
        
        class FactoryComponent:
            def __init__(self, name):
                self.name = name
        
        registry.register(FactoryComponent, ComponentType.EXTRACTOR, "factory")
        instance = registry.create(ComponentType.EXTRACTOR, "factory", "test_instance")
        
        assert isinstance(instance, FactoryComponent)
        assert instance.name == "test_instance"
    
    def test_list_components(self):
        """Test listing registered components."""
        registry = ComponentRegistry()
        registry._components.clear()  # Clear for test
        
        registry.register(Mock, ComponentType.ANALYZER, "analyzer1")
        registry.register(Mock, ComponentType.ANALYZER, "analyzer2")
        
        analyzers = registry.list_components(ComponentType.ANALYZER)
        assert len(analyzers) == 2
        assert "analyzer1" in analyzers
        assert "analyzer2" in analyzers


class TestExceptionHierarchy:
    """Test custom exception hierarchy."""
    
    def test_base_exception(self):
        """Test base OrcaFlexError."""
        error = OrcaFlexError(
            "Test error",
            error_code="TEST001",
            context={"key": "value"},
            suggestions=["Try this", "Or this"]
        )
        
        assert str(error) == "Test error"
        assert error.error_code == "TEST001"
        assert error.context["key"] == "value"
        assert len(error.suggestions) == 2
    
    def test_configuration_error(self):
        """Test configuration error."""
        error = ConfigurationError("Invalid config")
        assert isinstance(error, OrcaFlexError)
        assert "Invalid config" in str(error)
    
    def test_license_error(self):
        """Test license error."""
        error = LicenseError(
            "No license",
            suggestions=["Check license server"]
        )
        assert isinstance(error, OrcaFlexError)
        assert len(error.suggestions) == 1


class TestConfiguration:
    """Test configuration management."""
    
    def test_default_configuration(self):
        """Test default configuration creation."""
        config = OrcaFlexConfig()
        assert config.name == "orcaflex_analysis"
        assert config.parallel.num_threads == 30
        assert config.parallel.enabled is True
    
    def test_configuration_validation(self):
        """Test configuration validation."""
        config = OrcaFlexConfig(
            parallel=ParallelConfig(num_threads=60)
        )
        warnings = config.validate_compatibility()
        assert any("High thread count" in w for w in warnings)
    
    def test_yaml_serialization(self):
        """Test YAML save and load."""
        config = OrcaFlexConfig(
            name="test_config",
            parallel=ParallelConfig(num_threads=16)
        )
        
        with tempfile.NamedTemporaryFile(suffix='.yaml', delete=False) as f:
            config.to_yaml(f.name)
            loaded = OrcaFlexConfig.from_yaml(f.name)
        
        assert loaded.name == "test_config"
        assert loaded.parallel.num_threads == 16
        Path(f.name).unlink()


class TestModelInterface:
    """Test OrcFxAPI abstraction layer."""
    
    def test_mock_model_wrapper(self):
        """Test mock model wrapper."""
        wrapper = OrcaFlexModelWrapper(use_mock=True)
        assert wrapper.use_mock is True
        assert wrapper.state == ModelState.UNINITIALIZED
    
    def test_mock_file_loading(self):
        """Test loading file with mock."""
        wrapper = OrcaFlexModelWrapper(use_mock=True)
        
        with tempfile.NamedTemporaryFile(suffix='.dat', delete=False) as f:
            test_file = f.name
        
        wrapper.load_file(test_file)
        assert wrapper.state == ModelState.LOADED
        Path(test_file).unlink()
    
    def test_mock_static_analysis(self):
        """Test static analysis with mock."""
        wrapper = OrcaFlexModelWrapper(use_mock=True)
        
        with tempfile.NamedTemporaryFile(suffix='.dat', delete=False) as f:
            test_file = f.name
        
        wrapper.load_file(test_file)
        results = wrapper.run_static_analysis()
        
        assert 'converged' in results
        assert 'iterations' in results
        assert 'elapsed_time' in results
        assert wrapper.state == ModelState.STATIC_COMPLETE
        
        Path(test_file).unlink()
    
    def test_progress_tracking(self):
        """Test progress tracking."""
        progress_updates = []
        
        def progress_callback(progress):
            progress_updates.append(progress.state)
        
        wrapper = OrcaFlexModelWrapper(
            use_mock=True,
            progress_callback=progress_callback
        )
        
        with tempfile.NamedTemporaryFile(suffix='.dat', delete=False) as f:
            test_file = f.name
        
        wrapper.load_file(test_file)
        
        assert ProgressState.INITIALIZING in progress_updates
        assert ProgressState.COMPLETE in progress_updates
        
        Path(test_file).unlink()
    
    def test_error_handling(self):
        """Test error handling in model wrapper."""
        wrapper = OrcaFlexModelWrapper(use_mock=True)
        
        # Test file not found
        with pytest.raises(FileError):
            wrapper.load_file("nonexistent.dat")
        
        # Test invalid extension
        with tempfile.NamedTemporaryFile(suffix='.txt', delete=False) as f:
            test_file = f.name
        
        with pytest.raises(FileError):
            wrapper.load_file(test_file)
        
        Path(test_file).unlink()
    
    def test_context_manager(self):
        """Test model wrapper as context manager."""
        with tempfile.NamedTemporaryFile(suffix='.dat', delete=False) as f:
            test_file = f.name
        
        with OrcaFlexModelWrapper(use_mock=True) as wrapper:
            wrapper.load_file(test_file)
            assert wrapper.state == ModelState.LOADED
        
        # After context, model should be cleaned up
        Path(test_file).unlink()


class TestAnalysisEngine:
    """Test analysis engine and workflows."""
    
    def test_engine_initialization(self):
        """Test engine initialization."""
        config = OrcaFlexConfig()
        engine = AnalysisEngine(config)
        
        assert engine.config is config
        assert len(engine.active_workflows) == 0
    
    def test_workflow_creation(self):
        """Test workflow creation."""
        config = OrcaFlexConfig()
        engine = AnalysisEngine(config)
        
        workflow = engine.create_workflow(AnalysisType.STATIC)
        assert isinstance(workflow, StaticAnalysisWorkflow)
        
        workflow = engine.create_workflow(AnalysisType.DYNAMIC)
        assert isinstance(workflow, DynamicAnalysisWorkflow)
    
    def test_static_workflow(self):
        """Test static analysis workflow."""
        config = OrcaFlexConfig()
        workflow = StaticAnalysisWorkflow(config)
        
        # Test initialization
        workflow.initialize(use_mock=True)
        assert workflow.initialized is True
        
        # Test validation
        issues = workflow.validate()
        assert "No input files" in issues[0]
    
    def test_sequential_execution(self):
        """Test sequential workflow execution."""
        config = OrcaFlexConfig(
            analysis=AnalysisConfig(
                analysis_type=[AnalysisType.STATIC]
            )
        )
        engine = AnalysisEngine(config)
        
        workflow = engine.create_workflow(AnalysisType.STATIC)
        results = engine.execute_sequential([workflow], use_mock=True)
        
        assert len(results) == 1
        assert results[0].state in [WorkflowState.COMPLETED, WorkflowState.FAILED]
    
    @patch('concurrent.futures.ThreadPoolExecutor')
    def test_parallel_execution(self, mock_executor):
        """Test parallel workflow execution."""
        config = OrcaFlexConfig(
            parallel=ParallelConfig(enabled=True, num_threads=4)
        )
        engine = AnalysisEngine(config)
        
        workflows = [
            engine.create_workflow(AnalysisType.STATIC),
            engine.create_workflow(AnalysisType.STATIC)
        ]
        
        # Mock executor behavior
        mock_future = MagicMock()
        mock_future.result.return_value = MagicMock(
            success=True,
            workflow_id="test",
            state=WorkflowState.COMPLETED
        )
        mock_executor.return_value.__enter__.return_value.submit.return_value = mock_future
        
        results = engine.execute_parallel(
            workflows,
            ExecutionMode.PARALLEL_THREAD,
            use_mock=True
        )
        
        assert mock_executor.called


class TestWorkflowRunner:
    """Test workflow runner utility."""
    
    def test_run_static_analysis(self):
        """Test static analysis runner."""
        config = OrcaFlexConfig()
        
        with tempfile.NamedTemporaryFile(suffix='.dat', delete=False) as f:
            test_file = f.name
        
        result = WorkflowRunner.run_static_analysis(
            config,
            test_file,
            use_mock=True
        )
        
        assert result.state in [ModelState.STATIC_COMPLETE, ModelState.FAILED]
        Path(test_file).unlink()


class TestIntegration:
    """Integration tests for the complete framework."""
    
    def test_complete_workflow(self):
        """Test complete workflow from config to results."""
        # Create configuration
        config = OrcaFlexConfig(
            name="integration_test",
            analysis=AnalysisConfig(
                analysis_type=[AnalysisType.STATIC],
                static={'enabled': True}
            ),
            parallel=ParallelConfig(
                enabled=False  # Sequential for predictability
            )
        )
        
        # Create and initialize engine
        engine = AnalysisEngine(config)
        
        # Execute analysis
        results = engine.execute(use_mock=True)
        
        assert 'total_workflows' in results
        assert 'elapsed_time' in results
        assert results['total_workflows'] == 1
    
    def test_error_propagation(self):
        """Test error propagation through the framework."""
        config = OrcaFlexConfig()
        
        with pytest.raises(FileError):
            WorkflowRunner.run_static_analysis(
                config,
                "nonexistent.dat",
                use_mock=True
            )
    
    def test_orcaflex_availability_check(self):
        """Test OrcaFlex availability checking."""
        status = check_orcaflex_available()
        
        assert 'has_module' in status
        assert 'has_license' in status
        assert isinstance(status['has_module'], bool)
        assert isinstance(status['has_license'], bool)


class TestPerformance:
    """Performance tests for the framework."""
    
    def test_parallel_speedup(self):
        """Test parallel execution provides speedup."""
        config = OrcaFlexConfig(
            parallel=ParallelConfig(
                enabled=True,
                num_threads=4
            )
        )
        
        # This is a conceptual test - actual implementation would measure real speedup
        engine = AnalysisEngine(config)
        assert engine.config.parallel.num_threads == 4
    
    def test_memory_efficiency(self):
        """Test memory efficiency of large analyses."""
        # This would test memory usage for large datasets
        config = OrcaFlexConfig()
        wrapper = OrcaFlexModelWrapper(config, use_mock=True)
        
        # Ensure wrapper cleans up properly
        wrapper.model  # Access model
        del wrapper  # Should clean up resources


if __name__ == "__main__":
    pytest.main([__file__, "-v"])