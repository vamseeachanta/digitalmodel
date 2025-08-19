"""
Universal OrcaFlex Runner - Main Implementation
===============================================

Core runner class that provides flexible execution of OrcaFlex simulations
from any directory with comprehensive keyword argument support.
"""

import os
import sys
import logging
from pathlib import Path
from typing import List, Dict, Optional, Union, Any
from dataclasses import dataclass
import yaml
import time

# Setup logging
logger = logging.getLogger(__name__)


@dataclass
class RunResults:
    """Results from a simulation run."""
    total: int = 0
    successful: int = 0
    failed: int = 0
    results: List[Dict] = None
    total_time: float = 0.0
    sim_files_created: List[Path] = None
    error_details: List[Dict] = None
    
    def __post_init__(self):
        if self.results is None:
            self.results = []
        if self.sim_files_created is None:
            self.sim_files_created = []
        if self.error_details is None:
            self.error_details = []
    
    @property
    def success_rate(self) -> float:
        """Calculate success rate percentage."""
        return (self.successful / self.total * 100) if self.total > 0 else 0.0


class UniversalOrcaFlexRunner:
    """
    Universal OrcaFlex simulation runner with flexible configuration.
    
    This runner can process OrcaFlex models from any directory with support for:
    - Pattern-based file discovery
    - Batch processing with parallelization
    - Configuration file support
    - Mock mode for testing
    - Real-time status reporting
    """
    
    def __init__(self, 
                 base_dir: Optional[Path] = None,
                 mock_mode: bool = False,
                 max_workers: int = 30,
                 license_timeout: int = 300,
                 verbose: bool = False):
        """
        Initialize the Universal OrcaFlex Runner.
        
        Args:
            base_dir: Base directory for operations (default: current directory)
            mock_mode: If True, simulate without OrcaFlex license
            max_workers: Maximum number of parallel workers
            license_timeout: Timeout for license acquisition in seconds
            verbose: Enable verbose logging
        """
        self.base_dir = Path(base_dir) if base_dir else Path.cwd()
        self.mock_mode = mock_mode
        self.max_workers = max_workers
        self.license_timeout = license_timeout
        self.verbose = verbose
        
        # Configure logging
        log_level = logging.DEBUG if verbose else logging.INFO
        logging.basicConfig(
            level=log_level,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            datefmt='%H:%M:%S'
        )
        
        # Initialize components (will be imported when needed)
        self._path_resolver = None
        self._model_discovery = None
        self._batch_processor = None
        self._status_reporter = None
        
        # Check OrcaFlex availability
        self._check_orcaflex_availability()
        
        logger.info(f"UniversalOrcaFlexRunner initialized")
        logger.info(f"Base directory: {self.base_dir}")
        logger.info(f"Mock mode: {self.mock_mode}")
        logger.info(f"Max workers: {self.max_workers}")
    
    def _check_orcaflex_availability(self):
        """Check if OrcaFlex API is available."""
        if not self.mock_mode:
            try:
                import OrcFxAPI
                self.orcaflex_available = True
                logger.info("OrcaFlex API available")
            except ImportError:
                self.orcaflex_available = False
                logger.warning("OrcaFlex API not available - switching to mock mode")
                self.mock_mode = True
        else:
            self.orcaflex_available = False
            logger.info("Running in mock mode")
    
    @property
    def path_resolver(self):
        """Lazy load path resolver."""
        if self._path_resolver is None:
            from .path_resolver import PathResolver
            self._path_resolver = PathResolver()
        return self._path_resolver
    
    @property
    def model_discovery(self):
        """Lazy load model discovery."""
        if self._model_discovery is None:
            from .model_discovery import ModelDiscovery
            self._model_discovery = ModelDiscovery()
        return self._model_discovery
    
    @property
    def batch_processor(self):
        """Lazy load batch processor."""
        if self._batch_processor is None:
            from .batch_processor import BatchProcessor
            self._batch_processor = BatchProcessor(
                max_workers=self.max_workers,
                mock_mode=self.mock_mode
            )
        return self._batch_processor
    
    @property
    def status_reporter(self):
        """Lazy load status reporter."""
        if self._status_reporter is None:
            from .status_reporter import StatusReporter
            self._status_reporter = StatusReporter()
        return self._status_reporter
    
    def run(self,
            pattern: Optional[str] = None,
            input_directory: Optional[Union[str, Path]] = None,
            output_directory: Optional[Union[str, Path]] = None,
            models: Optional[List[Union[str, Path]]] = None,
            config_file: Optional[Union[str, Path]] = None,
            recursive: bool = False,
            parallel: bool = True,
            exclude_patterns: Optional[List[str]] = None,
            **kwargs) -> RunResults:
        """
        Execute simulations with flexible keyword arguments.
        
        Args:
            pattern: File pattern to match (e.g., "*.yml", "fsts_*.dat")
            input_directory: Directory to search for models
            output_directory: Directory to save .sim files
            models: Specific model files to process
            config_file: Configuration file with processing settings
            recursive: Search directories recursively
            parallel: Enable parallel processing
            exclude_patterns: Patterns to exclude from processing
            **kwargs: Additional keyword arguments for processing
        
        Returns:
            RunResults: Results of the simulation run
        
        Examples:
            # Pattern-based processing
            results = runner.run(pattern="fsts_*.yml", input_directory="./models")
            
            # Specific files
            results = runner.run(models=["model1.yml", "model2.yml"])
            
            # Configuration file
            results = runner.run(config_file="batch_config.yml")
        """
        logger.info("=" * 80)
        logger.info("UNIVERSAL ORCAFLEX RUNNER - STARTING")
        logger.info("=" * 80)
        
        start_time = time.time()
        
        # If config file provided, load and merge with arguments
        if config_file:
            config = self._load_config(config_file)
            # Merge config with provided arguments (arguments override config)
            kwargs = {**config, **kwargs}
            if pattern is None and 'pattern' in config:
                pattern = config['pattern']
            if input_directory is None and 'input_directory' in config:
                input_directory = config['input_directory']
            if output_directory is None and 'output_directory' in config:
                output_directory = config['output_directory']
        
        # Resolve directories
        input_dir = self.path_resolver.resolve_path(
            input_directory or self.base_dir
        )
        output_dir = self.path_resolver.resolve_path(
            output_directory or input_dir / "sim"
        )
        
        # Ensure output directory exists
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Discover models to process
        if models:
            # Specific models provided
            model_files = [self.path_resolver.resolve_path(m) for m in models]
            logger.info(f"Processing {len(model_files)} specified models")
        else:
            # Discover models based on pattern
            model_files = self.model_discovery.find_models(
                directory=input_dir,
                pattern=pattern or "*.yml",
                recursive=recursive,
                exclude_patterns=exclude_patterns
            )
            logger.info(f"Discovered {len(model_files)} models matching pattern")
        
        if not model_files:
            logger.warning("No models found to process")
            return RunResults(total=0)
        
        # Display models to process
        logger.info(f"\nModels to process: {len(model_files)}")
        for i, model in enumerate(model_files[:5], 1):
            logger.info(f"  {i}. {model.name}")
        if len(model_files) > 5:
            logger.info(f"  ... and {len(model_files) - 5} more")
        
        # Process models
        if parallel and len(model_files) > 1:
            results = self.batch_processor.process_batch(
                models=model_files,
                output_directory=output_dir,
                status_reporter=self.status_reporter,
                **kwargs
            )
        else:
            # Sequential processing
            results = self._process_sequential(
                model_files, output_dir, **kwargs
            )
        
        # Generate summary
        total_time = time.time() - start_time
        run_results = self._create_run_results(results, total_time)
        
        # Display summary
        self._display_summary(run_results)
        
        return run_results
    
    def _load_config(self, config_file: Union[str, Path]) -> Dict:
        """Load configuration from YAML file."""
        config_path = self.path_resolver.resolve_path(config_file)
        
        if not config_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_path}")
        
        with open(config_path, 'r') as f:
            config = yaml.safe_load(f)
        
        logger.info(f"Loaded configuration from: {config_path}")
        return config.get('processing', {})
    
    def _process_sequential(self, 
                          model_files: List[Path], 
                          output_dir: Path,
                          **kwargs) -> List[Dict]:
        """Process models sequentially."""
        results = []
        
        for i, model_file in enumerate(model_files, 1):
            logger.info(f"Processing [{i}/{len(model_files)}]: {model_file.name}")
            
            # Process single model
            result = self._process_single_model(model_file, output_dir, **kwargs)
            results.append(result)
            
            # Update status
            if result['success']:
                logger.info(f"  OK: Success: {result.get('sim_file', 'N/A')}")
            else:
                logger.error(f"  FAIL: {result.get('error', 'Unknown error')}")
        
        return results
    
    def _process_single_model(self, 
                            model_file: Path, 
                            output_dir: Path,
                            **kwargs) -> Dict:
        """Process a single model file."""
        result = {
            'model': model_file.name,
            'input': str(model_file),
            'success': False,
            'sim_file': None,
            'error': None,
            'duration': 0
        }
        
        start_time = time.time()
        
        try:
            if self.mock_mode:
                # Mock processing
                time.sleep(0.1)  # Simulate processing
                sim_file = output_dir / f"{model_file.stem}.sim"
                sim_file.write_text(f"Mock simulation for {model_file.name}")
                result['sim_file'] = str(sim_file)
                result['success'] = True
                result['mock'] = True
            else:
                # Real OrcaFlex processing
                import OrcFxAPI
                
                # Load model
                model = OrcFxAPI.Model()
                
                # Load model based on file extension
                if model_file.suffix.lower() in ['.yml', '.yaml', '.dat']:
                    model.LoadData(str(model_file))
                else:
                    raise ValueError(f"Unsupported file type: {model_file.suffix}")
                
                # Run static analysis
                model.CalculateStatics()
                
                # Save simulation
                sim_file = output_dir / f"{model_file.stem}.sim"
                model.SaveSimulation(str(sim_file))
                
                result['sim_file'] = str(sim_file)
                result['success'] = True
                
        except Exception as e:
            result['error'] = str(e)
            logger.debug(f"Error processing {model_file.name}: {e}")
        
        result['duration'] = time.time() - start_time
        return result
    
    def _create_run_results(self, results: List[Dict], total_time: float) -> RunResults:
        """Create RunResults from processing results."""
        run_results = RunResults(
            total=len(results),
            successful=sum(1 for r in results if r.get('success', False)),
            failed=sum(1 for r in results if not r.get('success', False)),
            results=results,
            total_time=total_time
        )
        
        # Extract sim files and errors
        for result in results:
            if result.get('success') and result.get('sim_file'):
                run_results.sim_files_created.append(Path(result['sim_file']))
            elif not result.get('success'):
                run_results.error_details.append({
                    'model': result.get('model', 'Unknown'),
                    'error': result.get('error', 'Unknown error')
                })
        
        return run_results
    
    def _display_summary(self, results: RunResults):
        """Display summary of run results."""
        logger.info("\n" + "=" * 80)
        logger.info("UNIVERSAL ORCAFLEX RUNNER - SUMMARY")
        logger.info("=" * 80)
        logger.info(f"Total Models: {results.total}")
        logger.info(f"Successful: {results.successful}")
        logger.info(f"Failed: {results.failed}")
        logger.info(f"Success Rate: {results.success_rate:.1f}%")
        logger.info(f"Total Time: {results.total_time:.2f} seconds")
        
        if results.total > 0:
            avg_time = results.total_time / results.total
            logger.info(f"Average Time per Model: {avg_time:.2f} seconds")
        
        if results.sim_files_created:
            logger.info(f"\nSimulation Files Created: {len(results.sim_files_created)}")
            for sim_file in results.sim_files_created[:5]:
                logger.info(f"  - {sim_file.name}")
            if len(results.sim_files_created) > 5:
                logger.info(f"  ... and {len(results.sim_files_created) - 5} more")
        
        if results.error_details:
            logger.error(f"\nFailed Models: {len(results.error_details)}")
            for error in results.error_details[:3]:
                logger.error(f"  - {error['model']}: {error['error'][:50]}...")
            if len(results.error_details) > 3:
                logger.error(f"  ... and {len(results.error_details) - 3} more")
        
        logger.info("=" * 80)