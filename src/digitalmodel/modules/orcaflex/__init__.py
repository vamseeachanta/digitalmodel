"""
OrcaFlex Integration Module

Comprehensive Python integration with OrcaFlex for offshore engineering analysis.
Provides model execution, post-processing, file conversion, and batch processing.

Key Features:
- Universal model runner (YAML/DAT files)
- Post-processing from .sim files
- Parallel batch execution
- File format conversions
- Model generation utilities

Complies with OrcaFlex API standards and offshore engineering best practices.
"""

__version__ = '1.0.0'

# Core runner functionality
try:
    from .universal import UniversalOrcaFlexRunner, StatusReporter
    UNIVERSAL_RUNNER_AVAILABLE = True
except ImportError:
    UNIVERSAL_RUNNER_AVAILABLE = False
    UniversalOrcaFlexRunner = None
    StatusReporter = None

# Post-processing from .sim files
try:
    from .opp import OrcaFlexPostProcessor
    POST_PROCESSING_AVAILABLE = True
except ImportError:
    POST_PROCESSING_AVAILABLE = False
    OrcaFlexPostProcessor = None

# Run-to-sim functionality
try:
    from .run_to_sim import run_models, ORCAFLEX_AVAILABLE
except ImportError:
    run_models = None
    ORCAFLEX_AVAILABLE = False

# Core infrastructure
try:
    from .core import (
        BaseAnalysisEngine,
        ModelInterface,
        AnalysisConfiguration,
    )
    CORE_AVAILABLE = True
except ImportError:
    CORE_AVAILABLE = False
    BaseAnalysisEngine = None
    ModelInterface = None
    AnalysisConfiguration = None

__all__ = [
    # Version
    '__version__',

    # Universal Runner
    'UniversalOrcaFlexRunner',
    'StatusReporter',
    'UNIVERSAL_RUNNER_AVAILABLE',

    # Post-processing
    'OrcaFlexPostProcessor',
    'POST_PROCESSING_AVAILABLE',

    # Run-to-sim
    'run_models',
    'ORCAFLEX_AVAILABLE',

    # Core
    'BaseAnalysisEngine',
    'ModelInterface',
    'AnalysisConfiguration',
    'CORE_AVAILABLE',

    # Template generator
    'TemplateManager',
    'TemplateGenerator',
    'ModelValidator',
    'TEMPLATE_GENERATOR_AVAILABLE',
]

# Template generator
try:
    from .template_generator import TemplateManager, TemplateGenerator, ModelValidator
    TEMPLATE_GENERATOR_AVAILABLE = True
except ImportError:
    TEMPLATE_GENERATOR_AVAILABLE = False
    TemplateManager = None
    TemplateGenerator = None
    ModelValidator = None

# CLI Commands available
CLI_COMMANDS = {
    'orcaflex-universal': 'Universal OrcaFlex simulation runner',
    'run-to-sim': 'Run OrcaFlex models to generate .sim files',
    'template-generator': 'Generate OrcaFlex models from hybrid templates',
}

def get_version():
    """Get module version."""
    return __version__

def check_availability():
    """
    Check which OrcaFlex components are available.

    Returns:
        dict: Dictionary of component availability status
    """
    return {
        'universal_runner': UNIVERSAL_RUNNER_AVAILABLE,
        'post_processing': POST_PROCESSING_AVAILABLE,
        'orcaflex_api': ORCAFLEX_AVAILABLE,
        'core': CORE_AVAILABLE,
        'template_generator': TEMPLATE_GENERATOR_AVAILABLE,
    }

def list_cli_commands():
    """
    List available CLI commands.

    Returns:
        dict: Dictionary of CLI command names and descriptions
    """
    return CLI_COMMANDS.copy()
