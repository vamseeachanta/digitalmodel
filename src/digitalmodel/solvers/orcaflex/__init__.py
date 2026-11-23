# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""OrcaFlex integration module.

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

from importlib import import_module
from typing import Any

# Resolve execution components only when their public exports are requested.
# Schema validation and YAML generation must not probe the solver binding.
_EXPORT_GROUPS = (
    ("universal", "UNIVERSAL_RUNNER_AVAILABLE",
     ("UniversalOrcaFlexRunner", "StatusReporter")),
    ("opp", "POST_PROCESSING_AVAILABLE", ("OrcaFlexPostProcessor",)),
    ("run_to_sim", "ORCAFLEX_AVAILABLE", ("run_models",)),
    ("core", "CORE_AVAILABLE",
     ("BaseAnalysisEngine", "ModelInterface", "AnalysisConfiguration")),
    ("template_generator", "TEMPLATE_GENERATOR_AVAILABLE",
     ("TemplateManager", "TemplateGenerator", "ModelValidator")),
)


def __getattr__(name: str) -> Any:
    """Resolve optional public components on first attribute access.

    Args:
        name: Public component or availability flag.

    Returns:
        The requested export, or its historical unavailable fallback.

    Raises:
        AttributeError: The name is not a public optional component.
    """
    for module_name, flag, exports in _EXPORT_GROUPS:
        if name not in (*exports, flag):
            continue
        try:
            module = import_module(f".{module_name}", __name__)
            values = {export: getattr(module, export) for export in exports}
            values[flag] = getattr(module, flag, True)
        except (ImportError, AttributeError):
            values = dict.fromkeys(exports)
            values[flag] = False
        globals().update(values)
        return values[name]
    raise AttributeError(f"Unknown optional component: {name}")

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

# Consolidated submodules (previously separate solver directories)
# - post_process: was solvers/orcaflex_post_process/
# - browser: was solvers/orcaflex_browser/
# - post_results: was solvers/orcaflex/OrcaFlex_Post/

# CLI Commands available
CLI_COMMANDS = {
    'orcaflex-universal': 'Universal OrcaFlex simulation runner',
    'run-to-sim': 'Run OrcaFlex models to generate .sim files',
    'template-generator': 'Generate OrcaFlex models from hybrid templates',
}

def get_version() -> str:
    """Get module version."""
    return __version__

def check_availability() -> dict[str, bool]:
    """Check which OrcaFlex components are available.

    Returns:
        dict: Dictionary of component availability status
    """
    return {
        'universal_runner': __getattr__('UNIVERSAL_RUNNER_AVAILABLE'),
        'post_processing': __getattr__('POST_PROCESSING_AVAILABLE'),
        'orcaflex_api': __getattr__('ORCAFLEX_AVAILABLE'),
        'core': __getattr__('CORE_AVAILABLE'),
        'template_generator': __getattr__('TEMPLATE_GENERATOR_AVAILABLE'),
        'browser': True,       # consolidated from solvers/orcaflex_browser
        'post_process': True,  # consolidated from solvers/orcaflex_post_process
        'post_results': True,  # renamed from OrcaFlex_Post
    }

def list_cli_commands() -> dict[str, str]:
    """List available CLI commands.

    Returns:
        dict: Dictionary of CLI command names and descriptions
    """
    return CLI_COMMANDS.copy()
