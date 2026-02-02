"""Backward compatibility layer for digitalmodel.modules.* -> digitalmodel.* migration.

This module provides sys.modules aliasing so that imports like:
    from digitalmodel.orcaflex.orcaflex import OrcaFlex
continue to work after modules have been flattened to:
    from digitalmodel.orcaflex.orcaflex import OrcaFlex

Usage:
    import digitalmodel._compat  # activates aliases
    # or
    digitalmodel._compat.install_aliases()
"""

import importlib
import sys
import warnings

# Modules that have been moved from digitalmodel.modules.X to digitalmodel.X
_MOVED_MODULES: set[str] = {
    "transformation",
    "vertical_riser",
    "rigging",
    "ct_hydraulics",
    "pipe_cross_section",
    "fea_model",
    "pyintegrity",
    "finance",
    "project_management",
    "digitalmarketing",
    "services",
    "skills",
    "visualization",
    # Phase 4 Batch 2
    "rao_analysis",
    "time_series",
    "pipe_capacity",
    "pipeline",
    "hydrodynamics",
    "structural_analysis",
    "viv_analysis",
    # Phase 4 Batch 3
    "signal_analysis",
    "fatigue_analysis",
    "mooring_analysis",
    "catenary",
    "catenary_riser",
    # Phase 4 Batch 4
    "orcaflex",
    "aqwa",
    "diffraction",
    "bemrosetta",
    "gis",
    "orcawave",
    # Phase 4 Batch 5
    "automation",
    "workflow_automation",
    "marine_analysis",
    "marine_engineering",
    "orcaflex_post_process",
    "artificial_lift",
    "ai_workflows",
    "api_analysis",
    "gmsh_meshing",
    # Phase 4 Batch 6
    "design_tools",
    "mcp_server",
    "orcaflex_browser",
    "data_scraping",
    "blender_automation",
    "diffraction_cli",
    "standards_lookup",
    # Phase 6 cleanup
    "reporting",
}

# Mapping from flat module name to group directory for Phase 12+ grouping
_FLAT_TO_GROUP: dict[str, str] = {
    # solvers/
    "orcaflex": "solvers",
    "orcawave": "solvers",
    "orcaflex_browser": "solvers",
    "orcaflex_post_process": "solvers",
    "fea_model": "solvers",
    "gmsh_meshing": "solvers",
    "blender_automation": "solvers",
    # hydrodynamics/
    "aqwa": "hydrodynamics",
    "diffraction": "hydrodynamics",
    "hydrodynamics": "hydrodynamics",
    "rao_analysis": "hydrodynamics",
    "bemrosetta": "hydrodynamics",
    # structural/
    "fatigue": "structural",
    "fatigue_analysis": "structural",
    "stress": "structural",
    "structural_analysis": "structural",
    "pipe_capacity": "structural",
    "pipe_cross_section": "structural",
    "analysis": "structural",
    # subsea/
    "mooring_analysis": "subsea",
    "catenary": "subsea",
    "catenary_riser": "subsea",
    "vertical_riser": "subsea",
    "viv_analysis": "subsea",
    "pipeline": "subsea",
    # marine_ops/
    "marine_analysis": "marine_ops",
    "marine_engineering": "marine_ops",
    "ct_hydraulics": "marine_ops",
    "reservoir": "marine_ops",
    "artificial_lift": "marine_ops",
    # signal_processing/
    "signal_analysis": "signal_processing",
    "time_series": "signal_processing",
    # infrastructure/
    "common": "infrastructure",
    "core": "infrastructure",
    "config": "infrastructure",
    "base_configs": "infrastructure",
    "base_solvers": "infrastructure",
    "services": "infrastructure",
    "domains": "infrastructure",
    "validation": "infrastructure",
    "validators": "infrastructure",
    "templates": "infrastructure",
    "calculations": "infrastructure",
    "transformation": "infrastructure",
    # data_systems/
    "data": "data_systems",
    "data_manager": "data_systems",
    "data_procurement": "data_systems",
    "data_scraping": "data_systems",
    "pyintegrity": "data_systems",
    # workflows/
    "automation": "workflows",
    "workflow_automation": "workflows",
    "ai_workflows": "workflows",
    "agents": "workflows",
    "mcp_server": "workflows",
    "skills": "workflows",
    # visualization/
    "visualization": "visualization",
    "reporting": "visualization",
    "design_tools": "visualization",
    # specialized/
    "gis": "specialized",
    "digitalmarketing": "specialized",
    "finance": "specialized",
    "project_management": "specialized",
    "rigging": "specialized",
    "custom": "specialized",
    "api_analysis": "specialized",
    "cli": "specialized",
}


def get_group_for_module(name: str) -> str | None:
    """Return the group name for a flat module, or None if not grouped."""
    return _FLAT_TO_GROUP.get(name)


def register_moved_module(name: str) -> None:
    """Register a module name that has been moved from modules/ to top-level."""
    _MOVED_MODULES.add(name)


def install_aliases() -> None:
    """Install sys.modules aliases for all moved modules.

    After calling this, `import digitalmodel.modules.X` will resolve
    to `digitalmodel.X` with a deprecation warning on first access.
    """
    # Patch digitalmodel.modules.__getattr__ for lazy aliasing
    modules_pkg = sys.modules.get("digitalmodel.modules")
    if modules_pkg is not None:
        original_getattr = getattr(modules_pkg, "__getattr__", None)

        def _patched_getattr(name: str):
            if name in _MOVED_MODULES:
                warnings.warn(
                    f"digitalmodel.modules.{name} is deprecated. "
                    f"Use digitalmodel.{name} instead.",
                    DeprecationWarning,
                    stacklevel=2,
                )
                mod = importlib.import_module(f"digitalmodel.{name}")
                # Cache in sys.modules so subsequent imports don't warn again
                sys.modules[f"digitalmodel.modules.{name}"] = mod
                return mod
            if original_getattr is not None:
                return original_getattr(name)
            raise AttributeError(
                f"module 'digitalmodel.modules' has no attribute {name!r}"
            )

        modules_pkg.__getattr__ = _patched_getattr


def is_moved(name: str) -> bool:
    """Check if a module name has been registered as moved."""
    return name in _MOVED_MODULES


def get_moved_modules() -> frozenset[str]:
    """Return the set of all moved module names."""
    return frozenset(_MOVED_MODULES)
