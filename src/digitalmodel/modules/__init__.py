"""Digital Model Modules Package - Backward Compatibility Layer.

All modules have been moved from digitalmodel.modules.X to digitalmodel.X.
This package provides backward compatibility by redirecting old import paths
to new locations with deprecation warnings.

Usage:
    # Old path (deprecated, emits DeprecationWarning):
    from digitalmodel.modules.orcaflex.orcaflex import OrcaFlex

    # New path (preferred):
    from digitalmodel.orcaflex.orcaflex import OrcaFlex
"""

import importlib
import importlib.abc
import importlib.machinery
import importlib.util
import sys
import warnings

__all__ = []

_MOVED_MODULES = {
    "ai_workflows", "api_analysis", "aqwa", "artificial_lift", "automation",
    "bemrosetta", "blender_automation", "catenary", "catenary_riser",
    "ct_hydraulics", "data_scraping", "design_tools", "diffraction",
    "digitalmarketing", "fatigue_analysis", "fea_model", "finance", "gis",
    "gmsh_meshing", "hydrodynamics", "marine_analysis", "marine_engineering",
    "mcp_server", "mooring_analysis", "orcaflex", "orcaflex_browser",
    "orcaflex_post_process", "orcawave", "pipe_capacity", "pipe_cross_section",
    "pipeline", "project_management", "pyintegrity", "rao_analysis",
    "reporting", "rigging",
    "services", "signal_analysis", "skills", "structural_analysis",
    "time_series", "transformation", "vertical_riser", "visualization",
    "viv_analysis", "workflow_automation",
}

# Modules that were renamed during the restructure (old_name -> new_name)
_RENAMED_MODULES = {
    "data_procurement": "data_scraping",
}

_PREFIX = "digitalmodel.modules."
_warned = set()


class _RedirectLoader(importlib.abc.Loader):
    """Loader that redirects to the new module location."""

    def __init__(self, new_name, old_name):
        self.new_name = new_name
        self.old_name = old_name

    def create_module(self, spec):
        return None  # Use default semantics

    def exec_module(self, module):
        if self.old_name not in _warned:
            _warned.add(self.old_name)
            warnings.warn(
                f"{self.old_name} is deprecated. Use {self.new_name} instead.",
                DeprecationWarning,
                stacklevel=2,
            )
        real_mod = importlib.import_module(self.new_name)
        # Copy all attributes from the real module
        module.__dict__.update(real_mod.__dict__)
        module.__path__ = getattr(real_mod, "__path__", [])
        module.__loader__ = self
        # Alias in sys.modules
        sys.modules[self.old_name] = real_mod


class _ModulesRedirectFinder(importlib.abc.MetaPathFinder):
    """Redirect digitalmodel.modules.X imports to digitalmodel.X."""

    def find_spec(self, fullname, path, target=None):
        if fullname.startswith(_PREFIX):
            remainder = fullname[len(_PREFIX):]
            top_module = remainder.split(".")[0]
            if top_module in _MOVED_MODULES:
                new_name = f"digitalmodel.{remainder}"
                loader = _RedirectLoader(new_name, fullname)
                is_pkg = self._check_is_package(new_name, remainder)
                return importlib.machinery.ModuleSpec(
                    fullname, loader,
                    is_package=is_pkg,
                )
            if top_module in _RENAMED_MODULES:
                new_top = _RENAMED_MODULES[top_module]
                sub = remainder[len(top_module):]  # e.g. ".submod" or ""
                new_name = f"digitalmodel.{new_top}{sub}"
                loader = _RedirectLoader(new_name, fullname)
                is_pkg = self._check_is_package(new_name, remainder)
                return importlib.machinery.ModuleSpec(
                    fullname, loader,
                    is_package=is_pkg,
                )
        return None

    @staticmethod
    def _check_is_package(canonical_name: str, remainder: str) -> bool:
        """Determine is_package by querying the real spec for the target module.

        Falls back to the '.' heuristic if the real spec cannot be resolved
        (e.g., the module hasn't been moved yet).
        """
        try:
            real_spec = importlib.util.find_spec(canonical_name)
        except (ModuleNotFoundError, ValueError):
            real_spec = None
        if real_spec is not None:
            return real_spec.submodule_search_locations is not None
        # Fallback: top-level names (no dot) are typically packages
        return "." not in remainder


# Install the finder once at import time
if not any(isinstance(f, _ModulesRedirectFinder) for f in sys.meta_path):
    sys.meta_path.insert(0, _ModulesRedirectFinder())


def __getattr__(name):
    """Handle attribute access for direct `from digitalmodel.modules import X`."""
    if name in _MOVED_MODULES:
        if name not in _warned:
            _warned.add(name)
            warnings.warn(
                f"digitalmodel.modules.{name} is deprecated. "
                f"Use digitalmodel.{name} instead.",
                DeprecationWarning,
                stacklevel=2,
            )
        mod = importlib.import_module(f"digitalmodel.{name}")
        sys.modules[f"digitalmodel.modules.{name}"] = mod
        return mod
    if name in _RENAMED_MODULES:
        new_name = _RENAMED_MODULES[name]
        if name not in _warned:
            _warned.add(name)
            warnings.warn(
                f"digitalmodel.modules.{name} is deprecated. "
                f"Use digitalmodel.{new_name} instead.",
                DeprecationWarning,
                stacklevel=2,
            )
        mod = importlib.import_module(f"digitalmodel.{new_name}")
        sys.modules[f"digitalmodel.modules.{name}"] = mod
        return mod
    raise AttributeError(f"module 'digitalmodel.modules' has no attribute {name!r}")
