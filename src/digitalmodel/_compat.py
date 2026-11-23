"""Backward compatibility layer for digitalmodel import migrations.

Handles two redirect layers:
  Layer 1: digitalmodel.modules.X -> digitalmodel.X (existing, via modules/__init__.py)
  Layer 2: digitalmodel.X -> digitalmodel.<group>.X (new, via _GroupRedirectFinder)

Usage:
    import digitalmodel._compat  # activates aliases
"""

import importlib
import importlib.abc
import importlib.machinery
import importlib.util
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
    "asset_integrity",
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
    "orcaflex_post_process",  # now at solvers.orcaflex.post_process
    "artificial_lift",
    "ai_workflows",
    "api_analysis",
    "gmsh_meshing",
    # Phase 4 Batch 6
    "design_tools",
    "mcp_server",
    "orcaflex_browser",  # now at solvers.orcaflex.browser
    "data_scraping",
    "blender_automation",
    "fatigue_apps",
    # Phase 6 cleanup
    "reporting",
}

# Mapping from flat module name to group directory for Phase 12+ grouping
_FLAT_TO_GROUP: dict[str, str] = {
    # solvers/
    "orcaflex": "solvers",
    "orcawave": "solvers",
    # orcaflex_browser -> solvers.orcaflex.browser (see _FLAT_TO_FULL_PATH)
    # orcaflex_post_process -> solvers.orcaflex.post_process (see _FLAT_TO_FULL_PATH)
    "fea_model": "solvers",
    "gmsh_meshing": "solvers",
    "blender_automation": "solvers",
    # hydrodynamics/
    # NOTE: "hydrodynamics" itself is excluded — it is a group name and is
    # caught by the skip set in _GroupRedirectFinder.find_spec before lookup.
    "aqwa": "hydrodynamics",
    "diffraction": "hydrodynamics",
    "rao_analysis": "hydrodynamics",
    "bemrosetta": "hydrodynamics",
    # structural/
    "fatigue": "structural",
    "fatigue_apps": "structural",
    "stress": "structural",
    "structural_analysis": "structural",
    "pipe_capacity": "structural",
    "pipe_cross_section": "structural",
    # subsea/
    "mooring_analysis": "subsea",
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
    # "validators" removed (WRK-415 Phase 2B): infrastructure/validators/ deleted,
    # all callers migrated to infrastructure/validation/
    "templates": "infrastructure",
    "calculations": "infrastructure",
    "transformation": "infrastructure",
    # data_systems/
    "data": "data_systems",
    "data_manager": "data_systems",
    "data_procurement": "data_systems",
    "data_scraping": "data_systems",
    # NOTE: asset_integrity (formerly pyintegrity) is now a top-level module,
    # no longer under data_systems/
    # workflows/
    "automation": "workflows",
    "workflow_automation": "workflows",
    "ai_workflows": "workflows",
    "agents": "workflows",
    "mcp_server": "workflows",
    "skills": "workflows",
    # visualization/
    # NOTE: "visualization" itself is excluded — it is a group name and is
    # caught by the skip set in _GroupRedirectFinder.find_spec before lookup.
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


# Explicit full-path redirects for modules that were renamed + relocated
# (cannot be handled by the simple group prefix redirect)
_FLAT_TO_FULL_PATH: dict[str, str] = {
    "orcaflex_browser": "digitalmodel.solvers.orcaflex.browser",
    "orcaflex_post_process": "digitalmodel.solvers.orcaflex.post_process",
    "fatigue_analysis": "digitalmodel.structural.fatigue_apps",
    "diffraction_cli": "digitalmodel.hydrodynamics.diffraction.diffraction_cli",
    "standards_lookup": "digitalmodel.infrastructure.utils.standards_lookup",
}


def get_group_for_module(name: str) -> str | None:
    """Return the group name for a flat module, or None if not grouped."""
    return _FLAT_TO_GROUP.get(name)


def register_moved_module(name: str) -> None:
    """Register a module name that has been moved from modules/ to top-level."""
    _MOVED_MODULES.add(name)


def is_moved(name: str) -> bool:
    """Check if a module name has been registered as moved."""
    return name in _MOVED_MODULES


def get_moved_modules() -> frozenset[str]:
    """Return the set of all moved module names."""
    return frozenset(_MOVED_MODULES)


# ── Layer 2 compat: digitalmodel.X -> digitalmodel.<group>.X ─────────

_group_warned: set[str] = set()
_finding: set[str] = set()  # Re-entrancy guard for _GroupRedirectFinder.find_spec


def warn_flat_import(module_name: str, group: str) -> None:
    """Emit deprecation warning for flat namespace import.

    Called from digitalmodel.__init__.__getattr__ so that
    ``from digitalmodel import orcaflex`` also warns consistently
    with the finder-based ``import digitalmodel.orcaflex`` path.
    """
    if module_name not in _group_warned:
        _group_warned.add(module_name)
        warnings.warn(
            f"digitalmodel.{module_name} is deprecated. "
            f"Use digitalmodel.{group}.{module_name} instead.",
            DeprecationWarning,
            stacklevel=4,  # __getattr__ -> warn_flat_import -> user code
        )


class _GroupRedirectLoader(importlib.abc.Loader):
    """Loader that redirects a flat module path to its grouped location."""

    def __init__(self, new_name: str, old_name: str):
        self.new_name = new_name
        self.old_name = old_name

    def create_module(self, spec):
        return None  # Use default semantics

    def exec_module(self, module):
        if self.old_name not in _group_warned:
            _group_warned.add(self.old_name)
            warnings.warn(
                f"{self.old_name} is deprecated. Use {self.new_name} instead.",
                DeprecationWarning,
                stacklevel=2,
            )
        real_mod = importlib.import_module(self.new_name)
        module.__dict__.update(real_mod.__dict__)
        module.__path__ = getattr(real_mod, "__path__", [])
        module.__loader__ = self
        sys.modules[self.old_name] = real_mod


class _GroupRedirectFinder(importlib.abc.MetaPathFinder):
    """Redirect digitalmodel.X imports to digitalmodel.<group>.X.

    Only activates when the flat path (digitalmodel.X) does NOT exist
    on disk — i.e., after modules have been physically moved to groups.
    This ensures the finder is a no-op before Phase 14 moves.
    """

    _PREFIX = "digitalmodel."

    def find_spec(self, fullname, path, target=None):
        # Re-entrancy guard: if we are already resolving this name,
        # return None to let the normal import machinery handle it.
        if fullname in _finding:
            return None

        if not fullname.startswith(self._PREFIX):
            return None

        parts = fullname.split(".")
        if len(parts) < 2 or parts[0] != "digitalmodel":
            return None

        top_module = parts[1]

        # Skip if the top_module is itself a group name (avoid recursion)
        if top_module in {
            "solvers", "hydrodynamics", "structural", "subsea",
            "marine_ops", "signal_processing", "infrastructure",
            "data_systems", "workflows", "visualization", "specialized",
            "modules", "_compat",
        }:
            return None

        # Check explicit full-path redirects first (renamed + relocated modules)
        if top_module in _FLAT_TO_FULL_PATH:
            base_target = _FLAT_TO_FULL_PATH[top_module]
            # Support sub-imports: digitalmodel.orcaflex_browser.X -> target.X
            sub_parts = ".".join(parts[2:])
            new_fullname = f"{base_target}.{sub_parts}" if sub_parts else base_target

            _finding.add(fullname)
            try:
                spec = importlib.util.find_spec(new_fullname)
            except (ModuleNotFoundError, ValueError):
                spec = None
            finally:
                _finding.discard(fullname)

            if spec is None:
                return None

            loader = _GroupRedirectLoader(new_fullname, fullname)
            return importlib.machinery.ModuleSpec(
                fullname,
                loader,
                is_package=spec.submodule_search_locations is not None,
            )

        if top_module not in _FLAT_TO_GROUP:
            return None

        group = _FLAT_TO_GROUP[top_module]
        remainder = ".".join(parts[1:])  # e.g. "orcaflex.orcaflex" or "orcaflex"
        new_fullname = f"digitalmodel.{group}.{remainder}"

        # Only redirect if the grouped path actually exists
        _finding.add(fullname)
        try:
            spec = importlib.util.find_spec(new_fullname)
        except (ModuleNotFoundError, ValueError):
            spec = None
        finally:
            _finding.discard(fullname)

        if spec is None:
            return None

        loader = _GroupRedirectLoader(new_fullname, fullname)
        return importlib.machinery.ModuleSpec(
            fullname,
            loader,
            is_package=spec.submodule_search_locations is not None,
        )


def install_group_redirect() -> None:
    """Install the _GroupRedirectFinder on sys.meta_path."""
    if not any(isinstance(f, _GroupRedirectFinder) for f in sys.meta_path):
        sys.meta_path.append(_GroupRedirectFinder())
