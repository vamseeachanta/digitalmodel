"""Test-scoped engine imports for routing tests, without loading solver APIs.

Only dependencies of the engine are replaced. The real engine source is always
executed; import failures propagate. No dependency double survives the context.
"""

from contextlib import contextmanager
import importlib.util
from pathlib import Path
import sys
from types import ModuleType
from unittest.mock import MagicMock

import pytest

import digitalmodel


class AttributeDict(dict):
    """Minimal attribute-access dictionary for routing fixtures."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self


# These are engine dependencies, not optional numerical-library replacements.
# Lazy handlers used by routing tests are included to avoid importing their
# transitive dependencies. Domain tests import the real modules after teardown.
DEPENDENCIES = {
    "assetutilities.common.ApplicationManager": ("ConfigureApplicationInputs",),
    "assetutilities.common.data": ("CopyAndPasteFiles",),
    "assetutilities.common.file_management": ("FileManagement",),
    "assetutilities.common.update_deep": ("AttributeDict",),
    "assetutilities.common.yml_utilities": ("WorkingWithYAML",),
    "digitalmodel.hydrodynamics.aqwa": ("Aqwa",),
    "digitalmodel.cathodic_protection.engine_adapter": ("run_cathodic_protection",),
    "digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_circular": (
        "DNVRPH103_hydrodynamics_circular",
    ),
    "digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular": (
        "DNVRPH103_hydrodynamics_rectangular",
    ),
    "digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis": (
        "FatigueAnalysis",
    ),
    "digitalmodel.infrastructure.base_solvers.marine.ship.ship_design": ("ShipDesign",),
    "digitalmodel.subsea.mooring_analysis": ("MooringDesigner",),
    "digitalmodel.subsea.mooring_analysis.cli": ("cli",),
    "digitalmodel.solvers.orcaflex.orcaflex": ("OrcaFlex",),
    "digitalmodel.solvers.orcaflex.orcaflex_file_management": (
        "OrcaflexFileManagement",
    ),
    "digitalmodel.solvers.orcaflex.orcaflex_installation": ("OrcInstallation",),
    "digitalmodel.solvers.orcaflex.orcaflex_modal_analysis": ("OrcModalAnalysis",),
    "digitalmodel.solvers.orcaflex.umbilical_analysis_components": (
        "UmbilicalAnalysis",
    ),
    "digitalmodel.structural.pipe_capacity.pipe_capacity": ("PipeCapacity",),
    "digitalmodel.subsea.pipeline.pipeline": ("Pipeline",),
    "digitalmodel.marine_ops.ct_hydraulics.ct_hydraulics": ("CTHydraulics",),
    "digitalmodel.hydrodynamics.rao_analysis.rao_analysis": ("RAOAnalysis",),
    "digitalmodel.signal_processing.time_series.time_series_analysis": (
        "TimeSeriesAnalysis",
    ),
    "digitalmodel.infrastructure.transformation.transformation": ("Transformation",),
    "digitalmodel.subsea.vertical_riser.vertical_riser": ("vertical_riser",),
    "digitalmodel.subsea.viv_analysis.viv_analysis": ("VIVAnalysis",),
    "digitalmodel.infrastructure.base_solvers.structural.plate_buckling": (
        "PlateBuckling",
    ),
    "digitalmodel.solvers.orcaflex.output_control": (
        "OutputController",
        "get_output_level_from_argv",
    ),
    "loguru": ("logger",),
    "digitalmodel.specialized.rigging.rigging": ("Rigging",),
    "digitalmodel.subsea.catenary_riser.legacy.catenary": ("Catenary",),
    "digitalmodel.specialized.gis": ("GISModule",),
}


@contextmanager
def isolated_engine():
    """Restore cache entries and the parent binding even when a test raises.

    Register missing ancestors explicitly so importing a stub never executes a
    real package initializer against fake dependencies. Restore only the keys
    changed here, rather than rolling back unrelated imports by pytest.
    """
    with pytest.MonkeyPatch.context() as scoped:
        for name, attributes in DEPENDENCIES.items():
            parts = name.split(".")
            for end in range(1, len(parts)):
                parent = ".".join(parts[:end])
                if parent not in sys.modules:
                    package = ModuleType(parent)
                    package.__path__ = []
                    scoped.setitem(sys.modules, parent, package)
            module = ModuleType(name)
            module.__path__ = []
            for attribute in attributes:
                setattr(module, attribute, MagicMock(name=f"{name}.{attribute}"))
            if name == "assetutilities.common.update_deep":
                module.AttributeDict = AttributeDict
            scoped.setitem(sys.modules, name, module)

        source = (
            Path(__file__).resolve().parents[1] / "src" / "digitalmodel" / "engine.py"
        )
        spec = importlib.util.spec_from_file_location("digitalmodel.engine", source)
        module = importlib.util.module_from_spec(spec)
        scoped.setitem(sys.modules, spec.name, module)
        scoped.setattr(digitalmodel, "engine", module, raising=False)
        spec.loader.exec_module(module)
        yield module


@pytest.fixture
def engine_test_doubles(request, monkeypatch):
    """Bind each requesting test module to its own engine and dictionaries."""
    with isolated_engine() as module:
        monkeypatch.setattr(request.module, "engine", module.engine, raising=False)
        monkeypatch.setattr(
            request.module, "AttributeDict", AttributeDict, raising=False
        )
        yield module
