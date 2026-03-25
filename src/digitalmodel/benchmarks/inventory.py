"""
Example Model Inventory — classification of all digitalmodel examples.

Each example is classified into one of four analysis domains:
    statics_only      — static structural / catenary analysis only
    time_domain       — produces time histories (needs random wave seed)
    frequency_domain  — produces RAOs, PSD, added mass / damping
    both              — supports both time-domain and frequency-domain

Usage
-----
    from digitalmodel.benchmarks.inventory import (
        ModelCategory,
        ModelInventoryEntry,
        build_model_inventory,
    )
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import List


# ---------------------------------------------------------------------------
# ModelCategory
# ---------------------------------------------------------------------------
class ModelCategory(str, Enum):
    """Classification of an example model by available analysis domains."""
    STATICS_ONLY = "statics_only"
    TIME_DOMAIN = "time_domain"
    FREQUENCY_DOMAIN = "frequency_domain"
    BOTH = "both"


# ---------------------------------------------------------------------------
# ModelInventoryEntry
# ---------------------------------------------------------------------------
@dataclass
class ModelInventoryEntry:
    """A single entry in the example model inventory.

    Parameters
    ----------
    name:
        Unique model identifier (kebab-case or snake_case).
    category:
        Analysis domain classification.
    path:
        Relative path from digitalmodel root to the example file.
    description:
        One-line description of what the example demonstrates.
    tags:
        Optional free-form tags (e.g. mooring, fatigue, riser).
    """
    name: str
    category: ModelCategory
    path: Path
    description: str
    tags: List[str] = field(default_factory=list)

    @property
    def has_time_domain(self) -> bool:
        return self.category in (ModelCategory.TIME_DOMAIN, ModelCategory.BOTH)

    @property
    def has_frequency_domain(self) -> bool:
        return self.category in (
            ModelCategory.FREQUENCY_DOMAIN, ModelCategory.BOTH)


# ---------------------------------------------------------------------------
# Inventory builder
# ---------------------------------------------------------------------------
def build_model_inventory() -> List[ModelInventoryEntry]:
    """Return the classified inventory of all example models.

    Returns
    -------
    list of ModelInventoryEntry, one per example model.
    """
    return [
        # ----------------------------------------------------------------
        # Catenary / Mooring statics
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="lazy_wave_catenary",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/mooring/lazy_wave_example.py"),
            description="Lazy-wave riser catenary static geometry and forces",
            tags=["catenary", "riser", "statics"],
        ),
        ModelInventoryEntry(
            name="mooring_component_design",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/mooring/mooring_example.py"),
            description="Mooring component database and catenary/taut-leg design",
            tags=["mooring", "catenary", "taut-leg", "statics"],
        ),
        # ----------------------------------------------------------------
        # Dynamic mooring / FPSO — time domain
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="spread_mooring_8point",
            category=ModelCategory.TIME_DOMAIN,
            path=Path("examples/domains/workflows/01_spread_mooring_8point.py"),
            description="8-point spread mooring FPSO in 1500 m water depth",
            tags=["mooring", "fpso", "spread", "time_domain", "api_rp_2sk"],
        ),
        ModelInventoryEntry(
            name="turret_mooring",
            category=ModelCategory.TIME_DOMAIN,
            path=Path("examples/domains/workflows/02_turret_mooring.py"),
            description="Internal turret mooring with 360-degree weathervaning",
            tags=["mooring", "fpso", "turret", "time_domain"],
        ),
        ModelInventoryEntry(
            name="calm_buoy_workflow",
            category=ModelCategory.TIME_DOMAIN,
            path=Path("examples/domains/workflows/03_calm_buoy.py"),
            description="CALM buoy mooring and hawser analysis",
            tags=["calm_buoy", "mooring", "time_domain"],
        ),
        ModelInventoryEntry(
            name="fpso_mooring_analysis",
            category=ModelCategory.TIME_DOMAIN,
            path=Path("examples/domains/fpso/fpso_mooring_analysis.py"),
            description="FPSO mooring analysis with environmental loading",
            tags=["fpso", "mooring", "time_domain"],
        ),
        # ----------------------------------------------------------------
        # Installation — static / quasi-static
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="pipeline_installation",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/workflows/04_pipeline_installation.py"),
            description="S-lay/J-lay pipeline installation analysis",
            tags=["pipeline", "installation", "statics"],
        ),
        ModelInventoryEntry(
            name="jackup_installation",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/workflows/05_jackup_installation.py"),
            description="Jack-up rig installation and load-out analysis",
            tags=["jackup", "installation", "statics"],
        ),
        # ----------------------------------------------------------------
        # Floating wind / wave energy — both domains
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="floating_wind_turbine",
            category=ModelCategory.BOTH,
            path=Path("examples/domains/workflows/06_floating_wind_turbine.py"),
            description="Floating offshore wind turbine dynamics (FOWT)",
            tags=["wind", "fowt", "mooring", "time_domain", "frequency_domain"],
        ),
        ModelInventoryEntry(
            name="wave_energy_converter",
            category=ModelCategory.BOTH,
            path=Path("examples/domains/workflows/07_wave_energy_converter.py"),
            description="Wave energy converter (WEC) motion and power analysis",
            tags=["wave_energy", "time_domain", "frequency_domain"],
        ),
        # ----------------------------------------------------------------
        # Hydrodynamics — frequency domain RAOs
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="hydrodynamics_rao_charts",
            category=ModelCategory.FREQUENCY_DOMAIN,
            path=Path(
                "examples/domains/hydrodynamics/generate_hydro_charts.py"),
            description="Vessel RAO and hydrodynamic coefficient charts",
            tags=["hydrodynamics", "rao", "frequency_domain"],
        ),
        ModelInventoryEntry(
            name="hydrodynamics_cache_integration",
            category=ModelCategory.FREQUENCY_DOMAIN,
            path=Path(
                "examples/domains/hydrodynamics/"
                "hydrodynamics_cache_integration.py"
            ),
            description=(
                "Cached hydrodynamic coefficient loading and RAO queries"),
            tags=["hydrodynamics", "rao", "frequency_domain", "cache"],
        ),
        # ----------------------------------------------------------------
        # Fatigue — time domain and frequency domain
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="fatigue_analysis_basic",
            category=ModelCategory.BOTH,
            path=Path(
                "examples/domains/fatigue/fatigue_analysis_examples.py"),
            description=(
                "Fatigue analysis: S-N curves, rainflow (time domain), "
                "spectral (frequency domain)"
            ),
            tags=["fatigue", "sn_curve", "rainflow", "time_domain",
                  "frequency_domain"],
        ),
        ModelInventoryEntry(
            name="fatigue_analysis_advanced",
            category=ModelCategory.BOTH,
            path=Path(
                "examples/domains/fatigue/advanced_examples/"
                "complete_fatigue_analysis.py"
            ),
            description=(
                "Complete fatigue analysis with Dirlik and TB methods"),
            tags=["fatigue", "dirlik", "tovo_benasciutti", "time_domain",
                  "frequency_domain"],
        ),
        # ----------------------------------------------------------------
        # Structural / stress — statics
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="apistd2rd_pipe_stress",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/api_standards/apistd2rd_demo.py"),
            description=(
                "API STD 2RD pipe stress analysis (burst, collapse, bending)"),
            tags=["api_std_2rd", "structural", "statics"],
        ),
        ModelInventoryEntry(
            name="plate_capacity",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/stress/plate_capacity_examples.py"),
            description="Plate buckling and capacity calculation examples",
            tags=["plate", "buckling", "structural", "statics"],
        ),
        ModelInventoryEntry(
            name="wall_thickness_design",
            category=ModelCategory.STATICS_ONLY,
            path=Path(
                "examples/domains/structural/wall_thickness_example.py"),
            description=(
                "Pipeline wall thickness design per pressure containment"),
            tags=["pipeline", "wall_thickness", "structural", "statics"],
        ),
        # ----------------------------------------------------------------
        # Metocean / environmental data (no solver domain)
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="metocean_streaming",
            category=ModelCategory.STATICS_ONLY,
            path=Path(
                "examples/domains/metocean/metocean_streaming_example.py"),
            description=(
                "Metocean data streaming: Hs, Tp, wind speed from databases"),
            tags=["metocean", "data", "environmental"],
        ),
        ModelInventoryEntry(
            name="vessel_data_streaming",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/vessel/vessel_streaming_example.py"),
            description="Vessel performance and resistance data streaming",
            tags=["vessel", "data", "streaming"],
        ),
        # ----------------------------------------------------------------
        # OrcaFlex workflows (requires OrcFxAPI on solver-tier run)
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="orcaflex_riser",
            category=ModelCategory.TIME_DOMAIN,
            path=Path("examples/domains/orcaflex/riser_example.py"),
            description="OrcaFlex riser time-domain dynamic analysis",
            tags=["orcaflex", "riser", "time_domain"],
        ),
        ModelInventoryEntry(
            name="orcaflex_complete_workflow",
            category=ModelCategory.BOTH,
            path=Path(
                "examples/domains/orcaflex/complete_orcaflex_workflow.py"),
            description=(
                "Complete OrcaFlex workflow: statics, dynamics, "
                "frequency-domain post-processing"
            ),
            tags=["orcaflex", "time_domain", "frequency_domain"],
        ),
        # ----------------------------------------------------------------
        # Artificial lift — dynacard (time-domain card analysis)
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="dynacard_analysis",
            category=ModelCategory.TIME_DOMAIN,
            path=Path(
                "examples/domains/artificial_lift/dynacard_demo.py"),
            description=(
                "Dynacard surface and downhole card analysis for rod pumps"),
            tags=["artificial_lift", "dynacard", "time_domain"],
        ),
        # ----------------------------------------------------------------
        # CALM buoy project YAML spec
        # ----------------------------------------------------------------
        ModelInventoryEntry(
            name="calm_buoy_north_sea",
            category=ModelCategory.TIME_DOMAIN,
            path=Path(
                "examples/domains/calm_buoy/north_sea_calm_project.yml"),
            description=(
                "North Sea CALM buoy mooring project specification"),
            tags=["calm_buoy", "mooring", "time_domain", "yaml"],
        ),
    ]
