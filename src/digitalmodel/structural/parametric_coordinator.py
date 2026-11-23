# ABOUTME: Unified parametric study coordinator (WRK-256) — dispatches wall-thickness,
# ABOUTME: fatigue, and OrcaFlex campaign sweeps and aggregates results into a single report.

"""Unified parametric study coordinator (WRK-256).

Dispatches Cartesian sweeps to wall-thickness, fatigue, and OrcaFlex
campaign engines and aggregates results. HTML builders live in
:mod:`digitalmodel.structural.parametric_report`.

Study types: WALL_THICKNESS, FATIGUE, ORCAFLEX_CAMPAIGN, COMBINED_WT_FATIGUE.
OrcaFlex path raises :class:`OrcaFlexUnavailableError` when OrcFxAPI is absent.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional, Tuple

import pandas as pd

from digitalmodel.structural.analysis.wall_thickness import DesignCode
from digitalmodel.structural.analysis.wall_thickness_parametric import (
    ParametricSweep,
    SweepConfig,
)
from digitalmodel.structural.fatigue.parametric_sweep import (
    generate_sweep_report as _gen_fatigue_html,
    run_sweep as _run_fatigue_sweep,
)
from digitalmodel.structural.parametric_report import (
    build_combined_html,
    build_simple_html,
    build_wt_html,
)

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# OrcaFlex availability
# ---------------------------------------------------------------------------

try:
    import OrcFxAPI  # noqa: F401
    ORCAFLEX_AVAILABLE: bool = True
except ImportError:
    ORCAFLEX_AVAILABLE = False


# ---------------------------------------------------------------------------
# Public exceptions
# ---------------------------------------------------------------------------

class OrcaFlexUnavailableError(RuntimeError):
    """Raised when an OrcaFlex campaign study is requested but OrcFxAPI is
    not installed in the current environment."""


# ---------------------------------------------------------------------------
# Study type enum
# ---------------------------------------------------------------------------

class StudyType(Enum):
    """Enumeration of supported parametric study types."""

    WALL_THICKNESS = "wall_thickness"
    FATIGUE = "fatigue"
    ORCAFLEX_CAMPAIGN = "orcaflex_campaign"
    COMBINED_WT_FATIGUE = "combined_wt_fatigue"


# ---------------------------------------------------------------------------
# Parameter dataclasses
# ---------------------------------------------------------------------------

@dataclass
class WallThicknessParams:
    """Parameters for a wall-thickness utilisation sweep.

    Wraps :class:`~digitalmodel.structural.analysis.wall_thickness_parametric.SweepConfig`.
    """

    wall_thicknesses: List[float]
    outer_diameters: List[float]
    grades: List[str]
    internal_pressures: List[float]
    external_pressures: List[float]
    bending_moments: List[float] = field(default_factory=lambda: [0.0])
    effective_tensions: List[float] = field(default_factory=lambda: [0.0])
    code: DesignCode = DesignCode.DNV_ST_F101
    corrosion_allowance: float = 0.0

    def __post_init__(self) -> None:
        if not self.wall_thicknesses:
            raise ValueError("wall_thicknesses must not be empty")
        if not self.grades:
            raise ValueError("grades must not be empty")
        if not self.outer_diameters:
            raise ValueError("outer_diameters must not be empty")
        if not self.internal_pressures:
            raise ValueError("internal_pressures must not be empty")
        if not self.external_pressures:
            raise ValueError("external_pressures must not be empty")


@dataclass
class FatigueParams:
    """Parameters for a fatigue parametric sweep.

    Wraps :func:`~digitalmodel.structural.fatigue.parametric_sweep.run_sweep`.
    """

    histogram: pd.DataFrame
    scf_values: List[float]
    curves: List[Tuple[str, str]]
    thickness_values: List[float]
    dff_values: List[float]
    reference_thickness_mm: float = 25.0

    def __post_init__(self) -> None:
        if not self.curves:
            raise ValueError("curves must not be empty")
        if not isinstance(self.histogram, pd.DataFrame):
            raise TypeError("histogram must be a pandas DataFrame")


@dataclass
class OrcaFlexParams:
    """Parameters for an OrcaFlex campaign sweep.

    Wraps
    :class:`~digitalmodel.solvers.orcaflex.modular_generator.schema.campaign.CampaignMatrix`.
    """

    water_depths: List[float]
    route_lengths: Optional[List[float]] = None
    tensions: Optional[List[float]] = None

    def __post_init__(self) -> None:
        if not self.water_depths:
            raise ValueError("water_depths must not be empty")


# ---------------------------------------------------------------------------
# StudySpec — validated input bundle
# ---------------------------------------------------------------------------

@dataclass
class StudySpec:
    """Validated input specification for a coordinator run.

    Raises :class:`ValueError` if the required parameter block for
    ``study_type`` is absent.
    """

    study_type: StudyType
    wt_params: Optional[WallThicknessParams] = None
    fatigue_params: Optional[FatigueParams] = None
    ofx_params: Optional[OrcaFlexParams] = None

    def __post_init__(self) -> None:
        if self.study_type == StudyType.WALL_THICKNESS and self.wt_params is None:
            raise ValueError("wt_params is required for StudyType.WALL_THICKNESS")
        if self.study_type == StudyType.FATIGUE and self.fatigue_params is None:
            raise ValueError("fatigue_params is required for StudyType.FATIGUE")
        if self.study_type == StudyType.ORCAFLEX_CAMPAIGN and self.ofx_params is None:
            raise ValueError("ofx_params is required for StudyType.ORCAFLEX_CAMPAIGN")
        if self.study_type == StudyType.COMBINED_WT_FATIGUE:
            if self.wt_params is None:
                raise ValueError(
                    "wt_params is required for StudyType.COMBINED_WT_FATIGUE"
                )
            if self.fatigue_params is None:
                raise ValueError(
                    "fatigue_params is required for StudyType.COMBINED_WT_FATIGUE"
                )


# ---------------------------------------------------------------------------
# StudyResult — aggregated output
# ---------------------------------------------------------------------------

@dataclass
class StudyResult:
    """Aggregated output from a coordinator run.

    Attributes
    ----------
    study_type:
        The study type that was executed.
    dataframe:
        Combined results DataFrame.  For ``COMBINED_WT_FATIGUE`` the frame
        merges both domains with a ``domain`` column distinguishing rows.
    summary:
        Scalar metrics: n_combinations, pass/fail tallies, study_type name.
    html_report:
        Self-contained HTML report string for browser review.
    """

    study_type: StudyType
    dataframe: pd.DataFrame
    summary: Dict
    html_report: str


# ---------------------------------------------------------------------------
# Coordinator
# ---------------------------------------------------------------------------

class ParametricCoordinator:
    """Orchestrates parametric sweeps across engineering domains.

    Usage::

        coord = ParametricCoordinator()
        result = coord.run(spec)
    """

    def run(self, spec: StudySpec) -> StudyResult:
        """Dispatch to the appropriate engine and return aggregated results.

        Raises
        ------
        OrcaFlexUnavailableError
            If ``study_type`` is ``ORCAFLEX_CAMPAIGN`` and OrcFxAPI is absent.
        ValueError
            If ``study_type`` is not recognised.
        """
        dispatch = {
            StudyType.WALL_THICKNESS: self._run_wall_thickness,
            StudyType.FATIGUE: self._run_fatigue,
            StudyType.ORCAFLEX_CAMPAIGN: self._run_orcaflex_campaign,
            StudyType.COMBINED_WT_FATIGUE: self._run_combined,
        }
        handler = dispatch.get(spec.study_type)
        if handler is None:
            raise ValueError(
                f"Unrecognised study_type: {spec.study_type!r}. "
                f"Expected one of {list(dispatch)}"
            )
        logger.info("Starting parametric study: %s", spec.study_type.name)
        result = handler(spec)
        logger.info(
            "Study complete: %s — %d combinations",
            spec.study_type.name,
            result.summary["n_combinations"],
        )
        return result

    # ------------------------------------------------------------------
    # Private engine dispatchers
    # ------------------------------------------------------------------

    def _run_wall_thickness(self, spec: StudySpec) -> StudyResult:
        p = spec.wt_params
        assert p is not None
        cfg = SweepConfig(
            wall_thicknesses=p.wall_thicknesses,
            outer_diameters=p.outer_diameters,
            grades=p.grades,
            internal_pressures=p.internal_pressures,
            external_pressures=p.external_pressures,
            bending_moments=p.bending_moments,
            effective_tensions=p.effective_tensions,
            corrosion_allowance=p.corrosion_allowance,
            code=p.code,
        )
        df = ParametricSweep(cfg).run()
        return StudyResult(
            study_type=StudyType.WALL_THICKNESS,
            dataframe=df,
            summary=_wt_summary(df, StudyType.WALL_THICKNESS),
            html_report=build_wt_html(df, "Wall Thickness Parametric Study"),
        )

    def _run_fatigue(self, spec: StudySpec) -> StudyResult:
        p = spec.fatigue_params
        assert p is not None
        df = _run_fatigue_sweep(
            histogram=p.histogram,
            scf_values=p.scf_values,
            curves=p.curves,
            thickness_values=p.thickness_values,
            dff_values=p.dff_values,
            reference_thickness_mm=p.reference_thickness_mm,
        )
        return StudyResult(
            study_type=StudyType.FATIGUE,
            dataframe=df,
            summary=_fatigue_summary(df, StudyType.FATIGUE),
            html_report=_gen_fatigue_html(p.histogram, df, "Fatigue Parametric Study"),
        )

    def _run_orcaflex_campaign(self, spec: StudySpec) -> StudyResult:
        if not ORCAFLEX_AVAILABLE:
            raise OrcaFlexUnavailableError(
                "OrcFxAPI is not installed in this environment. "
                "Install OrcFxAPI or run the OrcaFlex campaign on a "
                "licensed workstation. "
                "StudyType.ORCAFLEX_CAMPAIGN requires OrcFxAPI."
            )
        p = spec.ofx_params
        assert p is not None
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix,
        )
        matrix = CampaignMatrix(
            water_depths=p.water_depths,
            route_lengths=p.route_lengths,
            tensions=p.tensions,
        )
        df = pd.DataFrame(matrix.combinations())
        df["status"] = "generated"
        return StudyResult(
            study_type=StudyType.ORCAFLEX_CAMPAIGN,
            dataframe=df,
            summary={
                "study_type": StudyType.ORCAFLEX_CAMPAIGN.name,
                "n_combinations": len(df),
                "water_depths": p.water_depths,
            },
            html_report=build_simple_html(df, "OrcaFlex Campaign Parametric Study"),
        )

    def _run_combined(self, spec: StudySpec) -> StudyResult:
        wt_r = self._run_wall_thickness(spec)
        fat_r = self._run_fatigue(spec)
        wt_df = wt_r.dataframe.assign(domain="wall_thickness")
        fat_df = fat_r.dataframe.assign(domain="fatigue")
        combined = pd.concat([wt_df, fat_df], ignore_index=True, sort=False)
        return StudyResult(
            study_type=StudyType.COMBINED_WT_FATIGUE,
            dataframe=combined,
            summary={
                "study_type": StudyType.COMBINED_WT_FATIGUE.name,
                "n_combinations": len(combined),
                "wt_n_combinations": len(wt_df),
                "fatigue_n_combinations": len(fat_df),
                "wt_n_safe": int(wt_r.summary.get("n_safe", 0)),
                "fatigue_n_pass": int(fat_r.summary.get("n_pass", 0)),
                "fatigue_n_fail": int(fat_r.summary.get("n_fail", 0)),
            },
            html_report=build_combined_html(wt_r, fat_r),
        )


# ---------------------------------------------------------------------------
# Private summary helpers
# ---------------------------------------------------------------------------

def _wt_summary(df: pd.DataFrame, study_type: StudyType) -> Dict:
    n = len(df)
    n_safe = int(df["is_safe"].sum()) if "is_safe" in df.columns else 0
    return {
        "study_type": study_type.name,
        "n_combinations": n,
        "n_safe": n_safe,
        "n_unsafe": n - n_safe,
        "max_utilisation": float(df["max_utilisation"].max()) if n else float("nan"),
        "min_utilisation": float(df["max_utilisation"].min()) if n else float("nan"),
    }


def _fatigue_summary(df: pd.DataFrame, study_type: StudyType) -> Dict:
    n = len(df)
    n_pass = int(df["passes_dff"].sum()) if "passes_dff" in df.columns else 0
    return {
        "study_type": study_type.name,
        "n_combinations": n,
        "n_pass": n_pass,
        "n_fail": n - n_pass,
    }
