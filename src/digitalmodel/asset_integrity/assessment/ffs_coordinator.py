# ABOUTME: Canonical FFS coordinator — one data model + one orchestration call
# ABOUTME: chaining the validated Phase-1 assessment modules end to end.
"""Canonical Fitness-For-Service coordinator (Phase-1 path).

This is the single entry point and unified data model for an FFS metal-loss
assessment. It chains the validated Phase-1 modules in one call:

    GridParser -> FFSRouter -> Level1Screener -> Level2Engine
                -> FFSDecision -> MeasurementSufficiency

so callers (engine, reports, dashboards, the Deckhand API) get one
`FFSAssessmentResult` instead of having to wire six modules and remember each
field name. No assessment physics is implemented here — every number comes from
the modules that already have golden tests; this layer only orchestrates and
normalises the result.

The legacy `asset_integrity/common/API579_components.py` (GML/LML via the
`API579` engine basename) remains for backward compatibility but is **not** the
path new code should build on — see
`docs/domains/asset-integrity/ffs-architecture.md`.

Units: inches and psi (consistent with the Phase-1 modules).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional, Union

import numpy as np
import pandas as pd

from digitalmodel.codes import API_579
from .ffs_decision import FFSDecision
from .ffs_router import FFSRouter
from .grid_parser import GridParser
from .level1_screener import Level1Screener
from .level2_engine import Level2Engine
from .measurement_sufficiency import MeasurementSufficiency

GridLike = Union[pd.DataFrame, np.ndarray, str, Path]


@dataclass
class FFSComponent:
    """The component under assessment (geometry, code, material, condition)."""

    component_id: str
    design_code: str               # e.g. "B31.8", "B31.4", "ASME_VIII_DIV1"
    nominal_od_in: float
    nominal_wt_in: float
    design_pressure_psi: float
    smys_psi: Optional[float] = None
    corrosion_rate_in_per_yr: float = 0.0
    fca_in: float = 0.0            # future corrosion allowance
    rsf_a: float = 0.90            # allowable remaining strength factor
    component_type: str = "pipe"


@dataclass
class FFSAssessmentResult:
    """Unified FFS result — the canonical record for downstream consumers."""

    component_id: str
    assessment_type: str           # "GML" | "LML"
    level_reached: int             # 1 (screening sufficed) or 2 (RSF needed)
    t_nominal_in: float
    t_min_in: float
    t_measured_min_in: float
    t_measured_avg_in: float
    fca_in: float
    rsf: float
    rsf_a: float
    folias_factor: float
    remaining_life_yr: float
    verdict: str                   # ACCEPT/MONITOR/RE_RATE/REPAIR/REPLACE
    rerated_pressure_psi: float    # screening re-rate = P_design * min(1, rsf/rsf_a)
    sufficiency_status: str        # SUFFICIENT/TAKE_MORE/ESCALATE
    sufficiency: Any = None        # SufficiencyResult
    level1: dict = field(default_factory=dict)
    level2: dict = field(default_factory=dict)
    decision: dict = field(default_factory=dict)
    details: dict = field(default_factory=dict)
    code_reference: str = API_579.label  # governing FFS code

    @property
    def passes(self) -> bool:
        """True when the component is fit-for-service without re-rating."""
        return self.verdict in ("ACCEPT", "MONITOR")

    def to_dict(self) -> dict:
        """JSON-serialisable summary (for lookup tables / the Deckhand API)."""
        return {
            "component_id": self.component_id,
            "assessment_type": self.assessment_type,
            "level_reached": self.level_reached,
            "t_nominal_in": self.t_nominal_in,
            "t_min_in": self.t_min_in,
            "t_measured_min_in": self.t_measured_min_in,
            "t_measured_avg_in": self.t_measured_avg_in,
            "fca_in": self.fca_in,
            "rsf": self.rsf,
            "rsf_a": self.rsf_a,
            "folias_factor": self.folias_factor,
            "remaining_life_yr": self.remaining_life_yr,
            "verdict": self.verdict,
            "rerated_pressure_psi": self.rerated_pressure_psi,
            "sufficiency_status": self.sufficiency_status,
            "passes": self.passes,
            "code_reference": self.code_reference,
        }


def assess_component(
    component: FFSComponent,
    grid: GridLike,
    *,
    input_units: str = "in",
    force_type: Optional[str] = None,
) -> FFSAssessmentResult:
    """Run the full Phase-1 FFS chain and return one unified result.

    Args:
        component: the :class:`FFSComponent` being assessed.
        grid: wall-thickness measurement grid — a pandas DataFrame, a 2-D numpy
            array, or a path to a CSV.
        input_units: ``"in"`` (default) or ``"mm"`` — passed to GridParser.
        force_type: override the GML/LML auto-classification ("GML"/"LML").
    """
    df = _to_grid_df(grid, input_units)

    route = FFSRouter.classify(
        df, component.nominal_od_in, component.nominal_wt_in, force_type=force_type
    )
    atype = route["assessment_type"]

    screener = Level1Screener(
        design_code=component.design_code,
        nominal_od_in=component.nominal_od_in,
        design_pressure_psi=component.design_pressure_psi,
        smys_psi=component.smys_psi,
    )
    t_min = screener.t_min()
    t_mm = GridParser.min_thickness(df)
    t_am = GridParser.mean_thickness(df)
    l1 = screener.evaluate(t_mm)

    l2 = Level2Engine(
        assessment_type=atype,
        nominal_od_in=component.nominal_od_in,
        nominal_wt_in=component.nominal_wt_in,
        t_min_in=t_min,
        rsf_a=component.rsf_a,
    ).evaluate(df)

    decision = FFSDecision.decide(
        level1_verdict=l1["verdict"],
        level2_verdict=l2["verdict"],
        rsf=l2["rsf"],
        rsf_a=component.rsf_a,
        t_mm_in=t_mm,
        t_min_in=t_min,
        corrosion_rate_in_per_yr=component.corrosion_rate_in_per_yr,
        design_pressure_psi=component.design_pressure_psi,
    )

    sufficiency = MeasurementSufficiency().evaluate(
        df,
        nominal_wt_in=component.nominal_wt_in,
        assessment_type=atype,
        level1_result=l1,
        level2_result=l2,
    )

    rsf = float(l2["rsf"])
    # single source: the decision layer's MAWP_r (API 579-1 §2.4.2.2, #1273)
    _rr = decision.get("rerated_mawp_psi")
    rerated = float(_rr) if _rr is not None else float("nan")
    level_reached = 1 if l1["verdict"] == "ACCEPT" else 2

    return FFSAssessmentResult(
        component_id=component.component_id,
        assessment_type=atype,
        level_reached=level_reached,
        t_nominal_in=component.nominal_wt_in,
        t_min_in=float(t_min),
        t_measured_min_in=float(t_mm),
        t_measured_avg_in=float(t_am),
        fca_in=component.fca_in,
        rsf=rsf,
        rsf_a=component.rsf_a,
        folias_factor=float(l2.get("folias_factor", 1.0)),
        remaining_life_yr=float(decision.get("remaining_life_yr", float("nan"))),
        verdict=str(decision.get("verdict", "UNKNOWN")),
        rerated_pressure_psi=float(rerated),
        sufficiency_status=str(sufficiency.status),
        sufficiency=sufficiency,
        level1=l1,
        level2=l2,
        decision=decision,
        details={"router": route, "design_code": component.design_code},
    )


def _to_grid_df(grid: GridLike, input_units: str) -> pd.DataFrame:
    """Normalise any supported grid input to a thickness DataFrame (inches)."""
    if isinstance(grid, pd.DataFrame):
        return GridParser.from_dataframe(grid, input_units=input_units)
    if isinstance(grid, np.ndarray):
        return GridParser.from_numpy(grid, input_units=input_units)
    if isinstance(grid, (str, Path)):
        return GridParser.from_csv(str(grid), input_units=input_units)
    raise TypeError(
        f"grid must be a DataFrame, numpy array, or CSV path; got {type(grid)!r}."
    )
