# ABOUTME: WellDesign — slim/standard/big hole bore design comparison module
# ABOUTME: Covers casing programs, cost estimation, production potential, risk assessment, and decision matrix

"""
WellDesign
==========

Well bore design module comparing slim hole, standard hole, and big hole
drilling configurations.  Each configuration carries a casing program
sourced from issue #1958 research data.

Key outputs:
    casing_program()       — ordered list of CasingString from conductor to liner
    estimate_cost()        — CostBreakdown with drilling, casing, completion, total (USD)
    production_potential()  — estimated steady-state production rate, bbl/day
    risk_assessment()      — dict of risk factor scores (0-10 scale)
    recommend_hole_type()  — decision-matrix recommendation based on water depth + objective

Casing data (from issue research):
    Big Hole:  30"@30m → 20"@400m → 13-5/8"@1000m → 10-3/4"@1800m
    Standard:  30"@30m → 20"@150m → 13-3/4"@400m → 9-5/8"@1000m → 7"@1800m
    Slim:      9-5/8"@12m → 7"@100m → 5-1/2"@600m → 3-1/2"@1200m → 2.9"@2000m
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class CasingString:
    """Single casing string in a well design.

    Attributes
    ----------
    name : str
        Descriptive name (e.g. "conductor", "surface", "intermediate", "production", "liner").
    od_inch : float
        Outer diameter, inches.
    id_inch : float
        Inner diameter, inches.
    grade : str
        Steel grade designation (e.g. "K-55", "N-80", "P-110").
    set_depth_m : float
        Setting depth below mudline/seabed, metres.
    """

    name: str
    od_inch: float
    id_inch: float
    grade: str
    set_depth_m: float


@dataclass(frozen=True)
class CostBreakdown:
    """Cost estimate for a well bore design.

    Attributes
    ----------
    drilling_usd : float
        Drilling cost (rig time, bits, fuel), USD.
    casing_usd : float
        Casing material and running cost, USD.
    completion_usd : float
        Completion hardware and operations cost, USD.
    total_usd : float
        Sum of all cost components, USD.
    """

    drilling_usd: float
    casing_usd: float
    completion_usd: float
    total_usd: float


# ---- Casing program lookup tables ----

_BIG_HOLE_CASINGS = [
    CasingString(name="conductor", od_inch=30.0, id_inch=28.0, grade="X-56", set_depth_m=30.0),
    CasingString(name="surface", od_inch=20.0, id_inch=18.73, grade="K-55", set_depth_m=400.0),
    CasingString(name="intermediate", od_inch=13.625, id_inch=12.415, grade="N-80", set_depth_m=1000.0),
    CasingString(name="production", od_inch=10.75, id_inch=10.05, grade="P-110", set_depth_m=1800.0),
]

_STANDARD_CASINGS = [
    CasingString(name="conductor", od_inch=30.0, id_inch=28.0, grade="X-56", set_depth_m=30.0),
    CasingString(name="surface", od_inch=20.0, id_inch=18.73, grade="K-55", set_depth_m=150.0),
    CasingString(name="intermediate_1", od_inch=13.75, id_inch=12.615, grade="N-80", set_depth_m=400.0),
    CasingString(name="intermediate_2", od_inch=9.625, id_inch=8.681, grade="N-80", set_depth_m=1000.0),
    CasingString(name="production", od_inch=7.0, id_inch=6.184, grade="P-110", set_depth_m=1800.0),
]

_SLIM_HOLE_CASINGS = [
    CasingString(name="conductor", od_inch=9.625, id_inch=8.681, grade="N-80", set_depth_m=12.0),
    CasingString(name="surface", od_inch=7.0, id_inch=6.184, grade="N-80", set_depth_m=100.0),
    CasingString(name="intermediate", od_inch=5.5, id_inch=4.892, grade="N-80", set_depth_m=600.0),
    CasingString(name="production", od_inch=3.5, id_inch=2.992, grade="P-110", set_depth_m=1200.0),
    CasingString(name="liner", od_inch=2.9, id_inch=2.441, grade="P-110", set_depth_m=2000.0),
]

_CASING_PROGRAMS: dict[str, list[CasingString]] = {
    "big_hole": _BIG_HOLE_CASINGS,
    "standard_hole": _STANDARD_CASINGS,
    "slim_hole": _SLIM_HOLE_CASINGS,
}

# ---- Cost model parameters (baseline USD at reference water depth) ----

_COST_PARAMS: dict[str, dict[str, float]] = {
    "big_hole": {
        "drilling_usd": 18_000_000.0,
        "casing_usd": 6_000_000.0,
        "completion_usd": 8_000_000.0,
    },
    "standard_hole": {
        "drilling_usd": 12_000_000.0,
        "casing_usd": 4_000_000.0,
        "completion_usd": 5_500_000.0,
    },
    "slim_hole": {
        "drilling_usd": 6_000_000.0,
        "casing_usd": 2_000_000.0,
        "completion_usd": 3_500_000.0,
    },
}

# ---- Production potential (bbl/day) ----

_PRODUCTION_RATES: dict[str, float] = {
    "big_hole": 25_000.0,
    "standard_hole": 15_000.0,
    "slim_hole": 5_000.0,
}

# ---- Risk factor scores (0-10, higher = riskier) ----

_RISK_SCORES: dict[str, dict[str, float]] = {
    "big_hole": {
        "stuck_pipe": 3.0,
        "lost_circulation": 7.0,
        "wellbore_instability": 5.0,
        "overall_score": 5.0,
    },
    "standard_hole": {
        "stuck_pipe": 4.0,
        "lost_circulation": 4.0,
        "wellbore_instability": 4.0,
        "overall_score": 4.0,
    },
    "slim_hole": {
        "stuck_pipe": 8.0,
        "lost_circulation": 2.0,
        "wellbore_instability": 6.0,
        "overall_score": 5.5,
    },
}

# ---- Decision matrix valid objectives ----

_VALID_OBJECTIVES = {
    "exploration",
    "appraisal",
    "development",
    "high_rate_development",
}


class WellDesign:
    """Well bore design for slim/standard/big hole comparison.

    Parameters
    ----------
    hole_type : str
        One of ``"big_hole"``, ``"standard_hole"``, or ``"slim_hole"``.
    water_depth_m : float
        Water depth in metres.  Must be > 0.
    """

    HOLE_TYPES: set[str] = {"big_hole", "standard_hole", "slim_hole"}

    def __init__(self, hole_type: str, water_depth_m: float) -> None:
        if hole_type not in self.HOLE_TYPES:
            raise ValueError(
                f"hole_type must be one of {sorted(self.HOLE_TYPES)}, got {hole_type!r}"
            )
        if water_depth_m <= 0:
            raise ValueError(
                f"water_depth_m must be positive, got {water_depth_m}"
            )
        self.hole_type = hole_type
        self.water_depth_m = water_depth_m

    # ------------------------------------------------------------------ #
    # Public API
    # ------------------------------------------------------------------ #

    def casing_program(self) -> list[CasingString]:
        """Return the ordered casing program for this hole type.

        Returns
        -------
        list[CasingString]
            Casing strings ordered from surface (largest OD) to deepest (smallest OD).
        """
        return list(_CASING_PROGRAMS[self.hole_type])

    def estimate_cost(self) -> CostBreakdown:
        """Estimate well cost based on hole type and water depth.

        A simple depth multiplier is applied: costs increase linearly
        by 10% per 100 m beyond the 500 m reference depth, and decrease
        proportionally for shallower water.

        Returns
        -------
        CostBreakdown
            Itemised cost estimate in USD.
        """
        params = _COST_PARAMS[self.hole_type]
        depth_factor = 1.0 + 0.001 * (self.water_depth_m - 500.0)
        depth_factor = max(depth_factor, 0.5)  # floor at 50%

        drilling = params["drilling_usd"] * depth_factor
        casing = params["casing_usd"] * depth_factor
        completion = params["completion_usd"] * depth_factor

        return CostBreakdown(
            drilling_usd=drilling,
            casing_usd=casing,
            completion_usd=completion,
            total_usd=drilling + casing + completion,
        )

    def production_potential(self) -> float:
        """Estimate steady-state production potential in bbl/day.

        Returns
        -------
        float
            Estimated production rate, bbl/day.
        """
        return _PRODUCTION_RATES[self.hole_type]

    def risk_assessment(self) -> dict[str, float]:
        """Assess drilling risk factors for this hole configuration.

        Returns
        -------
        dict[str, float]
            Risk factor name → score on 0-10 scale (10 = highest risk).
            Always includes: ``stuck_pipe``, ``lost_circulation``,
            ``wellbore_instability``, ``overall_score``.
        """
        return dict(_RISK_SCORES[self.hole_type])

    def recommend_hole_type(
        self, water_depth_m: float, objective: str
    ) -> str:
        """Decision matrix: recommend a hole type for the given scenario.

        Parameters
        ----------
        water_depth_m : float
            Planned water depth, metres.
        objective : str
            Well objective — one of ``"exploration"``, ``"appraisal"``,
            ``"development"``, ``"high_rate_development"``.

        Returns
        -------
        str
            Recommended hole type string.

        Raises
        ------
        ValueError
            If *objective* is not a recognised value.
        """
        if objective not in _VALID_OBJECTIVES:
            raise ValueError(
                f"objective must be one of {sorted(_VALID_OBJECTIVES)}, got {objective!r}"
            )

        if objective == "high_rate_development":
            return "big_hole"

        if objective == "development":
            return "standard_hole"

        # exploration or appraisal
        if water_depth_m > 1000.0:
            # Deep water exploration/appraisal: slim hole too risky
            return "standard_hole"

        return "slim_hole"
