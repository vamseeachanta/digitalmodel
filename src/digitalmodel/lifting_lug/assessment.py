# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Typed padeye screening with explicit resistance inputs and coverage gaps.

No ABS resistance coefficient, lifting load factor or weld minimum is inferred.
The existing lifting-lug workflow and this assessment share plate mechanics.
Only a centred, axial, in-plane single-plate load and a concentric direct-shear
weld load path are represented. Supply effective weld length and throat;
minimum weld size, weld-group moments and practical detailing remain unresolved.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Literal

from digitalmodel.lifting_lug.mechanics import PlateDemands, plate_demands_mpa

Status = Literal["PASS", "FAIL", "NOT_EVALUATED"]
NORMAL_SOURCE = "MIT, Stress-Strain Curves (2001), equation (1): sigma=P/A"
SHEAR_SOURCE = "MIT, Shear and Torsion (2000), page 2: direct shear tau=P/A"
LIFTING_SOURCE = "NASA-STD-8719.9C (2024), section 13.3.2.4"


def _positive(value: float, name: str) -> None:
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")


def _basis(value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("a nonempty source or assumption basis is required")


@dataclass(frozen=True)
class PlateGeometry:
    """Single plate, centred hole; dimensions in mm, edge measured along load."""

    thickness_mm: float
    pin_diameter_mm: float
    hole_diameter_mm: float
    width_mm: float
    centre_to_loaded_edge_mm: float

    def __post_init__(self) -> None:
        """Reject impossible geometry before assessment."""
        self.demands(0)

    def demands(self, load_kn: float) -> PlateDemands:
        """Use the same nominal stress model as the existing workflow."""
        return plate_demands_mpa(
            load_kn, self.thickness_mm, self.pin_diameter_mm,
            self.hole_diameter_mm, self.width_mm, self.centre_to_loaded_edge_mm,
        )


@dataclass(frozen=True)
class SlingLoad:
    """Given sling tension including angle; factors and their basis are required.

    Multiplication defines the supplied load case. No standard minimum factors
    or load sharing assumptions are introduced by this assessment.
    """

    static_load_kn: float
    dynamic_amplification: float
    skew_factor: float
    basis: str

    def __post_init__(self) -> None:
        """Reject invalid numeric inputs and missing provenance."""
        if not math.isfinite(self.static_load_kn) or self.static_load_kn < 0:
            raise ValueError("static_load_kn must be finite and nonnegative")
        _positive(self.dynamic_amplification, "dynamic_amplification")
        _positive(self.skew_factor, "skew_factor")
        _basis(self.basis)


@dataclass(frozen=True)
class StressLimit:
    """User-selected allowable in MPa, with edition/clause or assumption basis.

    Source text is retained, not authenticated; entering a limit cannot establish
    code applicability or compliance. No yield-strength multiplier is assumed.
    """

    allowable_mpa: float
    source: str

    def __post_init__(self) -> None:
        """Require a finite resistance and an explicit provenance statement."""
        _positive(self.allowable_mpa, "allowable_mpa")
        _basis(self.source)


@dataclass(frozen=True)
class PadeyeAllowables:
    """Omitted allowables result in NOT_EVALUATED checks."""

    pin_bearing: StressLimit | None = None
    net_tension: StressLimit | None = None
    shear_tearout: StressLimit | None = None
    weld_direct_shear: StressLimit | None = None


@dataclass(frozen=True)
class DirectWeld:
    """Total effective length and effective throat in mm, for direct shear only.

    Total length is the sum of effective load-carrying segments; no implicit
    doubling for two sides, end deduction or fillet-leg conversion is applied.
    The caller must establish that the load passes through the weld-group
    centroid and produces only direct shear on the effective throat.
    """

    effective_length_mm: float
    effective_throat_mm: float

    def __post_init__(self) -> None:
        """Reject zero, negative and nonfinite weld dimensions."""
        _positive(self.effective_length_mm, "effective_length_mm")
        _positive(self.effective_throat_mm, "effective_throat_mm")


@dataclass(frozen=True)
class CheckResult:
    """A demand comparison or an explicit unevaluated requirement."""

    name: str
    demand_mpa: float | None
    allowable_mpa: float | None
    utilization: float | None
    status: Status
    method_source: str
    allowable_source: str
    reason: str


@dataclass(frozen=True)
class PadeyeAssessment:
    """Inputs and results; nominal passing checks do not imply qualification."""

    plate: PlateGeometry
    load: SlingLoad
    allowables: PadeyeAllowables
    weld: DirectWeld | None
    design_load_kn: float
    checks: tuple[CheckResult, ...]
    qualification_checks: tuple[CheckResult, ...]
    required_weld_throat_mm: float | None
    screening_status: Status
    status: Status


def _compare(name: str, demand: float | None, limit: StressLimit | None,
             method: str) -> CheckResult:
    if demand is None or limit is None:
        return CheckResult(
            name, demand, limit.allowable_mpa if limit else None, None,
            "NOT_EVALUATED", method, limit.source if limit else "",
            "Effective weld geometry is missing." if demand is None
            else "An allowable and its source are required.",
        )
    utilization = demand / limit.allowable_mpa
    if not math.isfinite(utilization):
        raise ValueError("derived utilization must be finite")
    # Unity is the definition of demand/allowable, not a rule-derived factor.
    return CheckResult(
        name, demand, limit.allowable_mpa, utilization,
        "PASS" if utilization <= 1 else "FAIL", method, limit.source,
        "Comparison uses the supplied allowable; rule applicability is unresolved.",
    )


def _status(checks: tuple[CheckResult, ...]) -> Status:
    if any(check.status == "FAIL" for check in checks):
        return "FAIL"
    if any(check.status == "NOT_EVALUATED" for check in checks):
        return "NOT_EVALUATED"
    return "PASS"


def assess_padeye(
    plate: PlateGeometry,
    load: SlingLoad,
    allowables: PadeyeAllowables,
    weld: DirectWeld | None = None,
) -> PadeyeAssessment:
    """Assess nominal plate stresses and a direct-shear weld under one load case.

    Effective throat sizing rearranges tau=P/(L*a), from MIT, Shear and
    Torsion (2000), p. 2: a_required=P/(L*tau_allow). It imposes no fabrication
    minimum, fatigue allowance or weld-group moment resistance.
    """
    design_load = load.static_load_kn * load.dynamic_amplification * load.skew_factor
    if not math.isfinite(design_load):
        raise ValueError("derived design load must be finite")
    demands = plate.demands(design_load)
    weld_demand = None
    required_throat = None
    if weld is not None:
        force_n = design_load * 1000  # SI kN to N; stress is N/mm² = MPa.
        area = weld.effective_length_mm * weld.effective_throat_mm
        _positive(area, "derived weld area")
        weld_demand = force_n / area
        if not math.isfinite(weld_demand):
            raise ValueError("derived weld stress must be finite")
        if allowables.weld_direct_shear is not None:
            capacity_per_throat = (
                weld.effective_length_mm * allowables.weld_direct_shear.allowable_mpa
            )
            _positive(capacity_per_throat, "derived weld capacity per throat")
            required_throat = force_n / capacity_per_throat
            if not math.isfinite(required_throat):
                raise ValueError("derived required throat must be finite")
    checks = (
        _compare("pin_bearing", demands.pin_bearing,
                 allowables.pin_bearing, NORMAL_SOURCE),
        _compare("net_tension", demands.net_tension,
                 allowables.net_tension, NORMAL_SOURCE),
        _compare("shear_tearout", demands.shear_tearout,
                 allowables.shear_tearout, SHEAR_SOURCE),
        _compare("weld_direct_shear", weld_demand,
                 allowables.weld_direct_shear, SHEAR_SOURCE),
    )
    qualification = (
        CheckResult(
            "abs_compliance", None, None, None, "NOT_EVALUATED",
            "ABS lifting-appliance rules: applicable edition and clause evidence required",
            "", "Accessible ABS equation and allowable evidence is unavailable. "
            "Supply the applicable edition, clauses and applicability assessment.",
        ),
        CheckResult(
            "lifting_qualification", None, None, None, "NOT_EVALUATED",
            LIFTING_SOURCE, "",
            "Analysis alone does not establish lifting qualification. This clause "
            "conditions substitution of analysis for testing of permanently attached "
            "interfaces on documented rationale and approval by the designated authority; "
            "its applicability and approval are not established here.",
        ),
    )
    return PadeyeAssessment(
        plate, load, allowables, weld, design_load, checks, qualification,
        required_throat, _status(checks), _status(checks + qualification),
    )
