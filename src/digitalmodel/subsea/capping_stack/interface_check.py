"""Capping-stack / wellhead interface compatibility (API 17W, API 17D).

Before a capping stack (API 17W) can land and seal on a flowing subsea
wellhead, the stack and the wellhead mandrel/hub must be mechanically and
pressure compatible.  This module encodes the *deterministic* interface
acceptance check used in emergency-response planning:

1. **Pressure-rating match** — the stack rated working pressure (API 17W
   pressure class) must be at least the wellhead rated working pressure so
   the stack can contain everything the wellhead can.
2. **Bore match** — the stack vertical bore must match (within tolerance)
   the wellhead through-bore so flow is routed and the seal lands.
3. **Connector / hub class match** — the stack lower connector must mate
   the wellhead hub family (e.g. an 18-3/4" subsea wellhead H4 mandrel per
   API 17D); a mismatched connector cannot make up.

The check returns a structured pass/fail with the failing reason
categories so an ops planner can see *why* a given stack is incompatible.

Notes / assumptions
-------------------
* Pressure classes and the canonical 18-3/4" subsea-wellhead bore/connector
  families are public API 17D / 17W summary values, encoded as enums.  Exact
  proprietary hub gasket geometry is out of scope (deferred).
* Bore match uses a default tolerance of 1 mm on nominal bore.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum

PSI_TO_PA = 6894.757
INCH_TO_M = 0.0254


class PressureClass(int, Enum):
    """API 17W / 17D rated-working-pressure classes [psi]."""

    PSI_5K = 5000
    PSI_10K = 10000
    PSI_15K = 15000
    PSI_20K = 20000

    @property
    def pascals(self) -> float:
        return float(self.value) * PSI_TO_PA


class ConnectorClass(str, Enum):
    """Subsea wellhead / capping-stack lower-connector hub families (API 17D)."""

    H4_18_75 = "H4-18.75"   # 18-3/4" H4 subsea wellhead mandrel (most common)
    DWHC = "DWHC"           # deepwater high-capacity collet connector
    HUB_16 = "HUB-16.75"    # 16-3/4" hub
    OTHER = "other"


@dataclass(frozen=True)
class WellheadSpec:
    """Subsea wellhead (or 17D tree) interface the stack must land on."""

    bore_inch: float                 # vertical through-bore [in]
    pressure_class: PressureClass
    connector_class: ConnectorClass


@dataclass(frozen=True)
class CappingStackSpec:
    """Capping-stack interface (API 17W)."""

    bore_inch: float                 # vertical bore [in]
    pressure_class: PressureClass
    connector_class: ConnectorClass


@dataclass
class InterfaceResult:
    """Result of a capping-stack / wellhead interface compatibility check."""

    compatible: bool
    reasons: list = field(default_factory=list)   # failing-category reasons


def check_compatibility(
    stack: CappingStackSpec,
    wellhead: WellheadSpec,
    bore_tolerance_inch: float = 1.0 / 25.4,   # 1 mm
) -> InterfaceResult:
    """Check capping-stack / wellhead interface compatibility (API 17W/17D).

    Parameters
    ----------
    stack : CappingStackSpec
        The candidate capping stack interface.
    wellhead : WellheadSpec
        The subsea wellhead / tree the stack must land and seal on.
    bore_tolerance_inch : float
        Allowed nominal-bore mismatch [in] (default 1 mm).

    Returns
    -------
    InterfaceResult
        ``compatible`` True only if pressure, bore and connector all match;
        otherwise ``reasons`` lists each failing category.
    """
    reasons: list = []

    # 1. Pressure rating: stack must contain >= wellhead rated pressure.
    if stack.pressure_class.value < wellhead.pressure_class.value:
        reasons.append(
            f"pressure: stack {stack.pressure_class.value} psi < wellhead "
            f"{wellhead.pressure_class.value} psi rated working pressure"
        )

    # 2. Bore match within tolerance.
    if abs(stack.bore_inch - wellhead.bore_inch) > bore_tolerance_inch:
        reasons.append(
            f"bore: stack {stack.bore_inch:.4g} in != wellhead "
            f"{wellhead.bore_inch:.4g} in (tol {bore_tolerance_inch:.4g} in)"
        )

    # 3. Connector / hub family match.
    if stack.connector_class is not wellhead.connector_class:
        reasons.append(
            f"connector: stack {stack.connector_class.value} cannot mate "
            f"wellhead {wellhead.connector_class.value} hub family"
        )

    return InterfaceResult(compatible=not reasons, reasons=reasons)
