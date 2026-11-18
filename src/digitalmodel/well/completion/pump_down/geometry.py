"""Circular casing/bypass geometry and nominal small-casing drift screen.

Areas and clearances follow Euclidean circle geometry (not a vendor method).
The 0.125 in deduction is the API Spec 5CT standard drift-mandrel allowance
for casing smaller than 9-5/8 in OD. Larger casing uses larger allowances and
special-drift products differ, so it is NOT a universal drift rule.
The 0.10 in minimum radial clearance is a representative task criterion,
not an API requirement. Verify against the casing tally and vendor chart.
"""

import math
from dataclasses import dataclass

from digitalmodel.units import Q_

from ._validation import scalar


@dataclass(frozen=True)
class BypassGeometry:
    """Areas [m2]; derived beta=Ab/Ac from circular concentric sections."""

    casing_area: Q_
    plug_area: Q_
    bypass_area: Q_
    beta: float

    def __post_init__(self):
        ac = scalar(self.casing_area, "m**2", "casing area", positive=True)
        ap = scalar(self.plug_area, "m**2", "plug area")
        ab = scalar(self.bypass_area, "m**2", "bypass area", positive=True)
        beta = scalar(self.beta, "dimensionless", "bypass ratio", positive=True)
        if ap >= ac or not math.isclose(ab, ac - ap, rel_tol=1e-12):
            raise ValueError("bypass area must equal casing area minus plug area")
        if not math.isclose(beta, ab / ac, rel_tol=1e-12):
            raise ValueError("bypass ratio must equal bypass area / casing area")
        for name, value in [
            ("casing_area", ac),
            ("plug_area", ap),
            ("bypass_area", ab),
        ]:
            object.__setattr__(self, name, Q_(value, "m**2"))


def bypass(casing_id_in, plug_od_in) -> BypassGeometry:
    """Return Ac, Ap, Ab=Ac-Ap and beta; bare diameters are inches.

    Source: circle area pi*d**2/4 and continuity, DOE-HDBK-1012/3-92,
    Fluid Flow, 'Continuity Equation'. Quantity inputs may use SI or field units.
    Zero plug OD denotes an unobstructed bore; a closed bypass is rejected.
    """
    diameter = scalar(casing_id_in, "inch", "casing ID", positive=True)
    od = scalar(plug_od_in, "inch", "plug OD")
    if od >= diameter:
        raise ValueError("plug OD must be less than casing ID")
    ac = Q_(math.pi * diameter**2 / 4, "inch**2").to("m**2")
    ap = Q_(math.pi * od**2 / 4, "inch**2").to("m**2")
    return BypassGeometry(ac, ap, ac - ap, float(((ac - ap) / ac).magnitude))


def api_drift(id_in) -> float:
    """Nominal small-casing drift [in] = ID-0.125 in; bare ID is inches.

    Basis: API Spec 5CT standard drift-mandrel allowance for casing smaller
    than 9-5/8 in OD. This helper does not apply the larger allowances for
    bigger casing; without OD/product type it cannot select those rules.
    """
    diameter = scalar(id_in, "inch", "casing ID", minimum=0.125, positive=True)
    return diameter - 0.125


def runnable(od, drift, min_radial=0.10) -> bool:
    """Return (drift-OD)/2 >= min_radial; bare lengths are inches.

    Representative 0.10 in radial criterion from the task, not a standard or
    a guarantee of passage through doglegs. Confirm vendor pumpability limits.
    """
    diameter = scalar(od, "inch", "tool OD")
    drift_diameter = scalar(drift, "inch", "drift", positive=True)
    clearance = scalar(min_radial, "inch", "minimum radial clearance")
    return (drift_diameter - diameter) / 2 >= clearance - 1e-12
