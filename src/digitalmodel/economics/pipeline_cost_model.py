# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Calculate a caller-priced subset of pipeline capital cost for issue #1740.

Basis: issue #1740, "Linepipe steel: weight/m x unit cost" and "Cost breakdown
by category". This module supplies arithmetic, with no price database or defaults.
All geometry uses metres, density uses kg/m^3, and rates use the supplied currency
per kg of steel, per m^2 of external coating, and per connecting weld including NDT.
Currency is a caller label; there is no currency conversion. Input provenance is a
required caller description of the geometry, density and rates, retained verbatim.

The circular annulus area is pi/4 * (OD^2 - ID^2), where ID = OD - 2*wall.
The equivalent pi*wall*(OD-wall) avoids subtracting nearly equal squares.
Coating covers pi*OD*length, excluding ends, field-joint coating and allowances.
Section count is ceil(length/joint_length); the final section may be cut short.
Boundaries use the exact ratios of the supplied floating-point lengths.
Connecting weld count is section count minus one; end connections are excluded.
Consequently weld costs change in steps, while steel and coating scale with length.
Positive geometry and density, a positive bore, and nonnegative rates are required.
Explicit zero rates are permitted and mean evaluated zero cost. Installation and
remaining categories are NOT_EVALUATED, never zero-cost estimates. Total cost and
cost per metre cover only steel, coating and weld/NDT, not complete pipeline CAPEX.
No waste, procurement allowance, tax, escalation or contingency is included.

Runnable synthetic example (standard library only; no downloads)::

    PYTHONPATH=src python -m doctest src/digitalmodel/economics/pipeline_cost_model.py

Every input below is chosen for arithmetic demonstration, not a market estimate.
Independent hand calculation: length 24 m, OD 1 m, wall 0.1 m give ID 0.8 m;
area = pi/4*(1^2 - 0.8^2) = 0.09*pi m^2. At density 1000 kg/m^3, mass is
24*1000*0.09*pi = 2160*pi kg, costing 4320*pi at 2 currency units/kg.
External area is 24*pi m^2, costing 72*pi at 3 currency units/m^2.
Two 12 m sections need one connecting weld, costing 100 at 100 units/weld.
The evaluated subtotal is 4392*pi + 100; per metre it is 183*pi + 100/24.

>>> from math import isclose, pi
>>> from digitalmodel.economics.pipeline_cost_model import estimate_pipeline_cost
>>> result = estimate_pipeline_cost(
...     length_m=24, outer_diameter_m=1, wall_thickness_m=0.1,
...     steel_density_kg_m3=1000, steel_rate_per_kg=2,
...     coating_rate_per_m2=3, joint_length_m=12, weld_ndt_rate_per_weld=100,
...     currency="USD", input_provenance="Synthetic example for issue #1740",
... )
>>> isclose(result.steel_mass_kg, 2160*pi)
True
>>> isclose(result.steel_cost, 4320*pi)
True
>>> isclose(result.coating_area_m2, 24*pi)
True
>>> isclose(result.coating_cost, 72*pi)
True
>>> result.weld_count == 1 and result.weld_ndt_cost == 100
True
>>> isclose(result.total_cost, 4392*pi + 100)
True
>>> isclose(result.cost_per_m, 183*pi + 100/24)
True
>>> result.category_status["installation"]
'NOT_EVALUATED'
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite, pi
from numbers import Real


@dataclass(frozen=True)
class PipelineCostInputs:
    """Snapshot of supplied SI geometry, currency rates and their stated source."""

    length_m: float
    outer_diameter_m: float
    wall_thickness_m: float
    steel_density_kg_m3: float
    steel_rate_per_kg: float
    coating_rate_per_m2: float
    joint_length_m: float
    weld_ndt_rate_per_weld: float
    currency: str
    input_provenance: str


@dataclass(frozen=True)
class PipelineCostEstimate:
    """Evaluated subtotals with explicit units, input snapshot and scope limits."""

    inputs: PipelineCostInputs
    steel_mass_kg: float
    coating_area_m2: float
    weld_count: int
    steel_cost: float
    coating_cost: float
    weld_ndt_cost: float
    total_cost: float
    cost_per_m: float

    @property
    def units(self) -> dict[str, str]:
        """Return units for every numeric output in the supplied currency."""
        currency = self.inputs.currency
        return {
            "steel_mass_kg": "kg",
            "coating_area_m2": "m^2",
            "weld_count": "1",
            "steel_cost": currency,
            "coating_cost": currency,
            "weld_ndt_cost": currency,
            "total_cost": currency,
            "cost_per_m": f"{currency}/m",
        }

    @property
    def category_status(self) -> dict[str, str]:
        """Distinguish evaluated costs from categories outside this calculation."""
        return {
            "steel": "EVALUATED",
            "coating": "EVALUATED",
            "weld_ndt": "EVALUATED",
            "installation": "NOT_EVALUATED",
            "remaining_categories": "NOT_EVALUATED",
        }


def _number(name: str, value: float, *, allow_zero: bool = False) -> float:
    """Validate a finite real scalar, rejecting boolean and implicit coercion."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real number, not a boolean")
    try:
        number = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"{name} must be representable as a finite float") from exc
    if not isfinite(number) or number < 0 or (number == 0 and not allow_zero):
        bound = "nonnegative" if allow_zero else "positive"
        raise ValueError(f"{name} must be finite and {bound}")
    return number


def estimate_pipeline_cost(
    *,
    length_m: float,
    outer_diameter_m: float,
    wall_thickness_m: float,
    steel_density_kg_m3: float,
    steel_rate_per_kg: float,
    coating_rate_per_m2: float,
    joint_length_m: float,
    weld_ndt_rate_per_weld: float,
    currency: str,
    input_provenance: str,
) -> PipelineCostEstimate:
    """Evaluate steel, external coating and connecting weld/NDT costs.

    Args:
        length_m: Positive installed pipe length in metres.
        outer_diameter_m: Positive outside steel diameter in metres.
        wall_thickness_m: Positive wall in metres, less than half the diameter.
        steel_density_kg_m3: Positive caller-supplied steel density in kg/m^3.
        steel_rate_per_kg: Nonnegative currency units per kg of steel.
        coating_rate_per_m2: Nonnegative currency units per m^2 of outside area.
        joint_length_m: Positive maximum section length in metres.
        weld_ndt_rate_per_weld: Nonnegative currency units per connecting weld.
        currency: Nonempty label shared by all supplied rates.
        input_provenance: Nonempty description of geometry, density and rate basis.

    Returns:
        Input snapshot, mass, area, weld count, evaluated subtotals and their sum
        and sum per metre, with units and category evaluation status.

    Raises:
        ValueError: An input is boolean, nonfinite, nonphysical or lacks metadata,
            or derived arithmetic exceeds floating-point range.
    """
    inputs = PipelineCostInputs(
        length_m=_number("length_m", length_m),
        outer_diameter_m=_number("outer_diameter_m", outer_diameter_m),
        wall_thickness_m=_number("wall_thickness_m", wall_thickness_m),
        steel_density_kg_m3=_number("steel_density_kg_m3", steel_density_kg_m3),
        steel_rate_per_kg=_number("steel_rate_per_kg", steel_rate_per_kg, allow_zero=True),
        coating_rate_per_m2=_number(
            "coating_rate_per_m2", coating_rate_per_m2, allow_zero=True
        ),
        joint_length_m=_number("joint_length_m", joint_length_m),
        weld_ndt_rate_per_weld=_number(
            "weld_ndt_rate_per_weld", weld_ndt_rate_per_weld, allow_zero=True
        ),
        currency=currency,
        input_provenance=input_provenance,
    )
    for name, text in (("currency", currency), ("input_provenance", input_provenance)):
        if not isinstance(text, str) or not text.strip():
            raise ValueError(f"{name} must be a nonempty string")
    if inputs.wall_thickness_m >= inputs.outer_diameter_m / 2:
        raise ValueError("wall_thickness_m must be less than half outer_diameter_m")

    try:
        area = pi * inputs.wall_thickness_m * (
            inputs.outer_diameter_m - inputs.wall_thickness_m
        )
        mass = area * inputs.length_m * inputs.steel_density_kg_m3
        coating_area = pi * inputs.outer_diameter_m * inputs.length_m
        length_num, length_den = inputs.length_m.as_integer_ratio()
        joint_num, joint_den = inputs.joint_length_m.as_integer_ratio()
        # For positive integer ratios, ceil(n/d) - 1 equals (n - 1) // d.
        welds = (length_num * joint_den - 1) // (length_den * joint_num)
        steel = mass * inputs.steel_rate_per_kg
        coating = coating_area * inputs.coating_rate_per_m2
        weld_ndt = welds * inputs.weld_ndt_rate_per_weld
        total = steel + coating + weld_ndt
        per_m = total / inputs.length_m
    except (OverflowError, ValueError) as exc:
        raise ValueError("derived arithmetic exceeds floating-point range") from exc
    if (
        not all(
            isfinite(v)
            for v in (area, mass, coating_area, steel, coating, weld_ndt, total, per_m)
        )
        or min(area, mass, coating_area) <= 0
        or (total > 0 and per_m == 0)
        or any(
            quantity > 0 and rate > 0 and cost == 0
            for quantity, rate, cost in (
                (mass, inputs.steel_rate_per_kg, steel),
                (coating_area, inputs.coating_rate_per_m2, coating),
                (welds, inputs.weld_ndt_rate_per_weld, weld_ndt),
            )
        )
    ):
        raise ValueError("derived arithmetic exceeds floating-point range")

    return PipelineCostEstimate(
        inputs=inputs,
        steel_mass_kg=mass,
        coating_area_m2=coating_area,
        weld_count=welds,
        steel_cost=steel,
        coating_cost=coating,
        weld_ndt_cost=weld_ndt,
        total_cost=total,
        cost_per_m=per_m,
    )
