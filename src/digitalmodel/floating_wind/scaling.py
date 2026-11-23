"""Turbine-size & farm-size economies of scale for LCOE (issue #1249).

The core engine (:mod:`.economics`, #1221) expresses every cost as ``$/kW``, so
its LCOE is invariant to turbine rating and wind-farm size. The Wood/Whooley
deck shows both matter (slide 8): 15 MW -> 20 MW at 33 turbines drops LCOE from
**$184 to $161**, and at 20 MW a 660 MW farm ($161) beats a 500 MW farm ($172).

This module supplies the missing scale response by giving each cost component a
**scaling basis** -- the split of its cost between three behaviours:

* **per-MW** (capacity-proportional): constant ``$/kW`` regardless of size --
  turbine, mooring, array cable.
* **per-turbine-unit** (fixed per turbine): ``$/kW`` falls as ``ref_MW / rating``
  -- fewer, bigger turbines are cheaper per MW. Drives the *turbine-size*
  economy: hull fabrication (part of the substructure) and most O&M (visits per
  turbine).
* **per-farm** (fixed per project): ``$/kW`` falls as ``ref_farm_MW / farm_MW``
  -- one export system and one installation campaign amortised over more
  capacity. Drives the *farm-size* economy: export cable, installation,
  development.

Scaling a base :class:`.economics.ProjectEconomics` to a target (rating, count)
adjusts the ``$/kW`` component values by these bases and re-computes LCOE. At the
reference point the scaling is the identity (all multipliers = 1), so the
packaged base case still reproduces **$184/MWh** exactly.

Calibration
-----------
The packaged default bases (:func:`default_scaling`) are calibrated so the base
case plus the deck's two turbine/farm points are reproduced to <0.6%:

===========================  =======  =====
case                         deck     model
===========================  =======  =====
15 MW x 33 (495 MW)          $184     184.0 (identity)
20 MW x 33 (660 MW)          $161     161.9
20 MW x 25 (500 MW)          $172     171.0
===========================  =======  =====

Composed with the cost-driver sweep (#1223) and reliability driver (#1222) this
lets a scenario walk the deck's full 2040 cost-reduction pathway toward
$51/MWh. The bases are inputs in a reviewable ``.yml`` -- override per study.

References
----------
* Wood plc / A. Whooley (2021), turbine & wind-farm size (slide 8), 2040
  cost-reduction pathway (slide 6).
* NREL 2019 / ORE Catapult -- component cost shares and scale behaviour.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Mapping

import yaml
from pydantic import BaseModel, Field, model_validator

from digitalmodel.floating_wind.economics import (
    LCOEResult,
    ProjectEconomics,
    compute_lcoe,
)

__all__ = [
    "ComponentScaling",
    "EconomiesOfScale",
    "scale_economics",
    "lcoe_at_scale",
    "default_scaling",
]

_SCALING_YML = (
    Path(__file__).resolve().parents[1]
    / "base_configs"
    / "modules"
    / "floating_wind_economics"
    / "economies_of_scale.yml"
)

# CAPEX components that can carry a scaling basis (mooring/turbine/array default
# to pure per-MW, so they need no entry).
_CAPEX_FIELDS = (
    "turbine",
    "substructure",
    "mooring",
    "array_cable",
    "export_cable",
    "installation",
    "development",
)


class ComponentScaling(BaseModel):
    """Split of one cost between per-unit, per-farm and per-MW behaviour.

    ``per_unit`` + ``per_farm`` must be in [0, 1]; the remainder is per-MW.
    """

    per_unit: float = Field(0.0, ge=0.0, le=1.0)
    per_farm: float = Field(0.0, ge=0.0, le=1.0)

    @model_validator(mode="after")
    def _sum_ok(self) -> "ComponentScaling":
        if self.per_unit + self.per_farm > 1.0 + 1e-9:
            raise ValueError("per_unit + per_farm must not exceed 1.0")
        return self

    @property
    def per_mw(self) -> float:
        return 1.0 - self.per_unit - self.per_farm

    def factor(self, unit_ratio: float, farm_ratio: float) -> float:
        """Multiplier on a base ``$/kW`` value at the given scale ratios.

        ``unit_ratio = ref_rating / rating`` and
        ``farm_ratio = ref_farm_MW / farm_MW``.
        """
        return self.per_mw + self.per_unit * unit_ratio + self.per_farm * farm_ratio


class EconomiesOfScale(BaseModel):
    """Scaling bases for each cost component, anchored at a reference design."""

    reference_turbine_mw: float = Field(..., gt=0.0)
    reference_farm_mw: float = Field(..., gt=0.0)
    capex: dict[str, ComponentScaling] = Field(default_factory=dict)
    opex: ComponentScaling = ComponentScaling()

    @model_validator(mode="after")
    def _known_fields(self) -> "EconomiesOfScale":
        unknown = set(self.capex) - set(_CAPEX_FIELDS)
        if unknown:
            raise ValueError(f"unknown CAPEX component(s): {sorted(unknown)}")
        return self

    @classmethod
    def from_mapping(cls, data: Mapping[str, Any]) -> "EconomiesOfScale":
        return cls.model_validate(dict(data))

    @classmethod
    def from_yaml(cls, path: str | Path) -> "EconomiesOfScale":
        raw = yaml.safe_load(Path(path).read_text())
        if isinstance(raw, Mapping) and "economies_of_scale" in raw:
            raw = raw["economies_of_scale"]
        return cls.from_mapping(raw)


def scale_economics(
    base: ProjectEconomics,
    *,
    turbine_rating_mw: float,
    turbine_count: int,
    scaling: EconomiesOfScale | None = None,
) -> ProjectEconomics:
    """Return a copy of ``base`` re-costed for a new turbine rating & count.

    Each ``$/kW`` component is multiplied by its scaling factor at the target
    scale; ``turbine_rating_mw`` and ``turbine_count`` are updated so downstream
    energy/CAPEX totals follow. Identity when the target equals the reference.
    """
    sc = scaling if scaling is not None else default_scaling()
    farm_mw = turbine_rating_mw * turbine_count
    unit_ratio = sc.reference_turbine_mw / turbine_rating_mw
    farm_ratio = sc.reference_farm_mw / farm_mw

    capex_updates: dict[str, float] = {}
    for field in _CAPEX_FIELDS:
        comp = sc.capex.get(field)
        if comp is None:
            continue
        current = getattr(base.capex, field)
        capex_updates[field] = current * comp.factor(unit_ratio, farm_ratio)
    new_capex = base.capex.model_copy(update=capex_updates)

    new_opex = base.opex_per_kw_year * sc.opex.factor(unit_ratio, farm_ratio)

    return base.model_copy(
        update={
            "turbine_rating_mw": turbine_rating_mw,
            "turbine_count": turbine_count,
            "capex": new_capex,
            "opex_per_kw_year": new_opex,
        }
    )


def lcoe_at_scale(
    base: ProjectEconomics,
    *,
    turbine_rating_mw: float,
    turbine_count: int,
    scaling: EconomiesOfScale | None = None,
) -> LCOEResult:
    """LCOE of ``base`` re-costed to a new turbine rating & count."""
    return compute_lcoe(
        scale_economics(
            base,
            turbine_rating_mw=turbine_rating_mw,
            turbine_count=turbine_count,
            scaling=scaling,
        )
    )


def default_scaling() -> EconomiesOfScale:
    """The packaged, deck-calibrated economies-of-scale bases."""
    return EconomiesOfScale.from_yaml(_SCALING_YML)
