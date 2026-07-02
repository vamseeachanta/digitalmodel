"""Standardization & industrialization cost lever for LCOE (issue #1256).

The Wood/Whooley deck's fabrication cost-reduction story is *standardization and
industrialization*: "JIP33 – standardization", "Design one, build many", "No
gold plating", industrialization (slides 10, 15). The cost-driver sweep (#1223)
can apply a "-50% fab cost" as a bare percentage; this module supplies the
**mechanism** behind that number, so a reduction becomes a *deployment target*
rather than an assumption.

Two composable pieces on top of the core engine (:mod:`.economics`, #1221):

1. **Learning curve (Wright's law)** -- the industrialization / "design one,
   build many" effect. Per-unit fabrication cost falls by a fixed **progress
   ratio** ``LR`` for every doubling of cumulative units built::

       factor(n) = (n / n_ref) ** log2(LR)

   ``LR = 0.90`` means each doubling of cumulative production costs 90% of the
   previous (a 10% learning rate). Applied to fabrication-heavy CAPEX
   (substructure, installation). Identity at ``n = n_ref``.

2. **Standardization discount** -- an up-front fractional reduction on the
   standardized-scope components (JIP33 spec standardization + "no gold plating"
   removing bespoke over-design), independent of volume.

Applied to the base case at the reference deployment the lever is the identity,
so the packaged base case still reproduces **$184/MWh**. Composed with the
economies-of-scale model (#1249), the sweep (#1223) and reliability (#1222) it
lets a scenario express the deck's fabrication reductions as an explicit
function of cumulative deployment.

Calibration note
----------------
The learning rate is a **calibration input**, not a deck number. Published
offshore-wind learning rates cluster around 8-12% per doubling (floating is
expected higher while immature); the packaged default is ``LR = 0.90`` (10%).
At that rate, reaching the deck's fabrication reductions corresponds to plausible
cumulative deployment: ~10x the reference gives ~-30% on the learning components,
~30x gives ~-40% -- bracketing the deck's -25%/-50% fab-cost levers as
deployment milestones.

References
----------
* Wright, T.P. (1936), *Factors Affecting the Cost of Airplanes* -- learning curve.
* Wiser et al. / IRENA -- offshore-wind learning rates (~8-12%/doubling).
* Wood plc / A. Whooley (2021) -- standardization, industrialization (slides 10, 15).
"""

from __future__ import annotations

import math
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
    "LearningCurve",
    "StandardizationDiscount",
    "apply_standardization",
    "lcoe_with_standardization",
    "default_learning_curve",
]

_LEARNING_YML = (
    Path(__file__).resolve().parents[1]
    / "base_configs"
    / "modules"
    / "floating_wind_economics"
    / "standardization.yml"
)

_CAPEX_FIELDS = (
    "turbine",
    "substructure",
    "mooring",
    "array_cable",
    "export_cable",
    "installation",
    "development",
)


def _check_components(components: list[str]) -> list[str]:
    unknown = set(components) - set(_CAPEX_FIELDS)
    if unknown:
        raise ValueError(f"unknown CAPEX component(s): {sorted(unknown)}")
    return components


class LearningCurve(BaseModel):
    """Wright's-law learning curve for fabrication CAPEX."""

    learning_rate: float = Field(
        0.90,
        gt=0.0,
        le=1.0,
        description="Progress ratio: cost multiplier per doubling (0.90 = 10% learning)",
    )
    reference_units: float = Field(
        100.0,
        gt=0.0,
        description="Cumulative units at which the base CAPEX applies (identity)",
    )
    components: list[str] = Field(
        default_factory=lambda: ["substructure", "installation"],
        description="CAPEX components subject to the learning curve",
    )

    @model_validator(mode="after")
    def _known(self) -> "LearningCurve":
        _check_components(self.components)
        return self

    def factor(self, cumulative_units: float) -> float:
        """Cost multiplier on the learning components at ``cumulative_units``."""
        if cumulative_units <= 0.0:
            raise ValueError("cumulative_units must be > 0")
        return (cumulative_units / self.reference_units) ** math.log2(
            self.learning_rate
        )


class StandardizationDiscount(BaseModel):
    """Up-front fractional CAPEX reduction from spec standardization (JIP33)."""

    fraction: float = Field(
        0.0, ge=0.0, lt=1.0, description="Fractional reduction on target components"
    )
    components: list[str] = Field(
        default_factory=lambda: ["substructure", "installation", "development"],
        description="Standardized-scope components the discount applies to",
    )

    @model_validator(mode="after")
    def _known(self) -> "StandardizationDiscount":
        _check_components(self.components)
        return self

    def factor(self) -> float:
        return 1.0 - self.fraction


def apply_standardization(
    econ: ProjectEconomics,
    *,
    cumulative_units: float,
    learning: LearningCurve | None = None,
    discount: StandardizationDiscount | None = None,
) -> ProjectEconomics:
    """Return a copy of ``econ`` with fabrication CAPEX reduced by learning +
    an optional standardization discount.

    Multipliers compose per component: the learning factor (on the curve's
    components) times the discount factor (on the discount's components).
    """
    lc = learning if learning is not None else default_learning_curve()
    learn_f = lc.factor(cumulative_units)

    updates: dict[str, float] = {}
    for field in lc.components:
        updates[field] = getattr(econ.capex, field) * learn_f
    if discount is not None:
        disc_f = discount.factor()
        for field in discount.components:
            base_val = updates.get(field, getattr(econ.capex, field))
            updates[field] = base_val * disc_f

    new_capex = econ.capex.model_copy(update=updates)
    return econ.model_copy(update={"capex": new_capex})


def lcoe_with_standardization(
    econ: ProjectEconomics,
    *,
    cumulative_units: float,
    learning: LearningCurve | None = None,
    discount: StandardizationDiscount | None = None,
) -> LCOEResult:
    """LCOE of ``econ`` after standardization/learning is applied."""
    return compute_lcoe(
        apply_standardization(
            econ,
            cumulative_units=cumulative_units,
            learning=learning,
            discount=discount,
        )
    )


def default_learning_curve() -> LearningCurve:
    """The packaged default learning curve (LR = 0.90)."""
    raw = yaml.safe_load(Path(_LEARNING_YML).read_text())
    if isinstance(raw, Mapping) and "standardization" in raw:
        raw = raw["standardization"]
    if isinstance(raw, Mapping) and "learning_curve" in raw:
        raw = raw["learning_curve"]
    return LearningCurve.model_validate(dict(raw))
