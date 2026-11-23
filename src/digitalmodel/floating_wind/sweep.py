"""Cost-driver sensitivity sweep for floating-wind LCOE (issue #1223).

The Wood/Whooley deck frames the cost-reduction story as a set of **driver
sensitivities** on LCOE -- design life, turbine/farm size, fabrication cost,
installation vessel rates, capacity factor -- taken individually and in
combination. This module runs those sweeps against the core engine
(:mod:`.economics`, issue #1221): apply one or more **lever changes** to a base
project, recompute LCOE, and roll the results into a tidy table with the delta
versus base.

A lever change targets any numeric input by dotted path -- a top-level field
(``capacity_factor``), a financial field (``financial.design_life_years``) or a
CAPEX component (``capex.substructure``) -- and either scales it (relative) or
sets it (absolute). Scenarios combine several changes, so a single call can
reproduce the deck's compound pathways as well as one-lever sensitivities.

Reproduced deck single-driver points (base $184/MWh), each within +/-2%:

===========================  =======  ====
lever                        deck     model
===========================  =======  ====
design life 25 -> 30 yr      $178     ~176
substructure CAPEX -25%      $172     ~174
substructure CAPEX -50%      $161     ~164
turbine CAPEX -25%           $173     ~174
turbine CAPEX -50%           $162     ~163
installation cost -50%       $177     ~174
===========================  =======  ====

Note: the deck's *vessel-rate* lever is a sub-fraction of the bundled
``installation`` CAPEX line (which also carries commissioning), so scaling the
whole line up (e.g. doubling) overshoots; the sign is always correct. Turbine-
size economies of scale (the deck's $51/MWh 2040 pathway) need a turbine-size
cost-scaling assumption and are out of scope for this tier.

References
----------
* Wood plc / A. Whooley (2021) -- cost-driver sensitivities (slides 5-8, 15-17).
"""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field, model_validator

from .economics import ProjectEconomics, compute_lcoe

__all__ = [
    "LeverChange",
    "DriverScenario",
    "SweepRow",
    "apply_change",
    "run_sweep",
]

# Fields that live one level down, keyed by their sub-model attribute.
_NESTED_GROUPS = ("financial", "capex")


class LeverChange(BaseModel):
    """A single change to one numeric input, by dotted ``path``.

    Exactly one of ``scale`` (relative multiplier) or ``value`` (absolute) must
    be given.
    """

    path: str = Field(
        ...,
        description="Dotted field path, e.g. 'capex.substructure' or 'capacity_factor'",
    )
    scale: float | None = Field(None, description="Relative multiplier (e.g. 0.75)")
    value: float | None = Field(None, description="Absolute replacement value")

    @model_validator(mode="after")
    def _exactly_one(self) -> "LeverChange":
        if (self.scale is None) == (self.value is None):
            raise ValueError("provide exactly one of 'scale' or 'value'")
        return self


class DriverScenario(BaseModel):
    """A named set of lever changes applied together."""

    name: str
    changes: list[LeverChange] = Field(default_factory=list)


class SweepRow(BaseModel):
    """One scenario's result."""

    name: str
    lcoe_usd_per_mwh: float
    delta_vs_base: float
    pct_vs_base: float


def _split(path: str) -> tuple[str | None, str]:
    """Split a dotted path into (group, field). group is None for top level."""
    if "." in path:
        group, field = path.split(".", 1)
        if group not in _NESTED_GROUPS or "." in field:
            raise ValueError(f"unsupported field path: {path!r}")
        return group, field
    return None, path


def _current(econ: ProjectEconomics, group: str | None, field: str) -> float:
    obj = getattr(econ, group) if group else econ
    if not hasattr(obj, field):
        raise ValueError(f"unknown field: {field!r}")
    return getattr(obj, field)


def apply_change(econ: ProjectEconomics, change: LeverChange) -> ProjectEconomics:
    """Return a copy of ``econ`` with a single lever change applied."""
    group, field = _split(change.path)
    if change.value is not None:
        new_val: Any = change.value
    else:
        new_val = _current(econ, group, field) * change.scale
    # Preserve int-typed fields (e.g. design_life_years).
    if isinstance(_current(econ, group, field), int) and not isinstance(
        new_val, bool
    ):
        new_val = round(new_val)

    if group is None:
        return econ.model_copy(update={field: new_val})
    sub = getattr(econ, group).model_copy(update={field: new_val})
    return econ.model_copy(update={group: sub})


def _apply_scenario(
    econ: ProjectEconomics, scenario: DriverScenario
) -> ProjectEconomics:
    out = econ
    for change in scenario.changes:
        out = apply_change(out, change)
    return out


def run_sweep(
    base: ProjectEconomics, scenarios: list[DriverScenario]
) -> list[SweepRow]:
    """Run each scenario against ``base`` and tabulate LCOE vs the base case."""
    base_lcoe = compute_lcoe(base).lcoe_usd_per_mwh
    rows: list[SweepRow] = []
    for scenario in scenarios:
        lcoe = compute_lcoe(_apply_scenario(base, scenario)).lcoe_usd_per_mwh
        delta = lcoe - base_lcoe
        rows.append(
            SweepRow(
                name=scenario.name,
                lcoe_usd_per_mwh=lcoe,
                delta_vs_base=delta,
                pct_vs_base=100.0 * delta / base_lcoe,
            )
        )
    return rows
