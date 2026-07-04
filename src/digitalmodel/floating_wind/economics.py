"""Levelised-cost (LCOE) / TOTEX core engine for floating-wind concepts.

The floating-wind concept screen (epic #1022: ``floaters -> screening ->
tradespace -> report``) ranks concepts on *physical feasibility* only. This
module adds the missing **economics axis**: given a project definition it
computes the **levelised cost of energy (LCOE, $/MWh)** from a life-cycle
``TOTEX = CAPEX + OPEX + DECOMEX`` cost stack discounted over the design life.

It is the spine of the floating-wind TOTEX/LCOE economics epic (issue #1221):
the reliability -> OPEX driver (#1222), the cost-driver sweep (#1223), the
tradespace ``LCOE`` objective (#1224) and the cost data sheet (#1226) all build
on the functions defined here.

Model
-----
Standard discounted levelised cost. With design life ``N`` years, (nominal)
discount rate ``d`` and OPEX escalation ``i``::

                 CAPEX + sum_{t=1..N} OPEX_0 (1+i)^{t-1} / (1+d)^t + DECOMEX/(1+d)^N
    LCOE  =  ---------------------------------------------------------------------------
                             sum_{t=1..N} AEP / (1+d)^t

* CAPEX is incurred at ``t = 0`` (overnight); DECOMEX at end of life ``t = N``.
* OPEX escalates at ``i`` from a year-1 base ``OPEX_0`` and is discounted at
  ``d`` -- costs and energy are discounted with the *same* rate so the ratio is
  a true levelised unit cost.
* Annual energy production is ``AEP = P_farm * 8760 * capacity_factor`` and is
  taken constant across the life (no degradation term in this first tier).

All cost bases are **inputs**, carried in a reviewable ``.yml`` -- nothing is
hardcoded. The packaged :func:`base_case` reproduces the public Wood/OREC base
case (see references) so the engine is self-validating.

References
----------
* Wood plc / A. Whooley, *Translating Oil & Gas Cost Reduction Experience into
  Floating Offshore Wind* (2021) -- base case 15 MW x 33 = 495 MW, 100 m water
  depth, 25-year design life, 6% discount, 3% inflation, Europe fabrication ->
  **$184/MWh**; and the cost-reduction pathway down to $51/MWh (1 GW, 2040).
* NREL, *2019 Cost of Wind Energy Review*, NREL/TP-5000-78471 -- floating-wind
  CAPEX/OPEX cost bases and component cost shares.
* ORE Catapult / OREC, *Floating Offshore Wind: Cost Reduction Pathways to
  Subsidy Free* (2021) -- CAPEX/OPEX ranges and reduction levers.

Only public references are used; the module ships no client or proprietary data.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Mapping

import yaml
from pydantic import BaseModel, Field, model_validator

__all__ = [
    "HOURS_PER_YEAR",
    "FinancialParameters",
    "CapexBreakdown",
    "ProjectEconomics",
    "LCOEResult",
    "annual_energy_production_mwh",
    "capex_total_usd",
    "opex_year_usd",
    "decomex_total_usd",
    "compute_lcoe",
    "base_case",
]

# --- constants ---------------------------------------------------------------

HOURS_PER_YEAR = 8760.0

# Packaged public base case (Wood/OREC), see module docstring.
_BASE_CASE_YML = (
    Path(__file__).resolve().parents[1]
    / "base_configs"
    / "modules"
    / "floating_wind_economics"
    / "floating_wind_economics.yml"
)


# --- financial parameters ----------------------------------------------------


class FinancialParameters(BaseModel):
    """Discounting parameters for the levelised-cost calculation."""

    discount_rate: float = Field(
        ..., gt=0.0, lt=1.0, description="Annual (nominal) discount rate, e.g. 0.06"
    )
    inflation_rate: float = Field(
        0.0,
        ge=0.0,
        lt=1.0,
        description="Annual OPEX escalation rate (real-terms if 0.0)",
    )
    design_life_years: int = Field(
        ..., gt=0, description="Project / operating design life (years)"
    )


# --- CAPEX -------------------------------------------------------------------


class CapexBreakdown(BaseModel):
    """Overnight CAPEX split, expressed per kW of installed capacity ($/kW).

    Component shares follow the public floating-wind cost breakdown (NREL 2019,
    ORE Catapult). Keeping them separate lets the sweep engine (#1223) and the
    reliability driver (#1222) move individual levers.
    """

    turbine: float = Field(..., ge=0.0, description="Turbine (RNA + tower) $/kW")
    substructure: float = Field(
        ..., ge=0.0, description="Floating substructure / hull $/kW"
    )
    mooring: float = Field(..., ge=0.0, description="Mooring lines + anchors $/kW")
    array_cable: float = Field(..., ge=0.0, description="Inter-array cabling $/kW")
    export_cable: float = Field(
        ..., ge=0.0, description="Export cable to shore $/kW"
    )
    installation: float = Field(
        ..., ge=0.0, description="Installation + commissioning $/kW"
    )
    development: float = Field(
        ..., ge=0.0, description="Development + project management $/kW"
    )

    @property
    def total_per_kw(self) -> float:
        """Total overnight CAPEX per kW ($/kW)."""
        return (
            self.turbine
            + self.substructure
            + self.mooring
            + self.array_cable
            + self.export_cable
            + self.installation
            + self.development
        )


# --- project definition ------------------------------------------------------


class ProjectEconomics(BaseModel):
    """Full economic definition of a floating-wind project.

    Physical inputs (turbine rating/count, capacity factor) plus the cost bases
    needed to levelise. ``water_depth_m``/``distance_to_shore_km``/
    ``fabrication_region`` are carried for provenance and for the sweep engine
    (#1223); the base tier does not itself re-derive costs from them.
    """

    name: str = Field("floating_wind_economics", description="Case label")

    turbine_rating_mw: float = Field(
        ..., gt=0.0, description="Rated power per turbine (MW)"
    )
    turbine_count: int = Field(..., gt=0, description="Number of turbines")
    capacity_factor: float = Field(
        ..., gt=0.0, le=1.0, description="Net capacity factor (fraction)"
    )

    water_depth_m: float = Field(100.0, gt=0.0, description="Site water depth (m)")
    distance_to_shore_km: float = Field(
        100.0, ge=0.0, description="Distance to shore (km)"
    )
    fabrication_region: str = Field(
        "Europe", description="Fabrication region (informational / sweep key)"
    )

    financial: FinancialParameters
    capex: CapexBreakdown
    opex_per_kw_year: float = Field(
        ..., ge=0.0, description="Annual OPEX per kW ($/kW/yr), year-1 basis"
    )
    decomex_per_kw: float = Field(
        0.0, ge=0.0, description="Decommissioning cost per kW ($/kW), at end of life"
    )

    @property
    def farm_capacity_mw(self) -> float:
        """Installed wind-farm capacity (MW)."""
        return self.turbine_rating_mw * self.turbine_count

    @property
    def farm_capacity_kw(self) -> float:
        """Installed wind-farm capacity (kW)."""
        return self.farm_capacity_mw * 1000.0

    @classmethod
    def from_mapping(cls, data: Mapping[str, Any]) -> "ProjectEconomics":
        """Build from a plain mapping (e.g. parsed YAML)."""
        return cls.model_validate(dict(data))

    @classmethod
    def from_yaml(cls, path: str | Path) -> "ProjectEconomics":
        """Load a project economics case from a YAML file.

        The YAML may nest the case under a ``floating_wind_economics`` key
        (workflow-config style) or hold the fields at the top level.
        """
        raw = yaml.safe_load(Path(path).read_text())
        if isinstance(raw, Mapping) and "floating_wind_economics" in raw:
            raw = raw["floating_wind_economics"]
        return cls.from_mapping(raw)


# --- result ------------------------------------------------------------------


class LCOEResult(BaseModel):
    """Levelised-cost result with a discounted TOTEX breakdown.

    The per-MWh component split (``capex_per_mwh`` + ``opex_per_mwh`` +
    ``decomex_per_mwh``) sums to ``lcoe_usd_per_mwh`` and is what the cost data
    sheet (#1226) renders.
    """

    lcoe_usd_per_mwh: float
    totex_discounted_usd: float
    capex_usd: float
    opex_discounted_usd: float
    decomex_discounted_usd: float
    aep_mwh: float
    energy_discounted_mwh: float
    capex_per_mwh: float
    opex_per_mwh: float
    decomex_per_mwh: float


# --- calculations ------------------------------------------------------------


def annual_energy_production_mwh(econ: ProjectEconomics) -> float:
    """Annual energy production of the farm (MWh/yr), constant across life."""
    return econ.farm_capacity_mw * HOURS_PER_YEAR * econ.capacity_factor


def capex_total_usd(econ: ProjectEconomics) -> float:
    """Total overnight CAPEX ($) incurred at t = 0."""
    return econ.capex.total_per_kw * econ.farm_capacity_kw


def opex_year_usd(econ: ProjectEconomics, year: int) -> float:
    """OPEX in operating ``year`` (1-based), escalated from the year-1 basis."""
    if year < 1:
        raise ValueError("operating year is 1-based (year >= 1)")
    base = econ.opex_per_kw_year * econ.farm_capacity_kw
    return base * (1.0 + econ.financial.inflation_rate) ** (year - 1)


def decomex_total_usd(econ: ProjectEconomics) -> float:
    """Total decommissioning cost ($) incurred at end of life."""
    return econ.decomex_per_kw * econ.farm_capacity_kw


def _discount_factor(rate: float, t: int) -> float:
    return 1.0 / (1.0 + rate) ** t


def compute_lcoe(econ: ProjectEconomics) -> LCOEResult:
    """Compute the levelised cost of energy and its discounted TOTEX split."""
    d = econ.financial.discount_rate
    n = econ.financial.design_life_years

    aep = annual_energy_production_mwh(econ)

    energy_disc = sum(aep * _discount_factor(d, t) for t in range(1, n + 1))
    if energy_disc <= 0.0:
        raise ValueError("discounted energy is non-positive; check inputs")

    capex = capex_total_usd(econ)
    opex_disc = sum(
        opex_year_usd(econ, t) * _discount_factor(d, t) for t in range(1, n + 1)
    )
    decomex_disc = decomex_total_usd(econ) * _discount_factor(d, n)

    totex_disc = capex + opex_disc + decomex_disc
    lcoe = totex_disc / energy_disc

    return LCOEResult(
        lcoe_usd_per_mwh=lcoe,
        totex_discounted_usd=totex_disc,
        capex_usd=capex,
        opex_discounted_usd=opex_disc,
        decomex_discounted_usd=decomex_disc,
        aep_mwh=aep,
        energy_discounted_mwh=energy_disc,
        capex_per_mwh=capex / energy_disc,
        opex_per_mwh=opex_disc / energy_disc,
        decomex_per_mwh=decomex_disc / energy_disc,
    )


def base_case() -> ProjectEconomics:
    """The packaged public Wood/OREC base case (~$184/MWh).

    Loads :data:`_BASE_CASE_YML`; used by examples, the golden test and as the
    starting point for the cost-driver sweeps (#1223).
    """
    return ProjectEconomics.from_yaml(_BASE_CASE_YML)
