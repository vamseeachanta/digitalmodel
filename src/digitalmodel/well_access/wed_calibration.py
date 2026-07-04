# ABOUTME: W7 adapter — loads the dated worldenergydata intervention access-gap SNAPSHOT.
# ABOUTME: Exposes empirical band demand, fleet supply (GoM-resident binding), dayrate/cost.
"""
digitalmodel.well_access.wed_calibration
========================================

Cross-repo calibration bridge for the W7 well-access model.

``digitalmodel`` cannot import the ``worldenergydata`` package, so the empirical
BSEE intervention access-gap figures are carried in as a **committed, dated
snapshot** (``base_configs/modules/well_access/wed_intervention_snapshot.yml``).
This module loads that snapshot and exposes it as structured calibration so the
well-access model (``intervention_rates`` / ``resource_queue``) can be calibrated
to REAL figures instead of bare engineering assumptions.

It is purely **additive** — nothing here changes default ``well_access`` behavior.
Callers opt in by loading a :class:`WedCalibration` and passing its derived inputs.

Load-bearing caveats (carried from worldenergydata#638; preserved on every object)
----------------------------------------------------------------------------------
* **FORWARD-LOOKING ACCESS RISK, not a measured crossover.** A per-band
  ``utilization_ratio > 1`` is an access-risk *indicator*, never an observed
  shortfall. Derived access multipliers inherit ``confidence='forward_looking_risk'``.
* **FLOOR (WAR ~6%).** The empirical interventions/well figure is a lifetime UPPER
  proxy on the ~6% depth-stamped WAR subset (#627/#630), NOT an annual lambda — do
  not feed it directly as an NHPP rate.
* **GEOGRAPHY (#628).** GoM has ~0 RESIDENT dedicated LIGHT-intervention fleet
  (geography + the 6% WAR coverage), NOT evidence that deepwater light intervention
  is avoided. The GoM-resident heavy fleet (~2 units) is the BINDING supply
  constraint; the global roster (471) is a pool a GoM operator merely competes for.
* **PRESSURE CLASS ABSENT.** Bands are water-depth only; HPHT / 20k-psi service
  capability is not resolved (only two rigs worldwide are rated to 20,000 psi).

Confidence labels from the snapshot are preserved verbatim on the returned objects.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

import yaml

# Snapshot lives in the shipped base_configs tree (sibling to well_access.yml).
_DEFAULT_SNAPSHOT = (
    Path(__file__).resolve().parents[1]
    / "base_configs"
    / "modules"
    / "well_access"
    / "wed_intervention_snapshot.yml"
)


def _triple(d: Optional[dict], *keys: str) -> Optional[float]:
    """Pull the first present key from a low/central/high (or conservative/...) dict."""
    if not d:
        return None
    for k in keys:
        if k in d and d[k] is not None:
            return float(d[k])
    return None


@dataclass(frozen=True)
class BandDemand:
    """Empirical + forward intervention demand for one water-depth band.

    ``interventions_per_well`` is the #630 lifetime UPPER proxy on the ~6%
    depth-stamped WAR subset — a FLOOR cross-check, NOT an annual lambda.
    ``utilization_ratio_gom_resident`` > 1 is a FORWARD access-risk indicator.
    """

    band: str
    label: str
    modu_servicing: bool
    subsea_wells: int
    interventions_per_well_floor: Optional[float]
    interventions_floor: Optional[int]
    distinct_wells_floor: Optional[int]
    total_rig_days_floor: Optional[int]
    heavy_duration_days_median: Optional[float]
    demand_rig_days_per_yr_central: Optional[float]
    interventions_per_yr_central: Optional[float]
    gap_vs_global_rig_days_per_yr_central: Optional[float]
    gap_vs_gom_resident_rig_days_per_yr_central: Optional[float]
    utilization_ratio_global_central: Optional[float]
    utilization_ratio_gom_resident_central: Optional[float]
    exposure_usd_per_yr_central: Optional[float]
    heavy_dead_well_cost_usd_median: Optional[float]
    confidence: str = "forward_looking_risk"


@dataclass(frozen=True)
class FleetSupply:
    """Eligible heavy-deepwater fleet supply; GoM-resident is the BINDING constraint.

    ``gom_resident_light_intervention_count == 0`` is GEOGRAPHY (#628), not avoidance.
    """

    global_eligible_fleet_count: int
    eligible_classes: dict
    rig_days_per_yr_global_central: float
    gom_resident_dedicated_total: int
    gom_resident_eligible_heavy_count: int
    gom_resident_eligible_heavy_units: list
    rig_days_per_yr_gom_resident_central: float
    gom_resident_light_intervention_count: int
    fleet_utilization_central: float
    confidence: str = "soft"


@dataclass(frozen=True)
class WedCalibration:
    """Loaded worldenergydata intervention access-gap snapshot (the calibration bridge)."""

    meta: dict
    parameters: dict
    bands: list[BandDemand]
    fleet: FleetSupply
    dayrates: dict
    caveats: list[str]
    method: str
    source_path: str = ""

    # ---- construction -----------------------------------------------------

    @classmethod
    def load(cls, path: str | Path | None = None) -> "WedCalibration":
        """Load the snapshot YAML (defaults to the shipped base_configs copy)."""
        p = Path(path) if path is not None else _DEFAULT_SNAPSHOT
        with open(p, "r", encoding="utf-8") as fh:
            raw = yaml.safe_load(fh)
        return cls.from_dict(raw, source_path=str(p))

    @classmethod
    def from_dict(cls, raw: dict[str, Any], source_path: str = "") -> "WedCalibration":
        bands: list[BandDemand] = []
        for key, b in (raw.get("bands") or {}).items():
            emp = b.get("empirical") or {}
            bands.append(
                BandDemand(
                    band=str(b.get("band", key)),
                    label=str(b.get("label", key)),
                    modu_servicing=bool(b.get("modu_servicing", False)),
                    subsea_wells=int(b.get("subsea_wells", 0)),
                    interventions_per_well_floor=(
                        float(emp["interventions_per_well"])
                        if emp.get("interventions_per_well") is not None
                        else None
                    ),
                    interventions_floor=(
                        int(emp["interventions"])
                        if emp.get("interventions") is not None
                        else None
                    ),
                    distinct_wells_floor=(
                        int(emp["distinct_wells"])
                        if emp.get("distinct_wells") is not None
                        else None
                    ),
                    total_rig_days_floor=(
                        int(emp["total_rig_days_floor"])
                        if emp.get("total_rig_days_floor") is not None
                        else None
                    ),
                    heavy_duration_days_median=_triple(
                        b.get("heavy_duration_days"), "median", "central"
                    ),
                    demand_rig_days_per_yr_central=_triple(
                        b.get("demand_rig_days_per_yr"), "central"
                    ),
                    interventions_per_yr_central=_triple(
                        b.get("interventions_per_yr"), "central"
                    ),
                    gap_vs_global_rig_days_per_yr_central=_triple(
                        b.get("gap_vs_global_rig_days_per_yr"), "central"
                    ),
                    gap_vs_gom_resident_rig_days_per_yr_central=_triple(
                        b.get("gap_vs_gom_resident_rig_days_per_yr"), "central"
                    ),
                    utilization_ratio_global_central=_triple(
                        b.get("utilization_ratio_global"), "central"
                    ),
                    utilization_ratio_gom_resident_central=_triple(
                        b.get("utilization_ratio_gom_resident"), "central"
                    ),
                    exposure_usd_per_yr_central=_triple(
                        b.get("exposure_usd_per_yr"), "central"
                    ),
                    heavy_dead_well_cost_usd_median=_triple(
                        b.get("heavy_dead_well_cost_usd"), "median", "central"
                    ),
                    confidence=str(b.get("confidence", "forward_looking_risk")),
                )
            )

        fs = raw.get("fleet_supply") or {}
        params = raw.get("parameters") or {}
        fleet = FleetSupply(
            global_eligible_fleet_count=int(fs.get("global_eligible_fleet_count", 0)),
            eligible_classes=dict(fs.get("eligible_classes") or {}),
            rig_days_per_yr_global_central=float(
                _triple(fs.get("rig_days_per_yr_global"), "central") or 0.0
            ),
            gom_resident_dedicated_total=int(fs.get("gom_resident_dedicated_total", 0)),
            gom_resident_eligible_heavy_count=int(
                fs.get("gom_resident_eligible_heavy_count", 0)
            ),
            gom_resident_eligible_heavy_units=list(
                fs.get("gom_resident_eligible_heavy_units") or []
            ),
            rig_days_per_yr_gom_resident_central=float(
                _triple(fs.get("rig_days_per_yr_gom_resident"), "central") or 0.0
            ),
            gom_resident_light_intervention_count=int(
                fs.get("gom_resident_light_intervention_count", 0)
            ),
            fleet_utilization_central=float(
                _triple(params.get("fleet_utilization"), "central") or 0.0
            ),
            confidence=str(fs.get("confidence", "soft")),
        )

        return cls(
            meta=dict(raw.get("meta") or {}),
            parameters=dict(params),
            bands=bands,
            fleet=fleet,
            dayrates=dict(raw.get("dayrates") or {}),
            caveats=list(raw.get("caveats") or []),
            method=str(raw.get("method", "")),
            source_path=source_path,
        )

    # ---- accessors --------------------------------------------------------

    def band(self, name: str) -> BandDemand:
        """Return the :class:`BandDemand` for a band key (e.g. ``band_5000_10000``)."""
        for b in self.bands:
            if b.band == name:
                return b
        raise KeyError(f"unknown band {name!r}; have {[b.band for b in self.bands]}")

    @property
    def as_of(self) -> str:
        return str(self.meta.get("as_of", ""))

    @property
    def provenance(self) -> dict:
        """Source repo / issue / commit / dates for downstream attribution."""
        m = self.meta
        return {
            "source_repo": m.get("source_repo"),
            "source_issue": m.get("source_issue"),
            "feeds_issue": m.get("feeds_issue"),
            "source_commit": m.get("source_commit"),
            "as_of": m.get("as_of"),
            "dayrate_snapshot_as_of": m.get("dayrate_snapshot_as_of"),
            "confidence": m.get("confidence", "forward_looking_risk"),
        }

    def empirical_demand_summary(self) -> list[dict]:
        """Per-band empirical demand + forward gap, FLOOR/forward-risk labels carried."""
        out: list[dict] = []
        for b in self.bands:
            out.append(
                {
                    "band": b.band,
                    "label": b.label,
                    "subsea_wells": b.subsea_wells,
                    "interventions_per_well_floor": b.interventions_per_well_floor,
                    "demand_rig_days_per_yr_central": b.demand_rig_days_per_yr_central,
                    "utilization_ratio_gom_resident_central": (
                        b.utilization_ratio_gom_resident_central
                    ),
                    "exposure_usd_per_yr_central": b.exposure_usd_per_yr_central,
                    "confidence": b.confidence,
                    "floor_note": (
                        "interventions_per_well is a lifetime UPPER proxy on the ~6% "
                        "depth-stamped WAR subset (#627/#630), NOT an annual lambda"
                    ),
                }
            )
        return out

    # ---- calibration derivations (opt-in; additive) -----------------------

    def heavy_pool_available_days_per_year(
        self, supply_scope: str = "gom_resident"
    ) -> float:
        """Available rig-days/yr for the heavy-deepwater intervention pool.

        ``supply_scope='gom_resident'`` (default) returns the BINDING GoM-resident
        constraint (~2 units); ``'global'`` returns the worldwide pool a GoM
        operator competes for (not exclusively available). See GEOGRAPHY caveat.
        """
        if supply_scope == "global":
            return self.fleet.rig_days_per_yr_global_central
        if supply_scope == "gom_resident":
            return self.fleet.rig_days_per_yr_gom_resident_central
        raise ValueError(
            f"supply_scope must be 'gom_resident' or 'global', got {supply_scope!r}"
        )

    def calibrated_resource_pools(
        self,
        resource_class: str = "modu",
        supply_scope: str = "gom_resident",
        mobilisation_lead_days: float = 45.0,
        batch_campaign: bool = True,
    ) -> list[dict]:
        """Resource-pool config calibrated to the empirical fleet supply.

        Returns a ``resources_cfg``-shaped list (consumable by
        ``lifecycle_summary._build_pools`` / ``resource_queue``) whose
        ``available_days_per_year`` is the real fleet supply for ``supply_scope``
        rather than an assumed capacity. Additive — callers choose to use it.
        """
        return [
            {
                "resource_class": resource_class,
                "available_days_per_year": self.heavy_pool_available_days_per_year(
                    supply_scope
                ),
                "mobilisation_lead_days": mobilisation_lead_days,
                "batch_campaign": batch_campaign,
                "_wed_supply_scope": supply_scope,
            }
        ]

    def access_multiplier_from_supply_ratio(
        self, band: str, supply_scope: str = "gom_resident"
    ) -> dict:
        """Empirically-anchored access multiplier for a band (m_access = min(1, supply/demand)).

        A FORWARD-LOOKING access-risk multiplier: where forward demand exceeds the
        binding supply (``utilization_ratio > 1``), access is capped at
        ``supply / demand`` (= ``1 / utilization_ratio``). This is an access-RISK
        indicator, NOT a measured crossover — the result carries
        ``confidence='forward_looking_risk'``.
        """
        b = self.band(band)
        ratio = (
            b.utilization_ratio_gom_resident_central
            if supply_scope == "gom_resident"
            else b.utilization_ratio_global_central
        )
        if ratio is None or ratio <= 0:
            m = 1.0  # no demand (or undefined) -> no access suppression signal
        else:
            m = min(1.0, 1.0 / ratio)
        return {
            "band": band,
            "supply_scope": supply_scope,
            "utilization_ratio": ratio,
            "m_access": round(m, 4),
            "confidence": "forward_looking_risk",
            "caveat": (
                "FORWARD-LOOKING access RISK, not a measured crossover; binding "
                "supply is the GoM-resident fleet (GEOGRAPHY #628). Pressure class "
                "(HPHT/20k psi) is NOT resolved by water-depth bands."
            ),
        }


def load_wed_calibration(path: str | Path | None = None) -> WedCalibration:
    """Convenience loader for the shipped (or a custom) worldenergydata snapshot."""
    return WedCalibration.load(path)
