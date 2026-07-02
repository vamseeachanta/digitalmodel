"""Stack-up assembly engine (#1280, epic #1279 child B).

Assembles a rig-specific riser string from normalized component records (the
``adapter`` SI vocabulary — sourced from the public wed component library or
from caller/wiki-side data) and evaluates the API RP 16Q minimum-top-tension
chain against a rig's tensioner system.

Two "top tension" answers coexist in this package by design:

* :func:`digitalmodel.drilling_riser.stackup.top_tension_required` — a flat
  dynamic-factor screening estimate;
* :func:`minimum_top_tension_16q` (here) — the code-basis minimum after
  tensioner-failure, efficiency and fleet-angle allowances.

Provenance discipline: this module carries NO standards-derived numeric
defaults — ``efficiency`` and ``fleet_angle_factor`` are required kwargs
supplied by the caller/rig data (the 16Q citation getters arrive with #1246).
Illustrative docstring numbers are deliberately disjoint from any documented
project calculation.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, Mapping, Optional

from digitalmodel.drilling_riser.adapter import (
    KIPS_TO_KN,
    compute_riser_string_weight_kn,
)

__all__ = [
    "RigCapabilityCheck",
    "RiserStackupModel",
    "StackupItem",
    "check_rig_capability",
    "minimum_top_tension_16q",
    "tensioner_system_factor",
]


@dataclass(frozen=True)
class StackupItem:
    """One component type in a stack-up: normalized record + joint count."""

    component: Mapping[str, object]
    count: int

    def __post_init__(self) -> None:
        if self.count < 1:
            raise ValueError(f"count must be >= 1, got {self.count}")

    @property
    def component_id(self) -> str:
        return str(self.component.get("component_id", "?"))


@dataclass(frozen=True)
class RiserStackupModel:
    """A quantity-expanded riser string with fail-closed aggregates.

    ``rsu_id``/``rig_ref`` are provenance HANDLES only (they resolve in the
    private registry / ``riser_database`` tables); this model never fetches
    wiki-side values itself.
    """

    items: tuple[StackupItem, ...]
    rsu_id: str = ""
    rig_ref: str = ""

    # -- constructors -------------------------------------------------------------

    @classmethod
    def from_component_counts(
        cls,
        counts: Mapping[str, int],
        registry: Mapping[str, Mapping[str, object]],
        *,
        rsu_id: str = "",
        rig_ref: str = "",
    ) -> "RiserStackupModel":
        """Build from ``{component_id: count}`` against an adapter registry
        (e.g. the vendored wed component library). Unknown ids raise KeyError
        (escalate — no fuzzy matching)."""
        items = tuple(
            StackupItem(component=registry[component_id], count=count)
            for component_id, count in counts.items()
        )
        return cls(items=items, rsu_id=rsu_id, rig_ref=rig_ref)

    @classmethod
    def from_records(
        cls,
        records: Iterable[Mapping[str, object]],
        *,
        rsu_id: str = "",
        rig_ref: str = "",
    ) -> "RiserStackupModel":
        """Build from caller-supplied normalized records (count defaults to 1;
        pass an explicit ``count`` key per record to expand)."""
        items = tuple(
            StackupItem(component=rec, count=int(rec.get("count", 1)))
            for rec in records
        )
        return cls(items=items, rsu_id=rsu_id, rig_ref=rig_ref)

    # -- aggregates (fail-closed) ---------------------------------------------------

    def total_submerged_weight_kn(self) -> float:
        """NET submerged weight (buoyed joints contribute negative values)."""
        return sum(
            compute_riser_string_weight_kn([item.component]) * item.count
            for item in self.items
        )

    def total_dry_weight_kn(self) -> float:
        total = 0.0
        for item in self.items:
            if "weight_air_kn" not in item.component:
                raise ValueError(
                    f"Component {item.component_id} lacks weight_air_kn"
                )
            total += float(item.component["weight_air_kn"]) * item.count
        return total

    def gross_submerged_weight_and_uplift(self) -> tuple[float, float]:
        """(gross submerged steel weight Ws, buoyancy uplift Bn), both kN.

        The 16Q factored split (f_wt*Ws - f_bt*Bn) needs the gross/uplift
        decomposition. A buoyed item (negative net submerged weight) WITHOUT
        ``buoyancy_uplift_kn`` raises — silently degrading to net weight
        under-estimates the factored demand (non-conservative), so the split
        fails closed and the caller must supply uplift data.
        """
        gross = 0.0
        uplift = 0.0
        offenders: list[str] = []
        for item in self.items:
            net = float(item.component["submerged_weight_kn"]) * item.count
            item_uplift = item.component.get("buoyancy_uplift_kn")
            if item_uplift is not None:
                item_uplift_total = float(item_uplift) * item.count
                gross += net + item_uplift_total
                uplift += item_uplift_total
            elif net < 0:
                offenders.append(item.component_id)
            else:
                gross += net
        if offenders:
            raise ValueError(
                "buoyed components without buoyancy_uplift_kn (cannot split "
                f"gross weight/uplift; net degrade is non-conservative): {offenders}"
            )
        return gross, uplift

    def total_length_m(
        self, length_overrides: Optional[Mapping[str, float]] = None
    ) -> float:
        """Stack length: ``length_m``, else ``height_m``, else a caller
        override keyed by component id (the caller states the convention,
        e.g. telescopic-joint effective length at a stroke position);
        still-unresolvable components raise, listing their ids."""
        overrides = dict(length_overrides or {})
        total = 0.0
        offenders: list[str] = []
        for item in self.items:
            if item.component_id in overrides:
                total += overrides[item.component_id] * item.count
                continue
            value = item.component.get("length_m", item.component.get("height_m"))
            if value is None:
                offenders.append(item.component_id)
                continue
            total += float(value) * item.count
        if offenders:
            raise ValueError(
                f"components with no length_m/height_m and no override: {offenders}"
            )
        return total


# -- API RP 16Q minimum top tension chain ---------------------------------------------


def tensioner_system_factor(
    n_units: int | float, n_fail: int | float, efficiency: float
) -> float:
    """N / (Rf * (N - n)): the tensioner-system allowance for ``n_fail``
    sudden unit/wire failures at mechanical efficiency ``Rf``."""
    n = int(n_units)
    f = int(n_fail)
    if n < 1:
        raise ValueError(f"n_units must be >= 1, got {n_units}")
    if not 0 <= f < n:
        raise ValueError(f"n_fail must be in [0, n_units), got {n_fail}")
    if not 0.0 < efficiency <= 1.0:
        raise ValueError(f"efficiency must be in (0, 1], got {efficiency}")
    return n / (efficiency * (n - f))


def minimum_top_tension_16q(
    t_srmin_kn: float,
    *,
    n_units: int | float,
    n_fail: int | float,
    efficiency: float,
    fleet_angle_factor: float = 1.0,
) -> float:
    """API RP 16Q minimum top tension: T_SRmin scaled by the tensioner-system
    factor and the fleet-angle factor.

    ``t_srmin_kn`` is the factored minimum slip-ring tension — see
    :func:`digitalmodel.drilling_riser.stackup.minimum_slip_ring_tension` for
    the Ws/Bn/mud composition. ``efficiency`` has no default here (supply it
    from rig data); ``fleet_angle_factor`` defaults to the neutral 1.0.
    The function is linear in ``t_srmin_kn`` (unit-consistent in/out).

    Example (synthetic): ``minimum_top_tension_16q(1000.0, n_units=8,
    n_fail=1, efficiency=0.9, fleet_angle_factor=1.02)``.
    """
    if fleet_angle_factor < 1.0:
        raise ValueError(
            f"fleet_angle_factor must be >= 1.0, got {fleet_angle_factor}"
        )
    return (
        t_srmin_kn
        * tensioner_system_factor(n_units, n_fail, efficiency)
        * fleet_angle_factor
    )


# -- rig capability -------------------------------------------------------------------


@dataclass(frozen=True)
class RigCapabilityCheck:
    """Outcome of checking a tension demand against a rig's tensioner system."""

    min_top_tension_kn: float
    capacity_kn: Optional[float]
    utilization: Optional[float]
    n_units: Optional[int]
    verdict: str  # PASS | EXCEEDS | INSUFFICIENT_DATA
    reason: str = ""


def _unit_count(rig) -> Optional[int]:
    """Unit-count selection rule: wireline rigs tension through wires
    (``n_tension_wires``); direct-acting rigs through tensioner units
    (``n_tensioners``)."""
    raw = (
        rig.n_tension_wires
        if getattr(rig, "tensioner_type", "") == "wireline"
        else rig.n_tensioners
    )
    return None if raw is None else int(raw)


def check_rig_capability(
    min_top_tension_kn: float,
    rig,
    *,
    n_fail: int,
) -> RigCapabilityCheck:
    """Check a 16Q minimum-top-tension demand against a
    ``RigRiserInterfaceRow``. Missing capability data yields
    ``INSUFFICIENT_DATA`` — never a guessed verdict. ``n_fail`` is
    caller-supplied (failure allowances are prose-only in the rig table)."""
    n_units = _unit_count(rig)
    capacity_kips = rig.tensioner_capacity_kips
    if n_units is None or capacity_kips is None:
        missing = "n_units" if n_units is None else "tensioner_capacity_kips"
        return RigCapabilityCheck(
            min_top_tension_kn=min_top_tension_kn,
            capacity_kn=None,
            utilization=None,
            n_units=n_units,
            verdict="INSUFFICIENT_DATA",
            reason=f"rig row {rig.rig_ref!r} lacks {missing}",
        )
    surviving = n_units - int(n_fail)
    if surviving < 1:
        return RigCapabilityCheck(
            min_top_tension_kn=min_top_tension_kn,
            capacity_kn=None,
            utilization=None,
            n_units=n_units,
            verdict="INSUFFICIENT_DATA",
            reason=f"n_fail={n_fail} leaves no surviving units of {n_units}",
        )
    capacity_kn = surviving * float(capacity_kips) * KIPS_TO_KN
    utilization = min_top_tension_kn / capacity_kn
    return RigCapabilityCheck(
        min_top_tension_kn=min_top_tension_kn,
        capacity_kn=capacity_kn,
        utilization=utilization,
        n_units=n_units,
        verdict="PASS" if utilization <= 1.0 else "EXCEEDS",
    )
