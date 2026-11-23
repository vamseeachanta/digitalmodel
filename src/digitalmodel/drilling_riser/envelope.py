"""Drilling riser operating-envelope engine — analytical screening tier (#1281a).

Sweeps vessel offset x surface current x seastate for an assembled, rig-specific
riser string and evaluates each response against its operating limit, producing
an allowable-region (envelope) surface. Child C of epic #1279.

Responses (all move with the sweep — the anti-degeneracy requirement):
  * flex-joint angle   — static beam-column angle (offset + current) plus an
    RAO motion contribution (seastate Hs);
  * von Mises utilisation — API STD 2RD combined tension+bending check fed the
    beam-column bending moment M(z) (so it responds to offset/current);
  * tensioner / TJ stroke — geometric setdown(offset) + RAO heave(Hs);
  * moonpool clearance — vessel excursion vs the moonpool half-dimension.

Criteria: flex-joint angle limits (API RP 16Q) and the von Mises design factor
(API STD 2RD) resolve fail-closed through cited getters
(:mod:`digitalmodel.riser_database.getters`). Stroke margin and moonpool
clearance are UNCITED project defaults (no corresponding public standards
provision identified — the Barlow / top-tension-SF precedent).

This is the ANALYTICAL tier: static tensioned beam-column response + a linear
RAO seastate contribution. Dynamic amplification, drift-off, recoil and VIV are
the solver tier (#1281c). Wellhead/conductor-moment limits need a net-new data
column and land in #1281b. SI internally.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from pathlib import Path
from typing import Optional, Sequence

import numpy as np
import yaml

from digitalmodel.drilling_riser.conductor_response import solve_conductor_moment
from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.orcaflex.code_check_engine import APIRP2RDInput, check_api_rp_2rd

#: Externalized mode -> active-limit mapping (externalize-config rule).
_MODE_CONFIG_PATH = Path(__file__).with_name("envelope_modes.yml")


@lru_cache(maxsize=1)
def _mode_config() -> dict:
    cfg = yaml.safe_load(_MODE_CONFIG_PATH.read_text(encoding="utf-8"))
    return cfg


@lru_cache(maxsize=1)
def _mode_active_limits() -> dict:
    cfg = _mode_config()
    return {m: set(v["active_limits"]) for m, v in cfg["modes"].items()}


@lru_cache(maxsize=1)
def _criteria_set_ids() -> set:
    return set(_mode_config().get("criteria_sets", {"16q": {}, "16q-amjig": {}}))


#: Solver-tier dynamic-amplification library atlas basename (#1346, stub).
DYNAMIC_ATLAS_BASENAME: str = "drilling_riser_envelope"
#: Uncited project defaults (no public standards provision identified).
DEFAULT_MOONPOOL_CLEARANCE_MARGIN_M: float = 0.5
#: Public hydrodynamic-coefficient values (AMJIG sub-critical Cd; seawater rho).
_DRAG_COEFFICIENT: float = 1.2
_SEAWATER_DENSITY_KG_M3: float = 1025.0


class OperatingMode(str, Enum):
    """Riser operating mode; selects the active criteria set."""

    DRILLING = "drilling"
    CONNECTED = "connected"  # connected non-drilling / extreme
    HANG_OFF = "hang_off"


@dataclass(frozen=True)
class SeaState:
    hs_m: float
    tp_s: float


@dataclass(frozen=True)
class CurrentProfile:
    """Current input for the sweep.

    The sweep uses the surface speed only (uniform static drag). Depth-resolved
    fields (#1282) are carried but DORMANT: ``depths_m`` / ``speeds_mps`` /
    ``directions_deg`` and ``current_type`` ("normal" or "loop") describe the
    metocean profile so it round-trips through the schema, but the envelope's
    ``drag_load_n_per_m`` still integrates only the surface speed — a
    depth-integrated drag is a deferred engine edit. Kept as tuples so the
    frozen dataclass stays hashable.
    """

    surface_speed_mps: float
    profile: str = "uniform"
    depths_m: tuple = ()
    speeds_mps: tuple = ()
    directions_deg: tuple = ()
    current_type: str = "normal"

    def drag_load_n_per_m(self, diameter_m: float) -> float:
        """Uniform static drag per unit length: w = 1/2 rho Cd D U^2 (SI).

        Uses the surface speed; the dormant depth profile is not yet integrated.
        """
        return (
            0.5
            * _SEAWATER_DENSITY_KG_M3
            * _DRAG_COEFFICIENT
            * diameter_m
            * self.surface_speed_mps**2
        )


@dataclass(frozen=True)
class RiserSection:
    outer_diameter_m: float
    wall_thickness_m: float
    youngs_modulus_pa: float = 2.07e11
    smys_pa: float = 4.48e8

    @property
    def inner_diameter_m(self) -> float:
        return self.outer_diameter_m - 2.0 * self.wall_thickness_m

    @property
    def second_moment_m4(self) -> float:
        return math.pi / 64.0 * (self.outer_diameter_m**4 - self.inner_diameter_m**4)

    @property
    def ei_nm2(self) -> float:
        return self.youngs_modulus_pa * self.second_moment_m4


@dataclass(frozen=True)
class RigEnvelopeLimits:
    """Rig capability that bounds the envelope (from rig_riser_interface data)."""

    tj_stroke_m: Optional[float] = None
    tensioner_stroke_m: Optional[float] = None
    moonpool_half_min_m: Optional[float] = None
    #: Rated wellhead/conductor bending-moment capacity [kN.m] and flex-joint
    #: hardware angular rating [deg] (#1345). None -> that limit is inactive
    #: (the public data columns ship empty; the paired wed issue populates them).
    conductor_moment_capacity_kn_m: Optional[float] = None
    flexjoint_angle_rating_deg: Optional[float] = None


@dataclass(frozen=True)
class ConductorInput:
    """Below-mudline conductor properties for the wellhead-moment check (#1345)."""

    outer_diameter_m: float
    wall_thickness_m: float
    soil_modulus_n_per_m2: float
    stand_off_m: float
    youngs_modulus_pa: float = 2.07e11

    @property
    def ei_nm2(self) -> float:
        inner = self.outer_diameter_m - 2.0 * self.wall_thickness_m
        second_moment = math.pi / 64.0 * (self.outer_diameter_m**4 - inner**4)
        return self.youngs_modulus_pa * second_moment


@dataclass(frozen=True)
class EnvelopeCriteria:
    flexjoint_angle_mean_deg: float
    flexjoint_angle_max_deg: float
    von_mises_design_factor: float
    criteria_set: str = "16q"
    category: str = "drilling"


@dataclass(frozen=True)
class EnvelopeResult:
    """Envelope surface over (offset, current, seastate) for one mode.

    ``per_limit_utilisation`` maps a limit name to an array of shape
    ``(n_offset, n_current, n_seastate)``; NaN marks a limit with no data
    (excluded from governing/allowable). ``allowable_mask`` is True where every
    evaluated limit is within its allowable.
    """

    mode: OperatingMode
    offsets_pct: np.ndarray
    current_speeds_mps: np.ndarray
    seastates: tuple
    per_limit_utilisation: dict
    governing_limit: np.ndarray
    allowable_mask: np.ndarray
    #: Populated only when the solver-tier dynamic path runs (#1346): carries the
    #: solver provenance + disclaimer so a dynamically-amplified verdict can never
    #: leave without its STUB/licensed marker. ``None`` for the pure C1 static path.
    dynamic_provenance: Optional[dict] = None
    #: Criteria-set provenance for the allowable mask (for example ``16q`` or
    #: ``16q-amjig``). The physical response arrays are standard-independent;
    #: only the allowable mask changes with this tag.
    criteria_set: str = "16q"
    criteria_category: str = ""

    @property
    def operable_fraction(self) -> float:
        return float(self.allowable_mask.mean())


def resolve_envelope_criteria(
    mode: OperatingMode = OperatingMode.DRILLING,
    *,
    criteria_set: str = "16q",
    repo_root=None,
) -> EnvelopeCriteria:
    """Resolve the cited criteria (fail-closed) for a mode.

    ``criteria_set="16q"`` keeps the plain API RP 16Q / API STD 2RD limits.
    ``criteria_set="16q-amjig"`` keeps the physical response unchanged but
    resolves AMJIG per-category allowable ceilings from the private wiki.
    """
    mode = OperatingMode(mode)
    if criteria_set not in _criteria_set_ids():
        allowed = ", ".join(sorted(_criteria_set_ids()))
        raise ValueError(f"criteria_set must be one of {allowed}, got {criteria_set!r}")
    from digitalmodel.riser_database.getters import (
        get_amjig_envelope_criteria,
        get_flexjoint_angle_limit,
        get_von_mises_design_factor,
    )

    if criteria_set == "16q-amjig":
        amjig = get_amjig_envelope_criteria(mode.value, repo_root=repo_root)
        return EnvelopeCriteria(
            flexjoint_angle_mean_deg=amjig.flexjoint_angle_mean_deg.value,
            flexjoint_angle_max_deg=amjig.flexjoint_angle_max_deg.value,
            von_mises_design_factor=amjig.von_mises_design_factor.value,
            criteria_set=amjig.criteria_set,
            category=amjig.category,
        )

    return EnvelopeCriteria(
        flexjoint_angle_mean_deg=get_flexjoint_angle_limit(
            "mean", repo_root=repo_root
        ).value,
        flexjoint_angle_max_deg=get_flexjoint_angle_limit(
            "max", repo_root=repo_root
        ).value,
        von_mises_design_factor=get_von_mises_design_factor(repo_root=repo_root).value,
        criteria_set=criteria_set,
        category=mode.value,
    )


def _von_mises_utilisation(
    section: RiserSection,
    response,
    tension_n: float,
    design_factor: float,
) -> float:
    pipe = APIRP2RDInput(
        outer_diameter=section.outer_diameter_m,
        wall_thickness=section.wall_thickness_m,
        smys=section.smys_pa,
        design_factor=design_factor,
    )
    n = response.z_m.size
    tensions_kn = np.full(n, tension_n / 1000.0)  # constant screening tension
    moments_knm = response.bending_moment_nm / 1000.0
    results = check_api_rp_2rd(pipe, response.z_m, tensions_kn, moments_knm)
    return max(r.utilisation for r in results)


def compute_operating_envelope(
    *,
    section: RiserSection,
    water_depth_m: float,
    length_m: float,
    tension_n: float,
    criteria: EnvelopeCriteria,
    offsets_pct: Sequence[float],
    current_speeds_mps: Sequence[float],
    seastates: Sequence[SeaState],
    rig_limits: Optional[RigEnvelopeLimits] = None,
    rao_angle_deg_per_m: float = 0.0,
    rao_heave_m_per_m: float = 0.0,
    mode: OperatingMode = OperatingMode.DRILLING,
    dynamic: bool = False,
    atlas_root: Optional[Path] = None,
    conductor: Optional["ConductorInput"] = None,
) -> EnvelopeResult:
    """Sweep offset x current x seastate → envelope surface. See module docstring.

    ``dynamic=True`` engages the solver-tier (#1346): the von Mises utilisation
    is multiplied by the dynamic amplification factor from the
    ``drilling_riser_envelope`` library atlas (a STUB until a licensed OrcaFlex
    run populates it), and ``EnvelopeResult.dynamic_provenance`` is populated
    with the solver marker + disclaimer. A point outside the atlas coverage
    ESCALATES (its von Mises → NaN), never extrapolates. ``dynamic=False``
    (default) is the pure C1 static path, byte-for-byte unchanged.
    """
    rig_limits = rig_limits or RigEnvelopeLimits()
    dyn_atlas = None
    dyn_escalated = 0
    if dynamic:
        from digitalmodel.parametric.atlas import Atlas
        from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

        dyn_atlas = Atlas.load(atlas_root or DEFAULT_ATLAS_ROOT, DYNAMIC_ATLAS_BASENAME)
    offsets = np.asarray(offsets_pct, dtype=float)
    currents = np.asarray(current_speeds_mps, dtype=float)
    seas = tuple(seastates)
    shape = (offsets.size, currents.size, len(seas))

    util = {
        k: np.full(shape, np.nan)
        for k in ("flexjoint_angle", "von_mises", "stroke", "moonpool", "wh_moment")
    }
    diameter = section.outer_diameter_m

    for i, off_pct in enumerate(offsets):
        x_offset = watch_circle_radius_m(float(off_pct), water_depth_m)
        setdown = x_offset**2 / (2.0 * length_m)
        for j, u in enumerate(currents):
            w = CurrentProfile(float(u)).drag_load_n_per_m(diameter)
            resp = solve_static_response(
                length_m=length_m,
                top_offset_m=x_offset,
                tension_n=tension_n,
                ei_nm2=section.ei_nm2,
                current_load_n_per_m=w,
            )
            static_angle_deg = max(abs(resp.angle_lower_deg), abs(resp.angle_upper_deg))
            vm = _von_mises_utilisation(
                section, resp, tension_n, criteria.von_mises_design_factor
            )
            # Wellhead/conductor moment (#1345): the riser lower-flex-joint shear
            # over the BOP/LMRP stand-off arm loads the conductor below mudline.
            wh_moment_knm = None
            if conductor is not None:
                wh_moment_knm = (
                    solve_conductor_moment(
                        shear_n=resp.shear_lower_n,
                        stand_off_m=conductor.stand_off_m,
                        soil_modulus_n_per_m2=conductor.soil_modulus_n_per_m2,
                        ei_nm2=conductor.ei_nm2,
                    ).max_moment_nm
                    / 1000.0
                )
            # Flex-joint max limit = governing min(16Q operating limit, per-rig
            # hardware angular rating) when the rating is present (#1345).
            fj_max_limit = criteria.flexjoint_angle_max_deg
            if rig_limits.flexjoint_angle_rating_deg is not None:
                fj_max_limit = min(fj_max_limit, rig_limits.flexjoint_angle_rating_deg)
            for k, sea in enumerate(seas):
                dyn_angle = rao_angle_deg_per_m * sea.hs_m
                total_angle = static_angle_deg + dyn_angle
                # governing of the mean-limit (static) and max-limit (total) checks
                util["flexjoint_angle"][i, j, k] = max(
                    static_angle_deg / criteria.flexjoint_angle_mean_deg,
                    total_angle / fj_max_limit,
                )
                if (
                    wh_moment_knm is not None
                    and rig_limits.conductor_moment_capacity_kn_m
                ):
                    util["wh_moment"][i, j, k] = (
                        wh_moment_knm / rig_limits.conductor_moment_capacity_kn_m
                    )
                util["von_mises"][i, j, k] = vm
                if dyn_atlas is not None:
                    pred = dyn_atlas.predict(
                        {
                            "mode": mode.value,
                            "offset_pct": float(off_pct),
                            "current_speed_mps": float(u),
                            "hs_m": sea.hs_m,
                            "tp_s": sea.tp_s,
                        }
                    )
                    if pred.in_range:
                        util["von_mises"][i, j, k] = vm * pred.value
                    else:
                        util["von_mises"][
                            i, j, k
                        ] = np.nan  # escalate, never extrapolate
                        dyn_escalated += 1
                heave = rao_heave_m_per_m * sea.hs_m
                stroke_avail = rig_limits.tj_stroke_m or rig_limits.tensioner_stroke_m
                if stroke_avail:
                    util["stroke"][i, j, k] = (setdown + heave) / stroke_avail
                if rig_limits.moonpool_half_min_m:
                    allow = (
                        rig_limits.moonpool_half_min_m
                        - DEFAULT_MOONPOOL_CLEARANCE_MARGIN_M
                    )
                    util["moonpool"][i, j, k] = (
                        x_offset / allow if allow > 0 else np.inf
                    )

    # Apply the mode's active-limit set: limits not gating this mode -> NaN
    # (excluded from governing/allowable), per envelope_modes.yml.
    active = _mode_active_limits().get(mode.value, set(util.keys()))
    for k in list(util):
        if k not in active:
            util[k][:] = np.nan

    stacked_names = list(util.keys())
    stacked = np.stack([util[k] for k in stacked_names], axis=-1)  # (..., n_limits)
    with np.errstate(invalid="ignore"):
        gov_idx = np.nanargmax(np.where(np.isnan(stacked), -np.inf, stacked), axis=-1)
        governing = np.array(stacked_names, dtype=object)[gov_idx]
        allowable = (
            np.nanmax(np.where(np.isnan(stacked), -np.inf, stacked), axis=-1) <= 1.0
        )

    dynamic_provenance = None
    if dyn_atlas is not None:
        from digitalmodel.parametric.query import SCREENING_DISCLAIMER

        solver = dyn_atlas.provenance.get("solver", {}) or {}
        dynamic_provenance = {
            "applies_to": "von_mises",
            "atlas_id": dyn_atlas.atlas_id,
            "solver_licensed": solver.get("licensed"),
            "solver_version": solver.get("version"),
            "escalated_points": dyn_escalated,
            "disclaimer": SCREENING_DISCLAIMER,
        }

    return EnvelopeResult(
        mode=mode,
        offsets_pct=offsets,
        current_speeds_mps=currents,
        seastates=seas,
        per_limit_utilisation=util,
        governing_limit=governing,
        allowable_mask=allowable,
        dynamic_provenance=dynamic_provenance,
        criteria_set=criteria.criteria_set,
        criteria_category=criteria.category,
    )
