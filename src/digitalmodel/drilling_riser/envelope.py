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

from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.orcaflex.code_check_engine import APIRP2RDInput, check_api_rp_2rd

#: Externalized mode -> active-limit mapping (externalize-config rule).
_MODE_CONFIG_PATH = Path(__file__).with_name("envelope_modes.yml")


@lru_cache(maxsize=1)
def _mode_active_limits() -> dict:
    cfg = yaml.safe_load(_MODE_CONFIG_PATH.read_text(encoding="utf-8"))
    return {m: set(v["active_limits"]) for m, v in cfg["modes"].items()}

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
    """Minimal current input for the sweep. #1282 refines to a depth profile."""

    surface_speed_mps: float
    profile: str = "uniform"

    def drag_load_n_per_m(self, diameter_m: float) -> float:
        """Uniform static drag per unit length: w = 1/2 rho Cd D U^2 (SI)."""
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


@dataclass(frozen=True)
class EnvelopeCriteria:
    flexjoint_angle_mean_deg: float
    flexjoint_angle_max_deg: float
    von_mises_design_factor: float


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

    @property
    def operable_fraction(self) -> float:
        return float(self.allowable_mask.mean())


def resolve_envelope_criteria(
    mode: OperatingMode = OperatingMode.DRILLING, *, repo_root=None
) -> EnvelopeCriteria:
    """Resolve the cited criteria (fail-closed) for a mode.

    Flex-joint angle limits cite API RP 16Q; the von Mises design factor cites
    API STD 2RD. Raises ``CitationResolutionError`` if the wiki does not resolve.
    (The mode currently selects the same 16Q/2RD criteria set; per-category /
    AMJIG divergence is #1281b.)
    """
    from digitalmodel.riser_database.getters import (
        get_flexjoint_angle_limit,
        get_von_mises_design_factor,
    )

    return EnvelopeCriteria(
        flexjoint_angle_mean_deg=get_flexjoint_angle_limit("mean", repo_root=repo_root).value,
        flexjoint_angle_max_deg=get_flexjoint_angle_limit("max", repo_root=repo_root).value,
        von_mises_design_factor=get_von_mises_design_factor(repo_root=repo_root).value,
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
    tensions_kn = np.full(n, tension_n / 1000.0)   # constant screening tension
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
) -> EnvelopeResult:
    """Sweep offset x current x seastate → envelope surface. See module docstring."""
    rig_limits = rig_limits or RigEnvelopeLimits()
    offsets = np.asarray(offsets_pct, dtype=float)
    currents = np.asarray(current_speeds_mps, dtype=float)
    seas = tuple(seastates)
    shape = (offsets.size, currents.size, len(seas))

    util = {k: np.full(shape, np.nan) for k in ("flexjoint_angle", "von_mises", "stroke", "moonpool")}
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
            vm = _von_mises_utilisation(section, resp, tension_n, criteria.von_mises_design_factor)
            for k, sea in enumerate(seas):
                dyn_angle = rao_angle_deg_per_m * sea.hs_m
                total_angle = static_angle_deg + dyn_angle
                # governing of the mean-limit (static) and max-limit (total) checks
                util["flexjoint_angle"][i, j, k] = max(
                    static_angle_deg / criteria.flexjoint_angle_mean_deg,
                    total_angle / criteria.flexjoint_angle_max_deg,
                )
                util["von_mises"][i, j, k] = vm
                heave = rao_heave_m_per_m * sea.hs_m
                stroke_avail = rig_limits.tj_stroke_m or rig_limits.tensioner_stroke_m
                if stroke_avail:
                    util["stroke"][i, j, k] = (setdown + heave) / stroke_avail
                if rig_limits.moonpool_half_min_m:
                    allow = rig_limits.moonpool_half_min_m - DEFAULT_MOONPOOL_CLEARANCE_MARGIN_M
                    util["moonpool"][i, j, k] = x_offset / allow if allow > 0 else np.inf

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
        allowable = np.nanmax(np.where(np.isnan(stacked), -np.inf, stacked), axis=-1) <= 1.0

    return EnvelopeResult(
        mode=mode,
        offsets_pct=offsets,
        current_speeds_mps=currents,
        seastates=seas,
        per_limit_utilisation=util,
        governing_limit=governing,
        allowable_mask=allowable,
    )
