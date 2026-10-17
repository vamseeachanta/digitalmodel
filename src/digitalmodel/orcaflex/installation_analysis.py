"""Subsea installation utilities for OrcaFlex pre-processing.

Provides crane tip motion estimation from vessel RAO, dynamic amplification
factor (DAF) calculation, sling tension analysis, lowering through splash
zone analysis, and weight management spreadsheet generation.

Does NOT require OrcFxAPI — all calculations are analytical.

References:
    - DNV-RP-H103 (now DNV-RP-N103): Modelling and Analysis of Marine Operations
    - DNV-OS-H101 / DNV-ST-N001: Marine Operations, General
    - API RP 2A-WSD: Planning, Designing and Constructing Fixed Offshore Platforms
    - Noble Denton 0027/ND: Guidelines for Marine Lifting & Lowering Operations
"""

import math
from enum import Enum
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field, PrivateAttr, model_validator


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class VesselType(str, Enum):
    """Installation vessel types."""
    CONSTRUCTION_VESSEL = "construction_vessel"
    SEMI_SUB_CRANE = "semi_sub_crane"
    MONOHULL_CRANE = "monohull_crane"
    BARGE = "barge"
    AHTS = "AHTS"


class LiftPhase(str, Enum):
    """Lift phase for installation analysis."""
    ON_DECK = "on_deck"
    OVERBOARDING = "overboarding"
    SPLASH_ZONE = "splash_zone"
    LOWERING = "lowering"
    LANDING = "landing"


# ---------------------------------------------------------------------------
# Vessel RAO & crane tip motion
# ---------------------------------------------------------------------------

class VesselRAO(BaseModel):
    """Simplified vessel Response Amplitude Operator.

    Provides characteristic single-amplitude motions at crane tip
    for preliminary engineering.

    Reference: DNV-RP-H103 Section 4.
    """
    vessel_type: VesselType = VesselType.SEMI_SUB_CRANE
    heave_rao: float = Field(0.8, ge=0.0, description="Heave RAO at crane tip (m/m)")
    pitch_rao: float = Field(1.5, ge=0.0, description="Pitch RAO (deg/m)")
    roll_rao: float = Field(2.0, ge=0.0, description="Roll RAO (deg/m)")
    crane_radius: float = Field(30.0, ge=0.0, description="Crane radius from vessel CG (m)")
    crane_height: float = Field(40.0, ge=0.0, description="Crane tip height above WL (m)")
    natural_period_heave: float = Field(20.0, gt=0.0, description="Vessel heave natural period (s)")
    natural_period_roll: float = Field(18.0, gt=0.0, description="Vessel roll natural period (s)")

    def crane_tip_heave(self, hs: float) -> float:
        """Estimate crane tip heave single amplitude (m).

        Crane tip heave = vessel heave + pitch contribution + roll contribution.
        Simplified: uses RAO * Hs/2 for each component combined via SRSS.

        Args:
            hs: Significant wave height (m).

        Returns:
            Crane tip heave single amplitude (m).
        """
        sa = hs / 2.0  # wave amplitude

        heave_amp = self.heave_rao * sa
        pitch_amp_rad = math.radians(self.pitch_rao * sa)
        roll_amp_rad = math.radians(self.roll_rao * sa)

        # Vertical contribution from pitch and roll at crane tip
        pitch_vert = self.crane_radius * math.sin(pitch_amp_rad)
        roll_vert = self.crane_radius * math.sin(roll_amp_rad)

        # SRSS combination
        return math.sqrt(heave_amp**2 + pitch_vert**2 + roll_vert**2)

    def crane_tip_velocity(self, hs: float, tp: float) -> float:
        """Estimate crane tip velocity (m/s).

        v_ct = 2*pi * z_ct / T  (sinusoidal assumption).

        Args:
            hs: Significant wave height (m).
            tp: Peak spectral period (s).

        Returns:
            Crane tip velocity single amplitude (m/s).
        """
        z_ct = self.crane_tip_heave(hs)
        return 2.0 * math.pi * z_ct / tp


# ---------------------------------------------------------------------------
# Dynamic Amplification Factor (DAF)
# ---------------------------------------------------------------------------

class DAFInput(BaseModel):
    """Input for Dynamic Amplification Factor calculation.

    Reference: DNV-RP-H103 Section 5.2 (simplified method).
    """
    static_weight_kN: float = Field(100.0, gt=0.0, description="Static hook load (kN)")
    crane_tip_heave_m: float = Field(1.0, ge=0.0, description="Crane tip heave SA (m)")
    crane_tip_velocity_mps: float = Field(0.5, ge=0.0, description="Crane tip velocity SA (m/s)")
    sling_stiffness_kN_per_m: float = Field(5000.0, gt=0.0, description="Total sling stiffness (kN/m)")
    lifted_mass_kg: float = Field(50000.0, gt=0.0, description="Lifted mass in air (kg)")
    added_mass_factor: float = Field(1.0, ge=0.0, description="Added mass as fraction of dry mass (for subsea)")

    @property
    def natural_period(self) -> float:
        """Natural period of crane-sling-load system (s)."""
        total_mass = self.lifted_mass_kg * (1.0 + self.added_mass_factor)
        k = self.sling_stiffness_kN_per_m * 1000.0  # N/m
        omega = math.sqrt(k / total_mass)
        return 2.0 * math.pi / omega if omega > 0 else float("inf")

    def calculate_daf(self, wave_period: float = 10.0) -> Dict[str, float]:
        """Calculate DAF using the simplified single-degree-of-freedom method.

        DAF = 1 + (a_ct / g) where a_ct = (2*pi/T_wave)^2 * z_ct
        for T_wave >> T_natural (quasi-static).

        More precise: DAF = max(1, 1 + delta_dyn / delta_static)

        Reference: DNV-RP-H103 Section 5.2.

        Args:
            wave_period: Wave period for dynamic response (s).

        Returns:
            Dict with DAF results.
        """
        g = 9.80665
        T_n = self.natural_period

        # Dynamic force from crane tip motion
        omega_wave = 2.0 * math.pi / wave_period
        omega_n = 2.0 * math.pi / T_n if T_n > 0 else float("inf")

        # Frequency ratio
        beta = omega_wave / omega_n if omega_n > 0 else 0.0

        # DAF for single DOF without damping
        if abs(1.0 - beta**2) > 1e-6:
            daf_resonance = 1.0 / abs(1.0 - beta**2)
        else:
            daf_resonance = 10.0  # near resonance, capped

        # Dynamic amplitude: crane tip motion amplified by DAF
        dyn_disp = self.crane_tip_heave_m * min(daf_resonance, 5.0)
        dyn_force = self.sling_stiffness_kN_per_m * dyn_disp

        # Total DAF
        daf = 1.0 + dyn_force / self.static_weight_kN if self.static_weight_kN > 0 else 1.0
        daf = max(1.0, min(daf, 5.0))  # clamp to reasonable range

        return {
            "natural_period_s": round(T_n, 3),
            "frequency_ratio": round(beta, 4),
            "dynamic_force_kN": round(dyn_force, 1),
            "daf": round(daf, 3),
            "dynamic_hook_load_kN": round(self.static_weight_kN * daf, 1),
        }


# ---------------------------------------------------------------------------
# Sling tension calculation
# ---------------------------------------------------------------------------

# DNVGL-ST-E273, 2.7-3 Portable offshore units, Edition April 2016,
# §3.5.5 (Lift points – in-plane loads), pp. 34–35; §3.5.4 covers skew effects.
# Minimum SKL for 2/3 legs and for 4 MATCHED legs, without spreader bars.
_SKL_BY_LEG_COUNT = {2: 1.1, 3: 1.1, 4: 1.33}
_SKL_REFERENCE = (
    "DNVGL-ST-E273, 2.7-3 Portable offshore units, Edition April 2016, "
    "§3.5.5 (skew effects: §3.5.4)"
)
_SKL_DOCUMENT_URL = (
    "https://www.rd-engineering.co/wp-content/uploads/2019/04/"
    "DNVGL-ST-E273APRIL2016.pdf#page=34"
)


class SlingConfig(BaseModel):
    """Common-angle sling tension screening with separate CoG and skew effects.

    SKL defaults follow DNVGL-ST-E273, 2.7-3 Portable offshore units,
    Edition April 2016, §3.5.5. Four-leg defaults assume matched slings.
    Supply the most-loaded quasi-static share at the extreme CoG, or supply
    lift-point coordinates and the extreme CoG offset for 2/3-leg statics.
    Four-leg reactions are indeterminate: supply an independently assessed PL.

    This is a hook-load tension screen, not the standard's full RSF padeye
    design check (which includes 1.2 and a separate design-load basis).
    The cited simplified RSF equation also requires no spreader bars and
    angle from vertical >=30 degrees. Other arrangements need separate design.
    """
    num_slings: int = Field(4, ge=1, description="Number of slings")
    sling_angle_deg: float = Field(60.0, gt=0.0, lt=90.0, description="Sling angle from horizontal (deg)")
    sling_mbl_kN: float = Field(2000.0, gt=0.0, description="Sling MBL per leg (kN)")
    safety_factor: float = Field(3.0, gt=1.0, description="Required safety factor on slings")
    skew_load_factor: Optional[float] = Field(
        None, ge=1.0, allow_inf_nan=False,
        description="Sling-length/fabrication tolerance factor in indeterminate rigging; excludes CoG offset. None selects leg-count default",
    )
    most_loaded_share: Optional[float] = Field(
        None, gt=0.0, le=1.0, allow_inf_nan=False,
        description="Quasi-static fraction of hook load at most-loaded padeye at extreme CoG (PL); excludes SKL",
    )
    lift_points_m: Optional[List[Tuple[float, float]]] = Field(
        None, description="Plan coordinates (x,y) of 2 or 3 lift points, metres, common datum",
    )
    cog_offset_m: Optional[Tuple[float, float]] = Field(
        None, description="Extreme CoG (x,y) offset from lift-point centroid, metres",
    )
    tilt_factor: float = Field(1.05, ge=1.0, description="Tilt/consequence factor")
    _skew_override: Optional[bool] = PrivateAttr(default=None)

    @model_validator(mode="after")
    def _select_skew_default(self) -> "SlingConfig":
        if self._skew_override is None:
            self._skew_override = self.skew_load_factor is not None
        if self.skew_load_factor is None:
            if self.num_slings == 1:
                self.skew_load_factor = 1.0  # No inter-leg skew; modelling convention.
            elif self.num_slings in _SKL_BY_LEG_COUNT:
                self.skew_load_factor = _SKL_BY_LEG_COUNT[self.num_slings]
            else:
                raise ValueError("More than four legs requires an explicit skew_load_factor")
        return self

    def _load_share(self) -> Tuple[float, str]:
        has_geometry = self.lift_points_m is not None or self.cog_offset_m is not None
        if self.most_loaded_share is not None:
            if has_geometry:
                raise ValueError("Supply most_loaded_share or CoG geometry, not both")
            if self.most_loaded_share < 1.0 / self.num_slings:
                raise ValueError("most_loaded_share cannot be below the equal share")
            return self.most_loaded_share, "supplied quasi-static PL at extreme CoG (§3.5.5)"
        if not has_geometry:
            return 1.0 / self.num_slings, "equal split assumed; no CoG/share input supplied"
        if self.lift_points_m is None or self.cog_offset_m is None:
            raise ValueError("CoG statics requires both lift_points_m and cog_offset_m")
        if self.num_slings not in (2, 3):
            raise ValueError("CoG statics supports 2/3 legs; supply most_loaded_share for other sets")
        if len(self.lift_points_m) != self.num_slings:
            raise ValueError("lift_points_m count must match num_slings")
        points = np.asarray(self.lift_points_m, dtype=float)
        offset = np.asarray(self.cog_offset_m, dtype=float)
        if not np.isfinite(points).all() or not np.isfinite(offset).all():
            raise ValueError("Lift-point coordinates and CoG offset must be finite")

        # Vertical force and both plan moments: sum(r)=1, sum(r*x)=CoG_x,
        # sum(r*y)=CoG_y. Coordinates are relative to the lift-point centroid.
        centred = points - points.mean(axis=0)
        span = np.max(np.abs(centred))
        if span == 0:
            raise ValueError("Lift points must be distinct")
        # Scale moments to avoid mixing metre magnitudes with the unit sum.
        equilibrium = np.vstack((np.ones(self.num_slings), (centred / span).T))
        target = np.r_[1.0, offset / span]
        shares, _, rank, _ = np.linalg.lstsq(equilibrium, target, rcond=None)
        if rank != self.num_slings:
            raise ValueError("Lift-point geometry is degenerate")
        if not np.allclose(equilibrium @ shares, target, rtol=0, atol=1e-9):
            raise ValueError("CoG is outside the two-point support line")
        if np.any(shares < -1e-9):
            raise ValueError("CoG requires a negative sling reaction; outside support polygon")
        return float(np.clip(shares, 0, 1).max()), "2/3-leg statics at extreme CoG"

    def calculate_sling_tension(self, hook_load_kN: float) -> Dict[str, object]:
        """Calculate the most-loaded sling tension, preserving the legacy result key.

        T_max = W_hook * PL * SKL * tilt / sin(theta).
        PL is a fraction, not a percentage number or a second tolerance factor.

        Args:
            hook_load_kN: Total hook load including dynamics (kN).

        Returns:
            Dict with sling tension results.
        """
        if not math.isfinite(hook_load_kN) or hook_load_kN < 0:
            raise ValueError("hook_load_kN must be finite and non-negative")
        share, share_basis = self._load_share()
        theta_rad = math.radians(self.sling_angle_deg)
        sin_theta = math.sin(theta_rad)

        sling_tension = (hook_load_kN * share * self.skew_load_factor *
                         self.tilt_factor / sin_theta)

        utilisation = sling_tension / self.sling_mbl_kN
        achieved_sf = self.sling_mbl_kN / sling_tension if sling_tension > 0 else float("inf")

        if self._skew_override:
            skew_source = "explicit override; default reference: " + _SKL_REFERENCE
        elif self.num_slings == 1:
            skew_source = "single leg: no inter-leg skew (modelling convention)"
        else:
            skew_source = _SKL_REFERENCE

        return {
            "sling_tension_per_leg_kN": round(sling_tension, 1),
            "sling_utilisation": round(utilisation, 4),
            "achieved_safety_factor": round(achieved_sf, 2),
            "pass": achieved_sf >= self.safety_factor,
            "most_loaded_share": share,
            "load_share_basis": share_basis,
            "skew_load_factor": self.skew_load_factor,
            "skew_load_factor_source": skew_source,
            "skew_load_factor_below_minimum": (
                self.num_slings in _SKL_BY_LEG_COUNT
                and self.skew_load_factor < _SKL_BY_LEG_COUNT[self.num_slings]
            ),
            # Direct-document sidecar: this screen does not depend on a wiki.
            "citations": [{
                "code_id": "DNVGL-ST-E273",
                "title": "2.7-3 Portable offshore units",
                "publisher": "DNV GL AS",
                "revision": "2016-04",
                "section": "3.5.5",
                "url": _SKL_DOCUMENT_URL,
                "note": "SKL minima and extreme-CoG PL; skew effects in §3.5.4",
            }],
        }


# ---------------------------------------------------------------------------
# Splash zone analysis
# ---------------------------------------------------------------------------

class SplashZoneInput(BaseModel):
    """Splash zone lowering analysis input.

    Reference: DNV-RP-H103 Section 6, Noble Denton 0027/ND Section 7.
    """
    dry_weight_kN: float = Field(500.0, gt=0.0, description="Dry weight (kN)")
    submerged_weight_kN: float = Field(350.0, gt=0.0, description="Fully submerged weight (kN)")
    projected_area_m2: float = Field(25.0, gt=0.0, description="Vertical projected area (m^2)")
    height_m: float = Field(5.0, gt=0.0, description="Object height (m)")
    slam_coefficient: float = Field(5.0, ge=0.0, description="Slamming coefficient Cs")
    drag_coefficient: float = Field(2.0, gt=0.0, description="Drag coefficient Cd")
    added_mass_coefficient: float = Field(1.0, ge=0.0, description="Added mass coefficient Ca")
    crane_tip_heave_m: float = Field(1.5, ge=0.0, description="Crane tip heave SA (m)")
    wave_period_s: float = Field(8.0, gt=0.0, description="Wave period (s)")
    seawater_density: float = Field(1025.0, gt=0.0, description="Seawater density (kg/m^3)")

    def calculate_splash_zone_loads(self) -> Dict[str, float]:
        """Calculate hydrodynamic loads during splash zone transit.

        Key loads:
        - Slamming: F_slam = 0.5 * rho * Cs * A_proj * v^2
        - Drag: F_drag = 0.5 * rho * Cd * A_proj * v_rel^2
        - Added mass (varying buoyancy): delta_F = Ca * rho * V_sub * a

        Reference: DNV-RP-H103 Section 6.

        Returns:
            Dict with splash zone force components.
        """
        g = 9.80665
        rho = self.seawater_density

        # Crane tip velocity at splash zone
        omega = 2.0 * math.pi / self.wave_period_s
        v_ct = omega * self.crane_tip_heave_m
        a_ct = omega**2 * self.crane_tip_heave_m

        # Slamming force (upon initial water entry)
        F_slam = 0.5 * rho * self.slam_coefficient * self.projected_area_m2 * v_ct**2

        # Drag force (during lowering)
        lowering_speed = 0.25  # m/s typical lowering speed
        v_rel = v_ct + lowering_speed
        F_drag = 0.5 * rho * self.drag_coefficient * self.projected_area_m2 * v_rel**2

        # Varying buoyancy force (simplified: half-immersed)
        half_submerged_buoyancy = (self.dry_weight_kN - self.submerged_weight_kN) * 0.5

        # Added mass force
        est_volume = self.projected_area_m2 * self.height_m * 0.5  # estimated submerged volume
        F_added_mass = self.added_mass_coefficient * rho * est_volume * a_ct

        # Snap load estimate (worst case during splash zone)
        snap_load = F_slam + F_drag + F_added_mass / 1000.0  # in kN
        snap_load_kN = snap_load / 1000.0

        # Total dynamic hook load
        min_hook_load = self.submerged_weight_kN - snap_load_kN - half_submerged_buoyancy
        max_hook_load = self.dry_weight_kN + snap_load_kN

        return {
            "crane_tip_velocity_mps": round(v_ct, 3),
            "crane_tip_acceleration_mps2": round(a_ct, 3),
            "slamming_force_kN": round(F_slam / 1000.0, 1),
            "drag_force_kN": round(F_drag / 1000.0, 1),
            "added_mass_force_kN": round(F_added_mass / 1000.0, 1),
            "half_buoyancy_variation_kN": round(half_submerged_buoyancy, 1),
            "max_dynamic_hook_load_kN": round(max_hook_load, 1),
            "min_dynamic_hook_load_kN": round(min_hook_load, 1),
        }


# ---------------------------------------------------------------------------
# Weight management
# ---------------------------------------------------------------------------

class WeightItem(BaseModel):
    """Single item in weight management spreadsheet."""
    description: str = Field(..., description="Item description")
    dry_weight_kN: float = Field(0.0, description="Dry weight (kN)")
    submerged_weight_kN: float = Field(0.0, description="Submerged weight (kN)")
    cog_x: float = Field(0.0, description="COG x from reference (m)")
    cog_y: float = Field(0.0, description="COG y from reference (m)")
    cog_z: float = Field(0.0, description="COG z from reference (m)")


class WeightManagement(BaseModel):
    """Weight management / load summary for installation.

    Reference: Noble Denton 0027/ND Section 3.
    """
    items: List[WeightItem] = Field(default_factory=list)
    contingency_pct: float = Field(10.0, ge=0.0, description="Weight contingency (%)")

    @property
    def total_dry_weight(self) -> float:
        """Total dry weight (kN)."""
        return sum(item.dry_weight_kN for item in self.items)

    @property
    def total_submerged_weight(self) -> float:
        """Total submerged weight (kN)."""
        return sum(item.submerged_weight_kN for item in self.items)

    @property
    def total_with_contingency(self) -> float:
        """Total dry weight with contingency (kN)."""
        return self.total_dry_weight * (1.0 + self.contingency_pct / 100.0)

    def calculate_cog(self) -> Dict[str, float]:
        """Calculate combined centre of gravity.

        Returns:
            Dict with combined COG coordinates.
        """
        total_w = self.total_dry_weight
        if total_w == 0:
            return {"cog_x": 0.0, "cog_y": 0.0, "cog_z": 0.0}

        cog_x = sum(item.dry_weight_kN * item.cog_x for item in self.items) / total_w
        cog_y = sum(item.dry_weight_kN * item.cog_y for item in self.items) / total_w
        cog_z = sum(item.dry_weight_kN * item.cog_z for item in self.items) / total_w

        return {
            "cog_x": round(cog_x, 3),
            "cog_y": round(cog_y, 3),
            "cog_z": round(cog_z, 3),
            "total_dry_weight_kN": round(total_w, 1),
            "total_with_contingency_kN": round(self.total_with_contingency, 1),
        }

    def generate_summary(self) -> List[Dict]:
        """Generate weight management summary table.

        Returns:
            List of item dicts with weight information.
        """
        rows = []
        for item in self.items:
            rows.append({
                "description": item.description,
                "dry_weight_kN": item.dry_weight_kN,
                "submerged_weight_kN": item.submerged_weight_kN,
                "cog_x": item.cog_x,
                "cog_y": item.cog_y,
                "cog_z": item.cog_z,
            })
        # Add totals row
        cog = self.calculate_cog()
        rows.append({
            "description": "TOTAL",
            "dry_weight_kN": round(self.total_dry_weight, 1),
            "submerged_weight_kN": round(self.total_submerged_weight, 1),
            "cog_x": cog["cog_x"],
            "cog_y": cog["cog_y"],
            "cog_z": cog["cog_z"],
        })
        rows.append({
            "description": f"TOTAL + {self.contingency_pct}% CONTINGENCY",
            "dry_weight_kN": round(self.total_with_contingency, 1),
            "submerged_weight_kN": round(self.total_submerged_weight * (1.0 + self.contingency_pct / 100.0), 1),
            "cog_x": cog["cog_x"],
            "cog_y": cog["cog_y"],
            "cog_z": cog["cog_z"],
        })
        return rows
