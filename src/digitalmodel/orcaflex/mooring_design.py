"""Mooring system preliminary design utilities.

Provides catenary equations, anchor radius calculation, line length estimation,
pretension calculation, and a library of chain/wire/polyester properties.
Supports spread mooring and turret mooring configurations.

Does NOT require OrcFxAPI — all calculations are analytical/numerical.

References:
    - API RP 2SK: Design and Analysis of Stationkeeping Systems
    - DNV-OS-E301: Position Mooring
    - Faltinsen (1990): Sea Loads on Ships and Offshore Structures, Ch. 6
"""

import math
import os
import warnings
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field

from digitalmodel.citations import Citation, CitationResolutionError, CitedValue
from digitalmodel.citations.registry import (
    MooringCondition,
    get_mooring_safety_factor,
)


_INTACT_FIELD_DEFAULT: float = 1.67
_DAMAGED_FIELD_DEFAULT: float = 1.25
_REPO_ROOT_WALK_HARD_CAP: int = 8
_REPO_ROOT_RESOLUTION_CACHE: dict = {}


def _default_repo_root(explicit: Optional[Path] = None) -> Optional[Path]:
    """Resolve wiki base for citation validation (#2685, #617).

    Delegates to digitalmodel.citations.resolver.resolve_wiki_base() which owns
    the canonical 6-level precedence chain (override → LLM_WIKI_PATH →
    DIGITALMODEL_REPO_ROOT legacy with DeprecationWarning → parent walk cap=8 →
    known local clones → fail-closed).

    Translates the resolver's fail-closed `CitationResolutionError` into `None`
    for this caller's standalone-mode-graceful-warn semantics: the
    `_resolve_sf_for_condition` helper above emits a one-shot RuntimeWarning
    when resolution returns None. Invalid-env-var errors (stale clone,
    nonexistent path) still propagate — those are user-configuration mistakes,
    not standalone mode.
    """
    from digitalmodel.citations.resolver import resolve_wiki_base

    try:
        return resolve_wiki_base(override=explicit)
    except CitationResolutionError as e:
        # resolver_unconfigured → caller wants None (standalone mode + warn)
        # Anything else (llm_wiki_path_invalid, llm_wiki_path_stale_clone,
        # override_invalid) → user-configuration mistake; propagate.
        if e.reason.startswith("resolver_unconfigured"):
            return None
        raise


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class MooringPattern(str, Enum):
    """Mooring pattern types."""
    SPREAD = "spread"
    TURRET = "turret"
    CALM = "CALM"


class SegmentMaterial(str, Enum):
    """Mooring line segment material."""
    CHAIN_R3 = "chain_R3"
    CHAIN_R3S = "chain_R3S"
    CHAIN_R4 = "chain_R4"
    CHAIN_R4S = "chain_R4S"
    CHAIN_R5 = "chain_R5"
    WIRE_ROPE = "wire_rope"
    WIRE_SPIRAL = "wire_spiral"
    POLYESTER = "polyester"
    HMPE = "hmpe"
    NYLON = "nylon"


# ---------------------------------------------------------------------------
# Material property library
# ---------------------------------------------------------------------------

class MooringMaterialProperties(BaseModel):
    """Material properties for a mooring line component.

    Reference: API RP 2SK Table C-1, DNV-OS-E302 Table B1.
    """
    material: SegmentMaterial
    nominal_diameter: float = Field(..., gt=0.0, description="Nominal diameter (mm)")
    mass_per_unit_length: float = Field(..., gt=0.0, description="Mass in air (kg/m)")
    submerged_weight: float = Field(..., description="Submerged weight (N/m)")
    mbl: float = Field(..., gt=0.0, description="Minimum breaking load (kN)")
    axial_stiffness: float = Field(..., gt=0.0, description="Axial stiffness EA (kN)")
    cd_normal: float = Field(2.4, gt=0.0, description="Normal drag coefficient")


# Standard mooring component library
MOORING_MATERIAL_LIBRARY: Dict[str, MooringMaterialProperties] = {
    "R4_76mm_chain": MooringMaterialProperties(
        material=SegmentMaterial.CHAIN_R4,
        nominal_diameter=76,
        mass_per_unit_length=115.0,
        submerged_weight=1000.0,
        mbl=5765.0,
        axial_stiffness=615_000.0,
    ),
    "R4_84mm_chain": MooringMaterialProperties(
        material=SegmentMaterial.CHAIN_R4,
        nominal_diameter=84,
        mass_per_unit_length=139.0,
        submerged_weight=1210.0,
        mbl=6978.0,
        axial_stiffness=757_000.0,
    ),
    "R4S_84mm_chain": MooringMaterialProperties(
        material=SegmentMaterial.CHAIN_R4S,
        nominal_diameter=84,
        mass_per_unit_length=139.0,
        submerged_weight=1210.0,
        mbl=7884.0,
        axial_stiffness=757_000.0,
    ),
    "R5_84mm_chain": MooringMaterialProperties(
        material=SegmentMaterial.CHAIN_R5,
        nominal_diameter=84,
        mass_per_unit_length=139.0,
        submerged_weight=1210.0,
        mbl=8533.0,
        axial_stiffness=757_000.0,
    ),
    "96mm_wire_rope": MooringMaterialProperties(
        material=SegmentMaterial.WIRE_ROPE,
        nominal_diameter=96,
        mass_per_unit_length=36.5,
        submerged_weight=295.0,
        mbl=6200.0,
        axial_stiffness=750_000.0,
        cd_normal=1.2,
    ),
    "137mm_wire_spiral": MooringMaterialProperties(
        material=SegmentMaterial.WIRE_SPIRAL,
        nominal_diameter=137,
        mass_per_unit_length=88.0,
        submerged_weight=720.0,
        mbl=13000.0,
        axial_stiffness=1_700_000.0,
        cd_normal=1.0,
    ),
    "160mm_polyester": MooringMaterialProperties(
        material=SegmentMaterial.POLYESTER,
        nominal_diameter=160,
        mass_per_unit_length=18.5,
        submerged_weight=7.5,
        mbl=8500.0,
        axial_stiffness=200_000.0,
        cd_normal=1.2,
    ),
    "220mm_polyester": MooringMaterialProperties(
        material=SegmentMaterial.POLYESTER,
        nominal_diameter=220,
        mass_per_unit_length=35.0,
        submerged_weight=14.0,
        mbl=16000.0,
        axial_stiffness=400_000.0,
        cd_normal=1.2,
    ),
}


# ---------------------------------------------------------------------------
# Catenary equations
# ---------------------------------------------------------------------------

class CatenaryResult(BaseModel):
    """Result of catenary equation solution."""
    horizontal_tension: float = Field(..., description="Horizontal tension component (kN)")
    vertical_tension_top: float = Field(..., description="Vertical tension at top (kN)")
    top_tension: float = Field(..., description="Tension at top of catenary (kN)")
    top_angle: float = Field(..., description="Angle at top from horizontal (deg)")
    suspended_length: float = Field(..., description="Suspended line length (m)")
    grounded_length: float = Field(..., description="Length of line on seabed (m)")
    anchor_radius: float = Field(..., description="Horizontal distance from fairlead to anchor (m)")
    catenary_parameter: float = Field(..., description="Catenary parameter a = H/w (m)")


def solve_catenary(
    water_depth: float,
    line_length: float,
    submerged_weight_per_m: float,
    fairlead_depth: float = 0.0,
    pretension: Optional[float] = None,
) -> CatenaryResult:
    """Solve the catenary equation for a single-segment mooring line.

    The classic catenary: y = a * cosh(x/a) - a
    where a = H/w (catenary parameter), H = horizontal tension, w = submerged weight/m.

    Args:
        water_depth: Water depth (m).
        line_length: Total line length (m).
        submerged_weight_per_m: Submerged weight per unit length (N/m).
        fairlead_depth: Depth of fairlead below water surface (m).
        pretension: If given, solve for this pretension (kN) at fairlead.

    Returns:
        CatenaryResult with tensions, angles, and geometry.

    Reference: Faltinsen (1990), Mooring Dynamics ch. 6.
    """
    h = water_depth - fairlead_depth  # vertical span
    w = submerged_weight_per_m  # N/m

    if w <= 0:
        raise ValueError("Submerged weight must be positive for catenary solution")
    if line_length <= h:
        raise ValueError(f"Line length ({line_length} m) must exceed vertical span ({h} m)")

    # Convert weight to kN/m for consistency
    w_kn = w / 1000.0

    if pretension is not None:
        # Given pretension T at top, solve for horizontal force H
        # T^2 = H^2 + (V_top)^2, V_top = w * s_suspended
        # h = (H/w) * [cosh(V/H) - 1]  where V = w*s
        # Iterative solution
        T = pretension
        # First estimate
        H = T * math.cos(math.radians(30.0))  # initial guess
        for _ in range(100):
            V = math.sqrt(max(0, T**2 - H**2))
            if V <= 0:
                break
            s_susp = V / w_kn
            h_calc = H / w_kn * (math.cosh(V / H) - 1.0) if H > 0 else 0
            err = h_calc - h
            # Newton-like step
            dh_dH = (1.0 / w_kn) * (math.cosh(V / H) - 1.0) + \
                     (H / w_kn) * (-V / H**2) * math.sinh(V / H) if H > 0 else 0
            if abs(dh_dH) > 1e-12:
                H = H - err / dh_dH
            H = max(1.0, H)
            if abs(err) < 0.01:
                break
    else:
        # No pretension given: solve for H that satisfies geometry
        # h = (H/w) * [cosh(w*s/H) - 1], horizontal scope = (H/w) * sinh(w*s/H)
        # s^2 = h^2 + 2*a*h  => a = (s^2 - h^2)/(2*h)  for no-grounded case first check
        s_max = line_length
        a = (s_max**2 - h**2) / (2.0 * h) if h > 0 else s_max
        H = a * w_kn

    # Compute solution
    a = H / w_kn if H > 0 else 1.0
    # Suspended length from catenary: s = a * sinh(x_h / a) where x_h satisfies depth
    # h = a*(cosh(x_h/a) - 1) => cosh(x_h/a) = 1 + h/a => x_h = a * acosh(1 + h/a)
    arg = 1.0 + h / a
    if arg < 1.0:
        arg = 1.0
    x_h = a * math.acosh(arg)
    s_suspended = a * math.sinh(x_h / a)
    grounded = max(0.0, line_length - s_suspended)

    V_top = w_kn * s_suspended
    T_top = math.sqrt(H**2 + V_top**2)
    top_angle = math.degrees(math.atan2(V_top, H))
    anchor_radius = x_h + grounded

    return CatenaryResult(
        horizontal_tension=round(H, 2),
        vertical_tension_top=round(V_top, 2),
        top_tension=round(T_top, 2),
        top_angle=round(top_angle, 2),
        suspended_length=round(s_suspended, 2),
        grounded_length=round(grounded, 2),
        anchor_radius=round(anchor_radius, 2),
        catenary_parameter=round(a, 2),
    )


# ---------------------------------------------------------------------------
# Mooring line sizing
# ---------------------------------------------------------------------------

class MooringLineSegment(BaseModel):
    """Single segment of a mooring line."""
    material_key: str = Field(..., description="Key in MOORING_MATERIAL_LIBRARY")
    length: float = Field(..., gt=0.0, description="Segment length (m)")


class MooringLineDesign(BaseModel):
    """Mooring line preliminary design input.

    Reference: API RP 2SK Section 5.
    """
    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    fairlead_depth: float = Field(10.0, ge=0.0, description="Fairlead depth below surface (m)")
    target_pretension: float = Field(1500.0, gt=0.0, description="Target pretension at fairlead (kN)")
    safety_factor_intact: float = Field(1.67, gt=1.0, description="FoS for intact condition (API RP 2SK)")
    safety_factor_damaged: float = Field(1.25, gt=1.0, description="FoS for damaged condition")
    max_offset_pct: float = Field(8.0, gt=0.0, description="Max vessel offset as % of water depth")
    segments: List[MooringLineSegment] = Field(
        default_factory=lambda: [
            MooringLineSegment(material_key="R4_84mm_chain", length=150.0),
            MooringLineSegment(material_key="160mm_polyester", length=1800.0),
            MooringLineSegment(material_key="R4_84mm_chain", length=300.0),
        ],
    )

    @property
    def total_length(self) -> float:
        """Total mooring line length (m)."""
        return sum(s.length for s in self.segments)

    def estimate_catenary(self) -> CatenaryResult:
        """Estimate catenary solution using average submerged weight."""
        total_length = self.total_length
        total_weight = 0.0
        for seg in self.segments:
            mat = MOORING_MATERIAL_LIBRARY.get(seg.material_key)
            if mat is None:
                raise ValueError(f"Material '{seg.material_key}' not found in library")
            total_weight += mat.submerged_weight * seg.length

        avg_w = total_weight / total_length if total_length > 0 else 100.0

        return solve_catenary(
            water_depth=self.water_depth,
            line_length=total_length,
            submerged_weight_per_m=avg_w,
            fairlead_depth=self.fairlead_depth,
            pretension=self.target_pretension,
        )

    def check_mbl(self, max_tension_kn: float) -> Dict[str, float]:
        """Check MBL utilisation for each segment.

        Args:
            max_tension_kn: Maximum line tension (kN).

        Returns:
            Dict of segment material -> utilisation (ratio of max tension / MBL).
        """
        results = {}
        for seg in self.segments:
            mat = MOORING_MATERIAL_LIBRARY[seg.material_key]
            utilisation = max_tension_kn / mat.mbl
            results[seg.material_key] = round(utilisation, 4)
        return results

    def _resolve_sf_for_condition(
        self,
        condition: str,
        *,
        repo_root_override: Optional[Path] = None,
    ) -> Tuple[float, Optional[Citation]]:
        """Return (safety_factor, citation_or_None) for DNV-OS-E301 pilot (#2685).

        Honors user override on self.safety_factor_intact / _damaged: if user's
        value differs from the field default, returns user value with the
        registry value cited as a 'user override' reference. Otherwise returns
        the registry value with its primary citation. Standalone mode (no
        knowledge/wikis/ tree) returns (user_value, None) plus a one-shot
        RuntimeWarning.
        """
        if condition == "intact":
            field_default = _INTACT_FIELD_DEFAULT
            registry_key = MooringCondition.INTACT_QUASI_STATIC
            user_value = self.safety_factor_intact
        elif condition == "damaged":
            field_default = _DAMAGED_FIELD_DEFAULT
            registry_key = MooringCondition.DAMAGED_QUASI_STATIC
            user_value = self.safety_factor_damaged
        else:
            raise ValueError(
                f"condition must be 'intact' or 'damaged', got {condition!r}"
            )

        user_overrode = user_value != field_default

        repo_root = _default_repo_root(repo_root_override)
        if repo_root is None:
            key = ("standalone_no_citation", condition)
            if key not in _REPO_ROOT_RESOLUTION_CACHE:
                warnings.warn(
                    "digitalmodel standalone mode: DNV-OS-E301 citation unavailable "
                    "(no knowledge/wikis/ tree found; set DIGITALMODEL_REPO_ROOT to "
                    f"enable). Calc proceeds with SF={user_value}.",
                    RuntimeWarning,
                    stacklevel=3,
                )
                _REPO_ROOT_RESOLUTION_CACHE[key] = True
            return user_value, None

        cited = get_mooring_safety_factor(registry_key, repo_root=repo_root)
        if user_overrode:
            ref_citation = Citation(
                code_id=cited.citation.code_id,
                publisher=cited.citation.publisher,
                revision=cited.citation.revision,
                section=cited.citation.section,
                wiki_path=cited.citation.wiki_path,
                note=(
                    f"user override (value={user_value}); "
                    f"registry reference value={cited.value}"
                ),
            )
            return user_value, ref_citation
        return cited.value, cited.citation

    def get_intact_safety_factor(
        self, *, repo_root: Optional[Path] = None
    ) -> CitedValue:
        """Return DNV-OS-E301 intact-condition SF as a CitedValue (#2685).

        Honors `safety_factor_intact` field override. Raises CitationResolutionError
        in standalone mode (no knowledge/wikis/ tree) — use
        check_mbl_with_safety_factor() if you need graceful degradation.
        """
        value, citation = self._resolve_sf_for_condition(
            "intact", repo_root_override=repo_root
        )
        if citation is None:
            raise CitationResolutionError(
                code_id="DNV-OS-E301",
                wiki_path="wikis/engineering/wiki/standards/dnv-os-e301.md",
                reason=(
                    "standalone_no_citation: set DIGITALMODEL_REPO_ROOT or use "
                    "check_mbl_with_safety_factor() which degrades gracefully"
                ),
            )
        return CitedValue(value=value, citation=citation, units="dimensionless")

    def get_damaged_safety_factor(
        self, *, repo_root: Optional[Path] = None
    ) -> CitedValue:
        """Return DNV-OS-E301 damaged-condition SF as a CitedValue (#2685)."""
        value, citation = self._resolve_sf_for_condition(
            "damaged", repo_root_override=repo_root
        )
        if citation is None:
            raise CitationResolutionError(
                code_id="DNV-OS-E301",
                wiki_path="wikis/engineering/wiki/standards/dnv-os-e301.md",
                reason=(
                    "standalone_no_citation: set DIGITALMODEL_REPO_ROOT or use "
                    "check_mbl_with_safety_factor() which degrades gracefully"
                ),
            )
        return CitedValue(value=value, citation=citation, units="dimensionless")

    def check_mbl_with_safety_factor(
        self,
        max_tension_kn: float,
        *,
        condition: str = "intact",
        repo_root: Optional[Path] = None,
    ) -> Dict:
        """Apply DNV-OS-E301 SF to MBL utilisation; emit citation sidecar (#2685).

        SEMANTIC DIFFERENCE FROM check_mbl():
          check_mbl():                       utilisation = max_tension / mbl
          check_mbl_with_safety_factor():    utilisation_with_sf = (max_tension * SF) / mbl

        A caller migrating from check_mbl() WILL see utilisation multiplied by
        1.67 (intact) or 1.25 (damaged). Loads at 0.60 utilisation under the
        legacy method land at ~1.002 (FAIL) under this one. Intentional per
        DNV-OS-E301 Section 2.2.3; callers MUST recalibrate utilisation
        thresholds before adopting.
        """
        if condition not in ("intact", "damaged"):
            raise ValueError(
                f"condition must be 'intact' or 'damaged', got {condition!r} "
                "(prevents typos like 'damagd' silently selecting wrong SF)"
            )
        sf, citation = self._resolve_sf_for_condition(
            condition, repo_root_override=repo_root
        )
        per_segment: Dict[str, Dict] = {}
        for seg in self.segments:
            mat = MOORING_MATERIAL_LIBRARY[seg.material_key]
            util_no_sf = max_tension_kn / mat.mbl
            util_with_sf = (max_tension_kn * sf) / mat.mbl
            per_segment[seg.material_key] = {
                "utilisation_no_sf": round(util_no_sf, 4),
                "utilisation_with_sf": round(util_with_sf, 4),
                "passes": util_with_sf < 1.0,
            }
        return {
            "results": per_segment,
            "safety_factor": sf,
            "condition": condition,
            "citations": [citation] if citation is not None else [],
        }


# ---------------------------------------------------------------------------
# Spread mooring layout
# ---------------------------------------------------------------------------

class SpreadMooringConfig(BaseModel):
    """Spread mooring system configuration.

    Reference: API RP 2SK Section 5, DNV-OS-E301.
    """
    pattern: MooringPattern = MooringPattern.SPREAD
    num_lines: int = Field(12, ge=3, description="Number of mooring lines")
    num_groups: int = Field(4, ge=1, description="Number of line groups (clusters)")
    group_angular_spread: float = Field(5.0, ge=0.0, description="Angular spread within a group (deg)")
    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    line_design: MooringLineDesign = Field(default_factory=MooringLineDesign)

    def generate_layout(self) -> List[Dict]:
        """Generate mooring line azimuths and anchor positions.

        Returns:
            List of dicts with azimuth, anchor_x, anchor_y for each line.
        """
        lines_per_group = self.num_lines // self.num_groups
        remainder = self.num_lines % self.num_groups
        group_base_azimuths = np.linspace(0, 360, self.num_groups, endpoint=False)

        layout = []
        for g, base_az in enumerate(group_base_azimuths):
            n = lines_per_group + (1 if g < remainder else 0)
            if n == 1:
                azimuths = [base_az]
            else:
                spread = np.linspace(-self.group_angular_spread * (n - 1) / 2,
                                     self.group_angular_spread * (n - 1) / 2, n)
                azimuths = [base_az + s for s in spread]

            for az in azimuths:
                az_norm = az % 360
                try:
                    cat = self.line_design.estimate_catenary()
                    radius = cat.anchor_radius
                except Exception:
                    radius = self.line_design.total_length * 0.8

                ax = radius * math.sin(math.radians(az_norm))
                ay = radius * math.cos(math.radians(az_norm))
                layout.append({
                    "azimuth_deg": round(az_norm, 2),
                    "anchor_x": round(ax, 1),
                    "anchor_y": round(ay, 1),
                    "anchor_radius": round(radius, 1),
                })

        return layout


class TurretMooringConfig(BaseModel):
    """Turret (single-point) mooring configuration.

    Reference: API RP 2SK Section 5.4.
    """
    pattern: MooringPattern = MooringPattern.TURRET
    num_lines: int = Field(12, ge=3, description="Number of mooring lines")
    turret_radius: float = Field(5.0, ge=0.0, description="Turret radius from vessel CG (m)")
    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    line_length: float = Field(2500.0, gt=0.0, description="Line length per line (m)")
    angular_spread: float = Field(30.0, ge=0.0, description="Total angular spread (deg)")

    def generate_layout(self) -> List[Dict]:
        """Generate turret mooring layout with even angular spacing."""
        azimuths = np.linspace(0, 360, self.num_lines, endpoint=False)
        layout = []
        for az in azimuths:
            # Simple estimate: assume taut -> radius ≈ sqrt(L^2 - d^2)
            if self.line_length > self.water_depth:
                radius = math.sqrt(self.line_length**2 - self.water_depth**2)
            else:
                radius = self.line_length * 0.5

            ax = radius * math.sin(math.radians(az))
            ay = radius * math.cos(math.radians(az))
            layout.append({
                "azimuth_deg": round(float(az), 2),
                "anchor_x": round(ax, 1),
                "anchor_y": round(ay, 1),
                "anchor_radius": round(radius, 1),
            })
        return layout


def estimate_line_length(
    water_depth: float,
    scope_ratio: float = 3.0,
    catenary: bool = True,
) -> float:
    """Quick estimate of mooring line length.

    Args:
        water_depth: Water depth (m).
        scope_ratio: Ratio of line length to water depth.
        catenary: If True, add 10% for catenary sag.

    Returns:
        Estimated line length (m).

    Reference: API RP 2SK Section 5 — scope ratios typically 3-5 for catenary.
    """
    length = water_depth * scope_ratio
    if catenary:
        length *= 1.10
    return round(length, 1)


def calculate_pretension(
    horizontal_tension: float,
    water_depth: float,
    submerged_weight_per_m: float,
    suspended_length: float,
) -> float:
    """Calculate pretension at fairlead from horizontal tension.

    T = sqrt(H^2 + (w*s)^2) where s is suspended length.

    Args:
        horizontal_tension: Horizontal tension H (kN).
        water_depth: Water depth (m).
        submerged_weight_per_m: Submerged weight (N/m).
        suspended_length: Suspended catenary length (m).

    Returns:
        Pretension at fairlead (kN).
    """
    w_kn = submerged_weight_per_m / 1000.0
    V = w_kn * suspended_length
    return math.sqrt(horizontal_tension**2 + V**2)
