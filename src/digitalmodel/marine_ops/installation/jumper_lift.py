"""
Jumper Lift Analysis – Ballymore Manifold-to-PLET Jumper V2

Converts the engineering workbook
  "Jumper_Input_Ballymore_Manifold-PLET V2.xlsx"
into a self-contained Python calculation module.

Covers:
  • GA (General Arrangement) – pipe geometry, bend arcs, clamp positions,
    OrcaFlex section breakdown
  • Bare Pipe – steel / insulation cross-section, linear weight, connectors, clamps
  • Buoyancy module – displaced weight, hydrostatic OD, buoyancy density
  • Strake module – same methodology as buoyancy for vortex-suppression strakes
  • Rigging – sling stiffness, spreader-bar geometry, sling lengths
  • Crane Configuration – SWL, DDF, dynamic capacity for two cranes (SZ / DZ)
  • Weight Check – kit weights, centres of gravity, total lift weight

Author : auto-generated from Excel workbook
Units  : SI unless noted (inch inputs converted internally)
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Dict, List, Tuple

# ──────────────────────────────────────────────────────────────────────
#  UNIT-CONVERSION CONSTANTS
# ──────────────────────────────────────────────────────────────────────
INCH_TO_M: float = 0.0254          # 1 inch = 0.0254 m
LB_TO_KG: float = 0.453592         # 1 lb = 0.453592 kg
FT_TO_M: float = 0.3048            # 1 ft = 0.3048 m
LB_PER_FT3_TO_KG_PER_M3: float = 16.0185  # 1 lb/ft³ → kg/m³  (approx 16.0185)
SEAWATER_DENSITY_KG_M3: float = 1025.0  # seawater density, kg/m³
STEEL_DENSITY_KG_M3: float = 7850.0     # structural steel density, kg/m³
GRAVITY_M_S2: float = 9.81              # gravitational acceleration, m/s²




# ──────────────────────────────────────────────────────────────────────
#  JUMPER CONFIGURATIONS – Both Ballymore jumper models
# ──────────────────────────────────────────────────────────────────────

@dataclass
class JumperConfig:
    """Configuration for a specific Ballymore jumper model.

    Supports both MF-PLET (Manifold-to-PLET) and PLET-PLEM (PLET-to-PLEM)
    jumper analysis using the same calculation pipeline.

    Source: Corresponding Excel workbooks or spec.yml overrides.
    """
    # Identification
    name: str = "ballymore_mf_plet"
    description: str = "Ballymore Manifold-to-PLET Jumper"

    # Pipe properties (same for both jumpers – 10.75" OD jumper pipe)
    pipe_od_inch: float = 10.75
    pipe_wt_inch: float = 1.79
    pipe_insul_od_inch: float = 16.75
    pipe_insul_density_lb_ft3: float = 61.1
    pipe_bend_radius_inch: float = 50.0
    clamp_weight_kg: float = 260.0
    clamp_wll_te: float = 15.0
    connector_weight_kg: float = 1678.5    # 3357/2 – pair divided
    connector_length_m: float = 1.3

    # Segment lengths in inches (GA sheet C5:C11 for MF-PLET; PLET-PLEM differs)
    seg_a_inch: float = 336.0
    seg_b_inch: float = 160.0
    seg_c_inch: float = 525.0     # 160+365
    seg_d_inch: float = 1046.3    # 552.1+494.2
    seg_e_inch: float = 370.0     # 210+160
    seg_f_inch: float = 160.0
    seg_g_inch: float = 352.0

    # Buoyancy modules
    buoyancy_length_m: float = 1.016
    buoyancy_dry_lbs: float = 1287.0
    buoyancy_wet_lbs: float = -613.0
    buoyancy_od_drag_m: float = 1.5403
    num_buoy_modules_in_c: int = 2
    num_buoy_modules_in_d: int = 20    # 10+10
    num_buoy_modules_in_e: int = 2
    strake_length_m: float = 2.159
    strake_dry_lbs_base: float = 36.12
    strake_wet_lbs_base: float = 1.82
    strake_lbs_per_joint: float = 0.63
    strake_wt_lbs_per_joint: float = 0.55
    strake_od_drag_m: float = 0.653

    # Kit weights (as-built, from Weight Check sheet)
    kit1_wo_insul_kg: float = 7438.0
    kit2_wo_insul_kg: float = 6333.0
    kit3_wo_insul_kg: float = 4684.0
    kit4_wo_insul_kg: float = 8189.0
    kit1_wi_insul_kg: float = 8682.0
    kit2_wi_insul_kg: float = 8309.0
    kit3_wi_insul_kg: float = 6134.0
    kit4_wi_insul_kg: float = 8730.0

    # Rigging
    sling_mbl_te: float = 1200.0
    sling_elongation: float = 0.045
    spreader_bar_length_ft: float = 110.0

    # Crane configurations
    crane_sz_radius_m: float = 18.0
    crane_sz_swl_te: float = 77.5    # 70+30/4
    crane_dz_radius_m: float = 12.0
    crane_dz_swl_te: float = 100.0
    ddf: float = 1.3


KNOWN_JUMPER_CONFIGS: Dict[str, JumperConfig] = {
    "ballymore_mf_plet": JumperConfig(
        name="ballymore_mf_plet",
        description="Ballymore Manifold-to-PLET Jumper",
        # Segment lengths from Jumper_Input_Ballymore_Manifold-PLET V2.xlsx
        seg_a_inch=336.0,
        seg_b_inch=160.0,
        seg_c_inch=525.0,
        seg_d_inch=1046.3,
        seg_e_inch=370.0,
        seg_f_inch=160.0,
        seg_g_inch=352.0,
        # Kit weights specific to MF-PLET
        kit1_wo_insul_kg=7438.0,
        kit2_wo_insul_kg=6333.0,
        kit3_wo_insul_kg=4684.0,
        kit4_wo_insul_kg=8189.0,
        kit1_wi_insul_kg=8682.0,
        kit2_wi_insul_kg=8309.0,
        kit3_wi_insul_kg=6134.0,
        kit4_wi_insul_kg=8730.0,
    ),
    "ballymore_plet_plem": JumperConfig(
        name="ballymore_plet_plem",
        description="Ballymore PLET-to-PLEM Jumper",
        # PLET-PLEM segment lengths from SZ_Ballymore_Jumper_MF.xlsm
        # (same pipe, different jumper geometry and AHC offsets)
        # TODO: Verify these from the SZ_Ballymore_Jumper_MF.xlsm workbook
        # For now, use MF-PLET as baseline (requires workbook conversion)
        seg_a_inch=336.0,
        seg_b_inch=160.0,
        seg_c_inch=525.0,
        seg_d_inch=1046.3,
        seg_e_inch=370.0,
        seg_f_inch=160.0,
        seg_g_inch=352.0,
        # AHC offset for PLET-PLEM DZ configuration
        crane_sz_swl_te=77.5,
        crane_dz_swl_te=100.0,
        crane_sz_radius_m=18.0,
        crane_dz_radius_m=12.0,
    ),
}


# ──────────────────────────────────────────────────────────────────────
#  DATA-CLASSES – INPUT GROUPS
# ──────────────────────────────────────────────────────────────────────

@dataclass
class BarePipeProperties:
    """Jumper pipe cross-section properties.
    Source: Sheet 'Bare pipe', rows 4-10.
    """
    od_inch: float = 10.75              # Source: Bare pipe!C4 -- outer diameter
    wall_thickness_inch: float = 1.79   # Source: Bare pipe!C6 -- wall thickness
    bend_radius_inch: float = 50.0      # Source: Bare pipe!C7 -- bend radius
    insulation_od_inch: float = 16.75   # Source: Bare pipe!C8 -- OD with insulation
    insulation_density_lb_ft3: float = 61.1  # Source: Bare pipe!C10 -- insulation density

    # Derived (populated by compute)
    id_inch: float = field(init=False, default=0.0)
    insulation_thickness_inch: float = field(init=False, default=0.0)
    od_m: float = field(init=False, default=0.0)
    id_m: float = field(init=False, default=0.0)
    wall_thickness_m: float = field(init=False, default=0.0)
    bend_radius_m: float = field(init=False, default=0.0)
    insulation_od_m: float = field(init=False, default=0.0)
    insulation_thickness_m: float = field(init=False, default=0.0)
    insulation_density_te_m3: float = field(init=False, default=0.0)
    steel_linear_mass_kg_m: float = field(init=False, default=0.0)

    def __post_init__(self) -> None:
        # Source: Bare pipe!C5 -- ID = OD - 2*wall
        self.id_inch = self.od_inch - self.wall_thickness_inch * 2
        # Source: Bare pipe!C9 -- insulation thickness
        self.insulation_thickness_inch = (self.insulation_od_inch - self.od_inch) / 2

        # Metric conversions
        self.od_m = self.od_inch * INCH_TO_M                   # Bare pipe!E4
        self.id_m = self.id_inch * INCH_TO_M                   # Bare pipe!E5
        self.wall_thickness_m = self.wall_thickness_inch * INCH_TO_M  # Bare pipe!E6
        self.bend_radius_m = self.bend_radius_inch * INCH_TO_M       # Bare pipe!E7
        self.insulation_od_m = self.insulation_od_inch * INCH_TO_M    # Bare pipe!E8
        self.insulation_thickness_m = self.insulation_thickness_inch * INCH_TO_M  # Bare pipe!E9
        # Source: Bare pipe!E10 -- density conversion lb/ft³ → Te/m³
        self.insulation_density_te_m3 = (
            LB_PER_FT3_TO_KG_PER_M3 * self.insulation_density_lb_ft3 / 1000.0
        )
        # Source: Bare pipe!H4 -- steel linear mass  PI*(OD²-ID²)/4 * 7850
        self.steel_linear_mass_kg_m = (
            math.pi * (self.od_m ** 2 - self.id_m ** 2) / 4.0 * STEEL_DENSITY_KG_M3
        )


@dataclass
class ConnectorProperties:
    """Connector (hub) properties.
    Source: Sheet 'Bare pipe', rows 18-19.
    """
    weight_in_air_kg: float = 1678.5  # Source: Bare pipe!C18 = 3357/2
    length_m: float = 1.3             # Source: Bare pipe!C19


@dataclass
class ClampProperties:
    """Clamp properties.
    Source: Sheet 'Bare pipe', rows 13-15.
    """
    wll_te: float = 15.0      # Source: Bare pipe!C13 -- working load limit
    weight_kg: float = 260.0   # Source: Bare pipe!C14 -- weight in air


@dataclass
class PipeSectionLengths:
    """Raw pipe-section lengths (inches) from GA sheet.
    Source: Sheet 'GA', cells C5:C11.
    Sections A through G (7 segments between bends).
    """
    A_inch: float = 336.0       # GA!C5
    B_inch: float = 160.0       # GA!C6
    C_inch: float = 525.0       # GA!C7 = 160 + 365
    D_inch: float = 1046.3      # GA!C8 = 552.1 + 494.2
    E_inch: float = 370.0       # GA!C9 = 210 + 160
    F_inch: float = 160.0       # GA!C10
    G_inch: float = 352.0       # GA!C11


@dataclass
class BuoyancyModuleProperties:
    """Single buoyancy module properties.
    Source: Sheet 'Bouyancy', rows 4-9.
    """
    length_m: float = 1.016            # Bouyancy!C4
    id_m: float = 0.0                  # computed from insulation OD (16.75" → m)
    od_drag_m: float = 1.5403          # Bouyancy!C6
    od_contact_m: float = 1.5403       # Bouyancy!C7 = C6
    dry_weight_lbs: float = 1287.0     # Bouyancy!E8
    wet_weight_lbs: float = -613.0     # Bouyancy!E9

    # Derived
    dry_weight_kg: float = field(init=False, default=0.0)
    wet_weight_kg: float = field(init=False, default=0.0)
    displaced_kg: float = field(init=False, default=0.0)
    volume_m3: float = field(init=False, default=0.0)
    od_hydro_m: float = field(init=False, default=0.0)
    wt_hydro_m: float = field(init=False, default=0.0)
    density_te_m3: float = field(init=False, default=0.0)

    def __post_init__(self) -> None:
        # ID from insulation OD  Source: Bouyancy!C5 = 16.75*0.0254
        self.id_m = 16.75 * INCH_TO_M

        self.dry_weight_kg = LB_TO_KG * self.dry_weight_lbs       # Bouyancy!C8
        self.wet_weight_kg = LB_TO_KG * self.wet_weight_lbs       # Bouyancy!C9
        self.displaced_kg = self.dry_weight_kg - self.wet_weight_kg  # Bouyancy!C12
        self.volume_m3 = self.displaced_kg / SEAWATER_DENSITY_KG_M3  # Bouyancy!C13
        # Source: Bouyancy!C14 -- OD_hydro = sqrt(4*V/(PI*L) + ID²)
        self.od_hydro_m = math.sqrt(
            4.0 * self.volume_m3 / (math.pi * self.length_m) + self.id_m ** 2
        )
        # Source: Bouyancy!C15 -- wall thickness hydro
        self.wt_hydro_m = (self.od_hydro_m - self.id_m) / 2.0
        # Source: Bouyancy!C16 -- density = dry_weight / (volume * 1000)
        self.density_te_m3 = self.dry_weight_kg / self.volume_m3 / 1000.0


@dataclass
class StrakeProperties:
    """Single strake module properties.
    Source: Sheet 'Strake', rows 4-9.
    """
    length_m: float = 2.159            # Strake!C4
    id_m: float = 0.0                  # computed from insulation OD (16.75" → m)
    od_drag_m: float = 0.653           # Strake!C6
    od_contact_m: float = 0.653        # Strake!C7 = C6
    dry_weight_lbs_total: float = 38.01  # Strake!E8 = 36.12 + 0.63*3
    wet_weight_lbs_total: float = 3.47   # Strake!E9 = 1.82 + 0.55*3

    # Derived
    dry_weight_kg: float = field(init=False, default=0.0)
    wet_weight_kg: float = field(init=False, default=0.0)
    displaced_kg: float = field(init=False, default=0.0)
    volume_m3: float = field(init=False, default=0.0)
    od_hydro_m: float = field(init=False, default=0.0)
    wt_hydro_m: float = field(init=False, default=0.0)
    density_te_m3: float = field(init=False, default=0.0)

    def __post_init__(self) -> None:
        self.id_m = 16.75 * INCH_TO_M  # same as buoyancy module ID

        self.dry_weight_kg = LB_TO_KG * self.dry_weight_lbs_total    # Strake!C8
        self.wet_weight_kg = LB_TO_KG * self.wet_weight_lbs_total    # Strake!C9
        self.displaced_kg = self.dry_weight_kg - self.wet_weight_kg   # Strake!C12
        self.volume_m3 = self.displaced_kg / SEAWATER_DENSITY_KG_M3  # Strake!C13
        # Source: Strake!C14 -- same hydro OD formula
        self.od_hydro_m = math.sqrt(
            4.0 * self.volume_m3 / (math.pi * self.length_m) + self.id_m ** 2
        )
        self.wt_hydro_m = (self.od_hydro_m - self.id_m) / 2.0       # Strake!C15
        self.density_te_m3 = self.dry_weight_kg / self.volume_m3 / 1000.0  # Strake!C16


@dataclass
class RiggingProperties:
    """Rigging component properties.
    Source: Sheet 'Rigging'.
    """
    # Masterlink  Source: Rigging!B4, F4
    masterlink_wll_te: float = 98.0         # Rigging!F4

    # Fiber rope (Dyneema SK75, D=78mm)  Source: Rigging!B5, C5, E5, G5
    fiber_rope_length_m: float = 0.6096     # Rigging!C5 = 2 * 0.3048
    fiber_rope_mbl_te: float = 600.0        # Rigging!E5
    fiber_rope_max_dyn_te: float = 82.5     # Rigging!G5

    # Upper wire rope 2.25in  Source: Rigging!B6, D6, E6, G6
    upper_wire_od_m: float = 0.05715        # Rigging!D6 = 2.25*0.0254
    upper_wire_mbl_te: float = 224.0        # Rigging!E6
    upper_wire_max_dyn_te: float = 73.2     # Rigging!G6

    # Lower wire rope 1.5in  Source: Rigging!B7, D7, E7, G7
    lower_wire_od_m: float = 0.0381         # Rigging!D7 = 1.5*0.0254
    lower_wire_mbl_te: float = 103.0        # Rigging!E7
    lower_wire_max_dyn_te: float = 33.7     # Rigging!G7

    # Turnbuckle 2.5in*24  Source: Rigging!B8, F8, G8, H8
    turnbuckle_wll_te: float = 27.0         # Rigging!F8
    turnbuckle_max_dyn_te: float = 35.4     # Rigging!G8
    turnbuckle_weight_te_m: float = 0.0133831552  # Rigging!H8

    # Sling stiffness parameters  Source: Rigging!C12-C14
    sling_mbl_te: float = 1200.0            # Rigging!C12
    sling_elongation: float = 0.045         # Rigging!C13
    # sling_ea_kn computed below

    # Spreader bar  Source: Rigging!G21-H22, R50
    spreader_od_inch: float = 20.0          # Rigging!G21
    spreader_wt_inch: float = 1.281         # Rigging!G22
    spreader_total_length_ft: float = 110.0  # derived from Rigging!R50 = 110*0.3048

    # Crane wire  Source: Rigging!F56
    crane_wire_length_ft: float = 120.0     # Rigging!F56 = 120*0.3048

    # Sling geometry from Rigging!AA2-AA3
    sling_total_length_inch: float = 480.0   # Rigging!AA2 source = 480*0.0254
    sling_angle_deg: float = 50.0            # Rigging!AA3 uses cos(50°)

    # Hook block OD  Source: Rigging!AA8-AA9
    hook_block_length_inch: float = 630.0    # Rigging!AA8 = 630*0.0254
    hook_block_alt_inch: float = 612.0       # Rigging!AA9 = 612*0.0254


@dataclass
class CraneConfig:
    """Crane configuration.
    Source: Sheet 'Crane Configuration'.
    """
    name: str = ""
    radius_m: float = 0.0      # Crane Configuration!C2/D2
    swl_te: float = 0.0        # Crane Configuration!C3/D3
    ddf: float = 1.3           # Crane Configuration!C4/D4
    hook_height_m: float = 0.0  # Crane Configuration!C9/D9
    tip_height_m: float = 0.0   # Crane Configuration!C10/D10

    # Derived
    dynamic_capacity_te: float = field(init=False, default=0.0)

    def __post_init__(self) -> None:
        # Source: Crane Configuration!C5 = SWL * DDF
        self.dynamic_capacity_te = self.swl_te * self.ddf


@dataclass
class KitWeight:
    """Weight and COG data for a single jumper kit (without insulation).
    Source: Sheet 'Weight Check'.
    """
    name: str = ""
    weight_no_insulation_kg: float = 0.0  # Weight Check!C10-C13
    cog_x_m: float = 0.0                  # Weight Check!D10-D13
    cog_z_m: float = 0.0                  # Weight Check!E10-E13


@dataclass
class KitWeightInsulated:
    """Weight and COG data for a single jumper kit (with insulation).
    Source: Sheet 'Weight Check', rows 20-23.
    """
    name: str = ""
    weight_with_insulation_kg: float = 0.0  # Weight Check!C20-C23
    cog_x_m: float = 0.0                    # Weight Check!D20-D23
    cog_z_m: float = 0.0                    # Weight Check!E20-E23


# ──────────────────────────────────────────────────────────────────────
#  CALCULATION FUNCTIONS
# ──────────────────────────────────────────────────────────────────────

def compute_pipe_geometry(
    sections: PipeSectionLengths,
    bend_radius_inch: float = 50.0,
) -> Dict:
    """Compute straight lengths, bend arcs, clamp locations, and total length.

    Each pipe section has its end-bend deducted (bend_radius at each end
    for interior sections, one end for terminal sections A and G).

    The bend arc for a 90° bend is  π * bend_radius / 2.

    Source: Sheet 'GA', rows 16-41.

    Args:
        sections: raw section lengths in inches.
        bend_radius_inch: bend radius in inches (default 50, from Bare pipe!C7).

    Returns:
        dict with keys:
          straight_lengths_inch, straight_lengths_m,
          bend_arc_inch, bend_arc_m,
          total_length_inch, total_length_m,
          clamp_locations_inch, clamp_locations_m
    """
    R = bend_radius_inch  # Source: GA!C14 = Bare pipe!C7

    # Straight lengths after deducting bends  Source: GA!C18-C30
    # Section A: one bend deducted (start terminal)
    A_str = sections.A_inch - R                         # GA!C18
    # Sections B-F: two bends deducted (interior sections)
    B_str = sections.B_inch - R * 2                     # GA!C20
    C_str = sections.C_inch - R * 2                     # GA!C22
    D_str = sections.D_inch - R * 2                     # GA!C24
    E_str = sections.E_inch - R * 2                     # GA!C26
    F_str = sections.F_inch - R * 2                     # GA!C28
    # Section G: one bend deducted (end terminal)
    G_str = sections.G_inch - R                         # GA!C30

    straight_inch = [A_str, B_str, C_str, D_str, E_str, F_str, G_str]
    straight_m = [v * INCH_TO_M for v in straight_inch]

    # Bend arc (6 bends between 7 sections)  Source: GA!C19 = PI()*$C$14/2
    bend_arc_inch = math.pi * R / 2.0
    bend_arc_m = bend_arc_inch * INCH_TO_M

    # Total length (7 straights + 6 bends)  Source: GA!C32
    total_inch = sum(straight_inch) + 6 * bend_arc_inch
    total_m = total_inch * INCH_TO_M

    # Clamp locations (cumulative arc lengths)  Source: GA!C37-C41
    # Clamp 1: A_str + bend_arc + (62 - R)   Source: GA!C37
    clamp_1 = A_str + bend_arc_inch + (62.0 - R)
    # Clamp 2: sum(A_str + bend + B_str + bend + C_str + bend) + (62-50)  Source: GA!C38
    clamp_2 = A_str + bend_arc_inch + B_str + bend_arc_inch + C_str + bend_arc_inch + (62.0 - 50.0)
    # Clamp 3: clamp_2 + 461.15  Source: GA!C39
    clamp_3 = clamp_2 + 461.15
    # Clamp 4: =SUM(C18:C24)-(62-50)  Source: GA!C40
    # C18..C24 = A_str, bend, B_str, bend, C_str, bend, D_str  (4 straights + 3 bends)
    clamp_4 = (A_str + B_str + C_str + D_str + 3 * bend_arc_inch - (62.0 - 50.0))
    # Clamp 5: =SUM(C18:C28)-(62-50)  Source: GA!C41
    # C18..C28 = A through F straight with 5 interleaved bends (6 straights + 5 bends)
    clamp_5 = (A_str + B_str + C_str + D_str + E_str + F_str
               + 5 * bend_arc_inch - (62.0 - 50.0))

    clamp_inch = [clamp_1, clamp_2, clamp_3, clamp_4, clamp_5]
    clamp_m = [v * INCH_TO_M for v in clamp_inch]

    return {
        "straight_lengths_inch": straight_inch,
        "straight_lengths_m": straight_m,
        "bend_arc_inch": bend_arc_inch,
        "bend_arc_m": bend_arc_m,
        "total_length_inch": total_inch,
        "total_length_m": total_m,
        "clamp_locations_inch": clamp_inch,
        "clamp_locations_m": clamp_m,
    }


def compute_bare_pipe(props: BarePipeProperties | None = None) -> BarePipeProperties:
    """Instantiate and return computed bare-pipe properties.

    Source: Sheet 'Bare pipe'.

    Returns:
        BarePipeProperties with all derived fields populated.
    """
    if props is None:
        props = BarePipeProperties()
    return props


def compute_buoyancy(props: BuoyancyModuleProperties | None = None) -> BuoyancyModuleProperties:
    """Instantiate and return computed buoyancy-module properties.

    Methodology:
        displaced_mass = dry_weight - wet_weight  (net buoyancy reaction)
        volume = displaced_mass / seawater_density
        OD_hydro = sqrt( 4·V / (π·L) + ID² )  – equivalent OD for hydrostatic loading
        density_buoy = dry_weight / (volume × 1000)

    Source: Sheet 'Bouyancy'.

    Returns:
        BuoyancyModuleProperties with all derived fields populated.
    """
    if props is None:
        props = BuoyancyModuleProperties()
    return props

def compute_strake(props: StrakeProperties | None = None) -> StrakeProperties:
    """Instantiate and return computed strake properties.

    Same methodology as buoyancy module.
    Source: Sheet 'Strake'.

    Returns:
        StrakeProperties with all derived fields populated.
    """
    if props is None:
        props = StrakeProperties()
    return props


def compute_sling_stiffness(rigging: RiggingProperties | None = None) -> Dict:
    """Compute sling axial stiffness EA.

    EA = MBL / elongation × g
    Source: Rigging!C14 = C12 / C13 * 9.81

    Returns:
        dict with sling_ea_kn.
    """
    if rigging is None:
        rigging = RiggingProperties()
    ea_kn = rigging.sling_mbl_te / rigging.sling_elongation * GRAVITY_M_S2
    return {"sling_ea_kn": ea_kn}


def compute_spreader_bar(rigging: RiggingProperties | None = None) -> Dict:
    """Compute spreader-bar cross-section and geometry.

    Source: Rigging!G21-H23, R50, F56-F57.

    Returns:
        dict with od_m, wt_m, id_m, total_length_m, half_length_m,
              crane_wire_length_m, crane_wire_half_m, crane_wire_segment_m.
    """
    if rigging is None:
        rigging = RiggingProperties()

    od_m = rigging.spreader_od_inch * INCH_TO_M       # Rigging!H21
    wt_m = rigging.spreader_wt_inch * INCH_TO_M       # Rigging!H22
    id_m = od_m - wt_m * 2                             # Rigging!H23

    total_length_m = rigging.spreader_total_length_ft * FT_TO_M  # Rigging!R50
    crane_wire_length_m = rigging.crane_wire_length_ft * FT_TO_M  # Rigging!F56
    half_wire_m = crane_wire_length_m / 2.0                        # Rigging!F57
    segment_m = crane_wire_length_m / 20.0                         # Rigging!G56

    return {
        "od_m": od_m,
        "wt_m": wt_m,
        "id_m": id_m,
        "total_length_m": total_length_m,
        "crane_wire_length_m": crane_wire_length_m,
        "crane_wire_half_m": half_wire_m,
        "crane_wire_segment_m": segment_m,
    }


def compute_sling_geometry(rigging: RiggingProperties | None = None) -> Dict:
    """Compute sling total length and horizontal projection.

    Source: Rigging!AA2-AA3.
        sling_length = 480" → m
        horizontal_projection = sling_length × cos(50°)

    Returns:
        dict with sling_length_m, sling_horizontal_m.
    """
    if rigging is None:
        rigging = RiggingProperties()

    sling_length_m = rigging.sling_total_length_inch * INCH_TO_M  # Rigging!AA2
    horiz_m = sling_length_m * math.cos(math.radians(rigging.sling_angle_deg))  # Rigging!AA3

    return {"sling_length_m": sling_length_m, "sling_horizontal_m": horiz_m}


def compute_rigging_sling_lengths(rigging: RiggingProperties | None = None) -> Dict:
    """Compute individual sling segment lengths from spreader bar attachment points.

    The spreader bar is divided into 6 sling attachment segments.
    Source: Rigging!W51-W57 (inches), X51-X57 (metres), Z51-Z56 (offsets from centre).

    Returns:
        dict with segment_lengths_inch, segment_lengths_m, cumulative_m,
              offsets_from_centre_m, total_sling_length_m.
    """
    if rigging is None:
        rigging = RiggingProperties()

    # Source: Rigging!W51:W56
    segment_lengths_inch = [98.85, 306.15, 323.85, 306.15, 306.15, 168.85]
    segment_lengths_m = [v * INCH_TO_M for v in segment_lengths_inch]
    total_m = sum(segment_lengths_m)  # Rigging!X57

    # Cumulative positions  Source: Rigging!Y51:Y56
    cumulative_m: List[float] = []
    running = 0.0
    for s in segment_lengths_m:
        running += s
        cumulative_m.append(running)

    # Offsets from centre  Source: Rigging!Z51:Z56
    half_total = total_m / 2.0
    offsets_m = [c - half_total for c in cumulative_m]

    # Sling wire free length  Source: Rigging!W61
    sling_wire_segment_m = 1.82879999999999  # Rigging!W61
    # Total sling wire  Source: Rigging!X61 = W61 * 20
    total_sling_wire_m = sling_wire_segment_m * 20.0
    # Excess length  Source: Rigging!X63
    excess_m = total_m - total_sling_wire_m
    half_excess_m = excess_m / 2.0  # Rigging!X64

    return {
        "segment_lengths_inch": segment_lengths_inch,
        "segment_lengths_m": segment_lengths_m,
        "cumulative_m": cumulative_m,
        "offsets_from_centre_m": offsets_m,
        "total_sling_length_m": total_m,
        "sling_wire_segment_m": sling_wire_segment_m,
        "total_sling_wire_m": total_sling_wire_m,
        "excess_m": excess_m,
        "half_excess_m": half_excess_m,
    }


def compute_crane_configs() -> Dict[str, CraneConfig]:
    """Return crane configurations for the two cranes on the vessel.

    Source: Sheet 'Crane Configuration'.
        SZ: radius 18 m, SWL = 70 + 30/4 = 77.5 Te, DDF 1.3
        DZ: radius 12 m, SWL 100 Te, DDF 1.3

    Returns:
        dict mapping crane name → CraneConfig.
    """
    sz = CraneConfig(
        name="SZ",
        radius_m=18.0,          # Crane Configuration!C2
        swl_te=77.5,            # Crane Configuration!C3 = 70 + 30/4
        ddf=1.3,                # Crane Configuration!C4
        hook_height_m=33.0,     # Crane Configuration!C9
        tip_height_m=39.0,      # Crane Configuration!C10
    )
    dz = CraneConfig(
        name="DZ",
        radius_m=12.0,          # Crane Configuration!D2
        swl_te=100.0,           # Crane Configuration!D3
        ddf=1.3,                # Crane Configuration!D4
        hook_height_m=21.0,     # Crane Configuration!D9
        tip_height_m=27.0,      # Crane Configuration!D10
    )
    return {"SZ": sz, "DZ": dz}


def compute_crane_utilisation(crane: CraneConfig, lift_weight_te: float) -> float:
    """Compute crane utilisation as percentage of SWL.

    Source: Crane Configuration!C6 = 64 / C3  (example value; generic formula = load / SWL)

    Args:
        crane: crane configuration.
        lift_weight_te: total lift weight in tonnes.

    Returns:
        Utilisation as a fraction (0-1+).
    """
    if crane.swl_te == 0:
        return float("inf")
    return lift_weight_te / crane.swl_te


def compute_orcaflex_sections(
    pipe_geom: Dict,
    pipe_props: BarePipeProperties,
) -> List[Dict]:
    """Build the OrcaFlex line-type section breakdown.

    The jumper is split into segments of different line types:
      - bare pipe (with coating)
      - bare pipe with strake
      - bare pipe with buoyancy
    Connector sections bookend the jumper.

    Source: Sheet 'GA', rows 47-75.

    Args:
        pipe_geom: output of compute_pipe_geometry.
        pipe_props: BarePipeProperties (for bend_radius_m and insulation_od_m).

    Returns:
        List of dicts, each with keys: name, line_type, length_m.
    """
    R_m = pipe_props.bend_radius_m   # = E14 in GA
    insul_od_m = pipe_props.insulation_od_m * INCH_TO_M if pipe_props.insulation_od_m > 1 else pipe_props.insulation_od_m
    # insulation_od_m is already in metres from BarePipeProperties

    sm = pipe_geom["straight_lengths_m"]
    bm = pipe_geom["bend_arc_m"]

    # Connector length  Source: GA!D48
    connector_length_m = 1.3

    # Section-D sub-breakdown  Source: GA!D61-D65
    # D is split into: bare | buoy×10 | bare(1.168m) | buoy×10 | bare
    buoy_10_length = 1.016 * 10      # 10 buoyancy modules  GA!D62, D64
    d_bare_centre = 1.168             # GA!D63
    d_bare_each = (sm[3] - buoy_10_length - d_bare_centre - buoy_10_length) / 2.0  # GA!D61

    # Section C sub-breakdown  Source: GA!D53-D59
    c_bare_start = 2.311 - R_m                        # GA!D53 = 2.311 - E14
    c_strake_1 = 2.159                                 # GA!D54 (strake length)
    c_bare_mid = 5.652 - 2.311 - c_strake_1            # GA!D55
    c_buoy = 1.016 * 2                                 # GA!D56 = 2 buoy modules
    c_remaining = sm[2] - c_bare_start - c_strake_1 - c_bare_mid - c_buoy  # GA!D57 approx
    c_strake_2 = 2.159                                 # GA!D58 = D54
    c_bare_end = 2.311 - R_m                           # GA!D59 = 2.311 - E14

    # Recalculate c_remaining more precisely
    # total C straight = 10.795 m, known sub-lengths:
    # c_bare_start + c_strake_1 + c_bare_mid + c_buoy + c_remaining + c_strake_2 + c_bare_end = 10.795
    c_remaining = sm[2] - c_bare_start - c_strake_1 - c_bare_mid - c_buoy - c_strake_2 - c_bare_end

    # Section E sub-breakdown  Source: GA!D67-D69
    e_buoy = 1.016 * 2                                 # GA!D68
    e_bare_end = 3.683 - R_m                           # GA!D69
    e_bare_start = sm[4] - e_buoy - e_bare_end         # GA!D67

    sections = [
        {"name": "Connector-start", "line_type": "OCS 200-V", "length_m": connector_length_m},
        {"name": "A-straight", "line_type": "10.75\"Jumper_wCoat", "length_m": sm[0]},
        {"name": "A-B bend", "line_type": "10.75\"Jumper_wCoat", "length_m": bm},
        {"name": "B-straight", "line_type": "10.75\"Jumper_wCoat", "length_m": sm[1]},
        {"name": "B-C bend", "line_type": "10.75\"Jumper_wCoat", "length_m": bm},
        {"name": "C-bare-start", "line_type": "10.75\"Jumper_wCoat", "length_m": c_bare_start},
        {"name": "C-strake-1", "line_type": "10.75\"Jumper_wCoat_wStrake", "length_m": c_strake_1},
        {"name": "C-bare-mid", "line_type": "10.75\"Jumper_wCoat", "length_m": c_bare_mid},
        {"name": "C-buoy", "line_type": "10.75\"Jumper_wCoat_wBuoy", "length_m": c_buoy},
        {"name": "C-bare-remaining", "line_type": "10.75\"Jumper_wCoat", "length_m": c_remaining},
        {"name": "C-strake-2", "line_type": "10.75\"Jumper_wCoat_wStrake", "length_m": c_strake_2},
        {"name": "C-bare-end", "line_type": "10.75\"Jumper_wCoat", "length_m": c_bare_end},
        {"name": "C-D bend", "line_type": "10.75\"Jumper_wCoat", "length_m": bm},
        {"name": "D-bare-1", "line_type": "10.75\"Jumper_wCoat", "length_m": d_bare_each},
        {"name": "D-buoy-10a", "line_type": "10.75\"Jumper_wCoat_wBuoy", "length_m": buoy_10_length},
        {"name": "D-bare-centre", "line_type": "10.75\"Jumper_wCoat", "length_m": d_bare_centre},
        {"name": "D-buoy-10b", "line_type": "10.75\"Jumper_wCoat_wBuoy", "length_m": buoy_10_length},
        {"name": "D-bare-2", "line_type": "10.75\"Jumper_wCoat", "length_m": d_bare_each},
        {"name": "D-E bend", "line_type": "10.75\"Jumper_wCoat", "length_m": bm},
        {"name": "E-bare-start", "line_type": "10.75\"Jumper_wCoat", "length_m": e_bare_start},
        {"name": "E-buoy", "line_type": "10.75\"Jumper_wCoat_wBuoy", "length_m": e_buoy},
        {"name": "E-bare-end", "line_type": "10.75\"Jumper_wCoat", "length_m": e_bare_end},
        {"name": "E-F bend", "line_type": "10.75\"Jumper_wCoat", "length_m": bm},
        {"name": "F-straight", "line_type": "10.75\"Jumper_wCoat", "length_m": sm[5]},
        {"name": "F-G bend", "line_type": "10.75\"Jumper_wCoat", "length_m": bm},
        {"name": "G-straight", "line_type": "10.75\"Jumper_wCoat", "length_m": sm[6]},
        {"name": "Connector-end", "line_type": "OCS 200-V", "length_m": connector_length_m},
    ]

    return sections


def compute_weight_check(
    buoy: BuoyancyModuleProperties | None = None,
    strake: StrakeProperties | None = None,
    clamp: ClampProperties | None = None,
) -> Dict:
    """Compute total lift weight from insulated kit weights, buoyancy modules,
    strakes, and clamps.

    Source: Sheet 'Weight Check', rows 16-27.

    Kit weights WITH insulation (from Weight Check!C20-C23):
        KIT1 = 8682 kg,  KIT2 = 8309 kg,  KIT3 = 6134 kg,  KIT4 = 8730 kg

    Additional items:
        Buoys: 22 × buoyancy dry weight          (Weight Check!C24)
        Strakes: 2 × strake dry weight            (Weight Check!C25)
        Clamps: 5 × 260 kg                        (Weight Check!C26)

    Returns:
        dict with kit_weights, total_buoy_kg, total_strake_kg,
              total_clamp_kg, grand_total_kg.
    """
    if buoy is None:
        buoy = compute_buoyancy()
    if strake is None:
        strake = compute_strake()
    if clamp is None:
        clamp = ClampProperties()

    # Insulated kit weights  Source: Weight Check!C20-C23
    kit_weights_kg = {
        "KIT1": 8682.0,
        "KIT2": 8309.0,
        "KIT3": 6134.0,
        "KIT4": 8730.0,
    }

    # Source: Weight Check!C24 = 22 * Bouyancy!C8
    num_buoy_modules = 22
    total_buoy_kg = num_buoy_modules * buoy.dry_weight_kg

    # Source: Weight Check!C25 = 2 * Strake!C8
    num_strake_modules = 2
    total_strake_kg = num_strake_modules * strake.dry_weight_kg

    # Source: Weight Check!C26 = 260 * 5
    num_clamps = 5
    total_clamp_kg = num_clamps * clamp.weight_kg

    grand_total_kg = (
        sum(kit_weights_kg.values())
        + total_buoy_kg
        + total_strake_kg
        + total_clamp_kg
    )

    return {
        "kit_weights_kg": kit_weights_kg,
        "total_buoy_kg": total_buoy_kg,
        "total_strake_kg": total_strake_kg,
        "total_clamp_kg": total_clamp_kg,
        "grand_total_kg": grand_total_kg,
    }


def compute_weight_check_uninsulated() -> Dict:
    """Compute total uninsulated kit weights.

    Source: Sheet 'Weight Check', rows 6-14.
    Kit weights WITHOUT insulation (Weight Check!C10-C13):
        KIT1 = 7438 kg,  KIT2 = 6333 kg,  KIT3 = 4684 kg,  KIT4 = 8189 kg

    Returns:
        dict with kit_weights, total_kg.
    """
    kit_weights_kg = {
        "KIT1": 7438.0,
        "KIT2": 6333.0,
        "KIT3": 4684.0,
        "KIT4": 8189.0,
    }
    total_kg = sum(kit_weights_kg.values())
    return {"kit_weights_kg": kit_weights_kg, "total_kg": total_kg}


def compute_cog_uninsulated() -> Dict:
    """Centre-of-gravity data for uninsulated kits.

    Source: Sheet 'Weight Check', D10-E13.
    X-COG is computed relative to a reference point at 13.288 m:
        KIT1: -(13.288 - 8.18)  = -5.108
        KIT2: 4.554 - 13.288    = -8.734
        KIT3: 13.288 - 4.374    =  8.914
        KIT4: 13.288 - 0.807    = 12.481

    Returns:
        dict mapping kit name → (cog_x_m, cog_z_m).
    """
    REF_X = 13.288  # reference point, metres
    return {
        "KIT1": {"cog_x_m": -(REF_X - 8.18), "cog_z_m": 13.335 - (8.534 - 3.194)},
        "KIT2": {"cog_x_m": 4.554 - REF_X, "cog_z_m": 1.732},
        "KIT3": {"cog_x_m": REF_X - 4.374, "cog_z_m": 0.778},
        "KIT4": {"cog_x_m": REF_X - 0.807, "cog_z_m": 9.398 - (8.941 - 3.381)},
    }


def compute_cog_insulated() -> Dict:
    """Centre-of-gravity data for insulated kits.

    Source: Sheet 'Weight Check', D20-E23.

    Returns:
        dict mapping kit name → (cog_x_m, cog_z_m).
    """
    REF_X = 13.288
    return {
        "KIT1": {"cog_x_m": -(REF_X - 0.907), "cog_z_m": 13.335 - (8.534 - 3.636)},
        "KIT2": {"cog_x_m": 4.479 - REF_X, "cog_z_m": 1.707},
        "KIT3": {"cog_x_m": REF_X - 4.36, "cog_z_m": 0.744},
        "KIT4": {"cog_x_m": REF_X - 0.892, "cog_z_m": 9.398 - (8.941 - 3.839)},
    }


def compute_pipe_lengths_for_weight_estimate(
    pipe_props: BarePipeProperties | None = None,
) -> Dict:
    """Compute estimated pipe lengths for weight-estimation purposes.

    These are the effective lengths of pipe within each kit, used to
    estimate steel weight. Some kits include bend arcs.

    Source: Sheet 'Weight Check', F10-F12.

    Returns:
        dict with KIT lengths in metres.
    """
    if pipe_props is None:
        pipe_props = BarePipeProperties()
    R_m = pipe_props.bend_radius_m  # 1.27 m

    # Source: Weight Check!F10 -- KIT1 (straight, no bends)
    kit1_length_m = 15.572

    # Source: Weight Check!F11 = 9.271 + 14.023 - 2*1.27 + 0.5*PI()*1.27
    kit2_length_m = 9.271 + 14.023 - 2 * R_m + 0.5 * math.pi * R_m

    # Source: Weight Check!F12 = 5.334 + 12.553 - 2*1.27 + 0.5*PI()*1.27
    kit3_length_m = 5.334 + 12.553 - 2 * R_m + 0.5 * math.pi * R_m

    return {
        "KIT1_length_m": kit1_length_m,
        "KIT2_length_m": kit2_length_m,
        "KIT3_length_m": kit3_length_m,
    }


def compute_estimated_pipe_weight(
    pipe_props: BarePipeProperties | None = None,
) -> Dict:
    """Estimate pipe steel weight for each kit from pipe length × linear mass.

    Source: Sheet 'Weight Check', G10-G12.
        KIT1: F10 * D1 * 1000  (where D1 = 0.255176313780436 Te/m)
        KIT2: F11 * Bare pipe!H4
        KIT3: F12 * Bare pipe!H4

    The difference between D1 (0.2552 Te/m) and Bare pipe!H4/1000 (0.2552 Te/m)
    is negligible – D1 appears to be an older value for pipe without insulation.

    Returns:
        dict with estimated_pipe_weight_kg per kit.
    """
    if pipe_props is None:
        pipe_props = BarePipeProperties()

    lengths = compute_pipe_lengths_for_weight_estimate(pipe_props)
    linear_mass_kg_m = pipe_props.steel_linear_mass_kg_m  # Bare pipe!H4

    # Source: Weight Check!D1 -- pipe density without insulation (Te/m)
    PIPE_DENSITY_NO_INSUL_TE_M = 0.255176313780436

    # KIT1 uses the "without insulation" density  Source: Weight Check!G10
    kit1_kg = lengths["KIT1_length_m"] * PIPE_DENSITY_NO_INSUL_TE_M * 1000.0
    # KIT2, KIT3 use Bare pipe!H4 directly  Source: Weight Check!G11, G12
    kit2_kg = lengths["KIT2_length_m"] * linear_mass_kg_m
    kit3_kg = lengths["KIT3_length_m"] * linear_mass_kg_m

    return {
        "KIT1_pipe_weight_kg": kit1_kg,
        "KIT2_pipe_weight_kg": kit2_kg,
        "KIT3_pipe_weight_kg": kit3_kg,
    }


def compute_connector_buoy_totals(
    buoy: BuoyancyModuleProperties | None = None,
) -> Dict:
    """Compute total buoyancy module weight for all 24 modules.

    Source: Bouyancy!C22 = C8 * 24

    Returns:
        dict with total_buoy_24_kg.
    """
    if buoy is None:
        buoy = compute_buoyancy()
    return {"total_buoy_24_kg": buoy.dry_weight_kg * 24}




def generate_orcaflex_line_sections_yaml(
    pipe_geom: Dict | None = None,
    pipe_props: BarePipeProperties | None = None,
) -> str:
    """Convert OrcaFlex section breakdown to YAML format.

    Produces YAML compatible with digitalmodel spec.yml pattern
    for the modular model generator.

    Args:
        pipe_geom: output of compute_pipe_geometry (or None for defaults).
        pipe_props: BarePipeProperties (or None for defaults).

    Returns:
        YAML string defining all line sections.
    """
    if pipe_props is None:
        pipe_props = BarePipeProperties()
    if pipe_geom is None:
        sections_input = PipeSectionLengths()
        pipe_geom = compute_pipe_geometry(sections_input, pipe_props.bend_radius_inch)

    sections = compute_orcaflex_sections(pipe_geom, pipe_props)

    yaml_lines = ["line_sections:"]
    for i, sec in enumerate(sections):
        yaml_lines.append(f"  - name: \"{sec['name']}\"")
        yaml_lines.append(f"    line_type: \"{sec['line_type']}\"")
        yaml_lines.append(f"    length_m: {sec['length_m']:.6f}")
    return "\n".join(yaml_lines)

# ──────────────────────────────────────────────────────────────────────
#  PIPELINE – END-TO-END COMPUTATION
# ──────────────────────────────────────────────────────────────────────

def run_all() -> Dict:
    """Execute every calculation section and return consolidated results.

    This is the top-level entry point that mirrors running the entire
    workbook. Each section is independent except for cross-sheet
    references (bend_radius from Bare pipe → GA, buoyancy/strake
    dry weights → Weight Check).

    Returns:
        dict with keys for each calculation section.
    """
    pipe_props = compute_bare_pipe()
    buoy = compute_buoyancy()
    strake = compute_strake()
    clamp = ClampProperties()
    connector = ConnectorProperties()
    sections = PipeSectionLengths()

    pipe_geom = compute_pipe_geometry(sections, pipe_props.bend_radius_inch)
    orcaflex = compute_orcaflex_sections(pipe_geom, pipe_props)
    sling_stiffness = compute_sling_stiffness()
    spreader = compute_spreader_bar()
    sling_geom = compute_sling_geometry()
    sling_lengths = compute_rigging_sling_lengths()
    cranes = compute_crane_configs()
    weight_check = compute_weight_check(buoy, strake, clamp)
    weight_uninsulated = compute_weight_check_uninsulated()
    cog_uninsulated = compute_cog_uninsulated()
    cog_insulated = compute_cog_insulated()
    pipe_lengths = compute_pipe_lengths_for_weight_estimate(pipe_props)
    pipe_weights = compute_estimated_pipe_weight(pipe_props)
    buoy_totals = compute_connector_buoy_totals(buoy)

    return {
        "pipe_properties": pipe_props,
        "connector": connector,
        "clamp": clamp,
        "buoyancy_module": buoy,
        "strake_module": strake,
        "pipe_geometry": pipe_geom,
        "orcaflex_sections": orcaflex,
        "sling_stiffness": sling_stiffness,
        "spreader_bar": spreader,
        "sling_geometry": sling_geom,
        "sling_lengths": sling_lengths,
        "cranes": cranes,
        "weight_check_insulated": weight_check,
        "weight_check_uninsulated": weight_uninsulated,
        "cog_uninsulated": cog_uninsulated,
        "cog_insulated": cog_insulated,
        "pipe_lengths_for_weight": pipe_lengths,
        "estimated_pipe_weights": pipe_weights,
        "buoyancy_totals": buoy_totals,
    }




# ──────────────────────────────────────────────────────────────────────
#  JUMPER ANALYSIS PIPELINE – spec.yml → calculate → report
# ──────────────────────────────────────────────────────────────────────

def run_jumper_analysis(config: JumperConfig | None = None) -> Dict:
    """Execute the complete jumper analysis pipeline.

    Pipeline stages:
      1. spec.yml → JumperConfig (read by caller, passed in)
      2. JumperConfig → jumper_lift (compute_pipe_properties, etc.)
      3. Geometry → OrcaFlex section YAML (generate_orcaflex_line_sections_yaml)
      4. Weight tally → crane utilisation
      5. return consolidated results dict for downstream orcaflex model gen

    Args:
        config: JumperConfig for specific jumper. Defaults to ballymore_mf_plet.

    Returns:
        dict with all calculation results plus the config used.
    """
    if config is None:
        config = KNOWN_JUMPER_CONFIGS["ballymore_mf_plet"]

    # Pipe properties (stage 2: compute)
    pipe = compute_bare_pipe()
    buoy = compute_buoyancy()
    strake = compute_strake()
    clamp = ClampProperties()
    connector = ConnectorProperties()

    # Pipe geometry using config segment lengths
    sections = PipeSectionLengths(
        A_inch=config.seg_a_inch,
        B_inch=config.seg_b_inch,
        C_inch=config.seg_c_inch,
        D_inch=config.seg_d_inch,
        E_inch=config.seg_e_inch,
        F_inch=config.seg_f_inch,
        G_inch=config.seg_g_inch,
    )
    pipe_geom = compute_pipe_geometry(sections, pipe.bend_radius_inch)

    # OrcaFlex sections (stage 3)
    orcaflex = compute_orcaflex_sections(pipe_geom, pipe)
    orcaflex_yaml = generate_orcaflex_line_sections_yaml(pipe_geom, pipe)

    # Weight tally and crane
    weight_check = compute_weight_check(buoy, strake, clamp)
    weight_uninsulated = compute_weight_check_uninsulated()
    cog_uninsulated = compute_cog_uninsulated()
    cog_insulated = compute_cog_insulated()
    cranes = compute_crane_configs()

    return {
        "config": config,
        "pipe_properties": pipe,
        "buoyancy_module": buoy,
        "strake_module": strake,
        "pipe_geometry": pipe_geom,
        "orcaflex_sections": orcaflex,
        "orcaflex_sections_yaml": orcaflex_yaml,
        "weight_check": weight_check,
        "weight_uninsulated": weight_uninsulated,
        "cog_uninsulated": cog_uninsulated,
        "cog_insulated": cog_insulated,
        "cranes": cranes,
    }


if __name__ == "__main__":
    results = run_all()
    print("=== Jumper Lift Analysis — Ballymore Manifold-to-PLET ===")
    print()

    pp = results["pipe_properties"]
    print(f"Pipe OD:               {pp.od_inch}\" ({pp.od_m:.5f} m)")
    print(f"Pipe ID:               {pp.id_inch}\" ({pp.id_m:.6f} m)")
    print(f"Steel linear mass:     {pp.steel_linear_mass_kg_m:.6f} kg/m")
    print(f"Bend radius:           {pp.bend_radius_inch}\" ({pp.bend_radius_m:.3f} m)")

    pg = results["pipe_geometry"]
    print(f"\nTotal pipe length:     {pg['total_length_inch']:.6f}\" ({pg['total_length_m']:.6f} m)")
    print(f"Bend arc:              {pg['bend_arc_inch']:.6f}\" ({pg['bend_arc_m']:.6f} m)")

    buoy = results["buoyancy_module"]
    print(f"\nBuoyancy dry weight:   {buoy.dry_weight_kg:.6f} kg")
    print(f"Buoyancy OD hydro:     {buoy.od_hydro_m:.6f} m")
    print(f"Buoyancy density:      {buoy.density_te_m3:.6f} Te/m³")

    strk = results["strake_module"]
    print(f"\nStrake dry weight:     {strk.dry_weight_kg:.6f} kg")
    print(f"Strake OD hydro:       {strk.od_hydro_m:.6f} m")

    wc = results["weight_check_insulated"]
    print(f"\nTotal lift weight:     {wc['grand_total_kg']:.6f} kg")
    print(f"  Kits:                {sum(wc['kit_weights_kg'].values()):.0f} kg")
    print(f"  Buoys (22×):         {wc['total_buoy_kg']:.6f} kg")
    print(f"  Strakes (2×):        {wc['total_strake_kg']:.6f} kg")
    print(f"  Clamps (5×):         {wc['total_clamp_kg']:.0f} kg")

    for name, crane in results["cranes"].items():
        uc = compute_crane_utilisation(crane, wc["grand_total_kg"] / 1000.0)
        print(f"\nCrane {name}: SWL={crane.swl_te} Te, "
              f"Dynamic Cap={crane.dynamic_capacity_te:.1f} Te, "
              f"UC={uc:.4f}")
