#!/usr/bin/env python3
"""
Calculate derived engineering values for GTM demo data files.
All values use SI units unless noted.
"""
import math
import json

# Constants
RHO_STEEL = 7850.0      # kg/m3
RHO_SEAWATER = 1025.0   # kg/m3
RHO_COATING = 950.0     # kg/m3 (FBE/3LPP anti-corrosion coating)
RHO_CONCRETE = 3040.0   # kg/m3 (concrete weight coating)
G = 9.80665              # m/s2
E_STEEL = 207e9          # Pa (Young's modulus)
POISSON = 0.3

# Pipe sizes: nominal, OD in mm (API 5L)
PIPE_SIZES = {
    "6in":  {"od_mm": 168.3},
    "8in":  {"od_mm": 219.1},
    "10in": {"od_mm": 273.1},
    "12in": {"od_mm": 323.9},
    "16in": {"od_mm": 406.4},
    "20in": {"od_mm": 508.0},
    "24in": {"od_mm": 609.6},
}

# Wall thicknesses per size (typical offshore schedules, mm)
# These are representative of commonly used subsea pipeline wall thicknesses
WALL_THICKNESSES = {
    "6in":  [7.11, 10.97, 14.27, 18.26],       # Sch 40, 80, 120, 160 approx
    "8in":  [8.18, 12.70, 15.09, 18.26, 20.62], # Sch 40, 80, 100, 120, 140
    "10in": [9.27, 12.70, 15.09, 18.26, 21.44], 
    "12in": [9.53, 12.70, 17.48, 21.44, 25.40],
    "16in": [9.53, 12.70, 16.66, 21.44, 26.19],
    "20in": [9.53, 12.70, 15.09, 20.62, 26.19],
    "24in": [9.53, 12.70, 17.48, 24.61, 30.96],
}

# Anti-corrosion coating thickness (FBE/3LPP) in mm - varies by size
COATING_THICKNESS = {
    "6in": 3.0, "8in": 3.0, "10in": 4.0, "12in": 4.0,
    "16in": 5.0, "20in": 5.0, "24in": 5.0,
}

# Concrete weight coating thickness in mm (for larger sizes in deeper water)
# Only for 12" and above typically
CWC_THICKNESS = {
    "6in": 0, "8in": 0, "10in": 0, "12in": 40,
    "16in": 50, "20in": 60, "24in": 75,
}

# Grades
GRADES = {
    "X52": {"smys_mpa": 358, "smts_mpa": 455},
    "X60": {"smys_mpa": 413, "smts_mpa": 517},
    "X65": {"smys_mpa": 448, "smts_mpa": 531},
    "X70": {"smys_mpa": 482, "smts_mpa": 565},
}

def calc_pipe_properties(od_mm, wt_mm, coating_mm, cwc_mm):
    """Calculate mass per meter and submerged weight."""
    od_m = od_mm / 1000.0
    wt_m = wt_mm / 1000.0
    id_m = od_m - 2.0 * wt_m
    coat_m = coating_mm / 1000.0
    cwc_m = cwc_mm / 1000.0
    
    # Cross-sectional areas
    a_steel = math.pi / 4.0 * (od_m**2 - id_m**2)
    od_coated = od_m + 2.0 * coat_m
    a_coating = math.pi / 4.0 * (od_coated**2 - od_m**2)
    od_cwc = od_coated + 2.0 * cwc_m
    a_cwc = math.pi / 4.0 * (od_cwc**2 - od_coated**2)
    
    # Mass per meter
    mass_steel = a_steel * RHO_STEEL       # kg/m
    mass_coating = a_coating * RHO_COATING  # kg/m
    mass_cwc = a_cwc * RHO_CONCRETE         # kg/m
    mass_total = mass_steel + mass_coating + mass_cwc
    
    # Displaced water volume per meter (based on outermost diameter)
    if cwc_m > 0:
        outer_d = od_cwc
    else:
        outer_d = od_coated
    a_displaced = math.pi / 4.0 * outer_d**2
    mass_displaced = a_displaced * RHO_SEAWATER
    
    # Submerged weight per meter (N/m)
    w_sub = (mass_total - mass_displaced) * G
    
    # Second moment of area for steel
    i_steel = math.pi / 64.0 * (od_m**4 - id_m**4)
    
    return {
        "a_steel_m2": round(a_steel, 8),
        "id_m": round(id_m, 4),
        "mass_steel_kg_per_m": round(mass_steel, 2),
        "mass_coating_kg_per_m": round(mass_coating, 2),
        "mass_cwc_kg_per_m": round(mass_cwc, 2),
        "mass_total_kg_per_m": round(mass_total, 2),
        "submerged_weight_n_per_m": round(w_sub, 2),
        "outer_diameter_m": round(outer_d, 4),
        "i_steel_m4": i_steel,
    }

# ===== Calculate all pipe catalog entries =====
print("=" * 60)
print("PIPE CATALOG CALCULATIONS")
print("=" * 60)

catalog = {}
for size_name, size_data in PIPE_SIZES.items():
    od_mm = size_data["od_mm"]
    coat_mm = COATING_THICKNESS[size_name]
    cwc_mm = CWC_THICKNESS[size_name]
    wts = WALL_THICKNESSES[size_name]
    
    entries = []
    for wt_mm in wts:
        props = calc_pipe_properties(od_mm, wt_mm, coat_mm, cwc_mm)
        print(f"{size_name} OD={od_mm}mm WT={wt_mm}mm: "
              f"steel={props['mass_steel_kg_per_m']:.1f} kg/m, "
              f"total={props['mass_total_kg_per_m']:.1f} kg/m, "
              f"W_sub={props['submerged_weight_n_per_m']:.1f} N/m")
        entries.append({
            "wt_mm": wt_mm,
            "wt_m": round(wt_mm / 1000.0, 4),
            **props,
        })
    catalog[size_name] = {
        "od_mm": od_mm,
        "od_m": round(od_mm / 1000.0, 4),
        "coating_thickness_mm": coat_mm,
        "cwc_thickness_mm": cwc_mm,
        "wall_thicknesses": entries,
    }

# ===== Rigid jumper calculations =====
print("\n" + "=" * 60)
print("RIGID JUMPER CALCULATIONS")
print("=" * 60)

jumper_od_mm = 219.1
jumper_wt_mm = 18.26  # ~18.3mm, standard for 8" heavy wall
jumper_coat_mm = 3.0   # FBE coating
jumper_od_m = jumper_od_mm / 1000.0
jumper_wt_m = jumper_wt_mm / 1000.0
jumper_id_m = jumper_od_m - 2.0 * jumper_wt_m

a_steel_jumper = math.pi / 4.0 * (jumper_od_m**2 - jumper_id_m**2)
mass_steel_per_m = a_steel_jumper * RHO_STEEL

coat_m = jumper_coat_mm / 1000.0
od_coated = jumper_od_m + 2.0 * coat_m
a_coat = math.pi / 4.0 * (od_coated**2 - jumper_od_m**2)
mass_coat_per_m = a_coat * RHO_COATING

mass_total_per_m = mass_steel_per_m + mass_coat_per_m
a_disp = math.pi / 4.0 * od_coated**2
mass_disp_per_m = a_disp * RHO_SEAWATER
w_sub_per_m = (mass_total_per_m - mass_disp_per_m) * G

print(f"8in jumper: OD={jumper_od_mm}mm, WT={jumper_wt_mm}mm, ID={jumper_id_m*1000:.1f}mm")
print(f"  A_steel = {a_steel_jumper*1e4:.4f} cm2 = {a_steel_jumper:.6f} m2")
print(f"  Steel mass = {mass_steel_per_m:.2f} kg/m")
print(f"  Coating mass = {mass_coat_per_m:.2f} kg/m")
print(f"  Total mass = {mass_total_per_m:.2f} kg/m")
print(f"  Submerged weight = {w_sub_per_m:.2f} N/m")

jumper_lengths = [20, 40, 60, 80, 100]
jumpers = []
for length in jumper_lengths:
    total_mass = mass_total_per_m * length
    total_mass_sub = (mass_total_per_m - mass_disp_per_m) * length
    print(f"  Jumper-{length}m: total mass = {total_mass:.1f} kg ({total_mass/1000:.2f} te)")
    jumpers.append({
        "name": f"Jumper-{length}m",
        "length_m": length,
        "total_mass_air_kg": round(total_mass, 1),
        "total_mass_submerged_kg": round(total_mass_sub, 1),
    })

print(f"\nJumper mass_per_meter_air = {mass_total_per_m:.2f} kg/m")
print(f"Jumper mass_per_meter_sub = {mass_total_per_m - mass_disp_per_m:.2f} kg/m")
print(f"Jumper submerged_weight_per_m = {w_sub_per_m:.2f} N/m")

# Write results to a JSON for reference
results = {
    "catalog": catalog,
    "jumper_mass_per_m_air": round(mass_total_per_m, 2),
    "jumper_mass_per_m_sub": round(mass_total_per_m - mass_disp_per_m, 2),
    "jumper_sub_weight_per_m": round(w_sub_per_m, 2),
    "jumper_a_steel": round(a_steel_jumper, 8),
    "jumpers": jumpers,
}

with open("/mnt/local-analysis/workspace-hub/digitalmodel/examples/demos/gtm/calc_results.json", "w") as f:
    json.dump(results, f, indent=2)

print("\nCalculations complete. Results saved to calc_results.json")
