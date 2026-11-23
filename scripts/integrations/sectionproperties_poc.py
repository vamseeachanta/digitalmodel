"""Proof-of-concept: sectionproperties integration for structural cross-section analysis.

Demonstrates:
1. AISC W14x90 I-section with validation against AISC Steel Manual values
2. Custom hollow circular section (offshore tubular CHS)
3. Cross-section property computation (A, Ix, Iy, Zx, Sx, J, Cw)
4. Cross-section plot generation

Reference: vamseeachanta/workspace-hub#1452
"""

import json
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")  # headless backend for PNG output
import matplotlib.pyplot as plt

from sectionproperties.analysis import Section
from sectionproperties.pre import Material
from sectionproperties.pre.library import circular_hollow_section, i_section

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------

def compute_properties(geom, label, mesh_size):
    """Mesh, analyse, and return key section properties as a dict."""
    geom.create_mesh(mesh_sizes=[mesh_size])
    sec = Section(geom)
    sec.calculate_geometric_properties()
    sec.calculate_warping_properties()
    sec.calculate_plastic_properties()

    # get_ic() -> (ixx_c, iyy_c, ixy_c) about centroidal axes
    # get_z()  -> (sxx, sxx, syy, syy)  plastic section moduli (AISC "Z")
    # get_s()  -> (zxx+, zxx-)           elastic section moduli (AISC "S")
    ic = sec.get_ic()
    z_plastic = sec.get_z()   # (sxx+, sxx-, syy+, syy-)
    s_elastic = sec.get_s()   # (zxx+, zxx-)

    props = {
        "label": label,
        "A": sec.get_area(),       # mm^2
        "Ix": ic[0],              # mm^4 (Ixx about centroidal x-axis)
        "Iy": ic[1],              # mm^4 (Iyy about centroidal y-axis)
        "Zx": z_plastic[0],      # mm^3 (plastic section modulus, AISC Zx)
        "Sx": s_elastic[0],      # mm^3 (elastic section modulus, AISC Sx)
        "J": sec.get_j(),         # mm^4 (St. Venant torsion constant)
        "Cw": sec.get_gamma(),    # mm^6 (warping constant)
    }
    return sec, props


def print_props(props, unit_label="mm"):
    """Pretty-print a property dict."""
    print(f"\n{'=' * 60}")
    print(f"  {props['label']}")
    print(f"{'=' * 60}")
    print(f"  A   = {props['A']:>14.1f} {unit_label}^2")
    print(f"  Ix  = {props['Ix']:>14.1f} {unit_label}^4")
    print(f"  Iy  = {props['Iy']:>14.1f} {unit_label}^4")
    print(f"  Zx  = {props['Zx']:>14.1f} {unit_label}^3  (plastic)")
    print(f"  Sx  = {props['Sx']:>14.1f} {unit_label}^3  (elastic)")
    print(f"  J   = {props['J']:>14.1f} {unit_label}^4")
    print(f"  Cw  = {props['Cw']:>14.1f} {unit_label}^6")


def pct_diff(computed, reference):
    """Percentage difference."""
    if reference == 0:
        return float("inf")
    return abs(computed - reference) / reference * 100


# ---------------------------------------------------------------------------
# 1. AISC W14x90 (W360x134 metric)
# ---------------------------------------------------------------------------
# AISC Steel Construction Manual, 16th Ed. — W14x90
# Imperial values converted to metric (mm):
#   d=356 mm, bf=368 mm, tf=18.0 mm, tw=11.2 mm, r(k_det - tf)~25.4 mm
# Reference properties (imperial, from AISC manual):
#   A = 26.5 in^2, Ix = 999 in^4, Iy = 362 in^4
#   Zx = 157 in^3, Sx = 143 in^3, J = 4.06 in^4, Cw = 16000 in^6

# Conversion factors
IN2_TO_MM2 = 645.16
IN4_TO_MM4 = 416231.426
IN3_TO_MM3 = 16387.064
IN6_TO_MM6 = 16387.064**2  # ~2.685e8

AISC_W14X90_REF = {
    "A": 26.5 * IN2_TO_MM2,       # 17096.7 mm^2
    "Ix": 999 * IN4_TO_MM4,       # 415,815,234 mm^4
    "Iy": 362 * IN4_TO_MM4,       # 150,675,776 mm^4
    "Zx": 157 * IN3_TO_MM3,       # 2,572,769 mm^3
    "Sx": 143 * IN3_TO_MM3,       # 2,343,350 mm^3
    "J": 4.06 * IN4_TO_MM4,       # 1,689,899 mm^4
    "Cw": 16000 * IN6_TO_MM6,     # 4.296e12 mm^6
}

print("=" * 60)
print("  sectionproperties PoC — workspace-hub#1452")
print("=" * 60)

# W14x90 dimensions in mm
steel_mat = Material(
    name="A992 Steel",
    elastic_modulus=200e3,  # MPa
    poissons_ratio=0.3,
    yield_strength=345,     # MPa
    density=7850e-9,        # t/mm^3
    color="lightblue",
)

w14x90 = i_section(
    d=356.0,    # depth, mm
    b=368.0,    # flange width, mm
    t_f=18.0,   # flange thickness, mm
    t_w=11.2,   # web thickness, mm
    r=25.4,     # fillet radius, mm
    n_r=16,
)

sec_w, props_w = compute_properties(w14x90, "AISC W14x90 (W360x134)", mesh_size=10)
print_props(props_w)

# Validation against AISC reference
print(f"\n{'- ' * 30}")
print("  Validation vs AISC Manual (16th Ed.)")
print(f"{'- ' * 30}")
for key in ["A", "Ix", "Iy", "Zx", "Sx", "J", "Cw"]:
    ref = AISC_W14X90_REF[key]
    comp = props_w[key]
    diff = pct_diff(comp, ref)
    status = "OK" if diff < 5.0 else "CHECK"
    print(f"  {key:4s}: computed={comp:>14.1f}  ref={ref:>14.1f}  diff={diff:5.2f}%  [{status}]")

# ---------------------------------------------------------------------------
# 2. Offshore Tubular CHS — 914.4 mm OD x 25.4 mm WT (36" x 1")
# ---------------------------------------------------------------------------
chs_od = 914.4   # mm (36 inches)
chs_wt = 25.4    # mm (1 inch)

chs = circular_hollow_section(
    d=chs_od,
    t=chs_wt,
    n=64,
)

sec_c, props_c = compute_properties(chs, f"Offshore CHS {chs_od:.0f}x{chs_wt:.0f} mm", mesh_size=15)
print_props(props_c)

# Analytical CHS validation
import math

r_o = chs_od / 2
r_i = (chs_od - 2 * chs_wt) / 2
A_analytical = math.pi * (r_o**2 - r_i**2)
Ix_analytical = math.pi / 4 * (r_o**4 - r_i**4)
J_analytical = math.pi / 2 * (r_o**4 - r_i**4)

print(f"\n{'- ' * 30}")
print("  CHS Analytical Validation")
print(f"{'- ' * 30}")
for key, ref in [("A", A_analytical), ("Ix", Ix_analytical), ("J", J_analytical)]:
    comp = props_c[key]
    diff = pct_diff(comp, ref)
    status = "OK" if diff < 2.0 else "CHECK"
    print(f"  {key:4s}: computed={comp:>14.1f}  analytical={ref:>14.1f}  diff={diff:5.2f}%  [{status}]")

# ---------------------------------------------------------------------------
# 3. Generate cross-section plots
# ---------------------------------------------------------------------------
output_dir = Path(__file__).parent / "output"
output_dir.mkdir(exist_ok=True)

fig, axes = plt.subplots(1, 2, figsize=(14, 6))

# W14x90 mesh plot
sec_w.plot_mesh(ax=axes[0], title="W14x90 — FEM Mesh", materials=False)
# CHS mesh plot
sec_c.plot_mesh(ax=axes[1], title=f"CHS {chs_od:.0f}x{chs_wt:.0f} — FEM Mesh", materials=False)

plt.tight_layout()
plot_path = output_dir / "sectionproperties_poc_meshes.png"
fig.savefig(plot_path, dpi=150, bbox_inches="tight")
plt.close(fig)
print(f"\nPlot saved: {plot_path}")

# ---------------------------------------------------------------------------
# 4. Summary JSON for downstream consumption
# ---------------------------------------------------------------------------
summary = {
    "library": "sectionproperties",
    "version": "3.10.2",
    "sections_analysed": [props_w, props_c],
}
summary_path = output_dir / "sectionproperties_poc_results.json"
summary_path.write_text(json.dumps(summary, indent=2, default=str))
print(f"Results saved: {summary_path}")

print("\nPoC complete.")
