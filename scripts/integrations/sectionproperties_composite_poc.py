# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Proof-of-concept: composite/multi-material section analysis with sectionproperties.

Demonstrates:
1. Case A — Concrete-Filled Tubular (CFT): steel CHS + concrete infill
2. Case B — Grouted Pile-Sleeve Connection: outer sleeve + grout annulus + inner pile
3. Case C — Steel-Concrete Composite Beam: W14x90 + concrete slab

Each case uses CompoundGeometry with Material objects and validates effective
stiffness against analytical (transformed-section) hand calculations.

Reference: vamseeachanta/workspace-hub#1499
Requires: sectionproperties >= 3.10, numpy, matplotlib
"""

import json
import math
from pathlib import Path

import matplotlib

matplotlib.use("Agg")  # headless backend — no display required
import matplotlib.pyplot as plt

from sectionproperties.analysis import Section
from sectionproperties.pre import Material
from sectionproperties.pre.geometry import CompoundGeometry
from sectionproperties.pre.library import (
    circular_hollow_section,
    circular_section,
    i_section,
    rectangular_section,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def pct_diff(computed, reference):
    """Percentage difference (absolute)."""
    if reference == 0:
        return float("inf")
    return abs(computed - reference) / reference * 100


def print_header(title):
    print(f"\n{'=' * 70}")
    print(f"  {title}")
    print(f"{'=' * 70}")


def print_validation_row(label, computed, analytical, tol=2.0):
    diff = pct_diff(computed, analytical)
    status = "OK" if diff < tol else "CHECK"
    print(f"  {label:12s}: computed={computed:>16.1f}  analytical={analytical:>16.1f}  diff={diff:5.2f}%  [{status}]")


# ---------------------------------------------------------------------------
# Materials
# ---------------------------------------------------------------------------

steel = Material(
    name="Steel",
    elastic_modulus=200_000,   # MPa
    poissons_ratio=0.3,
    yield_strength=345,        # MPa (Grade 50)
    density=7850e-9,           # t/mm^3
    color="steelblue",
)

concrete = Material(
    name="Concrete",
    elastic_modulus=30_000,    # MPa (f'c ~ 40 MPa)
    poissons_ratio=0.2,
    yield_strength=40,         # MPa (compressive)
    density=2400e-9,           # t/mm^3
    color="lightgrey",
)

grout = Material(
    name="Grout",
    elastic_modulus=20_000,    # MPa
    poissons_ratio=0.2,
    yield_strength=50,         # MPa
    density=2200e-9,           # t/mm^3
    color="khaki",
)

def run_cft_case(
    *,
    outer_diameter: float,
    wall_thickness: float,
    steel_material: Material,
    concrete_material: Material,
    output_dir: Path | str,
    n: int,
    mesh_sizes: tuple[float, float],
    calculate_warping: bool = False,
) -> tuple[Section, dict]:
    """Analyse a concentric CFT and save its results in the supplied directory.

    Dimensions are mm, elastic moduli MPa, and mesh sizes maximum triangle
    areas in mm^2 (steel, concrete). ``n`` is the polygon side count on both
    circular boundaries. Perfect bond and linear elastic materials are assumed.
    EA is in N; EI is in N mm^2; steel-transformed I = EI/E_s is in mm^4.
    Geometry alone gives EA and EI; warping is optional for the original EJ
    demonstration. Legacy ``EIxx_eff_mm4`` denotes transformed I, despite its name.
    """
    if not 0 < wall_thickness < outer_diameter / 2:
        raise ValueError("Require 0 < wall_thickness < outer_diameter / 2")

    print_header("CASE A: Concrete-Filled Tubular (CFT)")
    print(f"  Steel CHS: OD={outer_diameter} mm, WT={wall_thickness} mm")
    print(f"  Concrete infill: E={concrete_material.elastic_modulus} MPa")

    inner_diameter = outer_diameter - 2 * wall_thickness
    chs_geom = circular_hollow_section(
        d=outer_diameter, t=wall_thickness, n=n, material=steel_material
    )
    infill_geom = circular_section(
        d=inner_diameter, n=n, material=concrete_material
    )
    compound = CompoundGeometry([chs_geom, infill_geom])
    compound.create_mesh(mesh_sizes=list(mesh_sizes))
    section = Section(geometry=compound)
    section.calculate_geometric_properties()

    ea = section.get_ea()
    eixx, eiyy, _ = section.get_eic()
    ixx, iyy, _ = section.get_eic(e_ref=steel_material)
    area = section.get_area()
    print(f"\n  Geometric area   = {area:>14.1f} mm^2")
    print(f"  EA (axial)       = {ea:>14.1f} N")
    print(f"  EI_xx            = {eixx:>14.1f} N mm^2")
    print(f"  I_xx (ref steel) = {ixx:>14.1f} mm^4")
    print(f"  I_yy (ref steel) = {iyy:>14.1f} mm^4")

    # Independent circle integrals: A = integral(r dr dphi) = pi R^2;
    # I_x = integral(r^3 sin(phi)^2 dr dphi) = pi R^4/4.
    # Subtract concentric disks for the annulus, then sum each E-weighted term.
    r_o = outer_diameter / 2
    r_i = inner_diameter / 2
    a_steel = math.pi * (r_o**2 - r_i**2)
    a_concrete = math.pi * r_i**2
    i_steel = math.pi / 4 * (r_o**4 - r_i**4)
    i_concrete = math.pi / 4 * r_i**4
    ea_analytical = (
        steel_material.elastic_modulus * a_steel
        + concrete_material.elastic_modulus * a_concrete
    )
    ei_analytical = (
        steel_material.elastic_modulus * i_steel
        + concrete_material.elastic_modulus * i_concrete
    )
    i_transformed = ei_analytical / steel_material.elastic_modulus
    print("\n  --- Analytical Validation (Case A) ---")
    print_validation_row("A_total", area, a_steel + a_concrete)
    print_validation_row("EA", ea, ea_analytical)
    print_validation_row("I_xx (steel)", ixx, i_transformed)

    results = {
        "case": "A - CFT",
        "geometry": f"CHS OD={outer_diameter} WT={wall_thickness} + concrete infill",
        "A_total_mm2": area,
        "EA_N": ea,
        "EIxx_Nmm2": eixx,
        "EIyy_Nmm2": eiyy,
        "Ixx_steel_mm4": ixx,
        "Iyy_steel_mm4": iyy,
        "EIxx_eff_mm4": ixx,  # Retained for existing summary consumers.
        "validation": {
            "A_total_diff_pct": round(pct_diff(area, a_steel + a_concrete), 1),
            "EA_diff_pct": round(pct_diff(ea, ea_analytical), 1),
            "EI_diff_pct": round(pct_diff(ixx, i_transformed), 1),
        },
    }
    if calculate_warping:
        section.calculate_warping_properties()
        ej = section.get_ej(e_ref=steel_material)
        # Circular polar integral J = I_x + I_y = 2 I_x.
        print(f"  EJ (eff)         = {ej:>14.1f} mm^4 (ref: steel)")
        print_validation_row("EJ (eff)", ej, 2 * i_transformed)
        results["EJ_eff_mm4"] = ej

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "sectionproperties_cft_results.json").write_text(
        json.dumps(results, indent=2)
    )
    return section, results


def main(output_dir: Path | str) -> None:
    """Run the original three demonstrations with explicit output placement."""
    output_dir = Path(output_dir)
    # Original illustrative Case A inputs, assumed, based on previous experience.
    cft_sec, cft_results = run_cft_case(
        outer_diameter=508.0,
        wall_thickness=25.4,
        steel_material=steel,
        concrete_material=concrete,
        n=64,
        mesh_sizes=(200, 400),
        output_dir=output_dir,
        calculate_warping=True,
    )

    # ===================================================================
    # CASE B: Grouted Pile-Sleeve Connection
    # ===================================================================
    # Outer sleeve: OD=1200 mm, WT=30 mm
    # Grout annulus: fills gap between sleeve ID and pile OD
    # Inner pile:   OD=1000 mm, WT=25 mm

    print_header("CASE B: Grouted Pile-Sleeve Connection")

    sleeve_od = 1200.0   # mm
    sleeve_wt = 30.0     # mm
    sleeve_id = sleeve_od - 2 * sleeve_wt  # 1140 mm

    pile_od = 1000.0     # mm
    pile_wt = 25.0       # mm
    pile_id = pile_od - 2 * pile_wt        # 950 mm

    grout_od = sleeve_id  # 1140 mm (fills from sleeve ID to pile OD)
    grout_id = pile_od    # 1000 mm

    print(f"  Sleeve: OD={sleeve_od} mm, WT={sleeve_wt} mm")
    print(f"  Grout:  OD={grout_od} mm, ID={grout_id} mm (annulus)")
    print(f"  Pile:   OD={pile_od} mm, WT={pile_wt} mm")

    # Build geometries
    sleeve_geom = circular_hollow_section(d=sleeve_od, t=sleeve_wt, n=64, material=steel)
    grout_geom = circular_hollow_section(d=grout_od, t=(grout_od - grout_id) / 2, n=64, material=grout)
    pile_geom = circular_hollow_section(d=pile_od, t=pile_wt, n=64, material=steel)

    # Combine
    gpsc_compound = CompoundGeometry([sleeve_geom, grout_geom, pile_geom])
    gpsc_compound.create_mesh(mesh_sizes=[500, 800, 500])

    # Analyse
    gpsc_sec = Section(geometry=gpsc_compound)
    gpsc_sec.calculate_geometric_properties()
    gpsc_sec.calculate_warping_properties()

    # Extract effective properties
    gpsc_ea = gpsc_sec.get_ea()
    gpsc_eixx, gpsc_eiyy, gpsc_eixy = gpsc_sec.get_eic(e_ref=steel)
    gpsc_ej = gpsc_sec.get_ej(e_ref=steel)
    gpsc_area = gpsc_sec.get_area()

    print(f"\n  Geometric area   = {gpsc_area:>14.1f} mm^2")
    print(f"  EA (axial)       = {gpsc_ea:>14.1f} N")
    print(f"  EI_xx (eff)      = {gpsc_eixx:>14.1f} mm^4 (ref: steel)")
    print(f"  EJ (eff)         = {gpsc_ej:>14.1f} mm^4 (ref: steel)")

    # --- Analytical validation ---
    r_sleeve_o = sleeve_od / 2
    r_sleeve_i = sleeve_id / 2
    r_grout_o = grout_od / 2
    r_grout_i = grout_id / 2
    r_pile_o = pile_od / 2
    r_pile_i = pile_id / 2

    A_sleeve = math.pi * (r_sleeve_o**2 - r_sleeve_i**2)
    A_grout = math.pi * (r_grout_o**2 - r_grout_i**2)
    A_pile = math.pi * (r_pile_o**2 - r_pile_i**2)

    I_sleeve = math.pi / 4 * (r_sleeve_o**4 - r_sleeve_i**4)
    I_grout = math.pi / 4 * (r_grout_o**4 - r_grout_i**4)
    I_pile = math.pi / 4 * (r_pile_o**4 - r_pile_i**4)

    J_sleeve = math.pi / 2 * (r_sleeve_o**4 - r_sleeve_i**4)
    J_grout = math.pi / 2 * (r_grout_o**4 - r_grout_i**4)
    J_pile = math.pi / 2 * (r_pile_o**4 - r_pile_i**4)

    EA_b = steel.elastic_modulus * (A_sleeve + A_pile) + grout.elastic_modulus * A_grout
    EI_b = steel.elastic_modulus * (I_sleeve + I_pile) + grout.elastic_modulus * I_grout
    EJ_b = steel.elastic_modulus * (J_sleeve + J_pile) + grout.elastic_modulus * J_grout

    EI_eff_b = EI_b / steel.elastic_modulus
    EJ_eff_b = EJ_b / steel.elastic_modulus

    A_total_b = A_sleeve + A_grout + A_pile

    print(f"\n  --- Analytical Validation (Case B) ---")
    print_validation_row("A_total", gpsc_area, A_total_b)
    print_validation_row("EA", gpsc_ea, EA_b)
    print_validation_row("EI_xx (eff)", gpsc_eixx, EI_eff_b)
    print_validation_row("EJ (eff)", gpsc_ej, EJ_eff_b)

    gpsc_results = {
        "case": "B - Grouted Pile-Sleeve",
        "geometry": f"Sleeve OD={sleeve_od} WT={sleeve_wt} + Grout + Pile OD={pile_od} WT={pile_wt}",
        "A_total_mm2": gpsc_area,
        "EA_N": gpsc_ea,
        "EIxx_eff_mm4": gpsc_eixx,
        "EJ_eff_mm4": gpsc_ej,
        "validation": {
            "A_total_diff_pct": round(pct_diff(gpsc_area, A_total_b), 3),
            "EA_diff_pct": round(pct_diff(gpsc_ea, EA_b), 3),
            "EI_diff_pct": round(pct_diff(gpsc_eixx, EI_eff_b), 3),
        },
    }

    # ===================================================================
    # CASE C: Steel-Concrete Composite Beam (W14x90 + slab)
    # ===================================================================
    # W14x90: d=356, b=368, tf=18, tw=11.2, r=25.4
    # Concrete slab: 150 mm thick x 1500 mm wide, placed on top flange

    print_header("CASE C: Steel-Concrete Composite Beam")
    print("  W14x90 steel beam + 150 mm x 1500 mm concrete slab")

    # W14x90 steel section
    w_depth = 356.0
    w_bf = 368.0
    w_tf = 18.0
    w_tw = 11.2
    w_r = 25.4

    slab_width = 1500.0
    slab_thick = 150.0

    w14x90_geom = i_section(
        d=w_depth,
        b=w_bf,
        t_f=w_tf,
        t_w=w_tw,
        r=w_r,
        n_r=16,
        material=steel,
    )

    # Concrete slab — positioned on top of the steel beam
    # The I-section centroid is at (0, d/2) by default in sectionproperties.
    # The slab sits on top of the beam, so its bottom edge is at y = d (top of beam).
    slab_geom = rectangular_section(d=slab_thick, b=slab_width, material=concrete)

    # Align slab: shift so its bottom-left corner is at (-(slab_width - w_bf)/2, w_depth)
    # i_section places bottom-left at origin, so top flange top is at y = w_depth
    slab_geom = slab_geom.shift_section(
        x_offset=-(slab_width - w_bf) / 2,  # center slab on beam
        y_offset=w_depth,                     # place on top of beam
    )

    composite_beam = CompoundGeometry([w14x90_geom, slab_geom])
    composite_beam.create_mesh(mesh_sizes=[20, 200])

    # Analyse
    beam_sec = Section(geometry=composite_beam)
    beam_sec.calculate_geometric_properties()
    beam_sec.calculate_warping_properties()

    beam_ea = beam_sec.get_ea()
    beam_eixx, beam_eiyy, beam_eixy = beam_sec.get_eic(e_ref=steel)
    beam_area = beam_sec.get_area()

    print(f"\n  Geometric area   = {beam_area:>14.1f} mm^2")
    print(f"  EA (axial)       = {beam_ea:>14.1f} N")
    print(f"  EI_xx (eff)      = {beam_eixx:>14.1f} mm^4 (ref: steel)")
    print(f"  EI_yy (eff)      = {beam_eiyy:>14.1f} mm^4 (ref: steel)")

    # --- Analytical validation (transformed section method) ---
    # Steel area
    A_w14 = w_depth * w_tw + 2 * w_bf * w_tf  # approximate (ignores fillets)
    # More accurate: use sectionproperties for steel-only
    A_slab = slab_width * slab_thick
    n_ratio = concrete.elastic_modulus / steel.elastic_modulus  # modular ratio

    # Steel centroid at y = d/2 from bottom
    y_steel = w_depth / 2
    # Slab centroid at y = d + slab_thick/2
    y_slab = w_depth + slab_thick / 2

    # Transformed steel area of slab
    A_slab_transformed = n_ratio * A_slab

    # Composite centroid (transformed section, everything in steel units)
    y_bar = (A_w14 * y_steel + A_slab_transformed * y_slab) / (A_w14 + A_slab_transformed)

    # Steel I about own centroid (approximate)
    I_steel_own = (w_tw * (w_depth - 2 * w_tf)**3 / 12
                   + 2 * (w_bf * w_tf**3 / 12 + w_bf * w_tf * ((w_depth - w_tf) / 2)**2))
    # Slab I about own centroid
    I_slab_own = slab_width * slab_thick**3 / 12

    # Parallel axis to composite centroid
    I_steel_comp = I_steel_own + A_w14 * (y_steel - y_bar)**2
    I_slab_comp = I_slab_own + A_slab * (y_slab - y_bar)**2

    # Effective EI about composite centroid
    EI_comp_analytical = steel.elastic_modulus * I_steel_comp + concrete.elastic_modulus * I_slab_comp
    EI_eff_comp_analytical = EI_comp_analytical / steel.elastic_modulus

    print(f"\n  --- Analytical Validation (Case C, approximate) ---")
    print(f"  Note: analytical uses simplified W-shape (no fillets)")
    print_validation_row("EI_xx (eff)", beam_eixx, EI_eff_comp_analytical, tol=5.0)

    beam_results = {
        "case": "C - Composite Beam",
        "geometry": "W14x90 + 150x1500 concrete slab",
        "A_total_mm2": beam_area,
        "EA_N": beam_ea,
        "EIxx_eff_mm4": beam_eixx,
        "EIyy_eff_mm4": beam_eiyy,
        "validation": {
            "EI_diff_pct": round(pct_diff(beam_eixx, EI_eff_comp_analytical), 3),
            "note": "approximate — analytical ignores fillets",
        },
    }

    # ===================================================================
    # Generate cross-section mesh plots
    # ===================================================================
    print_header("Generating Plots")

    fig, axes = plt.subplots(1, 3, figsize=(20, 7))

    cft_sec.plot_mesh(ax=axes[0], title="Case A: CFT\n508x25.4 CHS + Concrete", materials=True)
    gpsc_sec.plot_mesh(ax=axes[1], title="Case B: Grouted Pile-Sleeve\n1200/1000 + Grout", materials=True)
    beam_sec.plot_mesh(ax=axes[2], title="Case C: Composite Beam\nW14x90 + Concrete Slab", materials=True)

    plt.tight_layout()
    output_dir.mkdir(exist_ok=True)
    plot_path = output_dir / "sectionproperties_composite_meshes.png"
    fig.savefig(plot_path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  Plot saved: {plot_path}")

    # ===================================================================
    # Summary JSON
    # ===================================================================
    summary = {
        "library": "sectionproperties",
        "version": "3.10.2",
        "reference": "workspace-hub#1499",
        "cases": [cft_results, gpsc_results, beam_results],
    }
    summary_path = output_dir / "sectionproperties_composite_results.json"
    summary_path.write_text(json.dumps(summary, indent=2, default=str))
    print(f"  Results saved: {summary_path}")

    print("\nComposite section PoC complete.")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    main(parser.parse_args().output_dir)
