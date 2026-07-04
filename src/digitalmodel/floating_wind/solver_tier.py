"""High-fidelity solver tier for floating-wind concept screening (issue #1025).

For variants that survive the closed-form screen (issues #1024/#1026), this tier
generates the **OrcaWave diffraction** and **OrcaFlex motion/mooring** inputs per
variant and dispatches them, fail-closed, through the existing licensed-run lane.
The solve itself is the licensed step; this module delivers the *generators*, the
*dispatch wiring* and the *result-fold* and is fully testable offline (inputs are
``prepared`` -- no OrcaWave/OrcaFlex licence needed to generate or assert them).

Pipeline per shortlisted variant
---------------------------------
1. ``build_panel_mesh`` -- an analytic WAMIT ``.gdf`` mesh for the floater hull
   (box for a barge, vertical cylinder for a spar; column-stabilised hulls
   reference a supplied mesh, consistent with the registry's spar/TLP note).
2. ``build_orcawave_spec`` -- a diffraction spec (mass + CoG + radii of gyration
   straight from :class:`FloaterProperties`, environment, frequency/heading grid)
   in the schema consumed by the ``diffraction`` / ``run_orcawave`` lane.
3. ``build_orcaflex_motion_config`` -- an OrcaFlex motion + mooring config that
   consumes the OrcaWave RAOs.
4. ``prepare_variant`` -- writes the mesh + spec to disk and returns a
   :class:`SolverCase` (status ``prepared``).
5. ``dispatch_case`` -- fail-closed: runs the licensed solve if a licence is
   available, otherwise leaves the case ``prepared`` (never raises for a missing
   licence -- mirrors the lane's gate-fail-closed contract).
6. ``fold_rao_results`` -- once a real RAO peak is available, replace the
   closed-form motion proxy on the screening result with the solved value.

References: OrcaWave spec schema (examples/workflows/orcawave-diffraction-solve),
the ``run_orcawave`` engine op (#900/#902), the licensed-run lane (#938).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.floating_wind.floaters import FloaterArchetype, FloaterProperties
from digitalmodel.floating_wind.screening import VariantScreening

__all__ = [
    "FrequencyGrid",
    "SolverCase",
    "box_gdf",
    "cylinder_gdf",
    "build_panel_mesh",
    "build_orcawave_spec",
    "build_orcaflex_motion_config",
    "prepare_variant",
    "prepare_shortlist",
    "dispatch_case",
    "fold_rao_results",
]


@dataclass(frozen=True)
class FrequencyGrid:
    """Frequency (rad/s) and heading (deg) grid for the diffraction solve."""

    frequencies_rad_s: tuple[float, ...] = (0.2, 0.4, 0.6, 0.8, 1.0, 1.2)
    headings_deg: tuple[float, ...] = (0.0, 45.0, 90.0, 135.0, 180.0)


@dataclass
class SolverCase:
    """A per-variant prepared (or solved) solver case."""

    case_id: str
    archetype: FloaterArchetype
    status: str  # "prepared" | "solved" | "needs_supplied_mesh"
    orcawave_spec_path: Path | None = None
    mesh_path: Path | None = None
    orcaflex_config: dict[str, Any] = field(default_factory=dict)
    notes: tuple[str, ...] = ()


# --- analytic WAMIT GDF meshes ----------------------------------------------
#
# WAMIT GDF (low-order) format: a header line, a line "ULEN GRAV", a line
# "ISX ISY" (symmetry flags), the panel count, then 4 vertices (x y z) per panel.
# We emit the submerged (wetted) surface only, panels ordered so the outward
# normal points into the fluid (counter-clockwise seen from outside).


def _gdf_text(panels: list[list[tuple[float, float, float]]], *, ulen: float = 1.0) -> str:
    lines = ["digitalmodel floating_wind analytic mesh", f"{ulen} 9.80665", "0 0", str(len(panels))]
    for panel in panels:
        for x, y, z in panel:
            lines.append(f"{x:.6f} {y:.6f} {z:.6f}")
    return "\n".join(lines) + "\n"


def box_gdf(length: float, beam: float, draft: float, *, nx: int = 6, ny: int = 6, nz: int = 3) -> str:
    """Analytic wetted-surface mesh of a rectangular box (barge), as GDF text.

    Meshes the bottom and the four sides below the waterline (``z`` from
    ``-draft`` to 0). Panel count = ``nx*ny`` (bottom) + side panels.
    """
    hl, hb = length / 2.0, beam / 2.0
    xs = [(-hl + length * i / nx) for i in range(nx + 1)]
    ys = [(-hb + beam * j / ny) for j in range(ny + 1)]
    zs = [(-draft + draft * k / nz) for k in range(nz + 1)]
    panels: list[list[tuple[float, float, float]]] = []

    # Bottom (z = -draft).
    for i in range(nx):
        for j in range(ny):
            panels.append([
                (xs[i], ys[j], -draft),
                (xs[i + 1], ys[j], -draft),
                (xs[i + 1], ys[j + 1], -draft),
                (xs[i], ys[j + 1], -draft),
            ])
    # Sides at y = -hb and y = +hb.
    for i in range(nx):
        for k in range(nz):
            panels.append([
                (xs[i], -hb, zs[k]), (xs[i + 1], -hb, zs[k]),
                (xs[i + 1], -hb, zs[k + 1]), (xs[i], -hb, zs[k + 1]),
            ])
            panels.append([
                (xs[i], hb, zs[k]), (xs[i + 1], hb, zs[k]),
                (xs[i + 1], hb, zs[k + 1]), (xs[i], hb, zs[k + 1]),
            ])
    # Sides at x = -hl and x = +hl.
    for j in range(ny):
        for k in range(nz):
            panels.append([
                (-hl, ys[j], zs[k]), (-hl, ys[j + 1], zs[k]),
                (-hl, ys[j + 1], zs[k + 1]), (-hl, ys[j], zs[k + 1]),
            ])
            panels.append([
                (hl, ys[j], zs[k]), (hl, ys[j + 1], zs[k]),
                (hl, ys[j + 1], zs[k + 1]), (hl, ys[j], zs[k + 1]),
            ])
    return _gdf_text(panels, ulen=max(length, beam))


def cylinder_gdf(diameter: float, draft: float, *, n_theta: int = 24, nz: int = 8) -> str:
    """Analytic wetted-surface mesh of a vertical cylinder (spar), as GDF text.

    Meshes the circular bottom and the cylindrical side below the waterline.
    """
    r = diameter / 2.0
    thetas = [2.0 * math.pi * t / n_theta for t in range(n_theta + 1)]
    zs = [(-draft + draft * k / nz) for k in range(nz + 1)]
    panels: list[list[tuple[float, float, float]]] = []

    # Bottom disc (triangular fans flattened to quads with a repeated centre).
    for t in range(n_theta):
        panels.append([
            (0.0, 0.0, -draft),
            (r * math.cos(thetas[t]), r * math.sin(thetas[t]), -draft),
            (r * math.cos(thetas[t + 1]), r * math.sin(thetas[t + 1]), -draft),
            (0.0, 0.0, -draft),
        ])
    # Cylindrical side.
    for t in range(n_theta):
        for k in range(nz):
            panels.append([
                (r * math.cos(thetas[t]), r * math.sin(thetas[t]), zs[k]),
                (r * math.cos(thetas[t + 1]), r * math.sin(thetas[t + 1]), zs[k]),
                (r * math.cos(thetas[t + 1]), r * math.sin(thetas[t + 1]), zs[k + 1]),
                (r * math.cos(thetas[t]), r * math.sin(thetas[t]), zs[k + 1]),
            ])
    return _gdf_text(panels, ulen=diameter)


def build_panel_mesh(variant: VariantScreening) -> tuple[str | None, str]:
    """Return ``(gdf_text_or_None, status)`` for the variant's hull.

    Box/cylinder primitives (barge/spar) get an analytic mesh; column-stabilised
    hulls (semi/TLP) return ``None`` with status ``needs_supplied_mesh`` -- their
    multi-body panelisation is supplied externally (registry note #942).
    """
    p, params = variant.properties, variant.params
    if p.archetype is FloaterArchetype.BARGE:
        return box_gdf(params["length"], params["beam"], p.draft_m), "prepared"
    if p.archetype is FloaterArchetype.SPAR:
        return cylinder_gdf(params["diameter"], p.draft_m), "prepared"
    return None, "needs_supplied_mesh"


# --- OrcaWave spec -----------------------------------------------------------


def build_orcawave_spec(
    variant: VariantScreening,
    *,
    mesh_file: str,
    grid: FrequencyGrid | None = None,
    water_depth: float = 200.0,
    water_density: float = 1025.0,
) -> dict[str, Any]:
    """Build an OrcaWave diffraction spec dict from a screened variant.

    Inertia comes straight from :class:`FloaterProperties`: mass = displacement,
    the CoG sits ``KG - draft`` below the still-water line, and the radii of
    gyration use the screen's pitch radius of gyration (consistent with #1024).
    """
    grid = grid or FrequencyGrid()
    p = variant.properties
    cog_z = p.KG_m - p.draft_m  # relative to the still-water line (z = 0)
    kyy = p.pitch_radius_gyration_m
    # Yaw radius of gyration ~ horizontal extent; use kyy as a screening proxy.
    radii = [round(kyy, 3), round(kyy, 3), round(kyy, 3)]

    return {
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": f"{p.archetype.value}_{variant.case_id}",
            "type": "ship",
            "geometry": {
                "mesh_file": mesh_file,
                "mesh_format": "gdf",
                "symmetry": "none",
                "reference_point": [0.0, 0.0, 0.0],
                "waterline_z": 0.0,
                "length_units": "m",
            },
            "inertia": {
                "mass": round(p.displacement_t * 1000.0, 1),  # kg
                "centre_of_gravity": [0.0, 0.0, round(cog_z, 3)],
                "radii_of_gyration": radii,
            },
        },
        "environment": {
            "water_depth": water_depth,
            "water_density": water_density,
            "gravity": 9.80665,
        },
        "frequencies": {
            "input_type": "frequency",
            "values": list(grid.frequencies_rad_s),
        },
        "wave_headings": {"values": list(grid.headings_deg), "symmetry": False},
        "solver_options": {
            "remove_irregular_frequencies": True,
            "qtf_calculation": False,
            "load_rao_method": "both",
            "precision": "double",
        },
        "outputs": {
            "formats": ["csv"],
            "components": ["raos", "added_mass", "damping"],
        },
        "metadata": {
            "project": "floating_wind_sizing",
            "author": "digitalmodel",
            "description": (
                f"Concept-screening high-fidelity tier for {p.archetype.value} "
                f"variant {variant.case_id} (#1025)"
            ),
            "tags": ["floating_wind", "concept_screening", p.archetype.value],
        },
    }


def build_orcaflex_motion_config(
    variant: VariantScreening, *, rao_source: str
) -> dict[str, Any]:
    """Build an OrcaFlex motion + mooring config consuming the OrcaWave RAOs."""
    p = variant.properties
    return {
        "basename": "orcaflex",
        "floating_wind_motion": {
            "case_id": variant.case_id,
            "archetype": p.archetype.value,
            "vessel": {
                "mass_t": round(p.total_mass_t, 1),
                "draft_m": p.draft_m,
                "rao_source": rao_source,
            },
            "mooring": {
                "type": "catenary",
                "n_lines": 3,
                # Watch-circle / tension are screened closed-form upstream; the
                # licensed run resolves the coupled response.
            },
            "status": "prepared",
        },
    }


# --- prepare / dispatch / fold ----------------------------------------------


def prepare_variant(variant: VariantScreening, out_dir: str | Path) -> SolverCase:
    """Write the mesh + OrcaWave spec for a variant; return a prepared case."""
    out = Path(out_dir) / variant.case_id
    out.mkdir(parents=True, exist_ok=True)

    gdf, mesh_status = build_panel_mesh(variant)
    notes: list[str] = []
    mesh_path: Path | None = None
    if gdf is not None:
        mesh_path = out / "hull.gdf"
        mesh_path.write_text(gdf)
        mesh_file = mesh_path.name
    else:
        mesh_file = "SUPPLIED_MESH.gdf"
        notes.append(
            "column-stabilised hull: supply a panel mesh (registry note #942)"
        )

    spec = build_orcawave_spec(variant, mesh_file=mesh_file)
    spec_path = out / "orcawave_spec.yml"
    spec_path.write_text(yaml.safe_dump(spec, sort_keys=False))

    ofx_cfg = build_orcaflex_motion_config(variant, rao_source=str(spec_path))

    status = "prepared" if gdf is not None else "needs_supplied_mesh"
    return SolverCase(
        case_id=variant.case_id,
        archetype=variant.properties.archetype,
        status=status,
        orcawave_spec_path=spec_path,
        mesh_path=mesh_path,
        orcaflex_config=ofx_cfg,
        notes=tuple(notes),
    )


def prepare_shortlist(
    variants: list[VariantScreening], out_dir: str | Path
) -> list[SolverCase]:
    """Prepare solver inputs for every variant in a shortlist."""
    return [prepare_variant(v, out_dir) for v in variants]


def dispatch_case(case: SolverCase, *, license_available: bool = False) -> SolverCase:
    """Fail-closed dispatch of a prepared case through the licensed-run lane.

    Without a licence (the default, and CI), the case stays ``prepared`` -- this
    never raises for a missing licence, mirroring the lane's gate-fail-closed
    contract. With a licence, the caller wires the engine ``run_orcawave`` op;
    here we only mark intent so the offline path is fully exercisable.
    """
    if case.status == "needs_supplied_mesh":
        return case
    if not license_available:
        return case  # prepared; licensed host will solve
    # On a licensed host: engine(inputfile=spec) with basename diffraction +
    # operation run_orcawave. Left to the lane; status flips to "solved" there.
    return SolverCase(**{**case.__dict__, "status": "dispatched"})


def fold_rao_results(
    variant: VariantScreening,
    *,
    solved_motion_value: float,
    daf_limit: float,
) -> VariantScreening:
    """Replace the closed-form motion proxy with a solved RAO-derived value.

    Returns a new :class:`VariantScreening` whose ``motion`` check carries the
    solved value; the verdict and governing check are recomputed.
    """
    checks = []
    for chk in variant.checks:
        if chk.name == "motion":
            margin = daf_limit - solved_motion_value
            checks.append(
                chk.model_copy(
                    update={
                        "value": solved_motion_value,
                        "limit": daf_limit,
                        "passed": margin >= 0.0,
                        "margin": margin,
                        "basis": "solved RAO peak (OrcaWave/OrcaFlex tier)",
                    }
                )
            )
        else:
            checks.append(chk)

    governing = min(checks, key=lambda c: c.margin)
    passed = variant.feasible and all(c.passed for c in checks)
    return variant.model_copy(
        update={
            "checks": checks,
            "passed": passed,
            "governing_check": governing.name,
            "governing_margin": governing.margin,
        }
    )
