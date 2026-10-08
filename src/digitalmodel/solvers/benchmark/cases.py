"""Fixed case definitions for the solver baseline pack (#2300).

Every input is generated here from pinned parameters, so the pack carries no
project data and two machines running the same ``PACK_VERSION`` solve the same
workload. Changing any parameter below changes the workload: bump
``PACK_VERSION`` in ``runner.py`` with it.

Standard library only, so the module imports on hosts without any solver.
"""

from __future__ import annotations

import hashlib
import json
import re
from pathlib import Path

# ----------------------------------------------------------------- variants


def resolve_variants(variants, cores: int) -> list[int]:
    """Expand ``"all"`` to ``cores``, cap at ``cores`` and drop duplicates.

    Any other string (``"default"``) is a solver-controlled setting and passes
    through unchanged.
    """
    resolved: list = []
    for variant in variants:
        if variant == "all":
            n = cores
        elif isinstance(variant, str):
            n = variant
        else:
            n = min(int(variant), cores)
        if n not in resolved:
            resolved.append(n)
    return resolved


def digest(payload) -> str:
    """SHA-256 of a parameter dict (sorted JSON) or of a file's bytes."""
    if isinstance(payload, Path):
        return hashlib.sha256(payload.read_bytes()).hexdigest()
    text = json.dumps(payload, sort_keys=True)
    return hashlib.sha256(text.encode()).hexdigest()


# ------------------------------------------------------------------ OrcaFlex

ORCAFLEX = {
    "water_depth_m": 200.0,
    "risers": 8,
    "riser_length_m": 350.0,
    "segment_length_m": 2.0,
    "end_a_radius_m": 10.0,
    "end_b_radius_m": 260.0,
    "end_b_z_m": -190.0,
    "wave": {"type": "JONSWAP", "hs_m": 4.0, "tp_s": 10.0, "seed": 12345,
             "components": 100},
    "stage_durations_s": [10.0, 200.0],
    "time_step_s": 0.1,
    "sample_times_s": [50.0, 100.0, 200.0],
}

# -------------------------------------------------------- OrcaWave / AQWA

BARGE = {"length_m": 100.0, "beam_m": 20.0, "draft_m": 8.0,
         "dx_m": 2.0, "dy_m": 2.0, "dz_m": 1.0}
_NX = int(BARGE["length_m"] / BARGE["dx_m"])
_NY = int(BARGE["beam_m"] / BARGE["dy_m"])
_NZ = int(BARGE["draft_m"] / BARGE["dz_m"])
BARGE_PANELS = _NX * _NY + 2 * _NX * _NZ + 2 * _NY * _NZ

# rad/s, ascending; indices 2, 9 and 16 are the fingerprint samples
DIFFRACTION_FREQUENCIES = [round(0.2 + 0.07 * i, 2) for i in range(20)]
DIFFRACTION_SAMPLE_INDICES = [2, 9, 16]
DIFFRACTION_HEADINGS = [0.0, 45.0, 90.0, 135.0, 180.0]


def _barge_panels():
    L, B, T = BARGE["length_m"], BARGE["beam_m"], BARGE["draft_m"]
    xs = [-L / 2 + i * BARGE["dx_m"] for i in range(_NX + 1)]
    ys = [-B / 2 + j * BARGE["dy_m"] for j in range(_NY + 1)]
    zs = [-T + k * BARGE["dz_m"] for k in range(_NZ + 1)]
    panels = []
    # Vertex order gives (v2 - v1) x (v3 - v1) pointing out of the body.
    for i in range(_NX):  # bottom, normal -z
        for j in range(_NY):
            x0, x1, y0, y1 = xs[i], xs[i + 1], ys[j], ys[j + 1]
            panels.append([(x0, y1, -T), (x1, y1, -T), (x1, y0, -T), (x0, y0, -T)])
    for i in range(_NX):  # sides y = -B/2 (normal -y) and y = +B/2 (normal +y)
        for k in range(_NZ):
            x0, x1, z0, z1 = xs[i], xs[i + 1], zs[k], zs[k + 1]
            y = -B / 2
            panels.append([(x0, y, z0), (x1, y, z0), (x1, y, z1), (x0, y, z1)])
            y = B / 2
            panels.append([(x1, y, z0), (x0, y, z0), (x0, y, z1), (x1, y, z1)])
    for j in range(_NY):  # ends x = -L/2 (normal -x) and x = +L/2 (normal +x)
        for k in range(_NZ):
            y0, y1, z0, z1 = ys[j], ys[j + 1], zs[k], zs[k + 1]
            x = -L / 2
            panels.append([(x, y1, z0), (x, y0, z0), (x, y0, z1), (x, y1, z1)])
            x = L / 2
            panels.append([(x, y0, z0), (x, y1, z0), (x, y1, z1), (x, y0, z1)])
    return panels


def write_barge_gdf(path: Path) -> Path:
    """Write the full-hull (no symmetry) WAMIT GDF for the benchmark barge."""
    panels = _barge_panels()
    lines = ["Solver baseline pack barge 100 x 20 x 8 m (#2300)", "1.0  9.80665",
             "0  0", str(len(panels))]
    for panel in panels:
        for x, y, z in panel:
            lines.append(f"{x:.6f}  {y:.6f}  {z:.6f}")
    path = Path(path)
    path.write_text("\n".join(lines) + "\n")
    return path


def diffraction_spec(mesh_file: str) -> dict:
    """DiffractionSpec mapping shared by the OrcaWave and AQWA cases."""
    L, B, T = BARGE["length_m"], BARGE["beam_m"], BARGE["draft_m"]
    return {
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "BaselineBarge",
            "type": "barge",
            "geometry": {"mesh_file": mesh_file, "mesh_format": "gdf",
                         "symmetry": "none", "reference_point": [0.0, 0.0, 0.0],
                         "waterline_z": 0.0, "length_units": "m"},
            "inertia": {"mass": L * B * T * 1025.0,
                        "centre_of_gravity": [0.0, 0.0, -2.0],
                        "radii_of_gyration": [7.0, 29.0, 29.0]},
        },
        "environment": {"water_depth": 200.0, "water_density": 1025.0,
                        "gravity": 9.80665},
        "frequencies": {"input_type": "frequency",
                        "values": list(DIFFRACTION_FREQUENCIES)},
        "wave_headings": {"values": list(DIFFRACTION_HEADINGS), "symmetry": False},
        "solver_options": {"remove_irregular_frequencies": False,
                           "qtf_calculation": False, "load_rao_method": "both",
                           "precision": "double"},
        "outputs": {"formats": ["csv"], "components": ["raos", "added_mass", "damping"]},
        "metadata": {"project": "solver_baseline_pack", "author": "benchmark",
                     "description": "Solver baseline pack barge (#2300)",
                     "tags": ["benchmark"]},
    }


# --------------------------------------------------------------------- MAPDL

MAPDL = {"length_m": 10.0, "width_m": 1.0, "height_m": 1.0, "esize_m": 0.0625,
         "e_pa": 2.1e11, "nu": 0.3, "pressure_pa": 1.0e6}


def apdl_deck() -> str:
    """Cantilever block, SOLID186, linear static sparse solve, tip fingerprint."""
    p = MAPDL
    return "\n".join([
        "/BATCH",
        "/PREP7",
        "ET,1,SOLID186",
        f"MP,EX,1,{p['e_pa']:.6E}",
        f"MP,PRXY,1,{p['nu']}",
        f"BLOCK,0,{p['length_m']:g},0,{p['width_m']:g},0,{p['height_m']:g}",
        f"ESIZE,{p['esize_m']}",
        "MSHKEY,1",
        "VMESH,ALL",
        "NSEL,S,LOC,X,0",
        "D,ALL,ALL,0",
        f"NSEL,S,LOC,Z,{p['height_m']:g}",
        f"SF,ALL,PRES,{p['pressure_pa']:.6E}",
        "ALLSEL",
        "FINISH",
        "/SOLU",
        "ANTYPE,STATIC",
        "EQSLV,SPARSE",
        "SOLVE",
        "FINISH",
        "/POST1",
        "NSORT,U,Z",
        "*GET,UZMIN,SORT,,MIN",
        "UZTIP=UZ(NODE(10,0.5,0.5))",
        "*GET,NNODE,NODE,0,COUNT",
        "*GET,NELEM,ELEM,0,COUNT",
        "*CFOPEN,fingerprint,txt",
        "*VWRITE,UZMIN,UZTIP,NNODE,NELEM",
        "(E20.12,1X,E20.12,1X,F12.0,1X,F12.0)",
        "*CFCLOS",
        "FINISH",
        "",
    ])


def parse_mapdl_fingerprint(path: Path) -> dict:
    uz_min, uz_tip, nodes, elements = Path(path).read_text().split()[:4]
    return {"uz_min_m": float(uz_min), "uz_tip_m": float(uz_tip),
            "nodes": int(float(nodes)), "elements": int(float(elements))}


def parse_mapdl_elapsed(text: str) -> dict:
    patterns = {
        "licence_s": r"Elapsed time spent obtaining a license\s*[:=]\s*([\d.]+)",
        "solution_s": r"Elapsed time spent computing solution\s*[:=]\s*([\d.]+)",
        "equation_solver_s": r"Elapsed time in equation solver\s*[:=]\s*([\d.]+)",
        "total_s": r"Elapsed Time \(sec\)\s*[:=]\s*([\d.]+)",
    }
    out = {}
    for key, pattern in patterns.items():
        m = re.search(pattern, text)
        if m:
            out[key] = float(m.group(1))
    return out


# ------------------------------------------------------------------ OpenFOAM

OPENFOAM_VERSION = "openfoam2312"
OPENFOAM_TUTORIAL = "incompressible/simpleFoam/motorBike"
OPENFOAM_ITERATIONS = 100
OPENFOAM_MESH_PROCS = 6
_DROPPED_FUNCTIONS = ("streamLines", "wallBoundedStreamLines", "cuttingPlane",
                      "ensightWrite")


def _decompose_dict(n: int) -> str:
    return (
        "FoamFile\n{\n    version     2.0;\n    format      ascii;\n"
        "    class       dictionary;\n    object      decomposeParDict;\n}\n\n"
        f"numberOfSubdomains {n};\n\nmethod          scotch;\n"
    )


def prepare_openfoam_case(case_dir: Path, n_procs: int) -> None:
    """Pin iterations, drop I/O-heavy function objects, write both decompositions."""
    case_dir = Path(case_dir)
    control = case_dir / "system" / "controlDict"
    text = control.read_text()
    text = re.sub(r"endTime\s+\d+;", f"endTime         {OPENFOAM_ITERATIONS};", text)
    text = re.sub(r"writeInterval\s+\d+;",
                  f"writeInterval   {OPENFOAM_ITERATIONS};", text)
    for name in _DROPPED_FUNCTIONS:
        text = re.sub(rf'[ \t]*#include "{name}"\n', "", text)
    control.write_text(text)
    system = case_dir / "system"
    (system / "decomposeParDict.mesh").write_text(_decompose_dict(OPENFOAM_MESH_PROCS))
    (system / "decomposeParDict").write_text(_decompose_dict(n_procs))


def parse_force_coeffs(path: Path) -> dict:
    lines = Path(path).read_text().splitlines()
    header = next((ln for ln in reversed(lines) if ln.startswith("#") and "Cd" in ln),
                  None)
    column = 1
    if header:
        names = header.lstrip("#").split()
        column = names.index("Cd")
    last = [ln for ln in lines if ln.strip() and not ln.startswith("#")][-1].split()
    return {"iterations": int(float(last[0])), "cd": float(last[column])}


def parse_cell_count(log: str) -> int | None:
    found = re.findall(r"^\s*cells:\s+(\d+)", log, flags=re.M)
    return int(found[-1]) if found else None


def parse_openfoam_procs(log: str) -> int | None:
    m = re.search(r"^nProcs\s*:\s*(\d+)", log, flags=re.M)
    return int(m.group(1)) if m else None


def parse_openfoam_last_time(log: str) -> int | None:
    found = re.findall(r"^Time = (\d+)", log, flags=re.M)
    return int(found[-1]) if found else None
