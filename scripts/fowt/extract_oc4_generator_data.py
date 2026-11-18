"""Extract the NREL 5 MW / OC4-DeepCwind semi-submersible values for the generator.

    python scripts/fowt/extract_oc4_generator_data.py <raw_dir> <output_folder>

Writes ``basis.yml`` and ``generator_data.yml`` into ``output_folder`` in the
same layout as the IEA 15 MW folder, so the same spec builder and checks
apply. Sources:

* Robertson, A., et al. (2014). Definition of the Semisubmersible Floating
  System for Phase II of OC4. NREL/TP-5000-60601: platform, tower,
  hydrostatic and mooring tables, transcribed below with table and printed
  page.
* OpenFAST/r-test at the pinned revision (Apache-2.0): ElastoDyn rotor,
  nacelle, blade and tower inputs; WAMIT ``marin_semi.hst`` and ``.1``; and
  the OpenFAST linearisation ``5MW_OC4Semi_Linear.1.lin``, whose state matrix
  gives the comparator natural periods.

``raw_dir`` holds the r-test files under their base names. Their SHA-256
digests are verified against the pinned values before parsing; any other
bytes are refused.
"""
from __future__ import annotations

import hashlib
import math
import sys
from pathlib import Path

import numpy as np
import yaml

COMMIT = "dd5feaaaa500ba7283140107806300d551cff0a7"
R = "OpenFAST/r-test"
FILES = {
    "elastodyn": "glue-codes/openfast/5MW_OC4Semi_Linear/NRELOffshrBsline5MW_OC4DeepCwindSemi_ElastoDyn.dat",
    "tower": "glue-codes/openfast/5MW_OC4Semi_Linear/NRELOffshrBsline5MW_OC4DeepCwindSemi_ElastoDyn_Tower.dat",
    "blade": "glue-codes/openfast/5MW_OC4Semi_Linear/NRELOffshrBsline5MW_Blade.dat",
    "hst": "glue-codes/openfast/5MW_Baseline/HydroData/marin_semi.hst",
    "wamit1": "glue-codes/openfast/5MW_Baseline/HydroData/marin_semi.1",
    "hydrodyn": "glue-codes/openfast/5MW_OC4Semi_Linear/NRELOffshrBsline5MW_OC4DeepCwindSemi_HydroDyn.dat",
    "map": "glue-codes/openfast/5MW_OC4Semi_Linear/NRELOffshrBsline5MW_OC4DeepCwindSemi_MAP.dat",
    "lin": "glue-codes/openfast/5MW_OC4Semi_Linear/5MW_OC4Semi_Linear.1.lin",
}
# Expected bytes at COMMIT, pinned independently of the generated output (as in
# extract_iea15_generator_data.py); verified before any file is parsed.
EXPECTED_SHA256 = {
    "elastodyn": "cbc370e5151625df2a4f1cab50ede39b8d6bd9a91747f362064e8821c288d699",
    "tower": "226d37eb0d5b4f7b387b7d3e0b29a15cf95a54ee1146fb595426affaf9279b83",
    "blade": "615b5352843f118e85c02616f8a0ae401e34c1df07d6dd3ed6f77bdd3d4198b5",
    "hst": "ba933c18034bacc87a932254ad47047d44eef006bc150b76fecd3445965482d8",
    "wamit1": "6d05fa1d1f159c1c0a44a0b4f9a2029113c7bf96b942742a1681267fdff84792",
    "hydrodyn": "6096dbe11cb79fcb770485bdcce30042669d8f9c781adf9e25d48db19837f07a",
    "map": "afbd4ed14f172a8ad07dc1bd1485e0154f2283badece7fe81b698f0ba0550daf",
    "lin": "4098c8badb84b9324d2fd36193bdf23fb5fe2b5d1fd000f96979cc3521fe0660",
}
# NumPy 2 renamed trapz to trapezoid; support both.
_trapezoid = getattr(np, "trapezoid", None) or np.trapz
REPORT = "Robertson et al. (2014), NREL/TP-5000-60601"
PHASE2 = "Robertson et al. (2014), NREL/CP-5000-61154"


def src(key: str, where: str) -> str:
    return f"{R}; {FILES[key]}; {where}; commit {COMMIT}"


def item(value, unit: str, source: str) -> dict:
    return {"value": value, "unit": unit, "source": source}


def text(path: Path) -> list[str]:
    return path.read_text(encoding="utf-8").split("\n")


def value(path: Path, label: str) -> float:
    for line in text(path):
        parts = line.split()
        if len(parts) > 1 and parts[1] == label:
            return float(parts[0])
    raise KeyError(label)


def table(path: Path, header_word: str, columns: int) -> np.ndarray:
    lines = text(path)
    start = next(k for k, line in enumerate(lines) if header_word in line.split())
    rows = []
    for line in lines[start + 2:]:
        parts = line.split()
        try:
            rows.append([float(x) for x in parts[:columns]])
        except ValueError:
            break
        if len(parts) < columns:
            break
    return np.array(rows)


# Eigensolver results differ between numerical environments by round-off
# (about 3e-12 relative was observed). The linearisation periods and damping
# ratios are serialised at this many significant figures so the committed
# bytes do not depend on the environment; this is a serialization convention,
# not a statement of the accuracy of the linearisation.
LINEARISATION_SIGNIFICANT_FIGURES = 9


def canonical(x: float) -> float:
    """Round to LINEARISATION_SIGNIFICANT_FIGURES significant figures."""
    return float(f"{x:.{LINEARISATION_SIGNIFICANT_FIGURES}g}")


def linearisation_periods(path: Path) -> dict:
    """Platform natural periods from the OpenFAST linearised state matrix.

    Each platform degree of freedom takes the lightly damped (damping ratio
    below 0.1, frequency below 0.1 Hz) eigenmode in which it has the largest
    share of the six platform displacement states.
    """
    lines = text(path)
    i = next(k for k, line in enumerate(lines) if line.startswith("A:"))
    n = int(lines[i].split()[1])
    a = np.array([[float(x) for x in lines[i + 1 + r].split()] for r in range(n)])
    eigenvalues, vectors = np.linalg.eig(a)
    modes = []
    for k, lam in enumerate(eigenvalues):
        if lam.imag <= 1e-9:
            continue
        frequency = abs(lam) / (2 * math.pi)
        zeta = -lam.real / abs(lam)
        if frequency < 0.1 and zeta < 0.1:
            share = np.abs(vectors[:6, k])
            modes.append((frequency, zeta, share / share.sum()))
    names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
    out = {}
    for d, name in enumerate(names):
        frequency, zeta, share = max(modes, key=lambda m: m[2][d])
        out[name] = {"period": canonical(1.0 / frequency), "damping_ratio": canonical(zeta),
                     "share": float(share[d])}
    return out


def read_hst(path: Path) -> list[list[float]]:
    m = [[0.0] * 6 for _ in range(6)]
    for line in text(path):
        p = line.split()
        if len(p) == 3:
            m[int(p[0]) - 1][int(p[1]) - 1] = float(p[2])
    return m


def read_wamit1(path: Path):
    blocks: dict[float, dict] = {}
    for line in text(path):
        p = line.split()
        if len(p) < 4:
            continue
        period, i, j = float(p[0]), int(p[1]) - 1, int(p[2]) - 1
        e = blocks.setdefault(period, {"a": [[0.0] * 6 for _ in range(6)],
                                       "b": [[0.0] * 6 for _ in range(6)]})
        e["a"][i][j] = float(p[3])
        if len(p) > 4:
            e["b"][i][j] = float(p[4])
    zero, infinite = blocks.pop(-1.0)["a"], blocks.pop(0.0)["a"]
    rows = [[t, b["a"][0][0], b["a"][2][2], b["a"][4][4], b["a"][0][4], b["a"][5][5],
             b["b"][0][0], b["b"][2][2], b["b"][4][4], b["b"][0][4], b["b"][5][5]]
            for t, b in sorted(blocks.items())]
    return zero, infinite, rows


def parallel(mass: float, centre: np.ndarray, inertia: np.ndarray, about: np.ndarray) -> np.ndarray:
    r = centre - about
    return inertia + mass * (np.dot(r, r) * np.eye(3) - np.outer(r, r))


def components(raw: Path) -> list[dict]:
    """Platform, tower, nacelle and rotor as rigid parts in platform axes (SWL origin)."""
    ed = raw / Path(FILES["elastodyn"]).name
    top = value(ed, "TowerHt")
    base = value(ed, "TowerBsHt")
    # Tower: ElastoDyn distributed mass; the report's Table 2-2 gives 249,718 kg at 43.4 m.
    tw = table(raw / Path(FILES["tower"]).name, "HtFract", 2)
    z = base + tw[:, 0] * (top - base)
    tower_mass = float(_trapezoid(tw[:, 1], z))
    tower_zc = float(_trapezoid(tw[:, 1] * z, z) / tower_mass)
    tower_iyy = float(_trapezoid(tw[:, 1] * (z - tower_zc) ** 2, z))
    # Blade: ElastoDyn distributed mass with its AdjBlMs factor.
    bl = table(raw / Path(FILES["blade"]).name, "BlFract", 3)
    tip, hub_r = value(ed, "TipRad"), value(ed, "HubRad")
    adjust = value(raw / Path(FILES["blade"]).name, "AdjBlMs")
    span = bl[:, 0] * (tip - hub_r)
    radius = hub_r + span
    density = bl[:, 2] * adjust
    blade_mass = float(_trapezoid(density, span))
    blade_first = float(_trapezoid(density * radius, span))
    blade_second = float(_trapezoid(density * radius ** 2, span))
    tilt = math.radians(abs(value(ed, "ShftTilt")))
    cone = math.radians(abs(value(ed, "PreCone(1)")))
    overhang = value(ed, "OverHang")
    apex = np.array([overhang * math.cos(tilt), 0.0,
                     top + value(ed, "Twr2Shft") + abs(overhang) * math.sin(tilt)])
    hub_mass = value(ed, "HubMass")
    rotor_mass = hub_mass + 3 * blade_mass
    shift = 3 * blade_mass * (blade_first / blade_mass) * math.sin(cone) / rotor_mass
    upwind = np.array([-math.cos(tilt), 0.0, math.sin(tilt)])
    rotor_centre = apex + shift * upwind
    polar = value(ed, "HubIner") + 3 * blade_second * math.cos(cone) ** 2
    nac_mass = value(ed, "NacMass")
    nac_centre = np.array([value(ed, "NacCMxn"), 0.0, top + value(ed, "NacCMzn")])
    nac_izz = value(ed, "NacYIner") - nac_mass * value(ed, "NacCMxn") ** 2
    return [
        {"name": "platform including ballast", "mass": 1.3473e7,
         "centre": [0.0, 0.0, -13.46],
         "inertia": [[6.827e9, 0, 0], [0, 6.827e9, 0], [0, 0, 1.226e10]],
         "source": f"{REPORT}; Table 3-3; printed page 9"},
        {"name": "tower", "mass": tower_mass, "centre": [0.0, 0.0, tower_zc],
         "inertia": [[tower_iyy, 0, 0], [0, tower_iyy, 0], [0, 0, 0.0]],
         "source": src("tower", "HtFract,TMassDen integrated between TowerBsHt and TowerHt; "
                                "torsional inertia neglected")},
        {"name": "nacelle", "mass": nac_mass, "centre": nac_centre.tolist(),
         "inertia": [[nac_izz, 0, 0], [0, nac_izz, 0], [0, 0, nac_izz]],
         "source": src("elastodyn", "NacMass,NacCMxn,NacCMzn,NacYIner; Izz about the nacelle "
                                    "centre from NacYIner by the parallel-axis theorem; Ixx and "
                                    "Iyy are not given and are assumed equal to Izz")},
        {"name": "rotor (hub and three blades)", "mass": rotor_mass,
         "centre": rotor_centre.tolist(),
         "inertia": [[polar, 0, 0], [0, polar / 2, 0], [0, 0, polar / 2]],
         "source": src("elastodyn", "HubMass,HubIner,OverHang,Twr2Shft,ShftTilt,PreCone,TipRad,"
                                    "HubRad; blade BMassDen x AdjBlMs integrated; transverse "
                                    "inertia taken as half the polar inertia (three-blade rotor); "
                                    "tilt rotation of the tensor neglected")},
    ]


def aggregate(parts: list[dict]) -> dict:
    mass = math.fsum(p["mass"] for p in parts)  # version-independent float sum
    centre = sum(p["mass"] * np.array(p["centre"]) for p in parts) / mass
    inertia = sum(parallel(p["mass"], np.array(p["centre"]), np.array(p["inertia"], float),
                           centre) for p in parts)
    return {"mass": mass, "centre": centre, "inertia": inertia}


def build(raw: Path) -> tuple[dict, dict]:
    names = {k: raw / Path(v).name for k, v in FILES.items()}
    digests = {k: hashlib.sha256(p.read_bytes()).hexdigest() for k, p in names.items()}
    for key, digest in digests.items():
        if digest != EXPECTED_SHA256[key]:
            raise ValueError(f"source digest mismatch: {names[key].name}")
    parts = components(raw)
    system = aggregate(parts)
    agg_src = ("aggregation of the components listed under system.components; "
               "M=sum(m), CG=sum(m r)/M, I_CG=sum(I_i + m_i((d.d)I - d d^T))")
    angles = [60.0, 180.0, 300.0]
    fair_r, anchor_r = 40.868, 837.6
    lin = linearisation_periods(names["lin"])
    basis = {
        "description": "NREL 5 MW reference turbine on the OC4-DeepCwind semi-submersible. "
                       "Values transcribed from the OC4 Phase II definition report or read "
                       "from the OpenFAST regression-test inputs.",
        "reference_commit": COMMIT,
        "environment": {
            "water_depth": item(200.0, "m", f"{REPORT}; Table 4-6; printed page 25"),
            "water_density": item(1025.0, "kg/m^3", f"{REPORT}; Table 4-6; printed page 25"),
        },
        "published": {
            "displacement": item(13917.0, "m^3", f"{REPORT}; Table 4-6; printed page 25"),
            "center_of_buoyancy": item([0.0, 0.0, -13.15], "m",
                                       f"{REPORT}; Table 4-6, 13.15 m below SWL; printed page 25"),
            "buoyancy_force": item(1.3989e8, "N", f"{REPORT}; Table 4-6; printed page 25"),
            "hydrostatic_stiffness": item(
                [0.0, 0.0, 3.836e6, -3.776e8, -3.776e8, 0.0],
                "N/m for heave; N m/rad for roll and pitch",
                f"{REPORT}; Table 4-6 and Eq. (4-3); gravity term excluded; printed pages 11, 25"),
            "natural_frequencies_text": item(
                {"surge": 0.01, "heave": 0.058, "pitch": 0.04}, "Hz",
                f"{PHASE2}; results text on the response spectra for irregular-wave load "
                "cases 2.2 and 3.2 (Hs 6 m, Tp 10 s), PDF page 13; "
                "quoted to one or two significant figures"),
        },
        "comparators": {
            "openfast_linearisation_periods": item(
                {k: v["period"] for k, v in lin.items()}, "s",
                src("lin", "state matrix A eigen-analysis; mode per DOF chosen by largest "
                           "platform-displacement share among modes with damping ratio < 0.1 "
                           "and frequency < 0.1 Hz; OpenFAST v4.1.2-727 output")),
            "openfast_linearisation_damping": item(
                {k: v["damping_ratio"] for k, v in lin.items()}, "fraction of critical",
                src("lin", "same eigenmodes")),
        },
        "system": {
            "mass": item(system["mass"], "kg", agg_src),
            "center_of_gravity": item(system["centre"].tolist(), "m", agg_src),
            "inertia_cg": item(system["inertia"].tolist(), "kg m^2", agg_src),
            "components": parts,
        },
        "mooring": {
            "line_count": item(3, "count", f"{REPORT}; Table 5-1; printed page 26"),
            "volume_equivalent_diameter": item(0.0766, "m", f"{REPORT}; Table 5-1; printed page 26"),
            "mass_per_length": item(113.35, "kg/m", f"{REPORT}; Table 5-1; printed page 26"),
            "axial_stiffness": item(7.536e8, "N", f"{REPORT}; Table 5-1, 753.6 MN; printed page 26"),
            "unstretched_length": item(835.5, "m",
                                       f"{REPORT}; Table 5-1; printed page 26 (the r-test MAP "
                                       "input uses 835.35 m)"),
            "normal_drag_coefficient": item(1.1, "-", f"{REPORT}; Table 5-1; printed page 26"),
            "added_mass_coefficient": item(1.0, "-", f"{REPORT}; Table 5-1; printed page 26"),
            "fairleads": item([[fair_r * math.cos(math.radians(a)), fair_r * math.sin(math.radians(a)),
                                -14.0] for a in angles], "m",
                              f"{REPORT}; Table 5-1 radius 40.868 m, depth 14 m; azimuths 60, 180, "
                              f"300 degree from {R} {FILES['map']} (Line 1 at 60 degree, repeat 240 "
                              "120)"),
            "anchors": item([[anchor_r * math.cos(math.radians(a)), anchor_r * math.sin(math.radians(a)),
                              -200.0] for a in angles], "m",
                            f"{REPORT}; Table 5-1 radius 837.6 m, depth 200 m; same azimuths"),
            "minimum_breaking_load": None,
        },
        "geometry": {
            "hull_length": item(14.43 + 28.87 + 24.0, "m",
                                f"{REPORT}; Table 3-1 and 3-2: offset columns at x 14.43 and "
                                "-28.87 m, base column diameter 24 m; printed page 7"),
        },
    }
    zero, infinite, rows = read_wamit1(names["wamit1"])
    drag = [3.95e5, 3.95e5, 3.88e6, 3.70e10, 3.70e10, 4.08e9]
    data = {
        "description": "Hydrodynamic values for the OC4-DeepCwind semi-submersible.",
        "reference_commit": COMMIT,
        "retrieved": "2026-11-12",
        "source_url": f"https://github.com/OpenFAST/r-test/tree/{COMMIT}",
        "licence": "Apache License 2.0 (LICENSE at the pinned revision)",
        "extraction": "scripts/fowt/extract_oc4_generator_data.py",
        "source_sha256": {FILES[k]: digest for k, digest in digests.items()},
        "hydrodynamics": {
            "wamit_length_scale": item(value(names["hydrodyn"], "WAMITULEN"), "m",
                                       src("hydrodyn", "WAMITULEN")),
            "reference_volume": item(value(names["hydrodyn"], "PtfmVol0"), "m^3",
                                     src("hydrodyn", "PtfmVol0")),
            "hydrostatic_stiffness_nondimensional": item(
                read_hst(names["hst"]), "C/(rho g L^k); L=1 m", src("hst", "all entries")),
            "added_mass_zero_frequency_nondimensional": item(
                zero, "A/(rho L^k); L=1 m", src("wamit1", "period -1 (zero frequency)")),
            "added_mass_infinite_frequency_nondimensional": item(
                infinite, "A/(rho L^k); L=1 m", src("wamit1", "period 0 (infinite frequency)")),
            "radiation_by_period_nondimensional": {
                "columns": ["period s", "A11", "A33", "A55", "A15", "A66",
                            "B11", "B33", "B55", "B15", "B66"],
                "unit": "A/(rho L^k); B/(rho omega L^k); L=1 m",
                "value": rows,
                "source": src("wamit1", "positive-period rows, entries 11,33,55,15,66"),
            },
            "additional_quadratic_damping": item(
                [[drag[i] if i == j else 0.0 for j in range(6)] for i in range(6)],
                "N/(m/s)^2 translation; N m/(rad/s)^2 rotation",
                f"{REPORT}; Table 4-6 additional quadratic drag; printed page 25"),
        },
    }
    return basis, data


def main(argv: list[str]) -> int:
    raw, out = Path(argv[1]), Path(argv[2])
    basis, data = build(raw)
    out.mkdir(parents=True, exist_ok=True)

    def plain(obj):
        if isinstance(obj, dict):
            return {k: plain(v) for k, v in obj.items()}
        if isinstance(obj, (list, tuple)):
            return [plain(v) for v in obj]
        if isinstance(obj, np.generic):
            return obj.item()
        return obj

    for name, content in (("basis.yml", basis), ("generator_data.yml", data)):
        (out / name).write_text(yaml.safe_dump(plain(content), sort_keys=False, width=100),
                                encoding="utf-8", newline="\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
