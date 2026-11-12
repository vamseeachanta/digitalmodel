"""Extract the IEA 15 MW / VolturnUS-S values the OrcaFlex generator needs.

Reads public files of the IEAWindTask37/IEA-15-240-RWT repository at the
pinned revision (Apache-2.0) and writes
``docs/domains/orcaflex/wind/fowt-volturnus/generator_data.yml``. Every value
carries its unit and its source file, sheet or row. The raw files are not
committed; their SHA-256 digests are recorded in the output so that a
re-extraction can be checked against the same bytes.

Usage::

    python scripts/fowt/extract_iea15_generator_data.py <raw_dir> <output.yml>

``raw_dir`` holds, under their repository base names:
``IEA-15-240-RWT_tabular.xlsx``, ``IEA-15-240-RWT-UMaineSemi.1``,
``IEA-15-240-RWT-UMaineSemi.hst`` and ``IEA-15-240-RWT-UMaineSemi_HydroDyn.dat``.
"""
from __future__ import annotations

import hashlib
import sys
from pathlib import Path

import yaml

COMMIT = "e4993d63de10f165389534461dd544006750fe60"
REPO = "IEA-15-240-RWT"
FILES = {
    "tabular": "Documentation/IEA-15-240-RWT_tabular.xlsx",
    "wamit1": "OpenFAST/IEA-15-240-RWT-UMaineSemi/HydroData/IEA-15-240-RWT-UMaineSemi.1",
    "hst": "OpenFAST/IEA-15-240-RWT-UMaineSemi/HydroData/IEA-15-240-RWT-UMaineSemi.hst",
    "hydrodyn": "OpenFAST/IEA-15-240-RWT-UMaineSemi/IEA-15-240-RWT-UMaineSemi_HydroDyn.dat",
}


def src(key: str, where: str) -> str:
    return f"{REPO}; {FILES[key]}; {where}; commit {COMMIT}"


def item(value, unit: str, source: str) -> dict:
    return {"value": value, "unit": unit, "source": source}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_tabular(path: Path) -> tuple[dict, dict, list]:
    import openpyxl

    book = openpyxl.load_workbook(path, data_only=True, read_only=True)
    overview = {row[0]: row[1] for row in book["Overview"].iter_rows(values_only=True)
                if row and row[0] is not None}
    nacelle = {row[0]: row for row in book["Nacelle Mass Properties"].iter_rows(values_only=True)
               if row and row[0] is not None}
    performance = []
    for index, row in enumerate(book["Rotor Performance"].iter_rows(values_only=True), start=1):
        if index >= 2 and isinstance(row[0], (int, float)):
            performance.append([float(row[0]), float(row[1]), float(row[5]),
                                float(row[7]) * 1e6, float(row[2]) * 1e6])
    return overview, nacelle, performance


def read_hst(path: Path) -> list[list[float]]:
    matrix = [[0.0] * 6 for _ in range(6)]
    for line in path.read_text(encoding="utf-8").split("\n"):
        parts = line.split()
        if len(parts) == 3:
            matrix[int(parts[0]) - 1][int(parts[1]) - 1] = float(parts[2])
    return matrix


def read_wamit1(path: Path) -> tuple[list, list, list]:
    """Return zero-frequency, infinite-frequency and per-period values.

    WAMIT convention: period -1 is zero frequency and period 0 is infinite
    frequency; positive periods carry added mass and damping.
    """
    blocks: dict[float, dict] = {}
    for line in path.read_text(encoding="utf-8").split("\n"):
        parts = line.split()
        if len(parts) < 4:
            continue
        period, i, j = float(parts[0]), int(parts[1]) - 1, int(parts[2]) - 1
        entry = blocks.setdefault(period, {"a": [[0.0] * 6 for _ in range(6)],
                                           "b": [[0.0] * 6 for _ in range(6)]})
        entry["a"][i][j] = float(parts[3])
        if len(parts) > 4:
            entry["b"][i][j] = float(parts[4])
    zero = blocks.pop(-1.0)["a"]
    infinite = blocks.pop(0.0)["a"]
    rows = []
    for period in sorted(blocks):
        a, b = blocks[period]["a"], blocks[period]["b"]
        rows.append([period, a[0][0], a[2][2], a[4][4], a[0][4], a[5][5],
                     b[0][0], b[2][2], b[4][4], b[0][4], b[5][5]])
    return zero, infinite, rows


def read_matrix_after(path: Path, label: str) -> list[list[float]]:
    lines = path.read_text(encoding="utf-8").split("\n")
    start = next(k for k, line in enumerate(lines) if label in line)
    return [[float(x) for x in lines[start + r].split()[:6]] for r in range(6)]


def read_value(path: Path, label: str) -> float:
    for line in path.read_text(encoding="utf-8").split("\n"):
        parts = line.split()
        if len(parts) > 1 and parts[1] == label:
            return float(parts[0])
    raise KeyError(label)


def build(raw: Path) -> dict:
    names = {key: raw / Path(rel).name for key, rel in FILES.items()}
    overview, nacelle, performance = read_tabular(names["tabular"])
    rotor_rows = [nacelle["Blades"], nacelle["Hub_System"]]
    rotor_mass = sum(float(r[1]) for r in rotor_rows)
    rotor_cm = [sum(float(r[1]) * float(r[c]) for r in rotor_rows) / rotor_mass
                for c in (2, 3, 4)]
    parts = rotor_rows + [nacelle["nacelle"]]
    combined = [sum(float(r[1]) * float(r[c]) for r in parts) / sum(float(r[1]) for r in parts)
                for c in (2, 3, 4)]
    zero, infinite, rows = read_wamit1(names["wamit1"])
    hst = read_hst(names["hst"])
    nms = "Nacelle Mass Properties"
    ov = "Overview"
    return {
        "description": "Turbine, rotor-nacelle and hull hydrodynamic values for the OrcaFlex "
                       "modular generator floating wind model type. Hull geometry, system mass, "
                       "mooring and metocean values are read from basis.yml in this folder.",
        "reference_commit": COMMIT,
        "retrieved": "2026-11-12",
        "source_url": f"https://github.com/IEAWindTask37/IEA-15-240-RWT/tree/{COMMIT}",
        "licence": "Apache License 2.0 (LICENSE at the pinned revision)",
        "extraction": "scripts/fowt/extract_iea15_generator_data.py",
        "source_sha256": {FILES[k]: sha256(p) for k, p in names.items()},
        "turbine": {
            "rotor_diameter": item(float(overview["Rotor diameter [m]"]), "m",
                                   src("tabular", f"{ov} B4")),
            "hub_diameter": item(float(overview["Hub diameter [m]"]), "m",
                                 src("tabular", f"{ov} B14")),
            "hub_height": item(float(overview["Hub height [m]"]), "m",
                               src("tabular", f"{ov} B13")),
            "hub_overhang": item(float(overview["Hub Overhang [m]"]), "m",
                                 src("tabular", f"{ov} B15; negative is upwind of the tower axis")),
            "shaft_tilt": item(float(overview["Shaft tilt angle [deg]"]), "degree",
                               src("tabular", f"{ov} B21")),
            "precone": item(float(overview["Rotor cone angle [deg]"]), "degree",
                            src("tabular", f"{ov} B22")),
            "tower_top_to_hub": item(float(overview["Tower top to hub flange height [m]"]), "m",
                                     src("tabular", f"{ov} B23")),
            "blade_mass": item(float(overview["Blade mass [t]"]) * 1000.0, "kg",
                               src("tabular", f"{ov} B26; t converted to kg")),
            "cut_in_wind_speed": item(float(overview["Cut-in wind speed [m/s]"]), "m/s",
                                      src("tabular", f"{ov} B9")),
            "cut_out_wind_speed": item(float(overview["Cut-out wind speed [m/s]"]), "m/s",
                                       src("tabular", f"{ov} B11")),
            "maximum_rotor_speed": item(float(overview["Maximum rotor speed [rpm]"]), "rpm",
                                        src("tabular", f"{ov} B19")),
            "rotor_mass": item(rotor_mass, "kg", src(
                "tabular", f"{nms} rows Blades and Hub_System column B; summed")),
            "rotor_center_of_mass_from_tower_top": item(rotor_cm, "m", src(
                "tabular", f"{nms} rows Blades and Hub_System C:E; mass-weighted")),
            "nacelle_mass": item(float(nacelle["nacelle"][1]), "kg",
                                 src("tabular", f"{nms} row nacelle column B")),
            "nacelle_center_of_mass_from_tower_top": item(
                [float(nacelle["nacelle"][c]) for c in (2, 3, 4)], "m",
                src("tabular", f"{nms} row nacelle C:E")),
            "rna_mass": item(float(nacelle["RNA"][1]), "kg",
                             src("tabular", f"{nms} row RNA column B")),
            "rna_center_of_mass_from_tower_top": item(
                [float(nacelle["RNA"][c]) for c in (2, 3, 4)], "m",
                src("tabular", f"{nms} row RNA C:E")),
            "rna_component_centre_discrepancy": item(
                [combined[k] - float(nacelle["RNA"][c]) for k, c in enumerate((2, 3, 4))], "m",
                src("tabular", f"{nms}; mass-weighted centre of rows nacelle, Blades and "
                               "Hub_System minus row RNA C:E. The RNA row is not the "
                               "mass-weighted combination of its component rows; the "
                               "generator adopts the RNA row, as basis.yml does")),
        },
        "rotor_performance": {
            "unit": "per column",
            "columns": ["wind speed m/s", "blade pitch degree", "rotor speed rpm",
                        "rotor thrust N", "electrical power W"],
            "value": performance,
            "source": src("tabular", "Rotor Performance rows 2:51 columns A,B,F,H,C; "
                                     "MN and MW converted to N and W; WISDEM steady-state values"),
        },
        "hydrodynamics": {
            "wamit_length_scale": item(read_value(names["hydrodyn"], "WAMITULEN"), "m",
                                       src("hydrodyn", "WAMITULEN")),
            "reference_volume": item(read_value(names["hydrodyn"], "PtfmVol0"), "m^3",
                                     src("hydrodyn", "PtfmVol0")),
            "hydrostatic_stiffness_nondimensional": item(
                hst, "C/(rho g L^k); k=2 heave, 3 heave-rotation, 4 rotation; L=1 m",
                src("hst", "all entries; centre of gravity placed at the origin, so the "
                           "gravity moment-arm term is excluded (HydroData/README.md)")),
            "added_mass_zero_frequency_nondimensional": item(
                zero, "A/(rho L^k); k=3 translation, 4 coupled, 5 rotation; L=1 m",
                src("wamit1", "period -1 (zero frequency) rows")),
            "added_mass_infinite_frequency_nondimensional": item(
                infinite, "A/(rho L^k); k=3 translation, 4 coupled, 5 rotation; L=1 m",
                src("wamit1", "period 0 (infinite frequency) rows")),
            "radiation_by_period_nondimensional": {
                "columns": ["period s", "A11", "A33", "A55", "A15", "A66",
                            "B11", "B33", "B55", "B15", "B66"],
                "unit": "A/(rho L^k); B/(rho omega L^k); L=1 m",
                "value": rows,
                "source": src("wamit1", "positive-period rows, entries 11,33,55,15,66"),
            },
            "additional_quadratic_damping": item(
                read_matrix_after(names["hydrodyn"], "AddBQuad"),
                "N/(m/s)^2 translation; N m/(rad/s)^2 rotation; coupled as given",
                src("hydrodyn", "AddBQuad 6x6")),
        },
    }


def main(argv: list[str]) -> int:
    raw, output = Path(argv[1]), Path(argv[2])
    text = yaml.safe_dump(build(raw), sort_keys=False, width=100)
    output.write_text(text, encoding="utf-8", newline=chr(10))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
