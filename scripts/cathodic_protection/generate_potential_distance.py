# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Write two screening chart CSVs and their traceability manifest.

Run with PYTHONPATH=src python scripts/cathodic_protection/generate_potential_distance.py
--output-dir <destination>. No report page or approval record is generated.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from dataclasses import asdict, dataclass
from pathlib import Path

import yaml

from digitalmodel.cathodic_protection import _kernels
from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeType,
    calculate_anode_resistance,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeMaterial,
    design_driving_voltage,
)
from digitalmodel.cathodic_protection.potential_distance import (
    finite_line_source_potential_shift_mv,
    point_source_potential_shift_mv,
)

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = "tests/fixtures/cathodic_protection/workflow_inputs/published_jacket_84m.yml"
INPUT_REVISION = "d34bda5cf18d11be48b17c7ba94e5c8127af2bf2"
# SHA-256 of the exact fixture bytes at INPUT_REVISION; verify before parsing.
INPUT_SHA256 = "38823145d7dc9dedd7ef13a1b4c4ebe73ef99ffafd1c6a6baf3ff1bd7a61eeed"
DERIVATION = "docs/domains/cathodic_protection/potential-distance-derivation.md"
MODEL_LABEL = "screening model, not a structure potential prediction"
# Requested illustrative samples, not a cited seawater range. See derivation.
RESISTIVITIES = (0.20, 0.22, 0.25, 0.30)
# Assumed plotting extent and resolution; member spacing is not established.
SURFACE_GAPS_M = tuple(float(i) for i in range(31))


@dataclass(frozen=True)
class ChartBasis:
    """Fresh mass-equivalent anode from the existing published-jacket fixture."""

    current_a: float
    base_resistivity_ohm_m: float
    anode_length_m: float
    anode_radius_m: float
    net_alloy_mass_kg: float
    alloy_density_kg_m3: float
    driving_voltage_v: float
    edition: str
    input_sha256: str


def load_basis() -> ChartBasis:
    """Reuse the existing B401 fresh-anode resistance route without rounding.

    Xu (2023), p. 242 section 2 supplies mass, length, resistivity and
    driving voltage. The fixture records the assumed net-mass interpretation,
    alloy density and edition. The existing B401 table lookup supplies its
    driving voltage; no standard document is opened. No section-based builder
    is present in the assessed base; this is the mass-equivalent cylinder.
    """
    source = (ROOT / FIXTURE).read_bytes()
    digest = hashlib.sha256(source).hexdigest()
    if digest != INPUT_SHA256:
        raise ValueError("Input fixture SHA-256 does not match the declared revision")
    inputs = yaml.safe_load(source.decode("utf-8"))["inputs"]
    anode = inputs["anode"]
    length = anode["length_m"]
    mass = anode["individual_anode_mass_kg"]
    rho = inputs["environment"]["seawater_resistivity_ohm_m"]
    density = anode.get("density_kg_m3", _kernels.ANODE_DENSITY_ALZNI)
    radius = _kernels.equivalent_radius_from_mass(mass, length, density)
    edition = inputs["design_data"]["edition"]
    voltage = design_driving_voltage(AnodeMaterial.ALUMINIUM, edition).value
    resistance = calculate_anode_resistance(AnodeType.STAND_OFF, length, radius, rho)
    current = _kernels.anode_current_output(voltage, resistance)
    return ChartBasis(current, rho, length, radius, mass, density, voltage, edition, digest)


def write_charts(output_dir: Path, basis: ChartBasis) -> list[Path]:
    """Write one CSV per form, input provenance and one manifest row per value.

    Each chart has one curve per requested resistivity. Current is held at
    its base-resistivity B401 output to isolate the field's rho dependence;
    it is not a coupled sacrificial-anode current/resistivity sensitivity.
    Source bytes and every supplied basis field must match the verified
    fixture calculation before any output directory or file is written.
    """
    if basis != load_basis():
        raise ValueError("Supplied basis does not match the verified fixture calculation")
    output_dir.mkdir(parents=True, exist_ok=True)
    paths = []
    manifest = []
    for form in ("point-source", "finite-line-source"):
        rows = []
        for rho in RESISTIVITIES:
            for gap in SURFACE_GAPS_M:
                axis = gap + basis.anode_radius_m
                if form == "point-source":
                    potential = point_source_potential_shift_mv(basis.current_a, rho, axis)
                else:
                    potential = finite_line_source_potential_shift_mv(
                        basis.current_a, rho, basis.anode_length_m, axis
                    )
                rows.append({
                    "distance_from_surface_m": gap,
                    "axis_distance_m": axis,
                    "resistivity_ohm_m": rho,
                    "current_a": basis.current_a,
                    "anode_length_m": basis.anode_length_m,
                    "anode_radius_m": basis.anode_radius_m,
                    "potential_shift_mv": potential,
                    "model_label": MODEL_LABEL,
                })
                quantity = f"{form}:rho={rho}:gap={gap}"
                manifest.append({
                    "quantity": quantity,
                    "value": potential,
                    "unit": "mV",
                    "basis": "prescribed fixed current; uniform infinite electrolyte; screening; review pending",
                    "source": f"{DERIVATION}; equation {'P' if form == 'point-source' else 'L'}; basis.json",
                    "phrases": quantity,
                })
        path = output_dir / f"{form}.csv"
        _write_csv(path, rows)
        paths.append(path)
    path = output_dir / "values.csv"
    _write_csv(path, manifest)
    paths.append(path)
    path = output_dir / "basis.json"
    metadata = {
        "basis": asdict(basis),
        "input_fixture": FIXTURE,
        "input_revision": INPUT_REVISION,
        "input_sha256": basis.input_sha256,
        "published_source": {
            "citation": "Xu (2023), Engineering Advances 3(3), 241-245",
            "doi": "10.26855/ea.2023.06.017",
            "locator": "p. 242, section 2",
            "url": "https://www.hillpublisher.com/UpFile/202308/20230803150221.pdf",
        },
        "current_basis": "initial output at base resistivity, reused B401 mass-equivalent cylinder route; fixed across curves",
        "resistance_source": "existing _kernels.long_slender_standoff; DNV-RP-B401 (2021), Anode resistance",
        "driving_voltage_source": "existing b401_tables.design_driving_voltage; DNV-RP-B401 (2021), Design driving voltage; also Xu (2023), p. 242 section 2",
        "assumptions": "net alloy mass interpretation, alloy density and edition assumed, based on previous experience; fixture verification_evidence.assumptions",
        "distance_basis": "surface gap 0 to 30 m at 1 m intervals; illustrative sampling assumed, based on previous experience; member spacing not published",
        "resistivity_basis": "0.22 ohm.m = 22 ohm.cm from Xu (2023), p. 242 section 2; 0.20, 0.25, 0.30 illustrative until replaced by a cited range",
        "model_label": MODEL_LABEL,
        "primary_plot": "finite-line-source.csv; point-source.csv is the comparison",
        "review_status": "pending",
        "derivation": DERIVATION,
    }
    path.write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    paths.append(path)
    return paths


def _write_csv(path: Path, rows: list[dict]) -> None:
    """Serialize floats without report rounding for independent reproduction."""
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    """Write the requested data bundle to an explicit destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    write_charts(args.output_dir, load_basis())


if __name__ == "__main__":
    main()
