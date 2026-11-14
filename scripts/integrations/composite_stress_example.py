# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Export library-computed material stresses for a synthetic elastic CFT.

Run from the repository root with ``python -m scripts.integrations.composite_stress_example
--output-dir RESULTS``. Nothing is calculated or written on import.
"""

import argparse
import csv
import hashlib
import json
import math
from datetime import datetime, timezone
from importlib.metadata import version
from pathlib import Path

from scripts.integrations.sectionproperties_composite_poc import run_cft_case

import matplotlib.pyplot as plt
from sectionproperties.analysis import Section
from sectionproperties.post.stress_post import StressPost
from sectionproperties.pre import Material


def _write_material_stresses(
    section: Section, post: StressPost, destination: Path
) -> None:
    """Retain each material's connectivity, including shared and zero-stress nodes."""
    with destination.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(["material", "node_id", "x_mm", "y_mm", "sigma_zz_MPa"])
        for group, result in zip(post.material_groups, post.get_stress(), strict=True):
            nodes = {
                int(node) for element in group.elements for node in element.node_ids
            }
            for node in sorted(nodes):
                x, y = section.mesh_nodes[node]
                writer.writerow(
                    [group.material.name, node, x, y, result["sig_zz"][node]]
                )


def _plot_stresses(post: StressPost, destination: Path) -> None:
    """Plot the library's masked material fields with explicit physical units."""
    fig, ax = plt.subplots()
    try:
        post.plot_stress(
            stress="zz",
            ax=ax,
            render=False,
            normalize=False,
            colorbar_label="Normal stress (MPa)",
            title="Synthetic CFT: axial strain and bending",
        )
        ax.set_xlabel("x (mm)")
        ax.set_ylabel("y (mm)")
        fig.savefig(destination, dpi=150, bbox_inches="tight")
    finally:
        plt.close(fig)


def _receipt(output: Path, stiffness: dict, inputs: dict, mesh: dict) -> dict:
    """Record units, assumptions and digests of the three payloads read back."""
    payloads = [
        "sectionproperties_cft_results.json",
        "composite_stress.png",
        "composite_stress.csv",
    ]
    return {
        "sectionproperties_version": version("sectionproperties"),
        "created_utc": datetime.now(timezone.utc).isoformat(),
        "inputs": inputs,
        "mesh": mesh,
        "units": {"length": "mm", "stress": "MPa", "force": "N", "moment": "N mm"},
        "stiffness": {
            key: stiffness[key] for key in ("EA_N", "EIxx_Nmm2", "Ixx_steel_mm4")
        },
        "actions": {
            "N_N": inputs["epsilon0"] * stiffness["EA_N"],
            "Mxx_Nmm": inputs["kappa_per_mm"] * stiffness["EIxx_Nmm2"],
        },
        "assumptions": [
            "Synthetic geometry, materials and imposed kinematics",
            "Perfect bond and plane sections; linear elastic, uncracked materials",
            "Concentric section centred at the origin; positive Mxx raises stress at positive y",
            "No shear, torsion, bond transfer or capacity assessment",
        ],
        "tolerance": {
            "relative_stress": (2 * math.pi / mesh["n"]) ** 2 / 3
            + 10 * math.sqrt(math.ulp(1.0)),
            "absolute_stress_MPa": 1e-6,
            "coordinate_mm": 1e-8,
        },
        "outputs": payloads + ["composite_stress_receipt.json"],
        "sha256": {
            name: hashlib.sha256((output / name).read_bytes()).hexdigest()
            for name in payloads
        },
    }


def run_example(
    output_dir: Path | str,
    *,
    epsilon0: float = 1e-4,
    kappa: float = 1e-6,
    n: int = 64,
    mesh_area: float = 4.0,
) -> dict:
    """Run the synthetic example; return the saved receipt after reading it back.

    Strain is dimensionless, curvature is per mm, and mesh_area is in mm squared.
    n is the polygon side count. No engineering load or capacity is inferred.
    """
    if not all(math.isfinite(value) for value in (epsilon0, kappa)):
        raise ValueError("Imposed strain and curvature must be finite")
    output = Path(output_dir)
    steel = Material("Steel", 210_000.0, 0.3, 300.0, 1.0, "steelblue")
    concrete = Material("Concrete", 35_000.0, 0.2, 30.0, 1.0, "lightgrey")
    section, stiffness = run_cft_case(
        outer_diameter=80.0,
        wall_thickness=8.0,
        steel_material=steel,
        concrete_material=concrete,
        output_dir=output,
        n=n,
        mesh_sizes=(mesh_area, mesh_area),
    )
    post = section.calculate_stress(
        n=epsilon0 * stiffness["EA_N"],
        mxx=kappa * stiffness["EIxx_Nmm2"],
    )
    _write_material_stresses(section, post, output / "composite_stress.csv")
    _plot_stresses(post, output / "composite_stress.png")
    receipt = _receipt(
        output,
        stiffness,
        {
            "outer_diameter_mm": 80.0,
            "wall_thickness_mm": 8.0,
            "steel_E_MPa": steel.elastic_modulus,
            "concrete_E_MPa": concrete.elastic_modulus,
            "epsilon0": epsilon0,
            "kappa_per_mm": kappa,
        },
        {"n": n, "maximum_triangle_area_mm2": mesh_area},
    )
    destination = output / "composite_stress_receipt.json"
    destination.write_text(
        json.dumps(receipt, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    saved: dict = json.loads(destination.read_text(encoding="utf-8"))
    return saved


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    run_example(parser.parse_args().output_dir)
