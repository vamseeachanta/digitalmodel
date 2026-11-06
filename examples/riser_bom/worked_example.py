# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic BOM example; supply an external output directory."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from digitalmodel.citations import Citation
from digitalmodel.riser_bom import Configuration, cost_bom, load_cost_basis


def run_example(external_directory: Path) -> dict:
    """Copy only the invented public fixture, then exercise the external loader."""
    example = Path(__file__).resolve().parent
    external = external_directory.resolve()
    repository = example.parents[1]
    if external.is_relative_to(repository):
        raise ValueError("Example output must be external to the repository")
    external.mkdir(parents=True, exist_ok=True)
    target = external / "basis.json"
    # Refuse overwrite: the external directory might already contain real data.
    with target.open("x", encoding="utf-8") as output:
        output.write(
            (example / "synthetic_cost_basis.json").read_text(encoding="utf-8")
        )
    basis = load_cost_basis(target)
    citation = Citation(
        **json.loads(target.read_text(encoding="utf-8"))["rates"][0]["citation"]
    )
    # All numeric inputs: synthetic-bom.md revision 1, Invented arithmetic.
    configuration = Configuration(
        riser_length_m="25",
        joint_length_m="10",
        coated_length_m="25",
        buoyancy_module_count=4,
        ancillary_count=2,
        installation_days="1.5",
        citation=citation,
    )
    return cost_bom(configuration, basis, citation_root=example)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("external_directory", type=Path)
    arguments = parser.parse_args()
    print(json.dumps(run_example(arguments.external_directory), indent=2))


if __name__ == "__main__":
    main()
