# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline worked example of case-insensitive object references.

Basis: Orcina OrcaFlex webhelp, "The model", object names paragraph
(unversioned, accessed 2026-10-21):
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Themodel.htm

These partial model dictionaries exercise name lookup only. They contain no
engineering properties and are not inputs for native loading or simulation.
"""

from copy import deepcopy

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
)


FILES = {
    "definitions.yml": {
        "LineTypes": [{"Name": "ChainType"}],
        "Vessels": [{"Name": "SupportObject"}],
        "6DBuoys": [{"Name": "BuoyObject"}],
        "Shapes": [{"Name": "GuideObject"}],
    },
    "references.yaml": {
        "Lines": [{
            "Name": "line_a",
            "LineType, Length, TargetSegmentLength": [["chaintype"]],
            "Connection, ConnectionX": [["SUPPORTOBJECT"], ["buoyobject"]],
        }],
        "Winches": [{
            "Name": "WinchObject",
            "Connection": ["supportobject", "LINE_A"],
        }],
        "Links": [{
            "Name": "LinkObject",
            "Connection": ["guideobject", "LINE_A"],
        }],
        "Constraints": [{
            "Name": "ConstraintObject",
            "InFrameConnection": "SUPPORTOBJECT",
            "OutFrameConnection": "buoyobject",
            "Connection": "GUIDEOBJECT",
        }],
    },
}


def main() -> None:
    """Print the clean result and the spelling-preserving missing-name error."""
    validator = PostGenerationValidator()
    print(f"Clean findings: {validator.validate_data(FILES)}")

    missing = deepcopy(FILES)
    missing["references.yaml"]["Winches"][0]["Connection"] = [
        "supportobject", "MissingLine",
    ]
    for finding in validator.validate_data(missing):
        print(finding.message)


if __name__ == "__main__":
    main()
