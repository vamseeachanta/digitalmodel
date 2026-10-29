# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the ASCII catalog example against independent hand calculations."""

import json
import runpy
import tempfile
from pathlib import Path


def test_calculix_ascii_catalog_example(tmp_path, monkeypatch, capsys):
    """Synthetic uniaxial stress is 120 MPa; sqrt(3² + 4² + 12²) is 13 mm.

    Independent calculation: sqrt(((120-0)² + (0-0)² + (0-120)²)/2)
    = sqrt(14400) = 120 MPa, with every shear component zero.
    Node identifier 1 and all input quantities are invented for this example.
    """
    monkeypatch.setattr(tempfile, "tempdir", str(tmp_path))
    example = (
        Path(__file__).resolve().parents[3]
        / "examples/calculix/ascii_adapter.py"
    )

    runpy.run_path(str(example), run_name="__main__")

    assert json.loads(capsys.readouterr().out) == {
        "units": {"stress": "MPa", "displacement": "mm"},
        "stress_at_node": {
            "sxx": 120.0,
            "syy": 0.0,
            "szz": 0.0,
            "sxy": 0.0,
            "syz": 0.0,
            "szx": 0.0,
        },
        "displacement_at_node": {
            "dx": 3.0,
            "dy": 4.0,
            "dz": 12.0,
            "magnitude": 13.0,
        },
        "max_von_mises": 120.0,
        "max_displacement": 13.0,
    }
