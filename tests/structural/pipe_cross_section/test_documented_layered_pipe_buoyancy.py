# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented example and check its independent circle integrals."""

import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
DOCUMENT = ROOT / (
    "src/digitalmodel/structural/pipe_cross_section/docs/"
    "worked-layered-pipe-buoyancy.md"
)
# The document's Inputs and Derivation sections give these synthetic densities
# and the exact coefficients of pi from the independent circle integrals.
STEEL_AREA = math.pi * (123201 / 64000000)
COATING_AREA = math.pi * (8631 / 8000000)
EXPECTED = {
    "bare": {
        "layers": [(STEEL_AREA, 7800.25, 180.0, 200.25, 10.125)],
        "displaced_area": math.pi * (641601 / 64000000),
        "summary_mass": 47.2,
        "summary_displaced": 0.0315,
        "summary_water": 32.1,
        "summary_submerged": 15.0,
    },
    "coated": {
        "layers": [
            (STEEL_AREA, 7800.25, 180.0, 200.25, 10.125),
            (COATING_AREA, 1200.75, 200.25, 210.75, 5.25),
        ],
        "displaced_area": math.pi * (710649 / 64000000),
        "summary_mass": 51.2,
        "summary_displaced": 0.0349,
        "summary_water": 35.6,
        "summary_submerged": 15.6,
    },
}
# Only a few floating operations are involved. A relative allowance of 1e-12
# exceeds their accumulated binary64 roundoff but resolves far below each
# displayed rounding increment. Absolute floors cover small areas and masses.
RELATIVE_TOLERANCE = 1e-12
AREA_TOLERANCE = 1e-15  # m²
MASS_TOLERANCE = 1e-10  # kg/m


@pytest.fixture(scope="module")
def documented_results(tmp_path_factory):
    """Run the sole Python fence verbatim, outside the source checkout."""
    text = DOCUMENT.read_text(encoding="utf-8")
    blocks = re.findall(r"^```python\n(.*?)^```$", text, re.MULTILINE | re.DOTALL)
    assert len(blocks) == 1, "The document must contain one executable example"
    work = tmp_path_factory.mktemp("documented-pipe")
    script = work / "example.py"
    script.write_text(blocks[0], encoding="utf-8")
    output = work / "results"
    environment = os.environ.copy()
    environment["PYTHONPATH"] = str(ROOT / "src")
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    subprocess.run(
        [sys.executable, str(script), str(output)],
        cwd=work,
        env=environment,
        check=True,
        capture_output=True,
        text=True,
        timeout=30,
    )
    result_file = output / "layered-pipe-buoyancy.json"
    assert {p.relative_to(work).as_posix() for p in work.rglob("*")} == {
        "example.py", "results", "results/layered-pipe-buoyancy.json"
    }
    results = json.loads(result_file.read_text(encoding="utf-8"))
    assert set(results) == set(EXPECTED)
    return results


@pytest.mark.parametrize("case", EXPECTED)
def test_documented_layered_pipe_buoyancy(documented_results, case):
    """Check every annulus and displaced disk before the mass subtraction."""
    expected = EXPECTED[case]
    live = documented_results[case]["live"]
    assert len(live["layers"]) == len(expected["layers"])
    for actual, (area, density, inner, outer, thickness) in zip(
        live["layers"], expected["layers"], strict=True
    ):
        assert actual["inner_diameter_mm"] == inner
        assert actual["outer_diameter_mm"] == outer
        assert actual["thickness_mm"] == thickness
        assert actual["density_kg_m3"] == density
        assert actual["area_m2"] == pytest.approx(
            area, rel=RELATIVE_TOLERANCE, abs=AREA_TOLERANCE
        )
        assert actual["mass_kg_m"] == pytest.approx(
            area * density, rel=RELATIVE_TOLERANCE, abs=MASS_TOLERANCE
        )
    mass = sum(area * density for area, density, *_ in expected["layers"])
    displaced = expected["displaced_area"]
    water_mass = displaced * 1020.5  # Synthetic water density from Inputs.
    assert live["displaced_area_m2"] == pytest.approx(
        displaced, rel=RELATIVE_TOLERANCE, abs=AREA_TOLERANCE
    )
    for field, value in (
        ("mass_in_air_kg_m", mass),
        ("displaced_water_mass_kg_m", water_mass),
        ("submerged_mass_kg_m", mass - water_mass),
    ):
        assert live[field] == pytest.approx(
            value, rel=RELATIVE_TOLERANCE, abs=MASS_TOLERANCE
        )


@pytest.mark.parametrize("case", EXPECTED)
def test_documented_pipe_buoyancy_serialization(documented_results, case):
    """Check unrounded configuration separately from rounded derived fields."""
    result = documented_results[case]
    expected = EXPECTED[case]
    serialized = result["serialized"]
    config = serialized["configuration"]
    # These fields retain their input precision; the mass fields do not.
    assert config["steel_od_mm"] == 200.25
    assert config["steel_wt_mm"] == 10.125
    assert config["steel_density"] == 7800.25
    assert config["lpp_density"] == 1200.75
    assert config["lpp_thickness_mm"] == (0.0 if case == "bare" else 5.25)
    assert config["concrete_thickness_mm"] == 0.0
    assert config["seawater_density"] == 1020.5
    assert config["internal_contents"] == "air"
    assert len(serialized["layers"]) == len(expected["layers"])
    for actual, (area, density, inner, outer, thickness) in zip(
        serialized["layers"], expected["layers"], strict=True
    ):
        assert actual["ID_mm"] == round(inner, 1)
        assert actual["OD_mm"] == round(outer, 1)
        assert actual["Thickness_mm"] == round(thickness, 2)
        assert actual["Density_kg_m3"] == density
        assert actual["Area_m2_m"] == round(area, 5)
        assert actual["Weight_kg_m"] == round(area * density, 1)
    summary = serialized["summary"]
    assert summary["Inner_Diameter_mm"] == 180.0
    assert summary["Outer_Diameter_mm"] == round(expected["layers"][-1][3], 1)
    assert summary["Total_Weight_Air_kg_m"] == expected["summary_mass"]
    assert summary["Seawater_Density_kg_m3"] == 1020.5
    assert summary["Internal_Contents"] == "air"
    for field, expected_field in (
        ("Displaced_Volume_m3_m", "summary_displaced"),
        ("Buoyancy_Force_kg_m", "summary_water"),
        ("Submerged_Weight_kg_m", "summary_submerged"),
    ):
        assert summary[field] == expected[expected_field]
        assert serialized["buoyancy"][field] == expected[expected_field]
    assert summary["Is_Buoyant"] is False
    assert serialized["buoyancy"]["Is_Buoyant"] is False
    assert result["live"]["submerged_mass_kg_m"] != summary["Submerged_Weight_kg_m"]
