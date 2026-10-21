# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Value checks for synthetic gauge-pressure CSVs; no measured data are used."""

import csv

import pytest

from digitalmodel.data.hydrostatic import read_pressure_csv, write_hydrostatic_csv


def test_pressure_values_survive_csv_retrieval(tmp_path):
    """Hand calculation using OpenStax University Physics Vol. 1 (2016), §14.1.

    Equation 14.9 gives gauge pressure rho*g*h. Chosen synthetic inputs are
    rho=1000 kg/m³, g=10 m/s², h=(0, 2, 5) m, hence (0, 20000, 50000) Pa.
    These inputs are arithmetic fixtures, not measured properties or defaults.
    """
    path = tmp_path / "pressure.csv"
    write_hydrostatic_csv(path, [0, 2, 5], density_kg_m3=1000, gravity_m_s2=10)
    rows = read_pressure_csv(path)
    assert [row["depth_m"] for row in rows] == [0, 2, 5]
    assert [row["pressure_pa"] for row in rows] == [0, 20000, 50000]
    # Assert the persisted answer too, rather than only a recomputation on read.
    with path.open(newline="") as stream:
        assert [float(row["pressure_pa"]) for row in csv.DictReader(stream)] == [
            0,
            20000,
            50000,
        ]


@pytest.mark.parametrize("depth", [-1, float("nan"), float("inf")])
def test_invalid_depth_does_not_overwrite_file(tmp_path, depth):
    """Nonnegative finite depth is the downward-depth domain of Eq. 14.9."""
    path = tmp_path / "pressure.csv"
    path.write_text("existing data\n")
    with pytest.raises(ValueError, match="depth"):
        write_hydrostatic_csv(path, [0, depth], density_kg_m3=1000, gravity_m_s2=10)
    assert path.read_text() == "existing data\n"


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf")])
@pytest.mark.parametrize("parameter", ["density_kg_m3", "gravity_m_s2"])
def test_invalid_material_or_gravity(tmp_path, parameter, value):
    """Positive finite rho and g define this constant-density model's domain."""
    inputs = {"density_kg_m3": 1000, "gravity_m_s2": 10}
    inputs[parameter] = value
    with pytest.raises(ValueError, match=parameter):
        write_hydrostatic_csv(tmp_path / "pressure.csv", [2], **inputs)


@pytest.mark.parametrize(
    "content",
    [
        "depth_m,pressure_kpa\n2,20\n",
        "depth_m,pressure_pa\n2,nan\n",
        "depth_m,pressure_pa\n-2,20\n",
        "depth_m,pressure_pa\n2,-20\n",
        "depth_m,pressure_pa\n2,\n",
    ],
)
def test_retrieval_rejects_invalid_schema_or_values(tmp_path, content):
    """Pa and nonnegative finite values are an explicit CSV interface contract."""
    path = tmp_path / "pressure.csv"
    path.write_text(content)
    with pytest.raises(ValueError):
        read_pressure_csv(path)
