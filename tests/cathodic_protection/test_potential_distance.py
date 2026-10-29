# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic for the first-principles potential-distance model.

Synthetic check inputs are algebraic examples, not environmental design data.
The derivation is in potential-distance-derivation.md, equations P and L.
"""

import csv
import hashlib
import json
import math
from dataclasses import replace
from pathlib import Path

import pytest

from scripts.cathodic_protection import generate_potential_distance as generator
from digitalmodel.cathodic_protection.potential_distance import (
    finite_line_source_potential_shift_mv,
    point_source_potential_shift_mv,
)
from scripts.cathodic_protection.generate_potential_distance import (
    load_basis,
    write_charts,
)


@pytest.mark.parametrize(
    "current,rho,distance,expected",
    [(4 * math.pi, 1, 1, 1000), (2 * math.pi, 2, 4, 250), (8 * math.pi, 0.5, 2, 500)],
)
def test_point_hand_calculations(current, rho, distance, expected):
    """P: 1000 rho I/(4 pi r), independently cancelling pi.

    1000*1*4pi/(4pi*1)=1000 mV;
    1000*2*2pi/(4pi*4)=250 mV;
    1000*0.5*8pi/(4pi*2)=500 mV.
    """
    assert point_source_potential_shift_mv(current, rho, distance) == pytest.approx(expected)


@pytest.mark.parametrize(
    "current,rho,length,distance,expected",
    [
        (4 * math.pi, 1, 2, 1, 1000 * math.log(1 + math.sqrt(2))),
        (8 * math.pi, 0.5, 4, 2, 500 * math.log(1 + math.sqrt(2))),
        (6 * math.pi, 2, 6, 4, 1000 * math.log(2)),
    ],
)
def test_line_hand_calculations(current, rho, length, distance, expected):
    """L: 1000 rho I/(2 pi L) ln((L/2+sqrt((L/2)^2+r^2))/r).

    (4pi,1,2,1): coefficient 1000, logarithm ln(1+sqrt(2));
    (8pi,0.5,4,2): coefficient 500, logarithm ln(1+sqrt(2));
    (6pi,2,6,4): coefficient 1000, logarithm ln((3+5)/4)=ln(2).
    All values are mV, calculated without calling the model.
    """
    assert finite_line_source_potential_shift_mv(current, rho, length, distance) == pytest.approx(expected)


def fields(current, rho, distance):
    """Evaluate both forms with a synthetic two-metre line."""
    return (
        point_source_potential_shift_mv(current, rho, distance),
        finite_line_source_potential_shift_mv(current, rho, 2, distance),
    )


def test_limits_scaling_and_units():
    """ohm.m*A/m = V; SI milli prefix multiplies volts by 1000."""
    assert fields(0, 1, 1) == (0, 0)
    assert fields(1, 1, math.inf) == (0, 0)
    assert max(fields(1, 1, 1e12)) < 1e-9
    assert fields(1, 2, 3) == pytest.approx(tuple(2 * x for x in fields(1, 1, 3)))
    assert fields(2, 1, 3) == pytest.approx(tuple(2 * x for x in fields(1, 1, 3)))
    assert point_source_potential_shift_mv(1, 1, 1) / 1000 == pytest.approx(1 / (4 * math.pi))


def test_line_far_field_and_short_line_recover_point_source():
    point = point_source_potential_shift_mv(1, 1, 1e6)
    line = finite_line_source_potential_shift_mv(1, 1, 2, 1e6)
    assert line == pytest.approx(point, rel=1e-10)
    assert finite_line_source_potential_shift_mv(1, 1, 1e-9, 1) == pytest.approx(
        point_source_potential_shift_mv(1, 1, 1), rel=1e-10
    )
    assert 0 < finite_line_source_potential_shift_mv(1, 1, 2, 1) < point_source_potential_shift_mv(1, 1, 1)


@pytest.mark.parametrize("bad", [-1, math.nan, -math.inf, math.inf])
def test_invalid_current(bad):
    with pytest.raises(ValueError, match="current_a"):
        fields(bad, 1, 1)
    with pytest.raises(ValueError, match="current_a"):
        finite_line_source_potential_shift_mv(bad, 1, 2, 1)


@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf])
def test_invalid_resistivity_and_length(bad):
    with pytest.raises(ValueError, match="resistivity"):
        fields(1, bad, 1)
    with pytest.raises(ValueError, match="resistivity"):
        finite_line_source_potential_shift_mv(1, bad, 2, 1)
    with pytest.raises(ValueError, match="length"):
        finite_line_source_potential_shift_mv(1, 1, bad, 1)


@pytest.mark.parametrize("bad", [0, -1, math.nan, -math.inf])
def test_singular_or_invalid_axis_distance(bad):
    with pytest.raises(ValueError, match="distance"):
        fields(1, 1, bad)
    with pytest.raises(ValueError, match="distance"):
        finite_line_source_potential_shift_mv(1, 1, 2, bad)


def test_published_basis_reproduces_existing_b401_route():
    """Independent check of the existing mass-equivalent B401 calculation.

    Xu (2023), p. 242 section 2: L=2.020 m, m=323.4 kg, rho=0.22,
    driving voltage=0.25 V. Assumed alloy density=2750 kg/m3, as recorded
    in the existing fixture. r=sqrt(323.4/(pi*2.020*2750));
    R=0.22/(2pi*2.020)*(ln(4*2.020/r)-1); I=0.25/R.
    Resistance expression: existing _kernels.long_slender_standoff,
    DNV-RP-B401 (2021), Anode resistance (no standard opened).
    """
    basis = load_basis()
    radius = math.sqrt(323.4 / (math.pi * 2.020 * 2750))
    resistance = 0.22 / (2 * math.pi * 2.020) * (math.log(4 * 2.020 / radius) - 1)
    assert basis.anode_radius_m == pytest.approx(radius)
    assert basis.current_a == pytest.approx(0.25 / resistance)
    assert basis.anode_length_m == 2.020
    assert basis.base_resistivity_ohm_m == 0.22


@pytest.mark.parametrize("operation", ["load", "write"])
@pytest.mark.parametrize("change", ["input", "whitespace"])
def test_changed_fixture_is_rejected_before_calculation(tmp_path, monkeypatch, operation, change):
    """Reject changed source bytes before calculation or output creation."""
    basis = load_basis()
    source = (generator.ROOT / generator.FIXTURE).read_bytes()
    if change == "input":
        changed = source.replace(b"323.4", b"324.4")
        assert changed != source
    else:
        changed = source + b"\n"
    fixture = tmp_path / generator.FIXTURE
    fixture.parent.mkdir(parents=True)
    fixture.write_bytes(changed)
    monkeypatch.setattr(generator, "ROOT", tmp_path)

    def unexpected_calculation(*args, **kwargs):
        pytest.fail("Unverified fixture reached calculation")

    monkeypatch.setattr(generator._kernels, "equivalent_radius_from_mass", unexpected_calculation)
    output = tmp_path / "charts"
    with pytest.raises(ValueError, match="fixture SHA-256"):
        if operation == "load":
            load_basis()
        else:
            write_charts(output, basis)
    assert not output.exists()


@pytest.mark.parametrize("field,value", [
    ("current_a", 1.0),
    ("base_resistivity_ohm_m", 0.23),
    ("anode_length_m", 3.0),
    ("anode_radius_m", 0.5),
    ("net_alloy_mass_kg", 1.0),
    ("alloy_density_kg_m3", 1.0),
    ("driving_voltage_v", 1.0),
    ("edition", "2005"),
    ("input_sha256", "0" * 64),
])
def test_supplied_basis_must_match_verified_fixture(tmp_path, field, value):
    """A supplied calculation cannot retain provenance from a different basis."""
    basis = replace(load_basis(), **{field: value})
    output = tmp_path / "charts"
    with pytest.raises(ValueError, match="basis does not match"):
        write_charts(output, basis)
    assert not output.exists()


def test_regeneration_matches_committed_data(tmp_path):
    basis = load_basis()
    paths = write_charts(tmp_path, basis)
    committed = Path(__file__).resolve().parents[2] / "docs/domains/cathodic_protection/data/potential-distance"
    assert {p.name for p in paths} == {
        "point-source.csv", "finite-line-source.csv", "values.csv", "basis.json"
    }
    for path in paths:
        assert path.read_bytes() == (committed / path.name).read_bytes()


def test_every_csv_and_manifest_row_matches_independent_equations(tmp_path):
    """Check equations P/L directly, with the logarithmic line integral."""
    basis = load_basis()
    write_charts(tmp_path, basis)
    manifest = {r["quantity"]: r for r in csv.DictReader((tmp_path / "values.csv").open())}
    assert len(manifest) == 248  # two forms * four illustrative resistivities * 31 samples
    for form in ("point-source", "finite-line-source"):
        rows = list(csv.DictReader((tmp_path / f"{form}.csv").open()))
        assert len(rows) == 124
        for rho in (0.20, 0.22, 0.25, 0.30):
            curve = [r for r in rows if float(r["resistivity_ohm_m"]) == rho]
            assert [float(r["distance_from_surface_m"]) for r in curve] == list(range(31))
            assert all(float(a["potential_shift_mv"]) > float(b["potential_shift_mv"]) for a, b in zip(curve, curve[1:]))
        for row in rows:
            gap, rho = float(row["distance_from_surface_m"]), float(row["resistivity_ohm_m"])
            axis_distance = float(row["axis_distance_m"])
            assert axis_distance == basis.anode_radius_m + gap
            assert float(row["current_a"]) == basis.current_a  # fixed current across all curves
            if form == "point-source":
                expected = 1000 * rho * basis.current_a / (4 * math.pi * axis_distance)
            else:
                half_length = basis.anode_length_m / 2
                integral = 2 * math.log(
                    (half_length + math.sqrt(half_length**2 + axis_distance**2)) / axis_distance
                )
                expected = 1000 * rho * (basis.current_a / basis.anode_length_m) / (4 * math.pi) * integral
            assert float(row["potential_shift_mv"]) == pytest.approx(expected, rel=1e-12)
            assert "screening" in row["model_label"]
            assert "not a structure potential prediction" in row["model_label"]
            quantity = f"{form}:rho={rho}:gap={gap}"
            assert float(manifest[quantity]["value"]) == pytest.approx(expected, rel=1e-12)
            assert manifest[quantity]["unit"] == "mV"
            assert manifest[quantity]["source"]
    metadata = json.loads((tmp_path / "basis.json").read_text())
    assert metadata["basis"]["current_a"] == basis.current_a
    digest = hashlib.sha256((generator.ROOT / generator.FIXTURE).read_bytes()).hexdigest()
    assert metadata["input_sha256"] == digest
    assert metadata["basis"]["input_sha256"] == digest
    assert metadata["review_status"] == "pending"
