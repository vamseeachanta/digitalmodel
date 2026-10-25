# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic oracles and public output contracts for rigid tanks."""

import importlib.util
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/cfd/sloshing_analytical_fixture.py"
UNITS = {
    "length_m": "m", "height_m": "m", "depth_m": "m",
    "displacement_m": "m", "gravity_m_s2": "m/s^2", "period_ratio": "1",
}


@pytest.fixture(scope="module")
def analytical():
    spec = importlib.util.spec_from_file_location("analytical_sloshing", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def inputs(**changes):
    values = dict(length_m=1.0, height_m=1.0, depth_m=0.5,
                  displacement_m=0.001, gravity_m_s2=9.81, period_ratio=8.0,
                  units=UNITS.copy())
    values.update(changes)
    return values


@pytest.mark.parametrize("x", [0.01, 10.0])
def test_asymptotic_bounds_without_tanh_oracle(analytical, x):
    length, gravity = 2.0, 9.81
    k = math.pi / length
    omega = analytical.natural_frequency(length, x / k, gravity)
    normalized = omega**2 / (gravity * k)
    lower = x - x**3 / 3 if x < 1 else 1 - 2 * math.exp(-2 * x)
    upper = x if x < 1 else 1.0
    assert lower - 1e-12 * lower <= normalized <= upper * (1 + 1e-12)


def test_scaling_period_and_depth(analytical):
    base = analytical.build_fixture(inputs())["results"]
    gravity = analytical.build_fixture(inputs(gravity_m_s2=39.24))["results"]
    scaled = analytical.build_fixture(inputs(length_m=4, height_m=4, depth_m=2,
                                             displacement_m=0.004))["results"]
    assert gravity["omega1_rad_s"] == pytest.approx(2 * base["omega1_rad_s"], rel=1e-12, abs=0.0)
    assert gravity["t1_s"] == pytest.approx(base["t1_s"] / 2, rel=1e-12, abs=0.0)
    assert scaled["omega1_rad_s"] == pytest.approx(base["omega1_rad_s"] / 2, rel=1e-12, abs=0.0)
    assert scaled["t1_s"] == pytest.approx(2 * base["t1_s"], rel=1e-12, abs=0.0)
    assert base["t1_s"] == pytest.approx(2 * math.pi / base["omega1_rad_s"], rel=1e-12, abs=0.0)
    frequencies = [analytical.natural_frequency(1.0, h, 9.81) for h in (0.01, 0.1, 0.5)]
    assert frequencies[0] < frequencies[1] < frequencies[2]


def test_modes_include_odd_and_even_without_forcing_coupling_claim(analytical):
    fixture = analytical.build_fixture(inputs())
    assert [row["mode"] for row in fixture["modes"]] == [1, 2, 3, 4]
    for row in fixture["modes"]:
        n = row["mode"]
        expected = analytical.natural_frequency(1 / n, 0.5, 9.81)
        assert row["omega_rad_s"] == pytest.approx(expected, rel=1e-12, abs=0.0)
        assert row["wave_number_rad_m"] == pytest.approx(n * math.pi, rel=1e-12, abs=0.0)


def test_conversion_acceleration_and_excursion(analytical):
    displacement = analytical.length_to_m(1.0, "mm")
    assert displacement == 0.001
    result = analytical.build_fixture(inputs(displacement_m=displacement))["results"]
    omega = 2 * math.pi / (8 * result["t1_s"])
    assert result["excitation_omega_rad_s"] == pytest.approx(omega, rel=1e-12, abs=0.0)
    assert result["acceleration_amplitude_m_s2"] == pytest.approx(displacement * omega**2, rel=1e-12, abs=0.0)
    assert result["quasi_static_end_to_end_m"] == pytest.approx(displacement * omega**2 / 9.81, rel=1e-12, abs=0.0)
    double = analytical.build_fixture(inputs(displacement_m=2 * displacement))["results"]
    assert double["quasi_static_end_to_end_m"] == pytest.approx(2 * result["quasi_static_end_to_end_m"], rel=1e-12, abs=0.0)


@pytest.mark.parametrize("ratio,classification", [
    (math.nextafter(8, 0), "DYNAMIC_REFERENCE"),
    (8.0, "QUASI_STATIC_ANCHOR"), (math.nextafter(8, math.inf), "QUASI_STATIC_ANCHOR"),
])
def test_exact_period_ratio_threshold(analytical, ratio, classification):
    fixture = analytical.build_fixture(inputs(period_ratio=ratio))
    assert fixture["results"]["classification"] == classification
    assert fixture["criteria"]["physical_acceptance"] == "NOT_ESTABLISHED"


@pytest.mark.parametrize("field", list(UNITS))
@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf, -math.inf, True, "1 m"])
def test_invalid_input_rejected(analytical, field, value):
    with pytest.raises(ValueError):
        analytical.build_fixture(inputs(**{field: value}))


@pytest.mark.parametrize("changes", [
    {"depth_m": 1.01}, {"acceleration_m_s2": 0.1}, {"acceleration_amplitude_m_s2": 0.1},
    {"units": {**UNITS, "displacement_m": "mm"}}, {"units": {}},
    {"units": {**UNITS, "gravity_m_s2": "m/s"}}, {"unexpected": 1},
    {"length_m": 1e-320}, {"gravity_m_s2": 1e308}, {"period_ratio": 1e-300},
])
def test_invalid_combinations_and_unrepresentable_results(analytical, changes):
    with pytest.raises(ValueError):
        analytical.build_fixture(inputs(**changes))


@pytest.mark.parametrize("unit", ["cm", "ft", "m/s^2", "", None])
def test_invalid_conversion_units(analytical, unit):
    with pytest.raises(ValueError):
        analytical.length_to_m(1, unit)


def test_evidence_gaps_do_not_become_qualified(analytical):
    fixture = analytical.build_fixture(inputs())
    assert fixture["model_revision"] == fixture["criteria_revision"] == "analytical-v1"
    assert fixture["inputs"]["units"] == UNITS
    assert fixture["assumptions"]["gravity_m_s2"] == "SYNTHETIC-ASSUMPTION"
    assert fixture["evidence_status"]["physical_validation"] == "BLOCKED"
    assert fixture["evidence_status"]["synthetic_model_regression"] == "ANALYTICAL_REFERENCE"
    assert {row["id"]: row["status"] for row in fixture["missing"]} == {
        "numerical-sway-reference": "BLOCKED",
        "sway-probe-boundary-equivalence": "UNVERIFIED",
        "calibrated-pitch-forcing": "BLOCKED",
        "native-input-output-evidence": "UNVERIFIED",
        "source-specific-public-numerical-reuse": "UNVERIFIED",
    }
    assert "QUALIFIED" not in json.dumps(fixture)
    assert fixture["counts"]["qualified_physical_results"] == 0
    assert fixture["counts"]["synthetic_reference_points"] == len(fixture["reference_points"]) == 2
    for source in fixture["sources"]:
        assert re.fullmatch(r"[a-f0-9]{64}", source["sha256"])
        assert source["verification"] == "SUPPLIED-HISTORICAL-ASSERTION"
    metadata = fixture["cited_metadata"]
    assert metadata["numerical_reuse"] == "UNVERIFIED"
    assert metadata["used_in_calculation"] is False
    assert metadata["historical_proposal"]["omega1_rad_s"] == 6.058
    assert [row["period_ratio"] for row in fixture["reference_points"]] == [1.1, 8.0]


def cli(out_dir, *extra):
    args = [sys.executable, str(SCRIPT), "--length-m", "1", "--height-m", "1",
            "--depth-m", "0.5", "--displacement-m", "0.001", "--gravity-m-s2", "9.81",
            "--period-ratio", "8", "--out-dir", str(out_dir), *extra]
    env = {k: v for k, v in os.environ.items()
           if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    return subprocess.run(args, env=env, capture_output=True, text=True, timeout=20)


def test_cli_deterministic_json_html_and_committed_readback(analytical, tmp_path):
    outputs = []
    for name in ("first", "second"):
        out_dir = tmp_path / name
        run = cli(out_dir)
        assert run.returncode == 0, run.stderr
        outputs.append([p.read_bytes() for p in sorted(out_dir.iterdir())])
        data = json.loads((out_dir / "sloshing-analytical-fixture.json").read_text())
        assert data == analytical.build_fixture(inputs())
        html = (out_dir / "sloshing-analytical-fixture.html").read_text()
        embedded = re.search(r'<script type="application/json" id="fixture-data">(.*?)</script>', html, re.S)
        assert embedded is not None
        assert json.loads(embedded.group(1)) == data
        assert re.search(r'</section>\s*<section id="revision-history">', html)
        for gap in data["missing"]:
            assert gap["id"] in html and gap["status"] in html
        for suffix in ("json", "html"):
            path = f"sloshing-analytical-fixture.{suffix}"
            assert (ROOT / "docs/benchmarks" / path).read_bytes() == (out_dir / path).read_bytes()
    assert outputs[0] == outputs[1]


@pytest.mark.parametrize("extra", [
    ("--acceleration-m-s2", "1"), ("--length-m", "1 mm"), ("--depth-m", "0"),
    ("--gravity-m-s2", "nan"), ("--period-ratio", "inf"),
])
def test_cli_rejects_invalid_values_without_outputs(tmp_path, extra):
    out_dir = tmp_path / "invalid"
    run = cli(out_dir, *extra)
    assert run.returncode == 2
    assert not out_dir.exists()


@pytest.mark.parametrize("mode", [0, -1, 1.5, True, "1"])
def test_noninteger_or_nonpositive_mode_rejected(analytical, mode):
    with pytest.raises(ValueError):
        analytical.natural_frequency(1, 0.5, 9.81, mode)


def test_near_mode_proxy_and_report_units_are_explicit(analytical, tmp_path):
    fixture = analytical.build_fixture(inputs())
    near, off = fixture["reference_points"]
    assert near["classification"] == "DYNAMIC_REFERENCE"
    assert "NOT a dynamic or near-resonant response amplitude" in near["excursion_interpretation"]
    assert off["classification"] == "QUASI_STATIC_ANCHOR"
    assert "NOT_ESTABLISHED" in off["excursion_interpretation"]
    analytical.write_fixture(fixture, tmp_path)
    html = (tmp_path / "sloshing-analytical-fixture.html").read_text()
    for header in ("omega1_rad_s [rad/s]", "t1_s [s]", "quasi_static_end_to_end_m [m]"):
        assert f"<th>{header}</th>" in html


def test_cited_metadata_cannot_change_computed_results(analytical, monkeypatch):
    before = analytical.build_fixture(inputs())
    monkeypatch.setitem(analytical.CITED_METADATA, "sway", {"length_m": 1234})
    after = analytical.build_fixture(inputs())
    assert after["results"] == before["results"]
    assert after["modes"] == before["modes"]
    assert after["reference_points"] == before["reference_points"]
    assert after["evidence_status"] == before["evidence_status"]


def test_report_escapes_text_and_embedded_json(analytical, tmp_path):
    fixture = analytical.build_fixture(inputs())
    fixture["next_evidence_step"] = '</script><img src=x onerror="alert(1)">'
    analytical.write_fixture(fixture, tmp_path)
    html = (tmp_path / "sloshing-analytical-fixture.html").read_text()
    assert fixture["next_evidence_step"] not in html
    embedded = re.search(r'<script type="application/json" id="fixture-data">(.*?)</script>', html, re.S)
    assert json.loads(embedded.group(1)) == fixture


def test_nonzero_acceleration_stays_visible_in_html(analytical, tmp_path):
    fixture = analytical.build_fixture(inputs())
    analytical.write_fixture(fixture, tmp_path)
    html = (tmp_path / "sloshing-analytical-fixture.html").read_text()
    cells = re.findall(r'<th>acceleration_amplitude_m_s2 \[m/s\^2\]</th><td>([^<]+)</td>', html)
    expected = [fixture["results"]["acceleration_amplitude_m_s2"]]
    expected += [row["acceleration_amplitude_m_s2"] for row in fixture["reference_points"]]
    assert len(cells) == len(expected)
    for cell, value in zip(cells, expected):
        assert float(cell) == pytest.approx(value, rel=0.02)


def test_visible_period_ratios_identify_the_computed_cases(analytical, tmp_path):
    fixture = analytical.build_fixture(inputs())
    analytical.write_fixture(fixture, tmp_path)
    html = (tmp_path / "sloshing-analytical-fixture.html").read_text()
    cells = re.findall(r'<th>period_ratio \[1\]</th><td>([^<]+)</td>', html)
    records = [fixture["inputs"], fixture["results"], *fixture["reference_points"]]
    assert [float(cell) for cell in cells] == [row["period_ratio"] for row in records]
