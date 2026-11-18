# ABOUTME: Independent request-matrix geometry and qualification safety tests.
# ABOUTME: Expected values are hand calculations, never strength-method outputs.
"""Synthetic assumptions from issue 2287 (2026), Parametric basis.

No test dimension, pressure or FCA is an acceptance limit.
"""

import hashlib
import json
import math

import pytest

from digitalmodel.asset_integrity.uniform_loss_study import main, prepare_study


# Explicit synthetic pressures; no material property or code factor is inferred.
PRESSURES = [100.0, 200.0, 300.0, 400.0]


def test_fixed_matrix_covers_each_size_length_width_fca_and_both_levels():
    """Issue 2287: 4 sizes * 7 lengths * 4 widths * 3 FCA requests = 336.

    Two unevaluated levels per cell give 672 level records and zero results.
    """
    result = prepare_study(pressures_psi=PRESSURES)
    assert len(result["cells"]) == 336
    assert len({cell["case_id"] for cell in result["cells"]}) == 336
    assert sum(len(cell["levels"]) for cell in result["cells"]) == 672
    assert {cell["inputs"]["pressure_psi"] for cell in result["cells"]} == set(
        PRESSURES
    )
    assert result["meta"]["qualified_cases"] == 0
    for cell in result["cells"]:
        assert cell["assessment_status"] == "UNQUALIFIED"
        assert cell["asset_acceptance"] is None
        assert all(level["status"] == "NOT_EVALUATED" for level in cell["levels"])
        assert all(
            level["allowable_remaining_wall_in"] is None for level in cell["levels"]
        )
    assert result == prepare_study(pressures_psi=PRESSURES)
    json.dumps(result, allow_nan=False)


def test_physical_coordinates_use_edges_and_cell_centers():
    """Hand calculation: s=0.5 in, N=16 => dx=1/32 in.

    First center=1/64, last=31/64, edges=0 and 1/2 in.
    Dm=12.75-0.375=12.375 in; c=0.02*pi*12.375=0.2475*pi in.
    Eight circumferential cells have centers (j+1/2)*c/8, j=0..7.
    Source: issue 2287 physical grid; OpenStax Precalculus 2e (2021), 5.1, s=r*theta.
    """
    cell = prepare_study(pressures_psi=PRESSURES)["cells"][0]
    grid = cell["physical_grid"]
    assert grid["shape"] == [16, 8]
    assert grid["axial_edges_in"] == [0.0, 0.5]
    assert grid["axial_centers_in"][0] == 1 / 64
    assert grid["axial_centers_in"][-1] == 31 / 64
    assert grid["circumferential_edges_in"] == pytest.approx([0, 0.2475 * math.pi])
    assert grid["circumferential_centers_in"][0] == pytest.approx(0.2475 * math.pi / 16)
    assert grid["circumferential_centers_in"][-1] == pytest.approx(
        15 * 0.2475 * math.pi / 16
    )
    assert "thickness_values" not in grid  # No measured or solved walls invented.


def test_separate_fca_requests_preserve_current_geometry_and_raw_subtractions():
    """Hand calculation: 0.375-0.020=0.355, 0.375/5-0.050=0.025 in.

    These are requested subtractions, not qualified Part 5 Tc/tmm definitions.
    """
    result = prepare_study(pressures_psi=PRESSURES, fca_pairs_in=[[0.02, 0.05]])
    cell = result["cells"][0]
    assert len(result["cells"]) == 112
    assert cell["inputs"]["sound_wall_in"] == 0.375
    assert cell["fca_request"]["sound_wall_after_subtraction_in"] == pytest.approx(
        0.355
    )
    assert cell["fca_request"]["lower_wall_after_subtraction_in"] == pytest.approx(
        0.025
    )
    assert cell["fca_request"]["status"] == "NOT_EVALUATED"
    assert cell["geometry"]["mean_diameter_in"] == 12.375


@pytest.mark.parametrize(
    "pressures",
    [
        [],
        [100],
        [100] * 5,
        [100, 200, 300, 0],
        [True, 200, 300, 400],
        [float("nan")] * 4,
    ],
)
def test_invalid_pressure_inventory_is_rejected(pressures):
    with pytest.raises(ValueError):
        prepare_study(pressures_psi=pressures)


@pytest.mark.parametrize(
    "pairs",
    [
        [],
        [[0, 0]] * 4,
        [[0]],
        [[-0.01, 0]],
        [[0, float("inf")]],
        [[False, 0]],
        [[0, 0.1]],
        [[0.4, 0]],
        [[0, 0], [0, 0]],
    ],
)
def test_invalid_or_unbounded_fca_requests_are_rejected(pairs):
    """Nonpositive reduced study bounds and duplicate scenarios are rejected.

    0.1 in flaw FCA exceeds the first pipe's 0.375/5=0.075 in lower bound.
    Three scenarios is the bounded study inventory, not a method restriction.
    """
    with pytest.raises(ValueError):
        prepare_study(pressures_psi=PRESSURES, fca_pairs_in=pairs)


def test_provenance_covers_sources_and_every_configuration_input():
    result = prepare_study(pressures_psi=PRESSURES)
    assert result["meta"]["source_sha256"]
    assert all(len(digest) == 64 for digest in result["meta"]["source_sha256"].values())
    canonical = json.dumps(
        result["configuration"], sort_keys=True, separators=(",", ":"), allow_nan=False
    ).encode()
    assert (
        result["meta"]["study_config_sha256"] == hashlib.sha256(canonical).hexdigest()
    )
    for overrides in [
        dict(pressures_psi=[101, 200, 300, 400]),
        dict(pressures_psi=PRESSURES, fca_pairs_in=[[0.01, 0.02]]),
    ]:
        assert (
            prepare_study(**overrides)["meta"]["study_config_sha256"]
            != result["meta"]["study_config_sha256"]
        )


def test_cli_reuses_identical_bytes_and_refuses_different_existing_data(
    tmp_path, capsys
):
    config, output = tmp_path / "inputs.json", tmp_path / "requests.json"
    config.write_text(json.dumps(dict(pressures_psi=PRESSURES)))
    args = ["--config", str(config), "--output", str(output)]
    main(args)
    original = output.read_bytes()
    original_time = output.stat().st_mtime_ns
    main(args)
    assert output.read_bytes() == original
    assert output.stat().st_mtime_ns == original_time
    assert "reused" in capsys.readouterr().out
    config.write_text(json.dumps(dict(pressures_psi=[101, 200, 300, 400])))
    with pytest.raises(SystemExit) as error:
        main(args)
    assert error.value.code == 2
    assert output.read_bytes() == original


def test_invalid_config_does_not_publish_partial_output(tmp_path):
    config, output = tmp_path / "inputs.json", tmp_path / "requests.json"
    config.write_text('{"pressures_psi": [100]}')
    with pytest.raises(SystemExit):
        main(["--config", str(config), "--output", str(output)])
    assert not output.exists()


def test_committed_worked_example_matches_independent_hand_calculation():
    """Example: OD=20, wall=0.5, s=8 in, f=1/4, pressure=300 psi.

    Di=19, Dm=19.5; c=19.5*pi/4=4.875*pi in; angle=90 deg.
    Sound subtraction=0.5-0.02=0.48; low=0.1-0.05=0.05 in.
    Pressure is a synthetic request, with no inference of material suitability.
    """
    from pathlib import Path

    root = Path(__file__).resolve().parents[2]
    config = json.loads(
        (root / "examples/asset_integrity/uniform_loss_requests.json").read_text()
    )
    result = prepare_study(**config)
    assert len(result["cells"]) == 112
    selected = next(
        cell
        for cell in result["cells"]
        if cell["inputs"]["od_in"] == 20
        and cell["inputs"]["axial_length_in"] == 8
        and cell["inputs"]["width_fraction"] == 0.25
    )
    assert selected["inputs"]["pressure_psi"] == 300
    assert selected["geometry"]["width_arc_in"] == pytest.approx(4.875 * math.pi)
    assert selected["geometry"]["width_angle_deg"] == 90
    assert selected["fca_request"]["sound_wall_after_subtraction_in"] == pytest.approx(
        0.48
    )
    assert selected["fca_request"]["lower_wall_after_subtraction_in"] == pytest.approx(
        0.05
    )
    for relative, digest in result["meta"]["source_sha256"].items():
        assert hashlib.sha256((root / relative).read_bytes()).hexdigest() == digest


def test_cli_refuses_reformatted_output_even_if_json_values_match(tmp_path):
    """Resumption requires identical bytes; a config digest alone is insufficient."""
    config, output = tmp_path / "inputs.json", tmp_path / "requests.json"
    config.write_text(json.dumps(dict(pressures_psi=PRESSURES)))
    args = ["--config", str(config), "--output", str(output)]
    main(args)
    changed = output.read_bytes().replace(b"\n", b"\r\n")
    output.write_bytes(changed)
    with pytest.raises(SystemExit):
        main(args)
    assert output.read_bytes() == changed
