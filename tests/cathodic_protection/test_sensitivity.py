# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Regenerate one-at-a-time sensitivities and check independent arithmetic."""

from copy import deepcopy
import json
from pathlib import Path

import pytest
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.sensitivity import (
    load_sweep,
    main,
    prepare_base,
    run_sweep,
    sweep_report,
)

INPUTS = Path(__file__).resolve().parents[1] / "fixtures/cathodic_protection/workflow_inputs"
CONFIG = INPUTS / "published_jacket_sweep.yml"
TEMP = "environment.seawater_temperature_C"
LIFE = "design_data.design_life"
RHO = "environment.seawater_resistivity_ohm_m"


def selected(rows, path):
    return [row for row in rows if row["parameter"] == path]


@pytest.fixture(scope="module")
def sweeps():
    return {mode: run_sweep(CONFIG, mode=mode)
            for mode in ("published", "standard_tables")}


def test_config_regenerates_temperature_plateaus(sweeps):
    published = selected(sweeps["published"], TEMP)
    assert [row["value"] for row in published] == [5, 10, 15, 20, 25]
    for row in published:
        assert [row[key] for key in ("initial_current_A", "mean_current_A", "final_current_A")] == pytest.approx(
            [2688.285, 1452.450, 1892.052], abs=0.01, rel=0)
        assert row["recommended_anode_count"] == 575
    standard = selected(sweeps["standard_tables"], TEMP)
    assert [row["initial_current_A"] for row in standard] == pytest.approx(
        [4319.925, 3661.475, 3077.561, 3077.561, 2688.285], abs=0.01, rel=0)
    assert [row["recommended_anode_count"] for row in standard] == [924, 783, 658, 658, 575]
    hand_check = 3726.8 * 0.20 + 15737 * 0.17 + 652.9 * 0.01 + 10814.8 * 0.02 + 9 * 2
    assert standard[1]["initial_current_A"] == pytest.approx(hand_check, abs=0.01, rel=0)


def test_design_life_and_resistivity_arithmetic(sweeps):
    rows = sweeps["published"]
    life = selected(rows, LIFE)
    assert [row["required_net_mass_kg"] for row in life] == pytest.approx(
        [141372, 176715, 212058], abs=1, rel=0)
    for row in life:
        expected_mass = 1452.450 * row["value"] * 8760 / (0.9 * 2000)
        assert row["required_net_mass_kg"] == pytest.approx(expected_mass, abs=1, rel=0)
    assert [row["recommended_anode_count"] for row in life] == [575, 575, 656]
    rho = selected(rows, RHO)
    assert [row["recommended_anode_count"] for row in rho] == [547, 575, 784, 1045]
    for row in rho:
        assert row["required_net_mass_kg"] == pytest.approx(176715, abs=1, rel=0)
        assert [row[key] for key in ("initial_current_A", "mean_current_A", "final_current_A")] == pytest.approx(
            [2688.285, 1452.450, 1892.052], abs=0.01, rel=0)


def test_base_is_not_mutated_and_flags_are_the_engines_own(sweeps):
    spec, base = load_sweep(CONFIG)
    original = deepcopy(base)
    modified = prepare_base(base, spec, "standard_tables")
    assert base == original
    original_zones = base["inputs"]["structure"]["zones"]
    for zone, old in zip(modified["inputs"]["structure"]["zones"], original_zones):
        if zone["zone"] in spec["standard_table_zones"]:
            assert "effective_current_density_A_m2" not in zone
            assert zone["depth_m"] == spec["standard_table_zones"][zone["zone"]]
        else:
            assert zone == old
    assert modified["inputs"]["structure"]["connected_current_allowances"] == base["inputs"]["structure"]["connected_current_allowances"]
    for mode, rows in sweeps.items():
        for row in rows:
            case = prepare_base(base, spec, mode)
            section, key = row["parameter"].split(".")
            case["inputs"][section][key] = row["value"]
            result = run_cathodic_protection(case)["results"]
            assert row["status"] == result["status"]
            assert row["adequacy"] == result["current_output_verification"]
    assert base == original
    assert len(sweeps["published"]) == 12  # One parameter changes at a time.


@pytest.mark.parametrize("bad", [None, float("nan"), float("inf"), "N/A", True])
def test_invalid_values_fail_closed(bad):
    spec, base = load_sweep(CONFIG)
    spec["parameters"][0]["values"] = [bad]
    with pytest.raises(ValueError, match="finite"):
        run_sweep(spec, base=base)


@pytest.mark.parametrize("bad", [None, float("nan"), float("inf"), "N/A", True])
@pytest.mark.parametrize("path", [TEMP, LIFE, RHO])
def test_invalid_base_values_fail_closed(path, bad):
    spec, base = load_sweep(CONFIG)
    section, key = path.split(".")
    base["inputs"][section][key] = bad
    with pytest.raises(ValueError, match="finite"):
        run_sweep(spec, base=base)


@pytest.mark.parametrize("bad", [None, [], {}, [{"path": TEMP, "values": []}],
                                  [{"path": TEMP, "values": None}]])
def test_missing_cases_fail_closed(bad):
    spec, base = load_sweep(CONFIG)
    spec["parameters"] = bad
    with pytest.raises(ValueError, match="non-empty list"):
        run_sweep(spec, base=base)


@pytest.mark.parametrize("path", [TEMP, LIFE, RHO, "anode.count"])
def test_missing_or_nonapplicable_parameter_fails_closed(path):
    spec, base = load_sweep(CONFIG)
    spec["parameters"] = [{"path": path, "values": [10]}]
    if path != "anode.count":
        section, key = path.split(".")
        del base["inputs"][section][key]
    with pytest.raises(ValueError, match="required|unsupported"):
        run_sweep(spec, base=base)


@pytest.mark.parametrize("mode", [None, "unknown"])
def test_invalid_config_mode(mode):
    spec, base = load_sweep(CONFIG)
    spec["mode"] = mode
    with pytest.raises(ValueError, match="mode"):
        run_sweep(spec, base=base)


def test_standard_mode_rejects_nonapplicable_zone():
    spec, base = load_sweep(CONFIG)
    spec["standard_table_zones"]["coated-seawater"] = 15
    with pytest.raises(ValueError, match="two bare seawater zones"):
        prepare_base(base, spec, "standard_tables")


def test_installed_count_is_required_for_adequacy():
    spec, base = load_sweep(CONFIG)
    del base["inputs"]["anode"]["count"]
    with pytest.raises(ValueError, match="installed anode count"):
        run_sweep(spec, base=base)


def test_climate_boundaries_are_stepwise():
    spec, base = load_sweep(CONFIG)
    spec["parameters"] = [{"path": TEMP, "values": [6.999, 7, 11.999, 12, 20, 20.001]}]
    rows = run_sweep(spec, base=base, mode="standard_tables")
    assert [row["recommended_anode_count"] for row in rows] == [924, 783, 783, 658, 658, 575]


@pytest.mark.parametrize("value", [0, -1])
@pytest.mark.parametrize("path", [LIFE, RHO])
def test_nonpositive_values_fail_closed(path, value):
    spec, base = load_sweep(CONFIG)
    spec["parameters"] = [{"path": path, "values": [value]}]
    with pytest.raises(ValueError, match="positive"):
        run_sweep(spec, base=base)


@pytest.mark.parametrize("missing", [None, float("nan"), "N/A"])
def test_published_density_cannot_silently_fall_back(missing):
    spec, base = load_sweep(CONFIG)
    base["inputs"]["structure"]["zones"][0]["effective_current_density_A_m2"] = missing
    with pytest.raises(ValueError, match="density"):
        run_sweep(spec, base=base)


def test_report_shortfall_survives_rounding_and_missing_flag_is_rejected(sweeps):
    from digitalmodel.cathodic_protection.report_sensitivity import sensitivity_section

    row = deepcopy(sweeps["published"][0])
    row["initial_current_A"] = 1.00001
    row["adequacy"].update(anode_count=1, count_by_initial_current=2,
                           count_by_mass=1, count_by_final_current=1,
                           initial_meets_demand=False, mass_meets_requirement=True,
                           final_meets_demand=True)
    table = sensitivity_section([row]).blocks[1]
    assert table.rows[0][2] == "1.000"
    assert "FAIL" in table.rows[0][7] and "margin -1 anodes" in table.rows[0][7]
    row["adequacy"]["initial_meets_demand"] = None
    with pytest.raises(ValueError, match="not evaluated"):
        sensitivity_section([row])


def test_cli_refuses_to_overwrite_inputs_or_alias_outputs(tmp_path):
    config = tmp_path / CONFIG.name
    base = tmp_path / "published_jacket_84m.yml"
    config.write_text(CONFIG.read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
    base.write_text((INPUTS / base.name).read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="distinct"):
        main([str(config), "--output", str(config)])
    with pytest.raises(ValueError, match="distinct"):
        main([str(config), "--report", str(base)])
    output = tmp_path / "same"
    with pytest.raises(ValueError, match="distinct"):
        main([str(config), "--output", str(output), "--report", str(output)])


def test_report_and_cli_use_existing_output(tmp_path, capsys):
    from digitalmodel.reporting import render_html
    from digitalmodel.reporting.spec import TableBlock

    spec = sweep_report(CONFIG, mode="standard_tables")
    section = next(section for section in spec.sections if section.key == "sensitivity")
    table = next(block for block in section.blocks if isinstance(block, TableBlock))
    assert table.title == "One-at-a-time sensitivity"
    assert table.source and "read-only" in table.source
    assert "published_jacket_84m.yml" in table.source
    assert "2021" in table.source
    assert any("A" in column for column in table.columns)
    assert any("kg" in column for column in table.columns)
    page = render_html(spec)
    assert "3661.475" in page
    assert "standard_tables" in page and "modified input" in page
    assert "B401 Sec. 7.8" in page and "N_verified" in page
    assert "FAIL" in page and "PASS" in page
    output = tmp_path / "folder with spaces" / "sweep.json"
    report = output.with_suffix(".html")
    assert main([str(CONFIG), "--mode", "published", "--output", str(output), "--report", str(report)]) == 0
    assert len(json.loads(output.read_text(encoding="utf-8"))) == 12
    assert b"\r\n" not in output.read_bytes()
    assert "One-at-a-time sensitivity" in report.read_text(encoding="utf-8")
    assert main([str(CONFIG)]) == 0
    assert len(json.loads(capsys.readouterr().out)) == 12
