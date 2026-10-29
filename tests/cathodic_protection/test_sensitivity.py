# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Regenerate one-at-a-time sensitivities and check independent arithmetic."""

from copy import deepcopy
import json
import math
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


def hand_demands(upper=(0.15, 0.07, 0.10), lower=(0.12, 0.06, 0.08)):
    # DOI 10.26855/ea.2023.06.017 (2023), p. 242, Tables 1-2:
    # areas [m2], coated/buried densities [A/m2], and nine wells at 2 A each.
    # DNV-RP-B401 (May 2021), [4.4.1], Eq. (4.1): sum(area * density).
    return [652.9 * coated + 3726.8 * high + 15737 * low + 10814.8 * 0.02 + 9 * 2
            for coated, high, low in zip((0.01, 0.02, 0.04), upper, lower)]


def hand_requirements(currents, *, life=25, rho=0.22):
    # Fixture verification_evidence.assumptions: life [yr], capacity [Ah/kg],
    # utilization and alloy density [kg/m3] are assumed, based on previous experience.
    # Capacity/utilization also match B401 (May 2021), Tables 8-6 and 8-8.
    # Paper DOI 10.26855/ea.2023.06.017 (2023), p. 242, Sec. 2:
    # mass = 323.4 kg, length = 2020 mm / 1000, driving voltage = 0.25 V,
    # and base resistivity = 22 ohm.cm / 100 = 0.22 ohm.m.
    mass, length, utilization, capacity, density = 323.4, 2.020, 0.90, 2000, 2750
    # Cylinder volume m/density = pi*r^2*L; depleted volume fraction = 1-u
    # at unchanged length (the fixture's declared equivalent-cylinder assumption).
    radius = math.sqrt(mass / (math.pi * length * density))
    final_radius = radius * math.sqrt(1 - utilization)
    # B401 (May 2021), Table 8-7, long slender stand-off: geometric factors
    # 2*pi, 4 and -1; [4.8.2], Eq. (4.3): individual output = voltage/R.
    resistance = [rho / (2 * math.pi * length) * (math.log(4 * length / r) - 1)
                  for r in (radius, final_radius)]
    outputs = [0.25 / value for value in resistance]
    # B401 (May 2021), [4.7.1], Eq. (4.2); 8760 = 365 days * 24 h/day.
    net_mass = currents[1] * life * 8760 / (utilization * capacity)
    # B401 (May 2021), [4.8.4], Eqs. (4.5)-(4.7): capacity/output counts round up.
    counts = {
        "count_by_mass": math.ceil(net_mass / mass),
        "count_by_initial_current": math.ceil(currents[0] / outputs[0]),
        "count_by_final_current": math.ceil(currents[2] / outputs[1]),
    }
    return net_mass, counts


def assert_hand_requirements(row, currents, *, life=25, rho=0.22):
    expected_mass, counts = hand_requirements(currents, life=life, rho=rho)
    assert row["required_net_mass_kg"] == pytest.approx(expected_mass, abs=1, rel=0)
    assert row["recommended_anode_count"] == max(counts.values())
    for key, expected in counts.items():
        assert row["adequacy"][key] == expected


def test_hand_basis_matches_printed_demands():
    currents = hand_demands()
    # Hand calculations from the paper's Tables 1-2, all terms in amperes:
    # Initial: 6.529 + 559.020 + 1888.440 + 216.296 + 18 = 2688.285.
    # Mean: 13.058 + 260.876 + 944.220 + 216.296 + 18 = 1452.450.
    # Final: 26.116 + 372.680 + 1258.960 + 216.296 + 18 = 1892.052.
    assert currents == pytest.approx([2688.285, 1452.450, 1892.052], abs=0.001, rel=0)
    # Paper (2023), p. 242, Sec. 2 prints demands to 0.1 A; allow rounding
    # at that precision plus 0.001 A for floating-point representation.
    assert currents == pytest.approx([2688.3, 1452.5, 1892.1], abs=0.051, rel=0)


def test_config_regenerates_temperature_plateaus(sweeps):
    published = selected(sweeps["published"], TEMP)
    assert [row["value"] for row in published] == [5, 10, 15, 20, 25]
    published_currents = hand_demands()
    for row in published:
        assert [row[key] for key in ("initial_current_A", "mean_current_A", "final_current_A")] == pytest.approx(
            published_currents, abs=0.01, rel=0)
        assert_hand_requirements(row, published_currents)
    standard = selected(sweeps["standard_tables"], TEMP)
    assert [row["value"] for row in standard] == [5, 10, 15, 20, 25]
    # B401 (May 2021), Table 8-1 (Table 10-1 in the 2010 edition):
    # initial/final densities [A/m2] at 0-30 m and >30-100 m; means from
    # Table 8-2. Columns: Arctic, Temperate, Sub-Tropical (twice), Tropical.
    densities = [((0.25, 0.12, 0.17), (0.20, 0.10, 0.13)),
                 ((0.20, 0.10, 0.13), (0.17, 0.08, 0.11)),
                 ((0.17, 0.08, 0.11), (0.14, 0.07, 0.09)),
                 ((0.17, 0.08, 0.11), (0.14, 0.07, 0.09)),
                 ((0.15, 0.07, 0.10), (0.12, 0.06, 0.08))]
    for row, (upper, lower) in zip(standard, densities):
        expected = hand_demands(upper, lower)
        assert [row[key] for key in ("initial_current_A", "mean_current_A", "final_current_A")] == pytest.approx(
            expected, abs=0.01, rel=0)
        assert_hand_requirements(row, expected)


def test_design_life_and_resistivity_arithmetic(sweeps):
    rows = sweeps["published"]
    life = selected(rows, LIFE)
    assert [row["value"] for row in life] == [20, 25, 30]
    currents = hand_demands()
    for row in life:
        assert_hand_requirements(row, currents, life=row["value"])
    rho = selected(rows, RHO)
    assert [row["value"] for row in rho] == [0.15, 0.22, 0.30, 0.40]
    for row in rho:
        assert_hand_requirements(row, currents, rho=row["value"])
        assert [row[key] for key in ("initial_current_A", "mean_current_A", "final_current_A")] == pytest.approx(
            currents, abs=0.01, rel=0)


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


@pytest.mark.parametrize("zone,depth", [
    ("upper-bare-seawater", 30.001), ("lower-bare-seawater", 29.999),
    ("lower-bare-seawater", 30), ("lower-bare-seawater", 84.101),
])
def test_standard_mode_rejects_depth_outside_geometry(zone, depth):
    spec, base = load_sweep(CONFIG)
    spec["standard_table_zones"][zone] = depth
    with pytest.raises(ValueError, match="depth.*zone bounds"):
        prepare_base(base, spec, "standard_tables")


@pytest.mark.parametrize("zone,depth", [
    ("upper-bare-seawater", 0), ("upper-bare-seawater", 30),
    ("lower-bare-seawater", 30.001), ("lower-bare-seawater", 84.1),
])
def test_standard_mode_accepts_geometry_boundaries(zone, depth):
    spec, base = load_sweep(CONFIG)
    spec["standard_table_zones"][zone] = depth
    prepared = prepare_base(base, spec, "standard_tables")
    matched = next(item for item in prepared["inputs"]["structure"]["zones"]
                   if item["zone"] == zone)
    assert matched["depth_m"] == depth
    rows = run_sweep(spec, base=base, mode="standard_tables")
    temperate = next(row for row in selected(rows, TEMP) if row["value"] == 10)
    # B401 (May 2021), Tables 8-1/8-2: 30 m is in 0-30 m;
    # 30.001 m is in >30-100 m, so both zones retain the intended densities.
    assert temperate["initial_current_A"] == pytest.approx(
        hand_demands((0.20, 0.10, 0.13), (0.17, 0.08, 0.11))[0], abs=0.01, rel=0)


@pytest.mark.parametrize("depth", [None, float("nan"), float("inf"), True, -1, "depth"])
def test_standard_mode_rejects_invalid_table_depths(depth):
    spec, base = load_sweep(CONFIG)
    spec["standard_table_zones"]["upper-bare-seawater"] = depth
    with pytest.raises(ValueError, match="finite|non-negative"):
        prepare_base(base, spec, "standard_tables")


@pytest.mark.parametrize("bad", [None, [], {}, [None], "invalid"])
def test_standard_mode_rejects_invalid_geometry_zones(bad):
    spec, base = load_sweep(CONFIG)
    base["inputs"]["structure"]["report_geometry"]["zones"] = bad
    with pytest.raises(ValueError, match="geometry"):
        prepare_base(base, spec, "standard_tables")


@pytest.mark.parametrize("change", ["missing", "duplicate", "reversed", "equal",
                                   "missing_bound", "nan", "infinite", "text", "bool"])
def test_standard_mode_rejects_ambiguous_or_invalid_geometry_bounds(change):
    spec, base = load_sweep(CONFIG)
    bands = base["inputs"]["structure"]["report_geometry"]["zones"]
    if change == "missing":
        bands.pop(0)
    elif change == "duplicate":
        bands.append(deepcopy(bands[0]))
    elif change in ("reversed", "equal"):
        bands[0]["lower_m"] = 1 if change == "reversed" else 0
    elif change == "missing_bound":
        del bands[0]["lower_m"]
    else:
        bands[0]["lower_m"] = {
            "nan": float("nan"), "infinite": float("inf"), "text": "-30", "bool": False,
        }[change]
    with pytest.raises(ValueError, match="geometry"):
        prepare_base(base, spec, "standard_tables")


def test_standard_mode_still_supports_inputs_without_geometry():
    spec, base = load_sweep(CONFIG)
    del base["inputs"]["structure"]["report_geometry"]
    assert prepare_base(base, spec, "standard_tables")["inputs"]["structure"]["zones"]


def test_installed_count_is_required_for_adequacy():
    spec, base = load_sweep(CONFIG)
    del base["inputs"]["anode"]["count"]
    with pytest.raises(ValueError, match="installed anode count"):
        run_sweep(spec, base=base)


def test_climate_boundaries_are_stepwise():
    spec, base = load_sweep(CONFIG)
    spec["parameters"] = [{"path": TEMP, "values": [6.999, 7, 11.999, 12, 20, 20.001]}]
    rows = run_sweep(spec, base=base, mode="standard_tables")
    assert len(rows) == 6
    # B401 (May 2021), Tables 8-1/8-2 climate columns at the temperature boundaries.
    # Selecting the warmer column at exactly 12 degC is the engine boundary convention.
    densities = [((0.25, 0.12, 0.17), (0.20, 0.10, 0.13)),
                 ((0.20, 0.10, 0.13), (0.17, 0.08, 0.11)),
                 ((0.20, 0.10, 0.13), (0.17, 0.08, 0.11)),
                 ((0.17, 0.08, 0.11), (0.14, 0.07, 0.09)),
                 ((0.17, 0.08, 0.11), (0.14, 0.07, 0.09)),
                 ((0.15, 0.07, 0.10), (0.12, 0.06, 0.08))]
    for row, (upper, lower) in zip(rows, densities):
        currents = hand_demands(upper, lower)
        assert [row[key] for key in ("initial_current_A", "mean_current_A", "final_current_A")] == pytest.approx(
            currents, abs=0.01, rel=0)
        assert_hand_requirements(row, currents)


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
    assert "FAIL; N_verified 1 < 2 required; margin -1 anodes" in table.rows[0][7]
    row["adequacy"]["initial_meets_demand"] = None
    with pytest.raises(ValueError, match="not evaluated"):
        sensitivity_section([row])


@pytest.mark.parametrize("provided,passed,comparator", [
    (569, False, "<"), (575, True, ">="), (576, True, ">="),
])
@pytest.mark.parametrize("column", [7, 8, 9])
def test_report_criterion_uses_true_comparator(sweeps, provided, passed, comparator, column):
    from digitalmodel.cathodic_protection.report_sensitivity import sensitivity_section

    row = deepcopy(sweeps["published"][0])
    row["adequacy"].update(
        anode_count=provided, count_by_initial_current=575, count_by_mass=575,
        count_by_final_current=575, initial_meets_demand=passed,
        mass_meets_requirement=passed, final_meets_demand=passed,
    )
    cell = sensitivity_section([row]).blocks[1].rows[0][column]
    assert f"N_verified {provided} {comparator} 575 required" in cell
    assert cell.startswith("PASS" if passed else "FAIL")


@pytest.mark.parametrize("standard,mass_ref,count_ref", [
    ("DNV-RP-B401 (2005, with 2008 amendments)", "Sec. 7.7.1", "Sec. 7.8.4"),
    ("DNV-RP-B401 (October 2010)", "Sec. 7.7.1", "Sec. 7.8.4"),
    ("DNVGL-RP-B401 (June 2017)", "[7.7.1]", "[7.8.4]"),
    ("DNV-RP-B401 (May 2021)", "[4.7.1]", "[4.8.4]"),
])
def test_report_criteria_follow_each_row_edition(sweeps, standard, mass_ref, count_ref):
    from digitalmodel.cathodic_protection.report_sensitivity import sensitivity_section

    row = deepcopy(sweeps["published"][0])
    row["basis"]["standard"] = standard
    before = deepcopy(row)
    table = sensitivity_section([row]).blocks[1]
    for column in (7, 8, 9):
        assert standard in table.rows[0][column]
        assert count_ref in table.rows[0][column]
    assert mass_ref in table.rows[0][8]
    assert mass_ref not in table.rows[0][7]
    assert mass_ref not in table.rows[0][9]
    assert standard in table.source
    assert mass_ref in table.source and count_ref in table.source
    if "2021" in standard:
        assert "Sec. 7.8" not in table.source
        assert all("Sec. 7.8" not in cell for cell in table.rows[0][7:])
    assert row == before


def test_report_mixed_editions_keep_their_own_criteria(sweeps):
    from digitalmodel.cathodic_protection.report_sensitivity import sensitivity_section

    rows = [deepcopy(sweeps["published"][0]) for _ in range(2)]
    rows[0]["basis"]["standard"] = "DNV-RP-B401 (October 2010)"
    table = sensitivity_section(rows).blocks[1]
    assert "Sec. 7.8.4" in table.rows[0][7]
    assert "[4.8.4]" not in table.rows[0][7]
    assert "[4.8.4]" in table.rows[1][7]
    assert "Sec. 7.8" not in table.rows[1][7]
    for reference in ("Sec. 7.7.1", "Sec. 7.8.4", "[4.7.1]", "[4.8.4]"):
        assert reference in table.source


@pytest.mark.parametrize("standard", ["DNV-RP-B401 (2099)", "DNV-RP-F103 (2010)"])
def test_report_rejects_unknown_criterion_edition(sweeps, standard):
    from digitalmodel.cathodic_protection.report_sensitivity import sensitivity_section

    row = deepcopy(sweeps["published"][0])
    row["basis"]["standard"] = standard
    with pytest.raises(ValueError, match="criterion.*edition"):
        sensitivity_section([row])


def test_report_explains_fixed_published_temperature_rows(sweeps):
    from digitalmodel.cathodic_protection.report_sensitivity import sensitivity_section

    text = sensitivity_section(sweeps["published"]).blocks[0].markdown.lower()
    assert "temperature has no effect in published mode" in text
    assert "effective phase densities" in text


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
    assert "[4.7.1]" in page and "[4.8.4]" in page and "N_verified" in page
    assert "B401 Sec. 7.8" not in section.blocks[1].source
    assert "FAIL" in page and "PASS" in page
    output = tmp_path / "folder with spaces" / "sweep.json"
    report = output.with_suffix(".html")
    assert main([str(CONFIG), "--mode", "published", "--output", str(output), "--report", str(report)]) == 0
    assert len(json.loads(output.read_text(encoding="utf-8"))) == 12
    assert b"\r\n" not in output.read_bytes()
    assert "One-at-a-time sensitivity" in report.read_text(encoding="utf-8")
    assert main([str(CONFIG)]) == 0
    assert len(json.loads(capsys.readouterr().out)) == 12
