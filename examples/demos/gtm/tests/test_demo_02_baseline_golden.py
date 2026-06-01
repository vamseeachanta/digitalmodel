# ABOUTME: Golden regression test — demo_02's Baseline Sweep Config reproduces the 72-case oracle.
# ABOUTME: Acceptance gate for the yaml-driven sweep: byte-identical to the frozen golden.
"""Golden regression + loader/normalize unit tests for demo_02's yaml-driven Sweep Config.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_02_baseline_golden.py -x -q
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import demo_02_wall_thickness_multicode as demo
from sweep_config_demo02 import (
    SweepConfigError,
    load_demo02_config,
)

_GTM_DIR = Path(demo.__file__).resolve().parent
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_02_baseline_results.json"
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_02_wall_thickness.yml"

_VALID_CODE_DISPLAY = ["DNV-ST-F101", "API RP 1111", "PD 8010-2"]


@pytest.fixture(scope="module")
def produced_results():
    """Run the sweep from the committed Baseline config (recompute path)."""
    demo._load_engineering_modules()
    demo._init_code_constants()
    with open(demo.DATA_DIR / "pipelines.json", "r") as fh:
        pipe_catalog = json.load(fh)["pipes"]
    code_name_map = {display: code for code, display in demo.CODE_NAMES.items()}
    config = load_demo02_config(
        _BASELINE_CONFIG_PATH,
        code_name_map=code_name_map,
        safety_class_enum=demo.SafetyClass,
    )
    results, _df = demo.run_parametric_sweep(pipe_catalog, config=config)
    return results


@pytest.fixture(scope="module")
def golden_results():
    with _GOLDEN_PATH.open("r", encoding="utf-8") as fh:
        return json.load(fh)["results"]


# ---------------------------------------------------------------------------
# Golden oracle
# ---------------------------------------------------------------------------


def test_produced_results_equal_golden_in_order_field_by_field(produced_results, golden_results):
    """Primary oracle: produced results[] == golden results[] in order, all fields.

    Comparing the full ordered lists subsumes count, emission order (size -> code ->
    pressure), every field — including nested ``checks{}`` and the literal ``pipe_size``
    ``"`` characters — and the per-case key set in a single assertion.
    """
    assert produced_results == golden_results


def test_case_count_and_safety_split(produced_results):
    """72 cases; is_safe split 48 True / 24 False."""
    assert len(produced_results) == 72
    safe = sum(1 for r in produced_results if r["is_safe"] is True)
    unsafe = sum(1 for r in produced_results if r["is_safe"] is False)
    assert safe == 48
    assert unsafe == 24


def test_every_case_has_exactly_the_14_frozen_keys(produced_results):
    """BD-1: every live case carries exactly the 14 FROZEN keys."""
    assert len(demo.FROZEN_RESULT_KEYS) == 14
    for r in produced_results:
        assert set(r.keys()) == set(demo.FROZEN_RESULT_KEYS)


def test_codes_emitted_as_spaced_display_strings(produced_results):
    """Codes are the SPACED display strings, never the hyphenated enum .value forms (BD-3)."""
    assert sorted(set(r["code"] for r in produced_results)) == sorted(_VALID_CODE_DISPLAY)


# ---------------------------------------------------------------------------
# Synthetic failure-path unit test (BD-1)
# ---------------------------------------------------------------------------


def test_failure_path_normalizes_to_14_keys_with_nulls():
    """The None/failure path emits the SAME 14 keys, with null where 'N/A' used to be."""
    demo._load_engineering_modules()
    demo._init_code_constants()
    ps = {"name": '6"', "od_mm": 168.3, "od_m": 0.1683}
    rec = demo._normalize_failure_record(ps, "DNV-ST-F101", 10, demo.external_pressure_pa())

    assert set(rec.keys()) == set(demo.FROZEN_RESULT_KEYS)
    # Nulls where the legacy "N/A" sentinels lived.
    assert rec["is_safe"] is None
    assert rec["governing_check"] is None
    assert rec["max_utilisation"] is None
    assert rec["checks"] is None
    assert rec["wt_mm"] is None
    assert rec["wt_m"] is None
    # Identity / context fields preserved (code stays the spaced display string).
    assert rec["pipe_size"] == '6"'
    assert rec["code"] == "DNV-ST-F101"
    assert rec["internal_pressure_mpa"] == 10


def test_failure_path_via_monkeypatched_run_single(monkeypatch):
    """Triggering the None path through the sweep yields normalized 14-key records."""
    demo._load_engineering_modules()
    demo._init_code_constants()
    with open(demo.DATA_DIR / "pipelines.json", "r") as fh:
        pipe_catalog = json.load(fh)["pipes"]
    monkeypatch.setattr(demo, "run_single_analysis", lambda *a, **k: None)
    code_name_map = {display: code for code, display in demo.CODE_NAMES.items()}
    config = load_demo02_config(
        _BASELINE_CONFIG_PATH,
        code_name_map=code_name_map,
        safety_class_enum=demo.SafetyClass,
    )
    results, _df = demo.run_parametric_sweep(pipe_catalog, config=config)
    assert len(results) == 72
    for r in results:
        assert set(r.keys()) == set(demo.FROZEN_RESULT_KEYS)
        assert r["is_safe"] is None
        assert r["checks"] is None


# ---------------------------------------------------------------------------
# Loader round-trip unit tests (BD-3)
# ---------------------------------------------------------------------------


def test_loader_round_trip_display_to_design_to_display():
    """For all 3 codes: display -> resolve -> CODE_NAMES[DesignCode] == display (BD-3)."""
    demo._load_engineering_modules()
    demo._init_code_constants()
    code_name_map = {display: code for code, display in demo.CODE_NAMES.items()}
    config = load_demo02_config(
        _BASELINE_CONFIG_PATH,
        code_name_map=code_name_map,
        safety_class_enum=demo.SafetyClass,
    )
    # The yaml code strings are exactly the 3 valid display keys.
    assert config.codes == _VALID_CODE_DISPLAY
    # Round-trip: each resolved DesignCode maps back to the same display string.
    for display, design_code in zip(config.codes, config.design_codes):
        assert demo.CODE_NAMES[design_code] == display
    # Safety class resolved to the MEDIUM enum member.
    assert config.safety_class is demo.SafetyClass.MEDIUM
    assert config.safety_class_label == "MEDIUM"


def test_loader_lazy_path_resolves_without_supplied_map():
    """The loader can build its own inverse map lazily (no circular-import explosion)."""
    config = load_demo02_config(_BASELINE_CONFIG_PATH)
    assert config.codes == _VALID_CODE_DISPLAY
    assert len(config.design_codes) == 3


def test_unknown_code_string_raises(tmp_path):
    """An unknown code display string is rejected with SweepConfigError (BD-3)."""
    demo._load_engineering_modules()
    demo._init_code_constants()
    code_name_map = {display: code for code, display in demo.CODE_NAMES.items()}
    bad = tmp_path / "bad.yml"
    bad.write_text(
        "meta: {demo_id: demo_02}\n"
        "sweep:\n"
        '  sizes: [\'6"\']\n'
        "  codes: [\"DNV-ST-F101\", \"API-RP-1111\"]\n"  # hyphenated .value form is INVALID here
        "  internal_pressures_mpa: [10]\n"
        "constants:\n"
        "  water_depth_m: 500.0\n"
        "  grade: X65\n"
        "  smys_pa: 448000000.0\n"
        "  smts_pa: 531000000.0\n"
        "  corrosion_allowance_m: 0.001\n"
        "  safety_class: MEDIUM\n"
        "  find_min_bounds_m: [0.003, 0.060]\n"
        "  find_min_tol_m: 0.0001\n"
        "physical_constants:\n"
        "  seawater_density_kg_m3: 1025.0\n"
        "  gravity_m_s2: 9.80665\n"
        "  steel_density_kg_m3: 7850.0\n"
        "report:\n"
        "  design_pressure_mpa: 20\n"
        "catalogs:\n"
        "  pipelines: ../data/pipelines.json\n"
        "  design_codes: ../data/design_codes.json\n"
        "artifacts:\n"
        "  results_root: ../results\n"
        "  output_root: ../output\n",
        encoding="utf-8",
    )
    with pytest.raises(SweepConfigError, match="not a known code|API-RP-1111|display string"):
        load_demo02_config(
            bad, code_name_map=code_name_map, safety_class_enum=demo.SafetyClass
        )


# ---------------------------------------------------------------------------
# Type asserts
# ---------------------------------------------------------------------------


def test_internal_pressure_is_int(produced_results):
    """internal_pressure_mpa stays an int through the pipeline."""
    assert all(
        isinstance(r["internal_pressure_mpa"], int)
        and not isinstance(r["internal_pressure_mpa"], bool)
        for r in produced_results
    )


def test_pipe_size_round_trips_the_quote_char(produced_results):
    """The pipe_size label preserves its literal `"` character."""
    sizes = [r["pipe_size"] for r in produced_results]
    assert '6"' in sizes
    assert '20"' in sizes
    assert all(s.endswith('"') for s in sizes)


# ---------------------------------------------------------------------------
# Edited-config flow (ADR-0005): the yaml is the COMPLETE, LIVE source of truth.
# Editing a constant must propagate into every result and diverge from the golden,
# while the committed baseline yaml still reproduces the golden byte-for-byte.
# ---------------------------------------------------------------------------


def _run_from_config(config):
    demo._load_engineering_modules()
    demo._init_code_constants()
    with open(demo.DATA_DIR / "pipelines.json", "r") as fh:
        pipe_catalog = json.load(fh)["pipes"]
    results, _df = demo.run_parametric_sweep(pipe_catalog, config=config)
    return results


def test_edited_water_depth_propagates_and_diverges_from_golden(tmp_path, golden_results):
    """Copy the yaml, set water_depth_m=2000.0, run; every result reflects the edit and
    the results DIFFER from the golden — while the baseline yaml still equals the golden."""
    import yaml as _yaml

    demo._load_engineering_modules()
    demo._init_code_constants()
    code_name_map = {display: code for code, display in demo.CODE_NAMES.items()}

    # Edit a single constant in a tmp copy of the committed baseline yaml.
    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["constants"]["water_depth_m"] = 2000.0
    edited = tmp_path / "demo_02_edited.yml"
    edited.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    edited_config = load_demo02_config(
        edited, code_name_map=code_name_map, safety_class_enum=demo.SafetyClass
    )
    edited_results = _run_from_config(edited_config)

    # The expected external pressure at 2000 m, computed from the SAME physical constants.
    expected_pe = round(1025.0 * 9.80665 * 2000.0 / 1e6, 3)
    assert len(edited_results) == 72
    for r in edited_results:
        assert r["water_depth_m"] == 2000.0
        assert r["external_pressure_mpa"] == expected_pe

    # The edited run must DIFFER from the golden (the edit is live, not cosmetic).
    assert edited_results != golden_results

    # The committed baseline yaml STILL reproduces the golden, byte-for-byte.
    baseline_config = load_demo02_config(
        _BASELINE_CONFIG_PATH, code_name_map=code_name_map, safety_class_enum=demo.SafetyClass
    )
    baseline_results = _run_from_config(baseline_config)
    assert baseline_results == golden_results


# ---------------------------------------------------------------------------
# Strengthened edited-config coverage (D5): the report scalar (design pressure), an axis
# subset (codes), and the report NARRATIVE (D1) are all driven by the yaml.
# ---------------------------------------------------------------------------


def _edited_config(tmp_path, code_name_map, **edits):
    """Write a tmp copy of the baseline yaml with deep-key edits and resolve it.

    ``edits`` keys are dotted paths into the yaml (e.g. ``report.design_pressure_mpa=60``,
    ``sweep.codes=[...]``).
    """
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    for dotted, value in edits.items():
        section, key = dotted.split(".", 1)
        raw[section][key] = value
    edited = tmp_path / "demo_02_edited.yml"
    edited.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    return load_demo02_config(
        edited, code_name_map=code_name_map, safety_class_enum=demo.SafetyClass
    )


@pytest.fixture(scope="module")
def _code_name_map():
    demo._load_engineering_modules()
    demo._init_code_constants()
    return {display: code for code, display in demo.CODE_NAMES.items()}


def test_edited_design_pressure_changes_summary_and_chart2(tmp_path, _code_name_map):
    """An edited report.design_pressure_mpa (60) changes build_summary_table AND
    build_chart_2_min_wt output vs the baseline design pressure (20)."""
    demo._load_engineering_modules()
    demo._init_code_constants()

    baseline_config = load_demo02_config(
        _BASELINE_CONFIG_PATH, code_name_map=_code_name_map, safety_class_enum=demo.SafetyClass
    )
    edited_config = _edited_config(
        tmp_path, _code_name_map, **{"report.design_pressure_mpa": 60}
    )
    assert edited_config.design_pressure_mpa == 60

    base_summary = demo.build_summary_table(config=baseline_config)
    edit_summary = demo.build_summary_table(config=edited_config)
    # Same shape (same axes), but the min-WT column must move with the higher pressure.
    assert list(base_summary["Min WT (mm)"]) != list(edit_summary["Min WT (mm)"])

    base_fig, base_data = demo.build_chart_2_min_wt(config=baseline_config)
    edit_fig, edit_data = demo.build_chart_2_min_wt(config=edited_config)
    assert base_data != edit_data


def test_edited_codes_subset_yields_right_number_of_traces(tmp_path, _code_name_map):
    """A codes subset (["PD 8010-2"]) yields one Chart-2 trace and one Chart-3 heatmap
    subplot, vs three for the baseline."""
    demo._load_engineering_modules()
    demo._init_code_constants()
    with open(demo.DATA_DIR / "pipelines.json", "r") as fh:
        pipe_catalog = json.load(fh)["pipes"]

    subset_config = _edited_config(
        tmp_path, _code_name_map, **{"sweep.codes": ["PD 8010-2"]}
    )
    assert subset_config.codes == ["PD 8010-2"]

    fig2, _ = demo.build_chart_2_min_wt(config=subset_config)
    # One scatter trace per code, plus the single "Code Penalty Band" fill trace.
    assert len(fig2.data) == 1 + 1

    fig3 = demo.build_chart_3_heatmap(pipe_catalog=pipe_catalog, config=subset_config)
    # One heatmap subplot/trace per code.
    assert len(fig3.data) == 1

    # Baseline has three codes: three line traces + the band for Chart 2, three heatmaps.
    baseline_config = load_demo02_config(
        _BASELINE_CONFIG_PATH, code_name_map=_code_name_map, safety_class_enum=demo.SafetyClass
    )
    base_fig2, _ = demo.build_chart_2_min_wt(config=baseline_config)
    base_fig3 = demo.build_chart_3_heatmap(pipe_catalog=pipe_catalog, config=baseline_config)
    assert len(base_fig2.data) == 3 + 1
    assert len(base_fig3.data) == 3


def test_build_report_narrative_reflects_edited_config(tmp_path, _code_name_map):
    """D1: build_report's HTML carries the EDITED config's values (new water depth +
    design pressure), while the baseline-config report carries today's literals."""
    import plotly.graph_objects as go

    demo._load_engineering_modules()
    demo._init_code_constants()

    edited_config = _edited_config(
        tmp_path,
        _code_name_map,
        **{"constants.water_depth_m": 1500.0, "report.design_pressure_mpa": 60},
    )

    empty = go.Figure()
    import pandas as pd

    summary_df = pd.DataFrame(
        [{"Pipe Size": '6"', "OD (mm)": 168.3, "Code": "PD 8010-2",
          "Min WT (mm)": 5.0, "Governing Check": "Burst", "Status": "PASS"}]
    )
    out = tmp_path / "edited_report.html"
    html = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, [], {}, 72,
        output_path=out, config=edited_config,
    )
    # Edited values appear; the baseline literals (500 m / 20 MPa) do not drive the prose.
    assert "1500 m" in html
    assert "1500m water depth" in html
    assert "At 60 MPa internal pressure" in html

    # The baseline-config report carries today's literals.
    baseline_config = load_demo02_config(
        _BASELINE_CONFIG_PATH, code_name_map=_code_name_map, safety_class_enum=demo.SafetyClass
    )
    out2 = tmp_path / "baseline_report.html"
    html_base = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, [], {}, 72,
        output_path=out2, config=baseline_config,
    )
    assert "500 m" in html_base
    assert "At 20 MPa internal pressure" in html_base
