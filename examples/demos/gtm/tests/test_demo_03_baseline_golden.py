# ABOUTME: Golden regression test — demo_03's Baseline Sweep Config reproduces the 180-case oracle.
# ABOUTME: Acceptance gate for the yaml-driven config externalization: byte-identical to the golden.
"""Golden regression + loader/normalize unit tests for demo_03's yaml-driven Sweep Config.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_03_baseline_golden.py -x -q
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import demo_03_deepwater_mudmat_installation as demo
from sweep_config_demo03 import (
    SweepConfigError,
    ResolvedDemo03Config,
    load_demo03_config,
    load_demo03_paths,
)

_GTM_DIR = Path(demo.__file__).resolve().parent
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_03_baseline_results.json"
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_03_mudmat.yml"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _load_catalogs():
    with open(demo.DATA_DIR / "csv_hlv_vessels.json", "r") as fh:
        vessels = json.load(fh)["vessels"]
    with open(demo.DATA_DIR / "mudmat_structures.json", "r") as fh:
        structures = json.load(fh)["structures"]
    return vessels, structures


def _run_from_config(config):
    vessels, structures = _load_catalogs()
    results, _df = demo.run_parametric_sweep(vessels, structures, config=config)
    return results


@pytest.fixture(scope="module")
def produced_results():
    """Run the sweep from the committed Baseline config (recompute path)."""
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    return _run_from_config(config)


@pytest.fixture(scope="module")
def produced_serialised(produced_results):
    """The serialised (phases_flat-bearing) per-case dicts."""
    return demo.serialise_results(produced_results)


@pytest.fixture(scope="module")
def golden_doc():
    with _GOLDEN_PATH.open("r", encoding="utf-8") as fh:
        return json.load(fh)


@pytest.fixture(scope="module")
def golden_results(golden_doc):
    return golden_doc["cases"]


# ---------------------------------------------------------------------------
# Golden oracle
# ---------------------------------------------------------------------------


def test_produced_results_equal_golden_in_order_field_by_field(
    produced_serialised, golden_results
):
    """Primary oracle: produced cases == golden cases in order, all fields.

    Comparing the full ordered lists subsumes count, emission order (vessel -> depth ->
    structure -> hs), every field — including the nested ``phases{}`` and the ``phases_flat``
    key set — in a single assertion.
    """
    assert produced_serialised == golden_results


def test_case_count_and_status_split(produced_serialised):
    """180 cases; overall_status split 174 GO / 6 MARGINAL / 0 NO_GO."""
    assert len(produced_serialised) == 180
    go = sum(1 for r in produced_serialised if r["overall_status"] == "GO")
    marginal = sum(1 for r in produced_serialised if r["overall_status"] == "MARGINAL")
    nogo = sum(1 for r in produced_serialised if r["overall_status"] == "NO_GO")
    assert go == 174
    assert marginal == 6
    assert nogo == 0


def test_every_case_has_exactly_the_11_frozen_keys(produced_results):
    """Every PRE-serialise case carries exactly the 11 FROZEN top-level keys."""
    assert len(demo.FROZEN_RESULT_KEYS) == 11
    for r in produced_results:
        assert set(r.keys()) == set(demo.FROZEN_RESULT_KEYS)


# ---------------------------------------------------------------------------
# B2: the two DELIBERATELY DIFFERENT flatten transforms.
# ---------------------------------------------------------------------------


def test_phases_flat_key_set_is_the_49_golden_keys(produced_serialised, golden_results):
    """B2: phases_flat preserves the hyphen (lift-off_, in-air_, splash_zone_).

    The full 49-key set must match the golden's case-0 phases_flat exactly.
    """
    golden_flat_keys = list(golden_results[0]["phases_flat"].keys())
    assert len(golden_flat_keys) == 49
    for r in produced_serialised:
        assert list(r["phases_flat"].keys()) == golden_flat_keys
    # The hyphen-preserving prefixes are present (NOT under-scored).
    flat = produced_serialised[0]["phases_flat"]
    assert "lift-off_phase" in flat
    assert "in-air_phase" in flat
    assert "splash_zone_phase" in flat
    assert "lowering_phase" in flat
    assert "landing_phase" in flat


def test_util_columns_drop_the_hyphen(produced_results):
    """B2: the DataFrame util_* columns DROP the hyphen (util_lift_off, util_in_air)."""
    vessels, structures = _load_catalogs()
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    _results, df = demo.run_parametric_sweep(vessels, structures, config=config)
    for col in (
        "util_lift_off",
        "util_in_air",
        "util_splash_zone",
        "util_lowering",
        "util_landing",
    ):
        assert col in df.columns
    # The hyphen-preserving forms must NOT appear as columns.
    assert "util_lift-off" not in df.columns
    assert "util_in-air" not in df.columns


# ---------------------------------------------------------------------------
# B1 type asserts
# ---------------------------------------------------------------------------


def test_water_depth_is_int(produced_serialised):
    """B1: water_depth_m stays an int through the pipeline."""
    assert all(
        isinstance(r["water_depth_m"], int) and not isinstance(r["water_depth_m"], bool)
        for r in produced_serialised
    )


def test_hs_is_float(produced_serialised):
    """B1: hs_m is a float."""
    assert all(isinstance(r["hs_m"], float) for r in produced_serialised)


# ---------------------------------------------------------------------------
# Metadata + summary byte-identity (SLICE 3 gate).
# ---------------------------------------------------------------------------


def test_metadata_constants_match_golden(produced_results, golden_doc):
    """The rebuilt metadata.constants sub-dict matches the golden."""
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    metadata = demo.build_metadata(produced_results, config=config)
    assert metadata["constants"] == golden_doc["metadata"]["constants"]
    assert metadata["water_depths_m"] == golden_doc["metadata"]["water_depths_m"]
    assert metadata["hs_values_m"] == golden_doc["metadata"]["hs_values_m"]
    assert metadata["reference_hs_m"] == golden_doc["metadata"]["reference_hs_m"]
    assert metadata == golden_doc["metadata"]


def test_summary_matches_golden(produced_results, golden_doc):
    """The summary table (records) matches the golden byte-for-byte."""
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    vessels, structures = _load_catalogs()
    _results, df = demo.run_parametric_sweep(vessels, structures, config=config)
    summary_df = demo.build_summary_table(df, config=config)
    assert summary_df.to_dict(orient="records") == golden_doc["summary"]


# ---------------------------------------------------------------------------
# Loader round-trip + type unit tests (SLICE 1 gate).
# ---------------------------------------------------------------------------


def test_loader_round_trips_baseline_axes():
    """The baseline yaml resolves to today's exact axes."""
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    assert isinstance(config, ResolvedDemo03Config)
    assert config.vessels == ["Large CSV", "Medium CSV"]
    assert config.depths == [500, 1000, 1500, 2000, 2500, 3000]
    assert config.mudmats == ["Mudmat-S-50te", "Mudmat-M-100te", "Mudmat-L-200te"]
    assert config.hs == [1.0, 1.5, 2.0, 2.5, 3.0]


def test_loader_depths_are_int_hs_are_float():
    """B1: resolved depths are ints (not bool), hs are floats."""
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    assert all(isinstance(d, int) and not isinstance(d, bool) for d in config.depths)
    assert all(isinstance(h, float) for h in config.hs)


def test_loader_resolves_constants_and_soil():
    """All constants + the honest soil label resolve to today's values."""
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    assert config.daf_liftoff == 1.10
    assert config.daf_splash == 1.30
    assert config.wire_mbl_sf == 0.85
    assert config.tilt_limit_deg == 5.0
    assert config.operating_radius_m == 40.0
    assert config.reference_hs == 2.0
    assert config.tp_coefficient == 4.0
    assert config.go_marginal_threshold == 0.85
    assert config.nogo_utilisation == 1.0
    assert config.seawater_density_kg_m3 == 1025.0
    assert config.gravity_m_s2 == 9.80665
    assert config.steel_density_kg_m3 == 7850.0
    # HONEST label: allowable bearing PRESSURE, value 50.0 (golden holds).
    assert config.allowable_bearing_pressure_kpa == 50.0


def test_loader_resolves_paths():
    """Catalog + artifact paths resolve absolute relative to the yaml dir."""
    paths = load_demo03_paths(_BASELINE_CONFIG_PATH)
    assert paths.vessels_path.name == "csv_hlv_vessels.json"
    assert paths.mudmats_path.name == "mudmat_structures.json"
    assert paths.vessels_path.exists()
    assert paths.mudmats_path.exists()


def test_loader_rejects_unknown_axis(tmp_path):
    """An unknown sweep axis key is rejected (additionalProperties: False)."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["bogus_axis"] = [1, 2, 3]
    bad = tmp_path / "bad.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError, match="schema validation failed|bogus_axis|Additional"):
        load_demo03_config(bad)


def test_loader_rejects_float_depth(tmp_path):
    """B1: a float in depths is rejected by the integer-item schema."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["depths"] = [500.5, 1000]
    bad = tmp_path / "bad_depth.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo03_config(bad)


def test_loader_rejects_bool_depth(tmp_path):
    """B1: a bool in depths (which is an int subclass) is rejected at resolution time."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    # YAML 'true' parses to a Python bool; jsonschema 'integer' rejects bools too, but the
    # runtime resolution assert is the belt-and-braces guard.
    raw["sweep"]["depths"] = [True, 1000]
    bad = tmp_path / "bad_bool_depth.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo03_config(bad)


# ---------------------------------------------------------------------------
# Edited-config flow (ADR-0005): the yaml is the COMPLETE, LIVE source of truth.
# Editing a constant must propagate into every result and diverge from the golden,
# while the committed baseline yaml still reproduces the golden byte-for-byte.
# ---------------------------------------------------------------------------


def _edited_config(tmp_path, **edits):
    """Write a tmp copy of the baseline yaml with dotted-path edits and resolve it.

    ``edits`` keys are dotted paths into the yaml (e.g. ``constants.operating_radius_m=30.0``,
    ``sweep.depths=[...]``).
    """
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    for dotted, value in edits.items():
        section, key = dotted.split(".", 1)
        raw[section][key] = value
    edited = tmp_path / "demo_03_edited.yml"
    edited.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    return load_demo03_config(edited)


def test_edited_operating_radius_propagates_and_diverges(tmp_path, golden_results):
    """Set operating_radius_m to a smaller (more capacity) radius; results change and
    DIFFER from the golden — while the baseline yaml still equals the golden."""
    edited_config = _edited_config(tmp_path, **{"constants.operating_radius_m": 30.0})
    assert edited_config.operating_radius_m == 30.0
    edited_results = demo.serialise_results(_run_from_config(edited_config))
    assert len(edited_results) == 180
    # The lift-off operating radius is now 30.0 in every case.
    for r in edited_results:
        assert r["phases"]["Lift-off"]["operating_radius_m"] == 30.0
    assert edited_results != golden_results

    # The committed baseline yaml STILL reproduces the golden byte-for-byte.
    baseline_results = demo.serialise_results(
        _run_from_config(load_demo03_config(_BASELINE_CONFIG_PATH))
    )
    assert baseline_results == golden_results


def test_edited_vessels_subset_changes_chart_trace_count(tmp_path):
    """SLICE 3: a vessels subset (["Large CSV"]) yields fewer Chart-2 traces and one
    Chart-1 heatmap subplot vs two for the baseline (charts are config-driven)."""
    vessels, structures = _load_catalogs()

    subset_config = _edited_config(tmp_path, **{"sweep.vessels": ["Large CSV"]})
    assert subset_config.vessels == ["Large CSV"]
    _results, df = demo.run_parametric_sweep(vessels, structures, config=subset_config)

    fig1 = demo.build_chart_1_go_nogo_heatmap(df, config=subset_config)
    fig2 = demo.build_chart_2_crane_utilisation(df, config=subset_config)
    # One vessel -> one heatmap subplot trace; 1 vessel x 3 mudmats = 3 Chart-2 lines.
    assert len(fig1.data) == 1
    assert len(fig2.data) == 3

    baseline_config = load_demo03_config(_BASELINE_CONFIG_PATH)
    _bres, bdf = demo.run_parametric_sweep(vessels, structures, config=baseline_config)
    base_fig1 = demo.build_chart_1_go_nogo_heatmap(bdf, config=baseline_config)
    base_fig2 = demo.build_chart_2_crane_utilisation(bdf, config=baseline_config)
    # Two vessels -> two heatmap subplots; 2 vessels x 3 mudmats = 6 Chart-2 lines.
    assert len(base_fig1.data) == 2
    assert len(base_fig2.data) == 6


def test_edited_reference_hs_changes_chart_titles(tmp_path):
    """SLICE 3: editing constants.reference_hs threads into chart titles."""
    vessels, structures = _load_catalogs()
    edited_config = _edited_config(tmp_path, **{"constants.reference_hs": 1.5})
    assert edited_config.reference_hs == 1.5
    _results, df = demo.run_parametric_sweep(vessels, structures, config=edited_config)
    fig1 = demo.build_chart_1_go_nogo_heatmap(df, config=edited_config)
    fig2 = demo.build_chart_2_crane_utilisation(df, config=edited_config)
    assert "Hs = 1.5 m" in fig1.layout.title.text
    assert "Hs = 1.5 m" in fig2.layout.title.text


def test_edited_depth_propagates(tmp_path, golden_results):
    """Editing a swept depth changes the cases (new depth appears; lowering reflects it)."""
    edited_config = _edited_config(tmp_path, **{"sweep.depths": [500, 4000]})
    assert edited_config.depths == [500, 4000]
    edited_results = demo.serialise_results(_run_from_config(edited_config))
    # 2 vessels x 2 depths x 3 mudmats x 5 hs = 60 cases.
    assert len(edited_results) == 60
    depths_seen = sorted({r["water_depth_m"] for r in edited_results})
    assert depths_seen == [500, 4000]
    assert edited_results != golden_results


# ---------------------------------------------------------------------------
# SLICE 4: report narrative + honesty defect fixes (B3a, B3b).
# ---------------------------------------------------------------------------


def _empty_report_args():
    import plotly.graph_objects as go
    import pandas as pd

    empty = go.Figure()
    summary_df = pd.DataFrame(
        [{"Vessel": "Large CSV", "Structure": "Mudmat-S-50te", "Depth (m)": 500,
          "Max Util.": "24.7%", "Governing Phase": "Landing", "Status": "GO"}]
    )
    return empty, summary_df


def test_report_honesty_b3a_b3b_baseline(tmp_path):
    """B3a: the crane-SWL assumption describes the 40 m DERATED operating radius, NOT the
    'maximum capacity radius (most favourable position)'. B3b: the landing criterion is an
    'allowable bearing pressure', NOT a 'bearing capacity'."""
    vessels, structures = _load_catalogs()
    config = load_demo03_config(_BASELINE_CONFIG_PATH)
    results = _run_from_config(config)
    empty, summary_df = _empty_report_args()
    out = tmp_path / "report.html"
    html = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, results, 180,
        config=config, output_path=out,
    )
    # B3a: the FALSE claim is gone; the honest derated-radius wording is present.
    assert "maximum capacity radius (most favourable position)" not in html
    assert "most favourable maximum-capacity position" in html
    assert "40 m overboard operating radius" in html
    # B3b: "bearing capacity" as the criterion is gone; "allowable bearing pressure" is present.
    assert "Allowable bearing pressure = 50 kPa" in html
    assert "50 kPa soft clay bearing capacity" not in html
    # B3b placeholder note: q_ult = su * Nc is a planned extension.
    assert "q_ult = su * Nc" in html or "q<sub>ult</sub>" in html


def test_report_narrative_reflects_edited_config(tmp_path):
    """The report prose carries the EDITED config's values (operating radius, DAFs, tilt
    limit), while the baseline-config report carries today's literals."""
    vessels, structures = _load_catalogs()
    edited_config = _edited_config(
        tmp_path,
        **{
            "constants.operating_radius_m": 35.0,
            "constants.daf_splash": 1.5,
            "constants.tilt_limit_deg": 4.0,
        },
    )
    results = demo.serialise_results(_run_from_config(edited_config))
    empty, summary_df = _empty_report_args()
    out = tmp_path / "edited_report.html"
    html = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, results, 180,
        config=edited_config, output_path=out,
    )
    assert "35 m overboard operating radius" in html
    assert "DAF<sub>splash</sub> = 1.5" in html
    assert "4&deg; limit" in html

    # Baseline report carries today's literals (40 m, 1.3, 5 deg).
    baseline_config = load_demo03_config(_BASELINE_CONFIG_PATH)
    base_results = _run_from_config(baseline_config)
    out2 = tmp_path / "baseline_report.html"
    html_base = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, base_results, 180,
        config=baseline_config, output_path=out2,
    )
    assert "40 m overboard operating radius" in html_base
    assert "DAF<sub>splash</sub> = 1.3" in html_base
    assert "5&deg; limit" in html_base


# ---------------------------------------------------------------------------
# D1/D4: --from-cache regression coverage.
#   (a) baseline --from-cache reproduces a byte-identical report (apart from the wall-clock
#       generation timestamp) and never crashes;
#   (b) --from-cache + a non-default --config is REFUSED with a clean error (the D1 choice),
#       which also covers the previously-crashing cached-config-whose-hs-excludes-2.0 case
#       (it is refused before any chart filters an empty ref_df -> ValueError).
# ---------------------------------------------------------------------------


def _normalise_report_timestamp(html: str) -> str:
    """Strip the wall-clock 'Generated <ts> UTC' so two runs compare content-only."""
    import re

    return re.sub(r"Generated [\d-]+ [\d:]+ UTC", "Generated TS UTC", html)


def _run_demo_cli(args, cwd):
    import os
    import subprocess

    env = dict(os.environ)
    env["PYTHONPATH"] = f"{_GTM_DIR}:{cwd / 'src'}"
    return subprocess.run(
        [__import__("sys").executable, str(_GTM_DIR / "demo_03_deepwater_mudmat_installation.py"), *args],
        cwd=str(cwd), env=env, capture_output=True, text=True, timeout=300,
    )


def test_from_cache_baseline_report_byte_identical_no_crash(tmp_path):
    """D1(a): baseline --from-cache (NO --config -> committed baseline) rebuilds the report from
    the REAL baseline config and reproduces the recompute report byte-for-byte (apart from the
    generation timestamp), and does NOT crash.

    The from-cache gate keys on the config path being the committed baseline, so this exercises
    the gate's happy path (default config). Artifacts land in the demo's own results/ and output/
    trees; we snapshot + restore them so the test leaves the worktree clean.
    """
    import shutil

    repo_root = _GTM_DIR.parents[3]
    paths = load_demo03_paths(_BASELINE_CONFIG_PATH)
    results_path = paths.results_root / "demo_03_mudmat_installation_results.json"
    report_path = paths.output_root / "demo_03_mudmat_installation_report.html"

    # Snapshot any pre-existing artifacts so we can restore the tree afterwards.
    backups = {}
    for p in (results_path, report_path):
        if p.exists():
            b = tmp_path / (p.name + ".bak")
            shutil.copy(p, b)
            backups[p] = b
    try:
        # Recompute (writes cache + report).
        proc1 = _run_demo_cli([], repo_root)
        assert proc1.returncode == 0, f"recompute exited {proc1.returncode}\n{proc1.stderr[-2000:]}"
        recompute_report = _normalise_report_timestamp(report_path.read_text(encoding="utf-8"))

        # --from-cache (no crash, rebuilds report from the real baseline config).
        proc2 = _run_demo_cli(["--from-cache"], repo_root)
        assert proc2.returncode == 0, f"from-cache exited {proc2.returncode}\n{proc2.stderr[-2000:]}"
        assert "Traceback" not in proc2.stderr
        fromcache_report = _normalise_report_timestamp(report_path.read_text(encoding="utf-8"))

        # Byte-identical content apart from the timestamp.
        assert fromcache_report == recompute_report
    finally:
        for p, b in backups.items():
            shutil.copy(b, p)


def test_from_cache_refuses_non_default_config_including_hs_excluding_reference(tmp_path):
    """D1(b): --from-cache + a NON-default --config is refused with a clean [ERROR] + exit 2.

    The config here has an hs axis that EXCLUDES 2.0 (the old crash trigger: a config=None
    chart 1 would filter hs_m == REFERENCE_HS=2.0 -> 0 rows -> ValueError). The refusal fires
    BEFORE any chart is built, so the crash is structurally impossible on from-cache.
    """
    import shutil
    import yaml as _yaml

    repo_root = _GTM_DIR.parents[3]
    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    results_root = tmp_path / "results"
    output_root = tmp_path / "output"
    data_dir = tmp_path / "data"
    for d in (results_root, output_root, data_dir):
        d.mkdir()
    shutil.copy(_GTM_DIR / "data" / "csv_hlv_vessels.json", data_dir / "csv_hlv_vessels.json")
    shutil.copy(_GTM_DIR / "data" / "mudmat_structures.json", data_dir / "mudmat_structures.json")
    raw["catalogs"] = {"vessels": "data/csv_hlv_vessels.json", "mudmats": "data/mudmat_structures.json"}
    raw["artifacts"] = {"results_root": "results", "output_root": "output"}
    raw["sweep"]["hs"] = [1.0, 1.5, 2.5, 3.0]      # EXCLUDES 2.0
    raw["constants"]["reference_hs"] = 2.5          # keep the reference within the axis
    cfg = tmp_path / "client_hs_no_2.yml"
    cfg.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    # Recompute with this config runs fine (no crash) because config is threaded.
    proc_recompute = _run_demo_cli(["--config", str(cfg)], repo_root)
    assert proc_recompute.returncode == 0, f"recompute exited {proc_recompute.returncode}\n{proc_recompute.stderr[-2000:]}"

    # --from-cache with this non-default config is REFUSED (clean error, exit 2, no traceback).
    proc = _run_demo_cli(["--from-cache", "--config", str(cfg)], repo_root)
    assert proc.returncode == 2, f"expected exit 2, got {proc.returncode}\n{proc.stdout[-2000:]}"
    assert "--from-cache is only supported with the committed baseline config" in proc.stdout
    assert "Traceback" not in proc.stderr


def test_schema_invalid_config_clean_exit_2(tmp_path):
    """D3: a schema-invalid (but existing) --config exits 2 with a clean [ERROR], NOT a raw
    SweepConfigError traceback nor a misleading 'using committed defaults' warning."""
    import yaml as _yaml

    repo_root = _GTM_DIR.parents[3]
    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["bogus_axis"] = [1, 2, 3]           # additionalProperties: False -> invalid
    cfg = tmp_path / "bad.yml"
    cfg.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    proc = _run_demo_cli(["--config", str(cfg)], repo_root)
    assert proc.returncode == 2, f"expected exit 2, got {proc.returncode}\n{proc.stdout[-2000:]}"
    assert "[ERROR]" in proc.stdout
    assert "schema validation failed" in proc.stdout
    assert "Traceback" not in proc.stderr
    assert "using committed defaults" not in proc.stdout


def test_cli_config_smoke(tmp_path):
    """--config smoke: a copied baseline yaml + a tmp results/output root runs end-to-end
    and writes a 180-case JSON whose metadata + summary + cases match the golden."""
    import shutil
    import subprocess

    repo_root = _GTM_DIR.parents[3]  # .../dm-gtm-demo03
    # Build a tmp config pointing artifacts at tmp dirs (so we don't touch the demo's tree).
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    results_root = tmp_path / "results"
    output_root = tmp_path / "output"
    results_root.mkdir()
    output_root.mkdir()
    # Catalog paths must still resolve relative to the tmp yaml dir; copy the catalogs in.
    data_dir = tmp_path / "data"
    data_dir.mkdir()
    shutil.copy(_GTM_DIR / "data" / "csv_hlv_vessels.json", data_dir / "csv_hlv_vessels.json")
    shutil.copy(_GTM_DIR / "data" / "mudmat_structures.json", data_dir / "mudmat_structures.json")
    raw["catalogs"] = {"vessels": "data/csv_hlv_vessels.json", "mudmats": "data/mudmat_structures.json"}
    raw["artifacts"] = {"results_root": "results", "output_root": "output"}
    cfg = tmp_path / "client.yml"
    cfg.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    env = dict(**__import__("os").environ)
    env["PYTHONPATH"] = f"{_GTM_DIR}:{repo_root / 'src'}"
    proc = subprocess.run(
        [
            __import__("sys").executable,
            str(_GTM_DIR / "demo_03_deepwater_mudmat_installation.py"),
            "--config", str(cfg),
        ],
        cwd=str(repo_root), env=env, capture_output=True, text=True, timeout=300,
    )
    assert proc.returncode == 0, f"demo exited {proc.returncode}\nSTDERR:\n{proc.stderr[-2000:]}"
    out_json = results_root / "demo_03_mudmat_installation_results.json"
    assert out_json.exists(), f"results json not written; stdout tail:\n{proc.stdout[-2000:]}"
    produced = json.loads(out_json.read_text())
    golden = json.loads(_GOLDEN_PATH.read_text())
    assert produced["cases"] == golden["cases"]
    assert produced["metadata"] == golden["metadata"]
    assert produced["summary"] == golden["summary"]
