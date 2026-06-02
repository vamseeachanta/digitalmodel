# ABOUTME: Golden regression test — demo_04's Baseline Sweep Config reproduces the 60-case oracle.
# ABOUTME: Acceptance gate for the yaml-driven config externalization + the §4 combined-stress fix.
"""Golden regression + loader/normalize unit tests for demo_04's yaml-driven Sweep Config.

The golden is frozen on the CORRECTED (combined bending+axial) sagbend output — the §4 fix
(user-approved default ``sagbend_stress_basis: combined``). Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_04_baseline_golden.py -x -q
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import demo_04_shallow_water_pipelay as demo
from sweep_config_demo04 import (
    SweepConfigError,
    ResolvedDemo04Config,
    load_demo04_config,
    load_demo04_paths,
)

_GTM_DIR = Path(demo.__file__).resolve().parent
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_04_baseline_results.json"
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_04_pipelay.yml"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _run_from_config(config):
    vessels = demo.load_vessels(config=config)
    pipes = demo.load_pipes(config=config)
    results, _df = demo.run_parametric_sweep(vessels, pipes, config=config)
    return results


@pytest.fixture(scope="module")
def produced_results():
    """Run the sweep from the committed Baseline config (recompute path)."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    return _run_from_config(config)


@pytest.fixture(scope="module")
def produced_serialised(produced_results):
    """The serialised (checks_flat-bearing) per-case dicts."""
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

    Comparing the full ordered lists subsumes count, emission order (vessel -> pipe -> depth),
    and every field — including the nested ``checks{}`` and the ``checks_flat`` key set — in a
    single assertion. The golden is the CORRECTED combined-stress output.
    """
    assert produced_serialised == golden_results


def test_case_count_and_status_split(produced_serialised):
    """60 cases; overall_status split 50 GO / 5 MARGINAL / 5 NO_GO (post combined-stress fix)."""
    assert len(produced_serialised) == 60
    go = sum(1 for r in produced_serialised if r["overall_status"] == "GO")
    marginal = sum(1 for r in produced_serialised if r["overall_status"] == "MARGINAL")
    nogo = sum(1 for r in produced_serialised if r["overall_status"] == "NO_GO")
    assert go == 50
    assert marginal == 5
    assert nogo == 5


# ---------------------------------------------------------------------------
# §4 combined-stress fix — the headline corrections.
# ---------------------------------------------------------------------------


def _case(results, vessel_id, pipe_size, depth):
    for r in results:
        if (
            r["vessel_id"] == vessel_id
            and r["pipe_size"] == pipe_size
            and r["water_depth_m"] == depth
        ):
            return r
    raise AssertionError(f"case not found: {vessel_id} {pipe_size} {depth}")


def test_combined_stress_24in_30m_governing_value(produced_serialised):
    """24in @ 30m (Large PLV) is the headline combined-stress case:
    sigma_bend ~ 20.7 + sigma_axial ~ 284.8 = 305.5 MPa -> util ~ 0.947 (sagbend governing)."""
    r = _case(produced_serialised, "PLV-001", "24in", 30)
    assert r["governing_check"] == "sagbend"
    assert r["max_utilisation"] == pytest.approx(0.947, abs=0.001)
    cf = r["checks_flat"]
    assert cf["sagbend_sigma_bending_mpa"] == pytest.approx(20.7, abs=0.1)
    assert cf["sagbend_sigma_axial_mpa"] == pytest.approx(284.8, abs=0.1)
    # sigma_combined_mpa is the SUM (no longer == bending only).
    assert cf["sagbend_sigma_combined_mpa"] == pytest.approx(305.5, abs=0.1)
    assert r["sagbend_stress_mpa"] == pytest.approx(305.5, abs=0.1)
    assert cf["sagbend_stress_basis"] == "combined"


def test_combined_stress_sigma_combined_equals_bending_plus_axial(produced_serialised):
    """For every feasible (H>0) case, sigma_combined_mpa == sigma_bending + sigma_axial."""
    for r in produced_serialised:
        cf = r["checks_flat"]
        if cf.get("sagbend_stress_basis") != "combined":
            continue
        if r["H_chosen_kn"] <= 0:
            continue
        expected = cf["sagbend_sigma_bending_mpa"] + cf["sagbend_sigma_axial_mpa"]
        assert cf["sagbend_sigma_combined_mpa"] == pytest.approx(expected, abs=0.2)


@pytest.mark.parametrize("pipe_size", ["12in", "16in", "20in"])
def test_go_to_marginal_flips_present(produced_serialised, pipe_size):
    """The 3 GO->MARGINAL flips at 30m (Large PLV) introduced by adding axial stress:
    12in ~ 0.855, 16in ~ 0.882, 20in ~ 0.851 — all now MARGINAL."""
    r = _case(produced_serialised, "PLV-001", pipe_size, 30)
    assert r["overall_status"] == "MARGINAL"
    assert 0.85 <= r["max_utilisation"] <= 1.0


def test_five_nogo_are_large_plv_at_7m_depth_envelope(produced_serialised):
    """The 5 NO_GO cases are all Large PLV (min_depth 10 > 7) at 7m — depth-envelope
    rejections, unchanged by the stress-basis fix."""
    nogo = [r for r in produced_serialised if r["overall_status"] == "NO_GO"]
    assert len(nogo) == 5
    for r in nogo:
        assert r["vessel_id"] == "PLV-001"
        assert r["water_depth_m"] == 7
        assert r["governing_check"] == "vessel_capability"


# ---------------------------------------------------------------------------
# Loader round-trip + type unit tests.
# ---------------------------------------------------------------------------


def test_loader_round_trips_baseline_axes():
    """The baseline yaml resolves to today's exact axes."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    assert isinstance(config, ResolvedDemo04Config)
    assert config.vessels == ["PLV-001", "PLV-002"]
    assert config.water_depths == [7, 10, 15, 20, 25, 30]
    # PS: the pipe selection map (nominal -> target WT mm) is ON the compute path.
    assert config.pipe_selection == {
        "8in": 8.18, "12in": 9.53, "16in": 9.53, "20in": 9.53, "24in": 9.53,
    }


def test_loader_depths_are_int():
    """B1: resolved water depths are ints (not bool)."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    assert all(isinstance(d, int) and not isinstance(d, bool) for d in config.water_depths)


def test_loader_resolves_constants_and_grade():
    """All constants + the material grade->SMYS/SMTS selection resolve to today's values."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    # Material grade selects SMYS/SMTS.
    assert config.grade == "X65"
    assert config.smys_pa == 448e6
    assert config.smts_pa == 531e6
    # Physical constants.
    assert config.seawater_density_kg_m3 == 1025.0
    assert config.gravity_m_s2 == 9.80665
    assert config.steel_density_kg_m3 == 7850.0
    assert config.youngs_modulus_pa == 207e9
    # Criteria (incl. the pulled-out TENSION_MARGIN + the §4 stress-basis toggle).
    assert config.stress_limit_factor == 0.72
    assert config.tension_margin == 1.10
    assert config.go_threshold == 0.85
    assert config.nogo_utilisation == 1.0
    assert config.sagbend_stress_basis == "combined"
    # Display-only colour thresholds — SEPARATE from the go/no-go logic bands.
    assert config.util_color_green_below == 0.70
    assert config.util_color_amber_below == 0.90


def test_loader_resolves_paths():
    """Catalog + artifact paths resolve absolute relative to the yaml dir."""
    paths = load_demo04_paths(_BASELINE_CONFIG_PATH)
    assert paths.vessels_path.name == "pipelay_vessels.json"
    assert paths.pipelines_path.name == "pipelines.json"
    assert paths.vessels_path.exists()
    assert paths.pipelines_path.exists()


def test_grade_selection_x70_changes_smys():
    """Selecting a different grade (X70) resolves a different SMYS/SMTS."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["material"]["grade"] = "X70"
    import tempfile

    with tempfile.NamedTemporaryFile("w", suffix=".yml", delete=False) as fh:
        _yaml.safe_dump(raw, fh, sort_keys=False)
        path = fh.name
    config = load_demo04_config(path)
    assert config.grade == "X70"
    assert config.smys_pa == 482e6
    assert config.smts_pa == 565e6


# ---------------------------------------------------------------------------
# Schema rejection tests.
# ---------------------------------------------------------------------------


def test_loader_rejects_unknown_axis(tmp_path):
    """An unknown sweep axis key is rejected (additionalProperties: False)."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["bogus_axis"] = [1, 2, 3]
    bad = tmp_path / "bad.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError, match="schema validation failed|bogus_axis|Additional"):
        load_demo04_config(bad)


def test_loader_rejects_float_depth(tmp_path):
    """B1: a float in water_depths is rejected by the integer-item schema."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["water_depths_m"] = [7.5, 10]
    bad = tmp_path / "bad_depth.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo04_config(bad)


def test_loader_rejects_bool_depth(tmp_path):
    """B1: a bool in water_depths (an int subclass) is rejected at resolution time."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["water_depths_m"] = [True, 10]
    bad = tmp_path / "bad_bool_depth.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo04_config(bad)


def test_loader_rejects_unknown_grade(tmp_path):
    """A material.grade not present in the grades map is rejected at resolution."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["material"]["grade"] = "X999"
    bad = tmp_path / "bad_grade.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError, match="grade"):
        load_demo04_config(bad)


def test_loader_rejects_invalid_stress_basis(tmp_path):
    """An out-of-enum sagbend_stress_basis is rejected (enum guard)."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["criteria"]["sagbend_stress_basis"] = "bogus"
    bad = tmp_path / "bad_basis.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo04_config(bad)


@pytest.mark.parametrize("section,field", [
    ("physical_constants", "youngs_modulus_pa"),
    ("criteria", "stress_limit_factor"),
    ("criteria", "tension_margin"),
])
@pytest.mark.parametrize("bad_value", [0, -1.0])
def test_loader_rejects_nonpositive_scalar(tmp_path, section, field, bad_value):
    """A zero/negative positive-physical/criteria scalar is rejected at load
    (exclusiveMinimum: 0), so it can never silently rubber-stamp a PASS or collapse a margin."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw[section][field] = bad_value
    bad = tmp_path / "bad_scalar.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo04_config(bad)


def test_loader_rejects_nonpositive_smys(tmp_path):
    """A zero/negative SMYS in the grades map is rejected (exclusiveMinimum: 0)."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["material"]["grades"]["X65"]["smys_pa"] = 0
    bad = tmp_path / "bad_smys.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo04_config(bad)


# ---------------------------------------------------------------------------
# Parametric: BOTH sagbend_stress_basis paths are pinned.
# ---------------------------------------------------------------------------


def _edited_config(tmp_path, **edits):
    """Write a tmp copy of the baseline yaml with dotted-path edits and resolve it.

    ``edits`` keys are dotted paths into the yaml (e.g. ``criteria.tension_margin=1.5``,
    ``criteria.sagbend_stress_basis=bending_only``).
    """
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    for dotted, value in edits.items():
        section, key = dotted.split(".", 1)
        raw[section][key] = value
    # Point catalogs/artifacts at ABSOLUTE real paths so the tmp yaml resolves to the committed
    # catalogs (the source of truth) regardless of the tmp yaml's directory.
    data_dir = _GTM_DIR / "data"
    raw["catalogs"] = {
        "vessels": str(data_dir / "pipelay_vessels.json"),
        "pipelines": str(data_dir / "pipelines.json"),
    }
    raw["artifacts"] = {"results_root": str(tmp_path), "output_root": str(tmp_path)}
    edited = tmp_path / "demo_04_edited.yml"
    edited.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    return load_demo04_config(edited)


@pytest.mark.parametrize("basis", ["combined", "bending_only"])
def test_sagbend_stress_basis_toggle_pinned(tmp_path, basis, golden_results):
    """Both toggle paths are pinned:
      - 'combined' (default) == the golden (sigma_combined = bending + axial),
      - 'bending_only' diverges from the golden, and sigma_combined_mpa == bending only
        (axial reported but not added) with strictly lower utilisations where axial > 0.
    """
    cfg = _edited_config(tmp_path, **{"criteria.sagbend_stress_basis": basis})
    assert cfg.sagbend_stress_basis == basis
    results = demo.serialise_results(_run_from_config(cfg))
    assert len(results) == 60

    if basis == "combined":
        assert results == golden_results
        return

    # bending_only: must DIFFER from the golden, and the combined field collapses to bending.
    assert results != golden_results
    diverged = False
    for r in results:
        cf = r["checks_flat"]
        if cf.get("sagbend_stress_basis") != "bending_only":
            continue
        if r["H_chosen_kn"] <= 0:
            continue
        # sigma_combined_mpa == bending only on this path.
        assert cf["sagbend_sigma_combined_mpa"] == pytest.approx(
            cf["sagbend_sigma_bending_mpa"], abs=0.05
        )
        if cf["sagbend_sigma_axial_mpa"] > 0:
            diverged = True
    assert diverged


def test_bending_only_24in_30m_lower_than_combined(tmp_path):
    """The 24in @ 30m case under bending_only has a much lower sagbend utilisation than the
    combined golden (axial term dropped): ~0.064 bending-only vs ~0.947 combined."""
    cfg = _edited_config(tmp_path, **{"criteria.sagbend_stress_basis": "bending_only"})
    results = demo.serialise_results(_run_from_config(cfg))
    r = _case(results, "PLV-001", "24in", 30)
    assert r["sagbend_util"] < 0.2  # bending-only is tiny vs the 0.947 combined


# ---------------------------------------------------------------------------
# Edited-config propagation (ADR-0002/0005): the yaml is the LIVE source of truth.
# ---------------------------------------------------------------------------


def test_edited_tension_margin_propagates_and_diverges(tmp_path, golden_results):
    """Editing criteria.tension_margin (previously BURIED at L509) propagates into the chosen
    tension and diverges from the golden, while the baseline still reproduces the golden."""
    edited_config = _edited_config(tmp_path, **{"criteria.tension_margin": 1.50})
    assert edited_config.tension_margin == 1.50
    edited_results = demo.serialise_results(_run_from_config(edited_config))
    assert len(edited_results) == 60
    assert edited_results != golden_results

    # The committed baseline yaml STILL reproduces the golden.
    baseline_results = demo.serialise_results(
        _run_from_config(load_demo04_config(_BASELINE_CONFIG_PATH))
    )
    assert baseline_results == golden_results


def test_edited_pipe_selection_wt_propagates(tmp_path, golden_results):
    """PS: editing a swept pipe WT (on the compute path) changes A_steel/bending -> diverges."""
    edited_config = _edited_config(
        tmp_path, **{"sweep.pipe_sizes": {"24in": 30.96}}
    )
    assert edited_config.pipe_selection == {"24in": 30.96}
    edited_results = demo.serialise_results(_run_from_config(edited_config))
    # 2 vessels x 1 pipe x 6 depths = 12 cases.
    assert len(edited_results) == 12
    assert all(r["pipe_wt_mm"] == 30.96 for r in edited_results)
    assert edited_results != golden_results


def test_edited_depth_propagates(tmp_path, golden_results):
    """Editing a swept depth changes the cases (new depth appears)."""
    edited_config = _edited_config(tmp_path, **{"sweep.water_depths_m": [7, 50]})
    assert edited_config.water_depths == [7, 50]
    edited_results = demo.serialise_results(_run_from_config(edited_config))
    # 2 vessels x 5 pipes x 2 depths = 20 cases.
    assert len(edited_results) == 20
    depths_seen = sorted({r["water_depth_m"] for r in edited_results})
    assert depths_seen == [7, 50]
    assert edited_results != golden_results


# ---------------------------------------------------------------------------
# Metadata + summary byte-identity.
# ---------------------------------------------------------------------------


def test_summary_matches_golden(produced_results, golden_doc):
    """The summary table (records) matches the golden byte-for-byte."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    vessels = demo.load_vessels(config=config)
    pipes = demo.load_pipes(config=config)
    _results, df = demo.run_parametric_sweep(vessels, pipes, config=config)
    summary_df = demo.build_summary_table(df, config=config)
    assert summary_df.to_dict(orient="records") == golden_doc["summary"]


def test_metadata_records_active_basis(golden_doc):
    """The frozen golden metadata records the active (combined) stress basis + the grade."""
    assert golden_doc["metadata"]["sagbend_stress_basis"] == "combined"
    assert golden_doc["metadata"]["material_grade"] == "X65"
    assert golden_doc["metadata"]["constants"]["tension_margin"] == 1.10
    assert golden_doc["metadata"]["constants"]["stress_limit_factor"] == 0.72


# ---------------------------------------------------------------------------
# Display thresholds are SEPARATE from the go/no-go logic bands (reviewer finding).
# ---------------------------------------------------------------------------


def test_display_color_thresholds_independent_of_go_logic():
    """utilisation_color uses the DISPLAY thresholds, NOT the go/no-go logic bands."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    from report_template import COLORS

    # 0.80 is amber for DISPLAY (>= 0.70, <= 0.90) even though 0.80 < go_threshold 0.85.
    assert demo.utilisation_color(0.80, config=config) == COLORS["warning"]
    # 0.65 is green for DISPLAY.
    assert demo.utilisation_color(0.65, config=config) == COLORS["success"]
    # 0.95 is red.
    assert demo.utilisation_color(0.95, config=config) == COLORS["danger"]
