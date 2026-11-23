# ABOUTME: Golden regression test — demo_05's Baseline Sweep Config reproduces the 300-case oracle.
# ABOUTME: Acceptance gate for the yaml-driven config + the Phase-2/Phase-5 physics-defect fixes.
"""Golden regression + loader/normalize unit tests for demo_05's yaml-driven Sweep Config.

The golden was frozen on the CORRECTED, DISCRIMINATING output. History:
  * original demo: 240 NO_GO governed by σ ≈ 2120 MPa in-air bending + 5789 mm tie-in deflection
    (both physically wrong: whole-jumper full-length-beam spans);
  * first fix: M/W-geometry-derived spans → physically sane but NON-DISCRIMINATING (all 300 GO at
    util < 0.20, spans nearly constant because number_of_bends grows with horizontal_span);
  * second fix: explicit length-scaling span FRACTIONS of horizontal_span (lift 0.5, tie-in 0.28)
    with conservative simply-supported BCs (moment_coeff 8, deflection_coeff 76.8);
  * third fix (THIS golden, USER-RATIFIED): the Phase-5 tie-in check is flipped to CURRENT-ONLY
    lateral loading (include_self_weight=false). The ±50 mm tie-in tolerance is a LATERAL
    connector-mating tolerance, so it is driven by the lateral current load (~34 N/m) alone; the
    vertical self-weight sag is reacted by the lowering rigging (a different DOF) and is excluded.
    With current-only loading the tie-in deflection collapses (util ~0.14 at L=100 m) and NEVER
    governs; in-air lift bending governs ALL 300 cases (util^2 length growth) and NO_GOes only the
    100 m jumper (lift util 1.22). The sweep DISCRIMINATES: 240 GO / 0 MARGINAL / 60 NO_GO,
    in_air_bending governing all 300. See the demo module Phase 2 / Phase 5 docstrings.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_05_baseline_golden.py -x -q
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import demo_05_deepwater_rigid_jumper_installation as demo
from sweep_config_demo05 import (
    SweepConfigError,
    ResolvedDemo05Config,
    load_demo05_config,
    load_demo05_paths,
)

_GTM_DIR = Path(demo.__file__).resolve().parent
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_05_baseline_results.json"
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_05_jumper.yml"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _load_catalogs(config=None):
    """Load the catalogs. Edited tmp configs resolve catalog paths relative to tmp_path (where
    no catalogs live), so always read from the committed demo data dir — edits never touch the
    catalog files."""
    vessels_path = _GTM_DIR / "data" / "csv_hlv_vessels.json"
    jumpers_path = _GTM_DIR / "data" / "rigid_jumpers.json"
    with open(vessels_path, "r") as fh:
        vessels = json.load(fh)["vessels"]
    with open(jumpers_path, "r") as fh:
        jdata = json.load(fh)
    return vessels, jdata["common_properties"], jdata["jumpers"]


def _run_from_config(config):
    vessels, common, jumpers = _load_catalogs(config)
    results, _df = demo.run_parametric_sweep(vessels, common, jumpers, config=config)
    return results


@pytest.fixture(scope="module")
def produced_results():
    """Run the sweep from the committed Baseline config (recompute path)."""
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    return _run_from_config(config)


@pytest.fixture(scope="module")
def golden_doc():
    with _GOLDEN_PATH.open("r", encoding="utf-8") as fh:
        return json.load(fh)


@pytest.fixture(scope="module")
def golden_results(golden_doc):
    return golden_doc["cases"]


def _roundtrip(results):
    """Round-trip results through json (default=str) so floats/keys match the serialised golden."""
    return json.loads(json.dumps(results, default=str))


# ---------------------------------------------------------------------------
# Golden oracle
# ---------------------------------------------------------------------------


def test_produced_results_equal_golden_in_order_field_by_field(produced_results, golden_results):
    """Primary oracle: produced cases == golden cases in order, all fields.

    Comparing the full ordered lists subsumes count, emission order (vessel -> jumper length ->
    depth -> hs), and every field — including the nested ``phases{}`` and the NEW Phase-2 /
    Phase-5 span-model fields — in a single assertion.
    """
    assert _roundtrip(produced_results) == golden_results


def test_case_count_and_status_split(produced_results, golden_results):
    """300 cases with a DISCRIMINATING spread (acceptance gate for the ratified span model).

    The length-scaling lift-bending span makes utilisation respond to jumper length, so the sweep
    is no longer all-GO: with the ratified current-only tie-in basis only in-air lift bending can
    govern, and only the 100 m jumper exceeds the allowable -> NO_GO (60 cases = 1 length x 2
    vessels x 6 depths x 5 Hs). No case is MARGINAL.
    """
    assert len(produced_results) == 300
    assert len(golden_results) == 300
    produced_split = _split(produced_results)
    golden_split = _split(golden_results)
    assert produced_split == golden_split
    # The ratified discriminating output (current-only tie-in): 240 GO / 0 MARGINAL / 60 NO_GO.
    assert produced_split == {"GO": 240, "MARGINAL": 0, "NO_GO": 60}


def _split(results):
    out = {"GO": 0, "MARGINAL": 0, "NO_GO": 0}
    for r in results:
        out[r["overall_status"]] = out.get(r["overall_status"], 0) + 1
    return out


def test_governing_phase_distribution(produced_results):
    """With the ratified current-only tie-in basis the lateral tie-in deflection collapses and
    NEVER governs; in-air lift bending (span^2 growth) governs ALL 300 cases."""
    from collections import Counter
    govs = Counter(r["governing_phase"] for r in produced_results)
    # in_air_bending governs every case (300); tie_in never governs.
    assert dict(govs) == {"in_air_bending": 300}
    for r in produced_results:
        assert r["governing_phase"] == "in_air_bending"


def test_utilisation_responds_to_jumper_length(produced_results):
    """ACCEPTANCE (HIGH-2/HIGH-3): utilisation must RESPOND to jumper length.

    For a fixed vessel/depth/Hs, max utilisation must strictly increase with jumper length across
    all five lengths (the discrimination the prior non-discriminating model lacked)."""
    for vessel in ("Large CSV", "Medium CSV"):
        utils = [
            _find(produced_results, vessel, L, 1500, 2.0)["max_utilisation"]
            for L in (20.0, 40.0, 60.0, 80.0, 100.0)
        ]
        assert utils == sorted(utils)
        assert all(b > a for a, b in zip(utils, utils[1:]))  # strictly increasing
        # The longest jumper's util is far above the shortest (strong length response).
        assert utils[-1] > utils[0] * 10


# ---------------------------------------------------------------------------
# Physics-fix regression guards (the two HIGH-finding defects).
# ---------------------------------------------------------------------------


def test_phase2_inair_bending_scales_and_is_bounded(produced_results):
    """Phase 2 (FIX): the explicit length-scaling lift span keeps σ physically sane (never a
    multiple of SMYS like the original 2120 MPa) AND makes it grow with the jumper.

    The lift span = 0.5 x horizontal_span is always a fraction of the full jumper length, and the
    short jumpers stay well under the 0.6 x SMYS allowable while the longest exceeds it.
    """
    for r in produced_results:
        p2 = r["phases"]["in_air_bending"]
        assert "lift_span_m" in p2 and p2["lift_span_m"] < r["length_m"]  # span < full length
        assert p2["bending_stress_mpa"] < 500.0  # never a multiple of SMYS (orig was ~2120 MPa)
    # Short jumpers pass in-air bending; the 100 m jumper exceeds the allowable (it NO_GOs).
    short = _find(produced_results, "Large CSV", 20.0, 1500, 2.0)["phases"]["in_air_bending"]
    assert short["status"] == "PASS" and short["bending_stress_mpa"] < 268.8
    # Hand-check anchor: L=100 m, Large CSV → span 0.5*75 = 37.5 m, σ ≈ 327.84 MPa, util ≈ 1.220.
    anchor = _find(produced_results, "Large CSV", 100.0, 1500, 2.0)
    p2 = anchor["phases"]["in_air_bending"]
    assert p2["lift_span_m"] == 37.5
    assert p2["w_eff_n_per_m"] == 997.14
    assert abs(p2["bending_moment_knm"] - 175.28) < 0.1
    assert abs(p2["bending_stress_mpa"] - 327.84) < 0.1
    assert abs(p2["utilisation"] - 1.2197) < 0.001


def test_phase5_tiein_current_only_lateral_never_governs(produced_results):
    """Phase 5 (RATIFIED current-only basis): the ±50 mm tie-in tolerance is a LATERAL
    connector-mating tolerance, so the transverse load is the lateral CURRENT load alone (~34 N/m);
    the vertical self-weight sag is reacted by the lowering rigging (a different DOF) and is
    EXCLUDED. The length^4-scaling deflection is therefore small in every case and the tie-in phase
    never fails — in-air lift bending governs throughout.

    Original: whole-jumper point-load formula gave 5789 mm (util 115.8) at L=100 m — physically
    wrong. The ratified current-only model gives ~7 mm at L=100 m (util ~0.14, PASS).
    (Setting include_self_weight=true would fold the ~506 N/m vertical self-weight back in: δ ≈ 106
    mm, util 2.12 — the conservative-magnitude alternative; see the toggle test below.)
    """
    for r in produced_results:
        p5 = r["phases"]["tie_in"]
        assert p5["tiein_span_m"] < r["length_m"]  # free span < full length
        assert p5["deflection_mm"] < 5000.0        # never metre-scale (orig was 5789 mm)
        # Current-only basis: self-weight is excluded from the lateral resultant.
        assert p5["self_weight_load_n_per_m"] == 0.0
        # Resultant collapses to the current-only lateral load.
        assert abs(p5["resultant_load_n_per_m"] - p5["current_load_n_per_m"]) < 0.01
        # Tie-in never governs and never fails the connector tolerance.
        assert p5["status"] == "PASS"
    # Short jumper: deflection well under tolerance (GO).
    short = _find(produced_results, "Large CSV", 20.0, 1500, 2.0)["phases"]["tie_in"]
    assert short["tiein_span_m"] == 4.2 and short["deflection_mm"] < 1.0
    # Hand-check anchor: L=100 m → span 0.28*75 = 21.0 m, w_res = w_current ≈ 33.69 N/m, δ ≈ 7.04 mm.
    anchor = _find(produced_results, "Large CSV", 100.0, 1500, 2.0)
    p5 = anchor["phases"]["tie_in"]
    assert p5["tiein_span_m"] == 21.0
    assert abs(p5["resultant_load_n_per_m"] - 33.69) < 0.1
    assert abs(p5["deflection_mm"] - 7.036) < 0.01
    assert abs(p5["utilisation"] - 0.1407) < 0.001
    assert p5["status"] == "PASS"  # well within the connector tolerance; tie-in never governs


def _find(results, vessel, length, depth, hs):
    for r in results:
        if (r["vessel"] == vessel and r["length_m"] == length
                and r["water_depth_m"] == depth and r["hs_m"] == hs):
            return r
    raise AssertionError(f"case not found: {vessel} {length} {depth} {hs}")


# ---------------------------------------------------------------------------
# Type asserts
# ---------------------------------------------------------------------------


def test_water_depth_is_int(produced_results):
    """water_depth_m stays an int through the pipeline."""
    assert all(
        isinstance(r["water_depth_m"], int) and not isinstance(r["water_depth_m"], bool)
        for r in produced_results
    )


def test_hs_is_float(produced_results):
    """hs_m is a float."""
    assert all(isinstance(r["hs_m"], float) for r in produced_results)


# ---------------------------------------------------------------------------
# Metadata + summary byte-identity.
# ---------------------------------------------------------------------------


def test_metadata_constants_match_golden(produced_results, golden_doc):
    """The rebuilt metadata.constants sub-dict (the reviewable record of the fix params) matches
    the golden, including the NEW span-model coefficients."""
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    _v, common, _j = _load_catalogs(config)
    metadata = demo.build_metadata(produced_results, common, config=config)
    metadata = json.loads(json.dumps(metadata, default=str))
    assert metadata["constants"] == golden_doc["metadata"]["constants"]
    assert metadata["water_depths_m"] == golden_doc["metadata"]["water_depths_m"]
    assert metadata["hs_values_m"] == golden_doc["metadata"]["hs_values_m"]
    assert metadata["reference_hs_m"] == golden_doc["metadata"]["reference_hs_m"]
    # The ratified span-model params are exposed in the metadata record.
    assert metadata["constants"]["lift_span_fraction"] == 0.5
    assert metadata["constants"]["lift_span_m"] is None
    assert metadata["constants"]["lift_moment_coeff"] == 8.0
    assert metadata["constants"]["tiein_unsupported_span_fraction"] == 0.28
    assert metadata["constants"]["tiein_unsupported_span_m"] is None
    assert metadata["constants"]["tiein_deflection_coeff"] == 76.8
    # RATIFIED: current-only lateral tie-in basis (self-weight excluded).
    assert metadata["constants"]["tiein_include_self_weight"] is False
    assert metadata == golden_doc["metadata"]


def test_summary_matches_golden(produced_results, golden_doc):
    """The summary table (records) matches the golden byte-for-byte."""
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    summary_df = demo.build_summary_table(produced_results, config=config)
    produced = json.loads(json.dumps(summary_df.to_dict(orient="records"), default=str))
    assert produced == golden_doc["summary"]


# ---------------------------------------------------------------------------
# Loader round-trip + type unit tests.
# ---------------------------------------------------------------------------


def test_loader_round_trips_baseline_axes():
    """The baseline yaml resolves to today's exact axes."""
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    assert isinstance(config, ResolvedDemo05Config)
    assert config.vessels == ["Large CSV", "Medium CSV"]
    assert config.lengths_m == [20.0, 40.0, 60.0, 80.0, 100.0]
    assert config.depths == [500, 1000, 1500, 2000, 2500, 3000]
    assert config.hs == [1.0, 1.5, 2.0, 2.5, 3.0]


def test_loader_depths_are_int_lengths_and_hs_are_float():
    """resolved depths are ints (not bool); lengths_m and hs are floats."""
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    assert all(isinstance(d, int) and not isinstance(d, bool) for d in config.depths)
    assert all(isinstance(L, float) for L in config.lengths_m)
    assert all(isinstance(h, float) for h in config.hs)


def test_loader_resolves_constants_and_span_models():
    """All constants + the NEW Phase-2 / Phase-5 span-model parameters resolve to today's values."""
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    assert config.daf_liftoff == 1.10
    assert config.daf_splash == 1.30
    assert config.rigging_mass_kg == 5000.0
    assert config.cd_cylinder == 1.2
    assert config.ca_cylinder == 1.0
    assert config.v_lowering == 0.5
    assert config.v_current == 0.5
    assert config.splash_submerged_length_m == 10.0
    assert config.wire_allowable_factor == 0.85
    assert config.bending_allowable == 0.6
    assert config.cable_unit_weight_sub_n_per_m == 50.0
    assert config.tie_in_tolerance_mm == 50.0
    assert config.reference_hs == 2.0
    assert config.tp_coefficient == 4.0
    assert config.go_marginal_threshold == 0.85
    assert config.nogo_utilisation == 1.0
    assert config.seawater_density_kg_m3 == 1025.0
    assert config.gravity_m_s2 == 9.80665
    # The fix: explicit length-scaling span-model parameters.
    assert config.lift_span_fraction == 0.5
    assert config.lift_span_m is None
    assert config.lift_moment_coeff == 8.0
    assert config.tiein_unsupported_span_fraction == 0.28
    assert config.tiein_unsupported_span_m is None
    assert config.tiein_include_self_weight is False  # RATIFIED current-only lateral basis
    assert config.tiein_deflection_coeff == 76.8


def test_loader_resolves_paths():
    """Catalog + artifact paths resolve absolute relative to the yaml dir."""
    paths = load_demo05_paths(_BASELINE_CONFIG_PATH)
    assert paths.vessels_path.name == "csv_hlv_vessels.json"
    assert paths.jumpers_path.name == "rigid_jumpers.json"
    assert paths.vessels_path.exists()
    assert paths.jumpers_path.exists()


# ---------------------------------------------------------------------------
# Schema rejection.
# ---------------------------------------------------------------------------


def test_loader_rejects_unknown_axis(tmp_path):
    """An unknown sweep axis key is rejected (additionalProperties: False)."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["bogus_axis"] = [1, 2, 3]
    bad = tmp_path / "bad.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError, match="schema validation failed|bogus_axis|Additional"):
        load_demo05_config(bad)


def test_loader_rejects_float_depth(tmp_path):
    """A float in depths is rejected by the integer-item schema."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["depths"] = [500.5, 1000]
    bad = tmp_path / "bad_depth.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo05_config(bad)


def test_loader_rejects_bool_depth(tmp_path):
    """A bool in depths (an int subclass) is rejected at resolution time."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    raw["sweep"]["depths"] = [True, 1000]
    bad = tmp_path / "bad_bool_depth.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo05_config(bad)


@pytest.mark.parametrize("dotted", [
    "constants.v_current",
    "constants.v_lowering",
    "constants.tie_in_tolerance_mm",
])
@pytest.mark.parametrize("bad_value", [0, -1.0])
def test_loader_rejects_nonpositive_guarded_constant(tmp_path, dotted, bad_value):
    """exclusiveMinimum:0 guards on velocities + tie-in tolerance reject zero/negative."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    section, key = dotted.split(".", 1)
    raw[section][key] = bad_value
    bad = tmp_path / "bad_const.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo05_config(bad)


@pytest.mark.parametrize("dotted", [
    "lift_span_model.moment_coeff",
    "lift_span_model.lift_span_fraction",
    "tiein_span_model.deflection_coeff",
    "tiein_span_model.tiein_unsupported_span_fraction",
])
@pytest.mark.parametrize("bad_value", [0, -1.0])
def test_loader_rejects_nonpositive_span_coeff(tmp_path, dotted, bad_value):
    """exclusiveMinimum:0 guards on the span-model coefficients/fractions reject zero/negative (a
    zero moment_coeff / deflection_coeff would div-by-zero, a zero span fraction => zero span)."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    section, key = dotted.split(".", 1)
    raw[section][key] = bad_value
    bad = tmp_path / "bad_span.yml"
    bad.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    with pytest.raises(SweepConfigError):
        load_demo05_config(bad)


# ---------------------------------------------------------------------------
# Edited-config flow (ADR-0005): the yaml is the COMPLETE, LIVE source of truth.
# ---------------------------------------------------------------------------


def _edited_config(tmp_path, **edits):
    """Write a tmp copy of the baseline yaml with dotted-path edits and resolve it."""
    import yaml as _yaml

    raw = _yaml.safe_load(_BASELINE_CONFIG_PATH.read_text(encoding="utf-8"))
    for dotted, value in edits.items():
        section, key = dotted.split(".", 1)
        raw[section][key] = value
    edited = tmp_path / "demo_05_edited.yml"
    edited.write_text(_yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    return load_demo05_config(edited)


def test_edited_moment_coeff_propagates_and_diverges(tmp_path, golden_results):
    """Switching the lift moment_coeff from 8 (pin-pin) to 12 (fixed-fixed) lowers Phase-2 stress
    in every case and DIVERGES from the golden — while the baseline yaml still equals the golden."""
    edited_config = _edited_config(tmp_path, **{"lift_span_model.moment_coeff": 12.0})
    assert edited_config.lift_moment_coeff == 12.0
    edited_results = _roundtrip(_run_from_config(edited_config))
    assert len(edited_results) == 300
    # M = w s^2 / 12 < w s^2 / 8 ⇒ every Phase-2 stress drops vs the golden.
    for er, gr in zip(edited_results, golden_results):
        assert er["phases"]["in_air_bending"]["bending_stress_mpa"] < \
            gr["phases"]["in_air_bending"]["bending_stress_mpa"]
    assert edited_results != golden_results
    # The committed baseline yaml STILL reproduces the golden.
    assert _roundtrip(_run_from_config(load_demo05_config(_BASELINE_CONFIG_PATH))) == golden_results


def test_edited_self_weight_toggle_changes_phase5(tmp_path):
    """The Phase-5 include_self_weight toggle. The RATIFIED baseline is OFF (current-only lateral):
    the resultant transverse load is the current-only term in every case. Toggling it ON (the
    conservative-magnitude stance) folds the vertical self-weight back into the resultant."""
    # Baseline (RATIFIED OFF): resultant collapses to the current-only lateral load.
    off = load_demo05_config(_BASELINE_CONFIG_PATH)
    assert off.tiein_include_self_weight is False
    off_results = _run_from_config(off)
    for r in off_results:
        p5 = r["phases"]["tie_in"]
        assert p5["self_weight_load_n_per_m"] == 0.0
        # resultant collapses to the current-only load (rounded at different precisions).
        assert abs(p5["resultant_load_n_per_m"] - p5["current_load_n_per_m"]) < 0.01
    # Toggled ON (conservative-magnitude alternative): self-weight becomes the dominant term.
    on = _edited_config(tmp_path, **{"tiein_span_model.include_self_weight": True})
    assert on.tiein_include_self_weight is True
    on_results = _run_from_config(on)
    for r in on_results:
        p5 = r["phases"]["tie_in"]
        assert p5["self_weight_load_n_per_m"] > 0
        # Self-weight (~506 N/m) dominates over current (~34 N/m).
        assert p5["self_weight_load_n_per_m"] > p5["current_load_n_per_m"]


def test_edited_depth_propagates(tmp_path, golden_results):
    """Editing a swept depth changes the cases (new depth appears)."""
    edited_config = _edited_config(tmp_path, **{"sweep.depths": [500, 4000]})
    assert edited_config.depths == [500, 4000]
    edited_results = _run_from_config(edited_config)
    # 2 vessels x 5 lengths x 2 depths x 5 hs = 100 cases.
    assert len(edited_results) == 100
    assert sorted({r["water_depth_m"] for r in edited_results}) == [500, 4000]
    assert _roundtrip(edited_results) != golden_results


def test_edited_vessels_subset_changes_chart_trace_count(tmp_path):
    """A vessels subset (["Large CSV"]) yields one Chart-1 heatmap subplot vs two for baseline
    (charts are config-driven)."""
    subset_config = _edited_config(tmp_path, **{"sweep.vessels": ["Large CSV"]})
    assert subset_config.vessels == ["Large CSV"]
    vessels, common, jumpers = _load_catalogs(subset_config)
    results, _df = demo.run_parametric_sweep(vessels, common, jumpers, config=subset_config)
    fig1 = demo.build_chart_1_heatmap(results, [vessels[0]], config=subset_config)
    assert len(fig1.data) == 1

    baseline_config = load_demo05_config(_BASELINE_CONFIG_PATH)
    bvessels, bcommon, bjumpers = _load_catalogs(baseline_config)
    bresults, _bdf = demo.run_parametric_sweep(bvessels, bcommon, bjumpers, config=baseline_config)
    bfig1 = demo.build_chart_1_heatmap(bresults, bvessels, config=baseline_config)
    assert len(bfig1.data) == 2


def test_edited_reference_hs_changes_chart_titles(tmp_path):
    """Editing constants.reference_hs threads into chart titles."""
    edited_config = _edited_config(tmp_path, **{"constants.reference_hs": 1.5})
    assert edited_config.reference_hs == 1.5
    vessels, common, jumpers = _load_catalogs(edited_config)
    results, _df = demo.run_parametric_sweep(vessels, common, jumpers, config=edited_config)
    fig1 = demo.build_chart_1_heatmap(results, vessels, config=edited_config)
    assert "Hs = 1.5 m" in fig1.layout.title.text


def test_report_narrative_reflects_edited_config(tmp_path):
    """The report prose carries the EDITED config's values (DAFs, tolerance), while the baseline
    report carries today's literals — and the methodology describes the FIXED span models."""
    import plotly.graph_objects as go
    import pandas as pd

    empty = go.Figure()
    summary_df = pd.DataFrame(
        [{"Vessel": "Large CSV", "Jumper": "Jumper-20m", "Length (m)": 20.0,
          "Depth (m)": 1500, "Hs (m)": 2.0, "Status": "GO", "Max Util": 0.049,
          "Governing Phase": "In-air Bending"}]
    )
    edited_config = _edited_config(
        tmp_path,
        **{"constants.daf_splash": 1.5, "constants.tie_in_tolerance_mm": 75.0},
    )
    results = _run_from_config(edited_config)
    out = tmp_path / "edited_report.html"
    html = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, results, 300,
        config=edited_config, output_path=out,
    )
    assert "DAF of 1.5" in html
    assert "75 mm" in html
    # The methodology describes the corrected, length-scaling span models.
    assert "spreader bar with sling picks" in html
    assert "unsupported leg" in html
    # RATIFIED current-only lateral tie-in basis: in-air lift bending governs, tie-in never does.
    assert "never govern" in html
    assert "lateral current load alone" in html
    assert "flagged for domain re-review" in html.lower()

    baseline_config = load_demo05_config(_BASELINE_CONFIG_PATH)
    base_results = _run_from_config(baseline_config)
    out2 = tmp_path / "baseline_report.html"
    html_base = demo.build_report(
        empty, empty, empty, empty, empty, summary_df, base_results, 300,
        config=baseline_config, output_path=out2,
    )
    assert "DAF of 1.3" in html_base
    assert "50 mm" in html_base
