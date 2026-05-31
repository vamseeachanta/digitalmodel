# ABOUTME: Golden regression test — the Baseline Sweep Config reproduces today's 680-case oracle.
# ABOUTME: Acceptance gate for the yaml-driven sweep (ADR-0002): byte-identical to the frozen golden.
"""Golden regression + loader unit tests for demo_01's yaml-driven Sweep Config.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_01_baseline_golden.py -x -q
"""
from __future__ import annotations

import json
import math
from pathlib import Path

import pytest

import demo_01_dnv_freespan_viv as demo
from sweep_config import (
    BOUNDARY_CONDITION_TO_C_N,
    SweepConfigError,
    load_sweep_config,
)

_GTM_DIR = Path(demo.__file__).resolve().parent
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_01_baseline_results.json"
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_01_freespan.yml"

# Fields excluded from row-by-row comparison: none beyond the volatile top-level metadata
# block, which is excluded by only comparing cases[].


@pytest.fixture(scope="module")
def produced_cases():
    """Run the sweep from the committed Baseline config (recompute path)."""
    _pipe_data, pipe_catalog = demo.load_pipe_catalog()
    _jumper_data, jumper_props = demo.load_jumper_catalog()
    config = load_sweep_config(_BASELINE_CONFIG_PATH)
    results, _df = demo.run_parametric_sweep(pipe_catalog, jumper_props, config)
    return results


@pytest.fixture(scope="module")
def golden_cases():
    with _GOLDEN_PATH.open("r", encoding="utf-8") as fh:
        return json.load(fh)["cases"]


def _physics_key(case: dict):
    """Stable physics ordering key, independent of emission order."""
    return (
        case["pipe_type"],
        case["nominal_size"],
        case["span_m"],
        case["v_current_ms"],
        str(case["e_over_d"]),
    )


def test_produced_cases_equal_golden_in_order_field_by_field(produced_cases, golden_cases):
    """Primary oracle: produced cases[] == golden cases[] in order, all fields, no exclusions.

    Comparing the full ordered lists subsumes count, emission order (BD-4), every field, and
    the per-case key set in a single assertion. The volatile top-level ``metadata`` block is
    excluded simply by comparing only ``cases[]`` (loaded by the golden_cases fixture).
    """
    assert produced_cases == golden_cases


def test_physics_sort_key_is_unique_on_baseline(produced_cases):
    """The physics sort key WOULD be unique on the Baseline.

    This documents that the maskable sort-by-physics-key oracle (now retired in favour of the
    strict ordered comparison above) would have been sound for the Baseline only: a reordering
    could not have hidden behind a duplicate key. This gate is Baseline-scoped — promoted axes
    in a non-Baseline config could legitimately produce duplicate physics keys.
    """
    keys = [_physics_key(c) for c in produced_cases]
    assert len(keys) == len(set(keys)) == 680


def test_span_m_is_int_for_every_case(produced_cases):
    """(c) BD-2: spans stay integers through the whole pipeline."""
    assert all(
        isinstance(c["span_m"], int) and not isinstance(c["span_m"], bool)
        for c in produced_cases
    )


def test_summary_counts_match(produced_cases):
    """(d) The four screening-status counts are unchanged."""
    counts = {"PASS": 0, "INLINE_ONLY": 0, "FAIL_CF": 0, "FAIL_LOCKIN": 0}
    for c in produced_cases:
        counts[c["status"]] += 1
    assert counts == {
        "PASS": 68,
        "INLINE_ONLY": 89,
        "FAIL_CF": 444,
        "FAIL_LOCKIN": 79,
    }


# ---------------------------------------------------------------------------
# Loader unit tests
# ---------------------------------------------------------------------------


def test_baseline_loader_resolves_types():
    config = load_sweep_config(_BASELINE_CONFIG_PATH)

    # Jumper gap axis last element is mid-water infinity (BD-3).
    last_gap = config.jumpers.gap_ratios[-1]
    assert math.isinf(last_gap)
    assert isinstance(last_gap, float)

    # Finite gaps are floats; spans are ints (BD-2).
    assert all(isinstance(g, float) for g in config.pipelines.gap_ratios)
    assert all(isinstance(s, int) and not isinstance(s, bool)
               for s in config.pipelines.span_lengths_m)

    # pinned label -> C_N_PINNED == 3.5596 (BD-5).
    assert config.pipelines.c_n_values[0] == BOUNDARY_CONDITION_TO_C_N["pinned"]
    assert config.pipelines.c_n_values[0] == demo.C_N_PINNED == 3.5596


def test_bare_inf_string_rejected(tmp_path):
    """BD-3: bare `inf` (loads as the string 'inf') must be rejected with a clear error."""
    bad = tmp_path / "bad.yml"
    bad.write_text(
        "meta: {demo_id: demo_01}\n"
        "catalogs: {pipelines: data/pipelines.json, jumpers: data/rigid_jumpers.json}\n"
        "sweeps:\n"
        "  pipelines: &base\n"
        "    sizes: [\"8in\"]\n"
        "    span_lengths_m: [10]\n"
        "    current_velocities_ms: [0.2]\n"
        "    gap_ratios: [1.0]\n"
        "    content_density_kg_m3: [800.0]\n"
        "    boundary_condition: [\"pinned\"]\n"
        "    wt_selection: [\"thinnest\"]\n"
        "  jumpers:\n"
        "    <<: *base\n"
        "    gap_ratios: [1.0, inf]\n",   # bare inf -> string
        encoding="utf-8",
    )
    with pytest.raises(SweepConfigError, match="not bare|bare `inf`|string"):
        load_sweep_config(bad)


def test_pinned_maps_to_3_5596():
    assert BOUNDARY_CONDITION_TO_C_N["pinned"] == 3.5596
