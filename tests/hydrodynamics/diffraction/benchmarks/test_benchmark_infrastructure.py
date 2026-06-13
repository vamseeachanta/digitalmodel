"""Benchmark infrastructure precursor tests (#501 Sub-task 0).

These must pass on the UNMODIFIED tree before any #501 schema change lands.
"""

from __future__ import annotations

import pytest

from tests.hydrodynamics.diffraction.benchmarks.golden_capture import (
    enumerate_byte_identity_fixtures,
    golden_path_for,
    render_orcawave_bytes,
)

FIXTURES = enumerate_byte_identity_fixtures()


def test_enumerate_byte_identity_fixtures_covers_l00_sub_specs():
    l00 = [p for p in FIXTURES if p.parent.parent.name == "L00_validation_wamit"]
    assert len(l00) == 10
    assert any("L02_" in str(p) for p in FIXTURES)
    assert any("L03_ship_benchmark" in str(p) for p in FIXTURES)


@pytest.mark.parametrize(
    "spec_path", FIXTURES, ids=[golden_path_for(p).stem for p in FIXTURES]
)
def test_benchmark_infrastructure_generates_golden_from_pre_change_tree(spec_path):
    golden = golden_path_for(spec_path)
    assert golden.exists(), (
        f"golden missing for {spec_path}; run "
        "`uv run python -m tests.hydrodynamics.diffraction.benchmarks.golden_capture`"
    )
    assert render_orcawave_bytes(spec_path) == golden.read_bytes()
