"""Audit the executable synthetic raw-MBL example against hand calculations."""

from pathlib import Path

import pytest

from digitalmodel.orcaflex.mooring_design import (
    MOORING_MATERIAL_LIBRARY,
    MooringLineDesign,
)


@pytest.fixture
def raw_mbl_example():
    """Load the executable example, including its reversible library context."""
    example = (
        Path(__file__).resolve().parents[2]
        / "docs/examples/mooring-explicit-factor-check.md"
    )
    code = example.read_text(encoding="utf-8").split("```python\n")[1].split("```")[0]
    original_library = dict(MOORING_MATERIAL_LIBRARY)
    namespace = {}
    try:
        exec(compile(code, str(example), "exec"), namespace)
        yield namespace["raw_mbl_audit"]
    finally:
        assert MOORING_MATERIAL_LIBRARY == original_library


@pytest.mark.parametrize(
    "tension_kn, expected",
    [
        pytest.param(
            400.0,
            {"audit_high_mbl": 0.2, "audit_mid_mbl": 0.4, "audit_low_mbl": 0.8},
            id="lower-tension",
        ),
        pytest.param(
            1000.0,
            {"audit_high_mbl": 0.5, "audit_mid_mbl": 1.0, "audit_low_mbl": 2.0},
            id="middle-tension",
        ),
        pytest.param(
            1600.0,
            {"audit_high_mbl": 0.8, "audit_mid_mbl": 1.6, "audit_low_mbl": 3.2},
            id="higher-tension",
        ),
    ],
)
def test_raw_mbl_audit_example(raw_mbl_example, tension_kn, expected):
    """Use the independent hand divisions in the example's synthetic input table."""
    original_library = dict(MOORING_MATERIAL_LIBRARY)
    assert raw_mbl_example(tension_kn) == expected
    assert MOORING_MATERIAL_LIBRARY == original_library


def test_raw_mbl_audit_restores_library_on_error(raw_mbl_example, monkeypatch):
    """Restore the library even when the helper raises inside the example."""
    original_library = dict(MOORING_MATERIAL_LIBRARY)

    def fail_check(self, max_tension_kn):
        assert set(MOORING_MATERIAL_LIBRARY) == {
            "audit_high_mbl", "audit_mid_mbl", "audit_low_mbl"
        }
        raise RuntimeError("synthetic audit failure")

    monkeypatch.setattr(MooringLineDesign, "check_mbl", fail_check)
    # Tension source: the middle-tension row of the example's synthetic table.
    with pytest.raises(RuntimeError, match="synthetic audit failure"):
        raw_mbl_example(1000.0)
    assert MOORING_MATERIAL_LIBRARY == original_library
