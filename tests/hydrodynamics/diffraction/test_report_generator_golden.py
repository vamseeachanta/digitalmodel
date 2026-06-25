#!/usr/bin/env python3
"""Golden / characterization tests for the diffraction report generator.

These tests pin the EXACT HTML output of ``generate_diffraction_report`` for a
set of representative fixtures. They exist to prove that extracting the shared
``digitalmodel.reporting`` block library (#1018) is a ZERO behavior change:
the refactored generator must reproduce these committed golden files
byte-for-byte.

The golden files under ``fixtures/golden/`` were generated from the
PRE-refactor code. Regenerate intentionally with::

    uv run python tests/hydrodynamics/diffraction/test_report_generator_golden.py

Fixtures are deterministic on purpose: ``report_date`` is pinned and no
plotly-backed sections are exercised (those builders emit non-deterministic
div ids), so the golden bytes are stable across runs and machines.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, Optional, Tuple

import pytest

from digitalmodel.hydrodynamics.diffraction.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)
from digitalmodel.hydrodynamics.diffraction.report_generator import (
    DiffractionReportData,
    HydrostaticData,
    MeshQualityData,
    generate_diffraction_report,
)

GOLDEN_DIR = Path(__file__).parent / "fixtures" / "golden"

_FIXED_DATE = "2026-01-01 00:00:00"


# ---------------------------------------------------------------------------
# Deterministic fixtures
# ---------------------------------------------------------------------------


def _hydrostatics() -> HydrostaticData:
    C = [[0.0] * 6 for _ in range(6)]
    C[2][2] = 1025.0 * 9.81 * 2000.0
    C[3][3] = 1.608e9
    C[4][4] = 2.5e10
    I = [[0.0] * 6 for _ in range(6)]
    I[0][0] = I[1][1] = I[2][2] = 16400000.0
    I[3][3] = 1.0e9
    I[4][4] = 1.5e10
    I[5][5] = 1.4e10
    return HydrostaticData(
        volume=16000.0,
        mass=16400000.0,
        centre_of_buoyancy=[0.0, 0.0, -4.0],
        centre_of_mass=[0.0, 0.0, -3.0],
        waterplane_area=2000.0,
        Lxx=266666.667,
        Lyy=6666666.667,
        Lxy=0.0,
        centre_of_floatation=[0.0, 0.0],
        restoring_matrix=C,
        inertia_matrix=I,
    )


def _full_basic() -> Tuple[DiffractionReportData, Dict[str, Any]]:
    data = DiffractionReportData(
        vessel_name="golden_barge",
        report_date=_FIXED_DATE,
        report_subtitle="Characterization fixture",
        hull_type="barge",
        solver_names=["OrcaWave"],
        source_file="golden.owr",
        frequencies_rad_s=[0.3, 0.6, 0.9, 1.2],
        periods_s=[20.94, 10.47, 6.98, 5.24],
        headings_deg=[0.0, 45.0, 90.0],
        hydrostatics=_hydrostatics(),
        mesh_quality=MeshQualityData(
            panel_count=520,
            mean_area=1.25,
            min_area=0.6,
            max_area=2.4,
            area_ratio=4.0,
            vertex_count=1100,
        ),
        gm_transverse=10.0,
        gm_longitudinal=155.0,
        bm_transverse=16.67,
        bm_longitudinal=416.67,
        kb=-4.0,
        radii_of_gyration={"r_xx": 7.81, "r_yy": 30.24, "r_zz": 29.21},
        executive_warnings=["NOTE: Example warning for characterization."],
    )
    return data, {}


def _compact() -> Tuple[DiffractionReportData, Dict[str, Any]]:
    data, _ = _full_basic()
    data.vessel_name = "golden_barge_compact"
    return data, {"mode": "compact"}


def _benchmark() -> Tuple[DiffractionReportData, Dict[str, Any]]:
    data = DiffractionReportData(
        vessel_name="golden_benchmark",
        report_date=_FIXED_DATE,
        solver_names=["OrcaWave", "AQWA"],
        source_file="bench.owr",
        frequencies_rad_s=[0.5, 1.0, 1.5],
        periods_s=[12.57, 6.28, 4.19],
        headings_deg=[0.0, 90.0, 180.0],
        benchmark_html_sections={
            "mesh_schematic": "<h2>Mesh</h2><p>BENCH_MESH</p>",
            "input_comparison": "<h2>Input Comparison</h2><p>BENCH_INPUT</p>",
            "input_files": "<h2>Input Files</h2><p>BENCH_FILES</p>",
            "hydro_coefficients": "<h2>Coefficients</h2><p>BENCH_COEFFS</p>",
            "consensus_summary": "<h2>Consensus</h2><p>BENCH_CONSENSUS</p>",
            "dof_sections": "<h2>Per-DOF</h2><p>BENCH_DOF</p>",
            "raw_rao_data": "<h2>Raw Data</h2><p>BENCH_RAW</p>",
            "overlay_plots": "<h2>Overlay</h2><p>BENCH_OVERLAY</p>",
            # Intentionally empty -> must NOT produce a section div.
            "input_extra_empty": "",
        },
        notes=["Note one", "Note two"],
    )
    return data, {}


def _validation_assumptions() -> Tuple[DiffractionReportData, Dict[str, Any]]:
    data = DiffractionReportData(
        vessel_name="golden_validation",
        report_date=_FIXED_DATE,
        frequencies_rad_s=[0.5, 1.0, 1.5],
        periods_s=[12.57, 6.28, 4.19],
        headings_deg=[0.0],
    )
    validation_report = {
        "overall_status": "WARNING",
        "vessel_name": "golden_validation",
        "physical_validity": {
            "raos": ["Surge: Maximum RAO magnitude 6.0 exceeds typical range"],
            "added_mass": [],
        },
        "frequency_coverage": {
            "discretization": ["Only 3 frequencies - may be insufficient"],
        },
    }
    ledger = AssumptionLedger()
    ledger.record(
        field="water_depth",
        value=1500.0,
        source=AssumptionSource.ASSUMED_DEFAULT,
        basis="Deepwater default",
        confidence=Confidence.MEDIUM,
        reference="DNV-RP-C205",
    )
    ledger.record(
        field="density",
        value=1025.0,
        source=AssumptionSource.ASSUMED_DEFAULT,
        basis="Seawater standard",
        confidence=Confidence.HIGH,
    )
    return data, {
        "validation_report": validation_report,
        "assumption_ledger": ledger,
    }


FIXTURES = {
    "full_basic": _full_basic,
    "compact": _compact,
    "benchmark": _benchmark,
    "validation_assumptions": _validation_assumptions,
}


def _render(name: str, tmp_dir: Path) -> str:
    data, kwargs = FIXTURES[name]()
    out = tmp_dir / f"{name}.html"
    result = generate_diffraction_report(data, out, **kwargs)
    return result.read_text(encoding="utf-8")


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("name", sorted(FIXTURES))
def test_output_matches_golden(name: str, tmp_path: Path) -> None:
    """Refactored output must be byte-identical to the committed golden file."""
    golden_path = GOLDEN_DIR / f"{name}.html"
    assert golden_path.exists(), (
        f"Missing golden file {golden_path}. Regenerate with: "
        "uv run python tests/hydrodynamics/diffraction/"
        "test_report_generator_golden.py"
    )
    expected = golden_path.read_text(encoding="utf-8")
    actual = _render(name, tmp_path)
    assert actual == expected, (
        f"Diffraction report output for fixture '{name}' diverged from the "
        f"golden snapshot (zero-behavior-change violated)."
    )


def _regenerate() -> None:
    """Write golden files from the CURRENT generator implementation."""
    import tempfile

    GOLDEN_DIR.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory() as td:
        tmp = Path(td)
        for name in sorted(FIXTURES):
            html = _render(name, tmp)
            (GOLDEN_DIR / f"{name}.html").write_text(html, encoding="utf-8")
            print(f"wrote {name}.html ({len(html)} bytes)")


if __name__ == "__main__":
    _regenerate()
