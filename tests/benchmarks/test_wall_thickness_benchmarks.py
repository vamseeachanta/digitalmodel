"""Performance benchmarks for WallThicknessAnalyzer (DNV-ST-F101).

Exercises pressure-containment and collapse checks on a synthetic 12-inch pipeline.
Run with:
    cd digitalmodel && PYTHONPATH=src uv run python -m pytest \
        tests/benchmarks/test_wall_thickness_benchmarks.py --benchmark-only -q
"""

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)

# Synthetic 12-inch export pipeline — representative DNV-ST-F101 inputs
_GEOMETRY = PipeGeometry(outer_diameter=0.3239, wall_thickness=0.0159)
_MATERIAL = PipeMaterial(smys=450.0, smts=535.0, grade="X65")
_LOADS = DesignLoads(
    internal_pressure=15.0,
    external_pressure=10.0,
)
_FACTORS = DesignFactors(safety_class=SafetyClass.MEDIUM)


def test_bench_wall_thickness_dnv(benchmark):
    """Benchmark DNV-ST-F101 pressure containment + collapse checks."""

    def _run_checks():
        analyzer = WallThicknessAnalyzer(
            geometry=_GEOMETRY,
            material=_MATERIAL,
            loads=_LOADS,
            factors=_FACTORS,
            code=DesignCode.DNV_ST_F101,
        )
        u_burst, _ = analyzer.check_pressure_containment()
        u_collapse, _ = analyzer.check_collapse()
        return u_burst, u_collapse

    u_burst, u_collapse = benchmark(_run_checks)
    assert 0.0 < u_burst <= 2.0, f"Unexpected burst utilisation: {u_burst}"
    assert 0.0 < u_collapse <= 2.0, f"Unexpected collapse utilisation: {u_collapse}"
