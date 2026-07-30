#!/usr/bin/env python
"""Build the wall-thickness tension-moment study as tidy JSON rows.

The effective-tension and bending-moment limits are derived at the 18 mm
midpoint of the 8--28 mm wall sweep.  For pipe area ``A`` and first-yield
moment ``M_y``:

    T_y = SMYS * A
    M_y = (pi / 4) * SMYS * (D - t)^2 * t

Both load axes end at 60% of those reference capacities (rounded to the
nearest N and N*m): 4,649,766 N and 355,591 N*m.  This spans elastic loading
through near-yield interaction without selecting arbitrary round limits.
Calculations use SI units; wall thickness is emitted in millimetres.

Output:
    docs/api/structural/wall-thickness-3d.json
"""
from __future__ import annotations

import math
from collections.abc import Sequence

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY


_OD_M = 0.3239
_GRADE = "X65"
_SMYS_PA = 448.0e6
_SMTS_PA = 530.9e6
_INTERNAL_PRESSURE_PA = 20.0e6
_EXTERNAL_PRESSURE_PA = 10.0e6
_SAFETY_CLASS = SafetyClass.MEDIUM
_FABRICATION_TOLERANCE = 0.125

_REFERENCE_WALL_M = 0.018
_REFERENCE_AREA_M2 = math.pi / 4 * (
    _OD_M**2 - (_OD_M - 2 * _REFERENCE_WALL_M) ** 2
)
_YIELD_TENSION_N = _SMYS_PA * _REFERENCE_AREA_M2
_YIELD_MOMENT_NM = (
    math.pi
    / 4
    * _SMYS_PA
    * (_OD_M - _REFERENCE_WALL_M) ** 2
    * _REFERENCE_WALL_M
)
_TENSION_MAX_N = round(0.6 * _YIELD_TENSION_N)
_MOMENT_MAX_NM = round(0.6 * _YIELD_MOMENT_NM)

_WALL_THICKNESS_MM = tuple(round(8.0 + 0.5 * index, 1) for index in range(41))
_EFFECTIVE_TENSION_N = tuple(
    _TENSION_MAX_N * index / 30 for index in range(31)
)
_BENDING_MOMENT_NM = tuple(
    _MOMENT_MAX_NM * index / 30 for index in range(31)
)
_DESIGN_CODES = tuple(DesignCode)
_CHECK_NAMES = tuple(
    sorted(
        {
            check_name
            for strategy in CODE_REGISTRY.values()
            for check_name in strategy.check_names
        }
    )
)


def _axis_metadata(values: Sequence[float], unit: str) -> dict:
    if not values:
        raise ValueError(f"{unit} grid axis must not be empty")
    return {
        "minimum": min(values),
        "maximum": max(values),
        "count": len(values),
        "values": list(values),
        "unit": unit,
    }


def build_metadata(
    wall_thickness_mm: Sequence[float] = _WALL_THICKNESS_MM,
    effective_tension_n: Sequence[float] = _EFFECTIVE_TENSION_N,
    bending_moment_nm: Sequence[float] = _BENDING_MOMENT_NM,
    codes: Sequence[DesignCode] = _DESIGN_CODES,
) -> dict:
    """Describe the reference case, capacity basis, units, and emitted grid."""
    grid = {
        "wall_thickness_mm": _axis_metadata(wall_thickness_mm, "mm"),
        "effective_tension_n": _axis_metadata(effective_tension_n, "N"),
        "bending_moment_nm": _axis_metadata(bending_moment_nm, "N*m"),
        "design_codes": [code.value for code in codes],
        "code_count": len(codes),
        "point_count_per_code": (
            len(wall_thickness_mm)
            * len(effective_tension_n)
            * len(bending_moment_nm)
        ),
    }
    grid["row_count"] = grid["point_count_per_code"] * grid["code_count"]
    grid["iteration_order"] = [
        "code",
        "wall_thickness_mm",
        "effective_tension_n",
        "bending_moment_nm",
    ]
    return {
        "schema_version": 1,
        "geometry": {
            "outer_diameter_m": _OD_M,
            "corrosion_allowance_m": 0.0,
            "fabrication_tolerance_fraction": _FABRICATION_TOLERANCE,
        },
        "material": {
            "grade": _GRADE,
            "smys_pa": _SMYS_PA,
            "smts_pa": _SMTS_PA,
            "youngs_modulus_pa": 207e9,
            "poissons_ratio": 0.3,
        },
        "pressures": {
            "internal_pa": _INTERNAL_PRESSURE_PA,
            "external_pa": _EXTERNAL_PRESSURE_PA,
            "net_internal_pa": _INTERNAL_PRESSURE_PA - _EXTERNAL_PRESSURE_PA,
        },
        "safety_class": _SAFETY_CLASS.value,
        "load_range_basis": {
            "reference_wall_thickness_mm": _REFERENCE_WALL_M * 1000.0,
            "reference_area_m2": _REFERENCE_AREA_M2,
            "tension": {
                "formula": "T_y = SMYS * A",
                "yield_capacity_n": _YIELD_TENSION_N,
                "sweep_fraction_of_yield": 0.6,
                "maximum_n": _TENSION_MAX_N,
            },
            "bending_moment": {
                "formula": "M_y = (pi/4) * SMYS * (D - t)^2 * t",
                "yield_capacity_nm": _YIELD_MOMENT_NM,
                "sweep_fraction_of_yield": 0.6,
                "maximum_nm": _MOMENT_MAX_NM,
            },
            "maximum_rounding": "nearest N and N*m",
        },
        "grid": grid,
        "units": {
            "internal_calculations": "SI: m, Pa, N, N*m",
            "outer_diameter": "m",
            "wall_thickness": "mm",
            "material_strength": "Pa",
            "pressure": "Pa",
            "effective_tension": "N",
            "bending_moment": "N*m",
            "utilisation": "dimensionless",
        },
        "check_utilisation_columns": [
            f"{name}_utilisation" for name in _CHECK_NAMES
        ],
    }


def _analyze_point(
    code: DesignCode,
    wall_thickness_mm: float,
    effective_tension_n: float,
    bending_moment_nm: float,
) -> dict:
    geometry = PipeGeometry(
        outer_diameter=_OD_M,
        wall_thickness=wall_thickness_mm / 1000.0,
        corrosion_allowance=0.0,
        fabrication_tolerance=_FABRICATION_TOLERANCE,
    )
    material = PipeMaterial(
        grade=_GRADE,
        smys=_SMYS_PA,
        smts=_SMTS_PA,
        youngs_modulus=207e9,
        poissons_ratio=0.3,
    )
    loads = DesignLoads(
        internal_pressure=_INTERNAL_PRESSURE_PA,
        external_pressure=_EXTERNAL_PRESSURE_PA,
        bending_moment=bending_moment_nm,
        effective_tension=effective_tension_n,
    )
    result = WallThicknessAnalyzer(
        geometry,
        material,
        loads,
        DesignFactors(safety_class=_SAFETY_CLASS),
        code=code,
    ).perform_analysis()
    row = {
        "code": code.value,
        "wall_thickness_mm": wall_thickness_mm,
        "effective_tension_n": effective_tension_n,
        "bending_moment_nm": bending_moment_nm,
        "utilisation": round(float(result.max_utilisation), 6),
        "governing_check": result.governing_check,
        "is_safe": bool(result.is_safe),
    }
    row.update({f"{name}_utilisation": None for name in _CHECK_NAMES})
    row.update(
        {
            f"{name}_utilisation": round(float(utilisation), 6)
            for name, utilisation in result.checks.items()
        }
    )
    return row


def build_study(
    wall_thickness_mm: Sequence[float] = _WALL_THICKNESS_MM,
    effective_tension_n: Sequence[float] = _EFFECTIVE_TENSION_N,
    bending_moment_nm: Sequence[float] = _BENDING_MOMENT_NM,
    codes: Sequence[DesignCode] = _DESIGN_CODES,
) -> dict:
    """Run the requested grid and return metadata plus tidy result rows."""
    rows = [
        _analyze_point(code, wall_mm, tension_n, moment_nm)
        for code in codes
        for wall_mm in wall_thickness_mm
        for tension_n in effective_tension_n
        for moment_nm in bending_moment_nm
    ]
    return {
        "meta": build_metadata(
            wall_thickness_mm,
            effective_tension_n,
            bending_moment_nm,
            codes,
        ),
        "rows": rows,
    }


def main() -> None:
    import json
    from pathlib import Path

    repo = Path(__file__).resolve().parents[2]
    out = repo / "docs" / "api" / "structural" / "wall-thickness-3d.json"
    study = build_study()
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(study, indent=1) + "\n", encoding="utf-8")
    meta = study["meta"]
    print(f"wrote {out} ({out.stat().st_size / 1048576:.1f} MB)")
    print(
        f"  rows: {meta['grid']['row_count']:,} "
        f"over {meta['grid']['code_count']} codes"
    )


if __name__ == "__main__":
    main()
