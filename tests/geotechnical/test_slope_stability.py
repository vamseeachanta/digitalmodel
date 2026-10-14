# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reproduce public xslope benchmarks; optional dependencies remain optional."""

from pathlib import Path

import pytest

pytest.importorskip("xslope")
pytest.importorskip("gmsh")

from xslope.fileio import load_slope_data
from digitalmodel.geotechnical.slope_stability import (
    analyze_model, build_slope_model, run_seepage,
)
from tests.geotechnical.test_slope_profile import profile

FIXTURES = Path(__file__).parents[1] / "fixtures/geotechnical/slope"


def equivalent_profile():
    """Convert the public imperial workbook into SI using Pint."""
    from pint import UnitRegistry

    units = UnitRegistry()
    source = load_slope_data(FIXTURES / "xslope_simple1.xlsx")
    length = (1 * units.foot).to("meter").magnitude
    material = source["materials"][0]
    weight = (material["gamma"] * units.force_pound / units.foot**3).to(
        "kilonewton / meter**3"
    ).magnitude
    cohesion = (material["c"] * units.force_pound / units.foot**2).to(
        "kilopascal"
    ).magnitude
    circle = source["circles"][0]
    return {
        "units": "SI", "water_unit_weight_kn_m3": 9.81,
        "ground_surface_m": [[x * length, y * length]
                             for x, y in source["ground_surface"].coords],
        "bottom_elevation_m": source["max_depth"] * length,
        "layers": [{"unit_weight_kn_m3": weight, "c_kpa": cohesion,
                    "phi_deg": material["phi"]}],
        "circles": [{"x_m": circle["Xo"] * length,
                     "y_m": circle["Yo"] * length,
                     "radius_m": circle["R"] * length}],
    }


def test_readme_bishop_and_equivalent_si_profile(tmp_path):
    """README prints FS = 1.279 (Bishop, 20 slices).

    https://github.com/njones61/xslope/blob/main/README.md read 2026-10-14.
    Relative tolerance 1% permits printed-value rounding and solver/platform
    variation; equivalence between the mapped model and workbook is tighter.
    """
    workbook = load_slope_data(FIXTURES / "xslope_simple1.xlsx")
    original = analyze_model(workbook, methods=("bishop",), search=False, num_slices=20)
    mapped = build_slope_model(equivalent_profile(), tmp_path)
    result = analyze_model(mapped, methods=("bishop",), search=False, num_slices=20)
    assert original.methods["bishop"].factor_of_safety == pytest.approx(1.279, rel=0.01)
    assert result.methods["bishop"].factor_of_safety == pytest.approx(
        original.methods["bishop"].factor_of_safety, rel=1e-8
    )
    assert result.xslope_version == "1.0.3"


def test_seep_one_discharge():
    """SEEP-1 prints total discharge 40.111 m³/yr per m at tri3 size 0.5 m.

    https://xslope.org/en/latest/tutorials/seep01_sheetpile/ read 2026-10-14.
    Relative tolerance 1% accommodates mesh-generator/platform variation and
    rounding; the mesh size and element order are held at the tutorial settings.
    """
    model = load_slope_data(FIXTURES / "xslope_clay_blanket.xlsx")
    result = run_seepage(model, element_size=0.5)
    assert result.discharge_per_width == pytest.approx(40.111, rel=0.01)
    assert result.time_unit == "yr"
    assert result.converged
    assert model["seep_u"] is not None


def test_material_water_and_layer_mapping(tmp_path):
    data = profile()
    data["water_table_m"] = [[0, 0], [100, 10]]
    data["seismic_coefficient"] = 0.1
    data["layers"] = [
        {"unit_weight_kn_m3": 18, "Su_kpa": 12,
         "su_gradient_kpa_per_m": 2, "reference_elevation_m": 20,
         "permeability_m_s": 1e-5},
        {"top_boundary_m": [[0, 0], [100, 5]],
         "unit_weight_kn_m3": 20, "c_kpa": 10, "phi_deg": 30,
         "saturated_unit_weight_kn_m3": 21, "permeability_m_s": 1e-6},
    ]
    # The public example begins at the model base; lower the domain floor so
    # both layers have positive thickness across the complete section.
    data["ground_surface_m"] = [[0, 10], [20, 20], [100, 20]]
    data["bottom_elevation_m"] = -10
    model = build_slope_model(data, tmp_path)
    first, second = model["materials"]
    assert (first["option"], first["c"], first["cp"], first["r_elev"]) == (
        "cp", 12, 2, 20
    )
    assert second["phi"] == 30
    assert second["gamma_sat"] == 21
    assert first["k1"] == first["k2"] == 1e-5
    assert second["u"] == "piezo"
    assert model["k_seismic"] == 0.1
    assert model["unit_system"] == "si"
    assert len(model["polygons"]) == 2
    assert list(tmp_path.iterdir()) == []


def test_both_circular_searches(tmp_path):
    model = build_slope_model(equivalent_profile(), tmp_path)
    result = analyze_model(model, num_slices=20)
    assert set(result.methods) == {"bishop", "spencer"}
    for method in result.methods.values():
        assert method.factor_of_safety > 0
        assert method.search_converged is True
        assert method.critical_circle is not None
        assert method.unsolved_trials["attempted"] >= method.unsolved_trials["unsolved"]
    from dataclasses import asdict
    import json
    print(json.dumps(asdict(result), sort_keys=True))


def test_seepage_boundary_mapping(tmp_path):
    data = profile()
    data["layers"][0]["permeability_m_s"] = 1e-5
    data["seepage"] = {"element_size_m": 0.5, "heads_m": [
        {"head_m": 20, "boundary_m": [[0, 0], [20, 20]]},
        {"head_m": 15, "boundary_m": [[20, 20], [100, 20]]},
    ]}
    model = build_slope_model(data, tmp_path)
    heads = model["seepage_bc"]["specified_heads"]
    assert [h["head"] for h in heads] == [20, 15]
    assert heads[0]["coords"] == [(0, 0), (20, 20)]


def test_rejected_search_surface_is_not_reported(monkeypatch):
    from xslope import search

    def rejected(*args, **kwargs):
        return ([{"FS": 9999, "solver_result": {"FS": 1.0},
                  "Xo": 0, "Yo": 2, "Depth": 0}], True, [], [])

    monkeypatch.setattr(search, "circular_search", rejected)
    with pytest.raises(RuntimeError, match="rejected"):
        analyze_model({"unit_system": "si"}, methods=("bishop",))
