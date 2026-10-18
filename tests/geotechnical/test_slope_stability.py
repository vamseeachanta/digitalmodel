# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reproduce public xslope benchmarks; optional dependencies remain optional."""

from pathlib import Path
from dataclasses import asdict
import json

import pytest

pytest.importorskip("xslope")

from xslope.fileio import load_slope_data
from digitalmodel.geotechnical.slope_stability import (
    analyze_model, analyze_profile, build_slope_model, run_seepage,
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
    assert original.methods["bishop"].search_mode == "fixed_circle"
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
    pytest.importorskip("gmsh")
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
        assert method.search_mode == "grid"
        assert method.factor_of_safety > 0
        assert method.search_converged is True
        assert method.critical_circle is not None
        assert method.unsolved_trials["attempted"] >= method.unsolved_trials["unsolved"]
    assert json.loads(json.dumps(asdict(result)))["num_slices"] == 20


@pytest.mark.parametrize("source", ["workbook", "profile"])
def test_readme_bishop_search_minimum(source, tmp_path):
    """Re-baseline README geometry with explicit grid seeding at 40 slices."""
    model = (load_slope_data(FIXTURES / "xslope_simple1.xlsx")
             if source == "workbook" else build_slope_model(equivalent_profile(), tmp_path))
    result = analyze_model(model, methods=("bishop",), num_slices=40)
    assert result.methods["bishop"].factor_of_safety == pytest.approx(1.214733, rel=1e-4)
    assert result.methods["bishop"].search_converged is True


def seepage_profile():
    """Use the public SI conversion with a head gradient along the surface."""
    data = equivalent_profile()
    data["layers"][0]["permeability_m_s"] = 1e-5
    surface = data["ground_surface_m"]
    data["seepage"] = {"element_size_m": 1.0, "heads_m": [
        {"head_m": 0.0, "boundary_m": surface[:2]},
        {"head_m": 1.0, "boundary_m": surface[1:]},
    ]}
    return data


def test_profile_seepage_then_both_methods(tmp_path, monkeypatch):
    pytest.importorskip("gmsh")
    from digitalmodel.geotechnical import slope_stability

    real_analyze = slope_stability.analyze_model
    observed = []

    def inspect_and_solve(model, **kwargs):
        observed.append(model)
        assert model["time_unit"] == "sec"
        assert model["unit_system"] == "si"
        assert model["seep_u"] is not None
        drained = [m for m in model["materials"] if m["option"] != "cp"]
        assert drained
        assert all(m["u"] == "seep" for m in drained)
        return real_analyze(model, **kwargs)

    monkeypatch.setattr(slope_stability, "analyze_model", inspect_and_solve)
    result = analyze_profile(seepage_profile(), tmp_path)
    assert len(observed) == 1
    assert (result.seepage.length_unit, result.seepage.time_unit) == ("m", "sec")
    assert result.seepage.converged
    assert set(result.methods) == {"bishop", "spencer"}
    for method in result.methods.values():
        assert method.search_mode == "grid"
        assert method.factor_of_safety > 0
        assert method.search_converged is True
        assert method.critical_circle is not None


@pytest.mark.parametrize("flow", [0.0, float("nan")])
def test_nonconverged_seepage_rejected_before_stability(tmp_path, monkeypatch, flow):
    pytest.importorskip("gmsh")
    from xslope import seep

    model = build_slope_model(seepage_profile(), tmp_path)
    monkeypatch.setattr(seep, "run_seepage_analysis", lambda _: {
        "converged": False, "flowrate": flow, "closure_fraction": flow,
    })
    with pytest.raises(RuntimeError, match="seepage did not converge"):
        run_seepage(model, element_size=1.0)
    assert model.get("seep_u") is None
    assert all(m["u"] != "seep" for m in model["materials"])


def test_example_matches_conversion_and_runs_engine(tmp_path):
    import yaml
    from digitalmodel.engine import engine

    example = Path(__file__).parents[2] / "examples/workflows/slope-stability/input.yml"
    config = yaml.safe_load(example.read_text())
    expected = equivalent_profile()
    actual = config["slope_stability"]["profile"]
    assert actual["units"] == expected["units"]
    for field in ("water_unit_weight_kn_m3", "bottom_elevation_m"):
        assert actual[field] == pytest.approx(expected[field], rel=1e-12)
    for point, converted in zip(actual["ground_surface_m"], expected["ground_surface_m"], strict=True):
        assert point == pytest.approx(converted, rel=1e-12)
    for field in ("layers", "circles"):
        assert len(actual[field]) == len(expected[field])
        for values, converted in zip(actual[field], expected[field], strict=True):
            assert values == pytest.approx(converted, rel=1e-12)
    config["slope_stability"]["outputs"]["directory"] = str(tmp_path / "results")
    input_path = tmp_path / "input.yml"
    input_path.write_text(yaml.safe_dump(config))
    result = engine(str(input_path))
    summary_path = Path(result["slope_stability"]["summary_json"])
    summary = json.loads(summary_path.read_text())
    assert summary["screening_status"] == "screening_only"
    assert set(summary["result"]["methods"]) == {"bishop", "spencer"}
    assert summary["result"]["xslope_version"] == "1.0.3"


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


def test_rejected_search_surface_is_not_reported(monkeypatch, tmp_path):
    from xslope import search

    def rejected(*args, **kwargs):
        return ([{"FS": 9999, "solver_result": {"FS": 1.0},
                  "Xo": 0, "Yo": 2, "Depth": 0}], True, [], [])

    monkeypatch.setattr(search, "circular_search", rejected)
    with pytest.raises(RuntimeError, match="rejected"):
        analyze_model(build_slope_model(equivalent_profile(), tmp_path), methods=("bishop",))


def test_grid_search_resolves_lower_foundation_family(tmp_path):
    """Synthetic fill over a stiff crust and weak undrained foundation.

    xslope 1.0.3 circular_search's grid rule retains competing tangent families.
    The deep trial below is a prior xslope 1.0.3 grid-search output with
    FS=0.689; the search must find a surface at least that low within
    1% numerical search tolerance. These values are regression bounds, not a
    design acceptance threshold or a proof of a global minimum.
    """
    data = {
        "units": "SI", "water_unit_weight_kn_m3": 9.81,
        "ground_surface_m": [[-50, 0], [0, 0], [20, 10], [100, 10]],
        "bottom_elevation_m": -20,
        "layers": [
            {"unit_weight_kn_m3": 20, "c_kpa": 2, "phi_deg": 25},
            {"top_boundary_m": [[-50, -1], [100, -1]],
             "unit_weight_kn_m3": 20, "c_kpa": 20, "phi_deg": 35},
            {"top_boundary_m": [[-50, -5], [100, -5]],
             "unit_weight_kn_m3": 20, "su_kpa": 1},
        ],
        "circles": [{"x_m": 10, "y_m": 15, "radius_m": 14}],
    }
    model = build_slope_model(data, tmp_path)
    result = analyze_model(model, methods=("bishop",), num_slices=20).methods["bishop"]
    # Re-evaluate a prior search output, tangent to the floor and near the edge.
    # This checks repeatability within this domain, not an independent minimum.
    model["circles"] = [{"Xo": 13.365017361111114, "Yo": 89.921875,
                         "Depth": -20, "R": 109.921875}]
    deep = analyze_model(model, methods=("bishop",), search=False,
                         num_slices=20).methods["bishop"]
    assert deep.factor_of_safety == pytest.approx(0.688645, rel=0.001)
    assert result.factor_of_safety <= deep.factor_of_safety * 1.01
    assert result.critical_circle["Depth"] < -5
    assert result.search_converged is True
    assert result.search_mode == "grid"


def test_search_mode_is_required():
    from digitalmodel.geotechnical.slope_stability import MethodResult

    with pytest.raises(TypeError, match="search_mode"):
        MethodResult(1.0, True, None)


def test_search_isolates_seeds_and_preserves_thin_user_surface(tmp_path, monkeypatch):
    """A worse coarse family may refine lower; centres must not share a cache."""
    from xslope import search

    model = build_slope_model(equivalent_profile(), tmp_path)
    user = dict(model["circles"][0])
    first = dict(user, Depth=user["Depth"] + 1.0)
    second = dict(user, Depth=user["Depth"] - 1.0)
    # Coarse ordering is deliberately opposite to the refined ordering.
    monkeypatch.setattr(search, "_grid_seed_circles", lambda *a, **k: [first, second])
    calls = []

    def refine(data, method, **kwargs):
        calls.append((list(data["circles"]), kwargs["seed"]))
        if kwargs["seed"] != "circles" or len(data["circles"]) != 1:
            fs = 1.3  # The shared-cache path stays in the first basin.
        else:
            circle = data["circles"][0]
            fs = 1.2 if circle == first else 0.9 if circle == second else 0.8
        kwargs["unsolved_out"].update(attempted=1, unsolved=0)
        return ([dict(FS=fs, solver_result={"FS": fs}, Xo=0, Yo=2, Depth=0)],
                True, [], [])

    monkeypatch.setattr(search, "circular_search", refine)
    result = analyze_model(model, methods=("bishop",)).methods["bishop"]
    assert result.factor_of_safety == 0.8
    assert result.search_factors_of_safety == {"grid": 0.9, "user_circles": 0.8}
    assert result.grid_thin_surface_filter == "coarse_only_5_percent"
    assert calls == [([first], "circles"), ([second], "circles"), ([user], "circles")]
    assert model["circles"] == [user]
    payload = json.loads(json.dumps(asdict(result)))
    assert payload["search_factors_of_safety"]["user_circles"] == 0.8


def test_workbook_search_requires_benchmarked_version(monkeypatch):
    from digitalmodel.geotechnical import slope_stability

    monkeypatch.setattr(slope_stability, "version", lambda _: "0.0")
    with pytest.raises(ValueError, match="requires benchmarked xslope version"):
        analyze_model({"unit_system": "si"}, methods=("bishop",))


def test_real_refinement_does_not_filter_thin_user_trials(tmp_path, monkeypatch):
    from math import hypot
    from xslope import search
    from xslope.slice import generate_slices

    data = equivalent_profile()
    (x0, y0), (x1, y1) = data["ground_surface_m"][:2]
    dx, dy = x1 - x0, y1 - y0
    length = hypot(dx, dy)
    radius = length / 2 + abs(dy) * 0.001
    data["circles"] = [{"x_m": (x0 + x1 - dy) / 2,
                        "y_m": (y0 + y1 + dx) / 2, "radius_m": radius}]
    model = build_slope_model(data, tmp_path)
    success, generated = generate_slices(model, circle=model["circles"][0], num_slices=20)
    assert success
    assert search._too_thin(generated[0], abs(dy), 0.05)
    real_filter = search._too_thin
    observed = []

    def inspect_filter(slices, height, fraction):
        observed.append((fraction, real_filter(slices, height, 0.05)))
        return real_filter(slices, height, fraction)

    monkeypatch.setattr(search, "_too_thin", inspect_filter)
    assert search.MIN_THICKNESS_FRAC == 0.05
    # Exercise the real pinned solver, including its filter, without a sweep.
    cache, _, _, _ = search.circular_search(model, "bishop", seed="circles", num_slices=20)
    assert cache
    assert observed
    assert {fraction for fraction, _ in observed} == {0.0}
    assert any(thin for _, thin in observed)
