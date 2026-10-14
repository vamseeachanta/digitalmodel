# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent static checks for generic deep-water SCR and SLWR cases."""

import ast
import json
import math
from pathlib import Path

import pytest

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfiguration,
    LazyWaveSolver,
)
from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
    CatenaryInput,
    CatenarySolver,
)
from digitalmodel.orcaflex.riser_config import (
    LazyWaveDesignInput,
    RiserPipeProperties,
    SCRDesignInput,
)
from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import (
    lazyWaveCatenaryEquation,
)

from . import independent_static_equilibrium as independent


FIXTURE = Path(__file__).parents[2] / "fixtures/test_vectors/mooring_risers/scr_slwr_generic_cases.json"
CASES = json.loads(FIXTURE.read_text(encoding="utf-8"))["cases"]
SCR_CASES = [case for case in CASES if case["type"] == "SCR"]
SLWR_CASES = [case for case in CASES if case["type"] == "SLWR"]
PIPE_FIELDS = set(RiserPipeProperties.model_fields) - {"grade"}


def pipe_for(case):
    """Build pipe properties from the explicit generic case inputs."""
    return RiserPipeProperties(**{key: case["values"][key] for key in PIPE_FIELDS})


def references(case, panels=4096):
    """Calculate independent geometry using only case inputs and pipe weight."""
    values = case["values"]
    pipe = pipe_for(case)
    weight = pipe.submerged_weight_per_m
    if case["type"] == "SCR":
        return independent.scr(values["water_depth"], values["angle"], weight, panels)
    design = lazy_design(case)
    return independent.slwr(
        values["water_depth"], values["angle"], weight,
        -design.net_uplift_per_m, values["sag_elevation"],
        values["hog_elevation"], panels,
    )


def lazy_design(case):
    """Map the common datum and buoyancy inputs into the sizing screen."""
    values = case["values"]
    return LazyWaveDesignInput(
        water_depth=values["water_depth"], pipe=pipe_for(case),
        buoyancy_od=values["buoyancy_od"], buoyancy_density=values["buoyancy_density"],
        hog_bend_depth=values["water_depth"] - values["hog_elevation"],
        sag_bend_clearance=values["sag_elevation"], hang_off_angle_deg=values["angle"],
    )


def lazy_results(case):
    """Evaluate both equilibrium kernels with an identical physical datum."""
    values = case["values"]
    weight = pipe_for(case).submerged_weight_per_m
    buoyed_weight = -lazy_design(case).net_uplift_per_m
    modern = LazyWaveSolver().solve(LazyWaveConfiguration(
        hangoff_angle=values["angle"], hangoff_below_msl=0.0,
        vertical_distance=values["water_depth"],
        sag_bend_elevation=values["sag_elevation"],
        hog_bend_above_seabed=values["hog_elevation"],
        weight_without_buoyancy=weight, weight_with_buoyancy=buoyed_weight,
    ))
    legacy = lazyWaveCatenaryEquation({
        "HangOff": {"q": values["angle"], "d": values["water_depth"] - values["sag_elevation"],
                    "F": None, "X": None, "w": weight},
        "HogBendAboveSeabed": values["hog_elevation"],
        "SagBendElevationAboveSeabed": values["sag_elevation"],
        "WeightPerUnitLengthWithOutBuoyancy": weight,
        "WeightPerUnitLengthWithBuoyancy": buoyed_weight,
    })
    return modern, legacy


def test_independent_checker_has_only_standard_library_imports():
    """Prevent the independent checker from acquiring solver dependencies."""
    tree = ast.parse(Path(independent.__file__).read_text(encoding="utf-8"))
    imports = [node for node in ast.walk(tree) if isinstance(node, (ast.Import, ast.ImportFrom))]
    assert imports
    for node in imports:
        names = [alias.name for alias in node.names] if isinstance(node, ast.Import) else [node.module]
        assert all(name == "math" for name in names)
    assert not any(isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
                   and node.func.id in {"eval", "exec", "__import__"} for node in ast.walk(tree))


@pytest.mark.parametrize("case", CASES, ids=lambda case: case["id"])
def test_independent_geometry_closes_and_quadrature_converges(case):
    """Check closure, force triangle and global weight with doubled panels."""
    reference = references(case)
    finer = references(case, panels=8192)
    assert reference["drop"] == pytest.approx(case["values"]["water_depth"], abs=1e-6)
    assert reference["angle"] == pytest.approx(case["values"]["angle"], abs=1e-9)
    assert reference["vertical"] == pytest.approx(reference["global_weight"], rel=1e-12)
    for key in ("drop", "length", "offset"):
        assert reference[key] == pytest.approx(finer[key], rel=1e-9)


@pytest.mark.parametrize("case", SCR_CASES, ids=lambda case: case["id"])
def test_scr_design_and_general_solver_in_inextensible_limit(case):
    """Compare rounded design geometry and the BVP to independent integration."""
    reference = references(case)
    values = case["values"]
    design = SCRDesignInput(water_depth=values["water_depth"], pipe=pipe_for(case),
                            hang_off_angle_from_vertical=values["angle"],
                            hang_off_elevation=values["hang_off_elevation"]).calculate_geometry()
    for key, target in (("suspended_length_m", "length"), ("horizontal_offset_m", "offset")):
        assert design[key] == pytest.approx(reference[target], abs=0.05)
    assert design["horizontal_tension_kN"] * 1000 == pytest.approx(reference["horizontal"], abs=50)
    result = CatenarySolver().solve(CatenaryInput(
        length=reference["length"], horizontal_span=reference["offset"],
        vertical_span=values["water_depth"], weight_per_length=pipe_for(case).submerged_weight_per_m,
        ea_stiffness=1e20,
    ))
    assert result.converged
    assert result.horizontal_tension == pytest.approx(reference["horizontal"], rel=1e-8)
    assert result.vertical_tension_fairlead == pytest.approx(reference["vertical"], rel=1e-8)
    assert result.shape_y[-1] == pytest.approx(values["water_depth"], abs=1e-6)


@pytest.mark.parametrize("case", SLWR_CASES, ids=lambda case: case["id"])
def test_slwr_equilibrium_and_recorded_legacy_force_divergence(case):
    """Check every segment and pin the legacy excess without accepting it."""
    reference = references(case)
    result, legacy = lazy_results(case)
    for segment, expected in zip(result.segments, reference["segments"], strict=True):
        assert segment.arc_length == pytest.approx(expected["length"], rel=1e-9)
        assert segment.horizontal_distance == pytest.approx(expected["offset"], rel=1e-9)
        assert segment.vertical_distance == pytest.approx(expected["drop"], rel=1e-9)
    assert result.vertical_force == pytest.approx(reference["global_weight"], rel=1e-12)
    assert result.horizontal_force == pytest.approx(reference["horizontal"], rel=1e-12)
    assert math.degrees(math.atan2(result.horizontal_force, result.vertical_force)) == pytest.approx(case["values"]["angle"])
    assert legacy["Summary"]["HangoffToTDP"]["S"] == pytest.approx(reference["length"], rel=1e-9)
    assert legacy["Summary"]["Fv"] - reference["vertical"] == pytest.approx(reference["horizontal"], rel=1e-12)


@pytest.mark.parametrize("case", SLWR_CASES, ids=lambda case: case["id"])
def test_lazy_wave_sizing_is_not_an_equilibrium_kernel(case):
    """Expose the heuristic length divergence and absent equilibrium outputs."""
    reference = references(case)
    sizing = lazy_design(case).size_buoyancy_section()
    assert sizing["total_riser_length_m"] != pytest.approx(reference["length"], abs=0.05)
    assert {"horizontal_force", "vertical_force", "vertical_closure_error"}.isdisjoint(sizing)
