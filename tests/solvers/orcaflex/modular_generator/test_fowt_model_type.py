"""Tests for the floating offshore wind turbine (fowt) model type: schema and golden.

No licensed solver is used. Schema validation and the golden comparison for
the IEA 15 MW example spec are here; the router's unit and convention
handling and the OrcaFlex key names against Orcina's public K03 and H01
examples are in test_fowt_model_physics.py; routing safeguards (radiation
symmetry, RNA inertia, repeated generation, finite inputs) are in
test_fowt_model_routing.py.
"""
from __future__ import annotations

import copy
import math

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.floating_wind.iea15_orcaflex_spec import (
    build_spec,
    load_sources,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import (
    FowtRouter,
    buoy_attitude,
    vessel_rotation,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

from ._fowt_support import EXAMPLES, GOLDEN, SPEC_FILE, _flexible


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def spec_dict(sources):
    basis, data = sources
    return build_spec(basis, data)


# ----------------------------------------------------------------- schema
@pytest.mark.parametrize("field", ["normal_drag_coefficient", "axial_drag_coefficient",
                                   "normal_added_mass_coefficient", "axial_added_mass_coefficient"])
def test_mooring_hydrodynamic_coefficients_require_explicit_inputs(spec_dict, field):
    fowt = copy.deepcopy(spec_dict["fowt"])
    del fowt["mooring_line_types"][0][field]
    with pytest.raises(ValidationError, match=field):
        FowtSpec(**fowt)


def test_tower_poisson_ratio_requires_explicit_input(spec_dict):
    fowt = _flexible(spec_dict)
    del fowt["tower"]["sections"][0]["poisson_ratio"]
    with pytest.raises(ValidationError, match="poisson_ratio"):
        FowtSpec(**fowt)


def test_tower_poisson_ratio_reaches_each_line_type(spec_dict):
    routed = FowtRouter().route(FowtSpec(**_flexible(spec_dict)))
    tower_types = [t for t in routed["line_types"] if t["name"].startswith("Tower section")]
    assert [t["properties"]["PoissonRatio"] for t in tower_types] == [0.3, 0.25]


def test_shaft_tilt_requires_explicit_input(spec_dict):
    fowt = _flexible(spec_dict)
    del fowt["rotor_thrust"]["shaft_tilt"]
    with pytest.raises(ValidationError, match="shaft_tilt"):
        FowtSpec(**fowt)


@pytest.mark.parametrize("tilt", [-25.0, 25.0])
def test_shaft_tilt_has_no_reference_design_cap(spec_dict, tilt):
    fowt = _flexible(spec_dict)
    fowt["rotor_thrust"]["shaft_tilt"] = tilt
    routed = FowtRouter().route(FowtSpec(**fowt))
    hub = next(b for b in routed["buoys_6d"] if b["name"] == "Hub load point")
    force = hub["properties"]["LocalAppliedLoads"][0]["Force"]
    assert force == pytest.approx([2457.2 * math.cos(math.radians(tilt)), 0.0,
                                   -2457.2 * math.sin(math.radians(tilt))])


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("field", ["shaft_tilt", "poisson_ratio", "normal_drag_coefficient",
                                   "axial_drag_coefficient", "normal_added_mass_coefficient",
                                   "axial_added_mass_coefficient"])
def test_engineering_inputs_must_be_finite(spec_dict, field, value):
    fowt = _flexible(spec_dict)
    if field == "shaft_tilt":
        fowt["rotor_thrust"][field] = value
    elif field == "poisson_ratio":
        fowt["tower"]["sections"][0][field] = value
    else:
        fowt["mooring_line_types"][0][field] = value
    with pytest.raises(ValidationError, match=field):
        FowtSpec(**fowt)


def test_example_spec_file_matches_builder(spec_dict):
    committed = yaml.safe_load(SPEC_FILE.read_text(encoding="utf-8"))
    assert committed == spec_dict


def test_spec_validates_as_fowt_model(spec_dict):
    spec = ProjectInputSpec(**spec_dict)
    assert spec.is_fowt()
    assert spec.fowt.representation == "rigid"


def test_fowt_and_generic_together_rejected(spec_dict):
    data = copy.deepcopy(spec_dict)
    data["generic"] = {}
    with pytest.raises(ValidationError, match="multiple model types"):
        ProjectInputSpec(**data)


def test_fowt_with_riser_and_generic_fallback_rejected(spec_dict):
    """The riser + generic fallback exception does not admit a third domain."""
    template = EXAMPLES.parent / "library" / "templates" / "riser_catenary" / "spec.yml"
    data = yaml.safe_load(template.read_text(encoding="utf-8"))
    data["generic"] = {}
    ProjectInputSpec(**copy.deepcopy(data))  # the fallback pair alone is accepted
    data["fowt"] = copy.deepcopy(spec_dict["fowt"])
    with pytest.raises(ValidationError, match="multiple model types"):
        ProjectInputSpec(**data)


def test_rigid_with_tower_rejected(spec_dict):
    fowt = _flexible(spec_dict)
    fowt["representation"] = "rigid"
    with pytest.raises(ValidationError, match="rigid representation"):
        FowtSpec(**fowt)


def test_flexible_without_rna_rejected(spec_dict):
    fowt = _flexible(spec_dict)
    del fowt["rna"]
    with pytest.raises(ValidationError, match="needs tower and rna"):
        FowtSpec(**fowt)


def test_non_default_gravity_rejected(spec_dict):
    """The General section does not set gravity, so the weight term must use the default."""
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["gravity"] = 9.81
    with pytest.raises(ValidationError, match="gravity 9.81 is not supported"):
        FowtSpec(**fowt)
    fowt["gravity"] = 9.80665
    assert FowtSpec(**fowt).gravity == 9.80665


def _rna(fowt: dict) -> dict:
    routed = FowtRouter().route(FowtSpec(**fowt))
    return next(b for b in routed["buoys_6d"] if b["name"] == "RNA")["properties"]


def test_flexible_pitched_start_carries_tower_top_with_hull(spec_dict):
    """Pure trim: the tower top moves to (top sin t, 0, top cos t) and the RNA pitches by t."""
    fowt = _flexible(spec_dict)
    fowt["hull"]["initial_position"] = [10.0, 0.0, -0.4]
    fowt["hull"]["initial_orientation"] = [0.0, 5.0, 0.0]
    top = FowtSpec(**fowt).tower.top_elevation
    rna = _rna(fowt)
    t = math.radians(5.0)
    assert rna["InitialPosition"] == pytest.approx(
        [10.0 + top * math.sin(t), 0.0, -0.4 + top * math.cos(t)], abs=1e-9)
    assert rna["InitialAttitude"] == pytest.approx([0.0, 5.0, 0.0], abs=1e-9)


def test_flexible_rolled_start_moves_tower_top_to_negative_y(spec_dict):
    """Positive heel is right-handed about x: the tower top moves toward -y."""
    fowt = _flexible(spec_dict)
    fowt["hull"]["initial_orientation"] = [4.0, 0.0, 0.0]
    rna = _rna(fowt)
    assert rna["InitialPosition"][1] < 0.0
    assert rna["InitialAttitude"] == pytest.approx([4.0, 0.0, 0.0], abs=1e-9)


def test_buoy_attitude_reproduces_vessel_rotation():
    """Vessel order Rz Ry Rx and buoy order Rx Ry Rz describe the same attitude."""
    def axis(angle, k):
        c, s = math.cos(math.radians(angle)), math.sin(math.radians(angle))
        return {0: [[1, 0, 0], [0, c, -s], [0, s, c]], 1: [[c, 0, s], [0, 1, 0], [-s, 0, c]],
                2: [[c, -s, 0], [s, c, 0], [0, 0, 1]]}[k]

    def mul(a, b):
        return [[sum(a[i][k] * b[k][j] for k in range(3)) for j in range(3)] for i in range(3)]

    for heel, trim, heading in [(3.0, -5.0, 30.0), (-8.0, 4.0, 200.0), (0.0, 0.0, -45.0)]:
        r = vessel_rotation(heel, trim, heading)
        for row, want in zip(r, mul(axis(heading, 2), mul(axis(trim, 1), axis(heel, 0)))):
            assert row == pytest.approx(want, abs=1e-12)
        r1, r2, r3 = buoy_attitude(r)
        rebuilt = mul(axis(r1, 0), mul(axis(r2, 1), axis(r3, 2)))
        for row, want in zip(rebuilt, r):
            assert row == pytest.approx(want, abs=1e-12)


def test_flexible_with_heading_only_accepted(spec_dict):
    fowt = _flexible(spec_dict)
    fowt["hull"]["initial_orientation"] = [0.0, 0.0, 30.0]
    assert FowtSpec(**fowt).hull.initial_orientation == [0.0, 0.0, 30.0]


def test_rigid_with_initial_pitch_accepted(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["initial_orientation"] = [0.0, 3.0, 0.0]
    fowt["hull"]["included_in_statics"] = "None"
    assert FowtSpec(**fowt).hull.initial_orientation[1] == 3.0


def test_unknown_mooring_line_type_rejected(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["mooring_lines"][0]["line_type"] = "missing"
    with pytest.raises(ValidationError, match="unknown type"):
        FowtSpec(**fowt)


def test_radiation_periods_must_increase(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["radiation"].reverse()
    with pytest.raises(ValidationError, match="strictly increasing"):
        FowtSpec(**fowt)


# ----------------------------------------------------------------- golden
def test_golden_lc0_model(tmp_path):
    ModularModelGenerator(SPEC_FILE).generate(tmp_path)
    expected = sorted(p.relative_to(GOLDEN).as_posix() for p in GOLDEN.rglob("*.yml"))
    produced = sorted(p.relative_to(tmp_path).as_posix() for p in tmp_path.rglob("*.yml"))
    assert produced == expected
    for rel in expected:
        got = (tmp_path / rel).read_text(encoding="utf-8").replace("\r\n", "\n")
        want = (GOLDEN / rel).read_text(encoding="utf-8").replace("\r\n", "\n")
        assert got == want, rel


def test_decay_start_controls_reach_the_vessel(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["initial_position"] = [10.0, 0.0, -0.4]
    fowt["hull"]["initial_orientation"] = [0.0, 3.5, 0.0]
    fowt["hull"]["included_in_statics"] = "None"
    vessel = FowtRouter().route(FowtSpec(**fowt))["vessels"][0]["properties"]
    assert vessel["InitialPosition"] == [10.0, 0.0, -0.4]
    assert vessel["Orientation"] == [0.0, 3.5, 0.0]
    assert vessel["IncludedInStatics"] == "None"
    assert vessel["IncludeWaveLoad1stOrder"] == "No"


def test_included_in_statics_rejects_other_values(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["included_in_statics"] = "3 DOF"
    with pytest.raises(ValidationError):
        FowtSpec(**fowt)
