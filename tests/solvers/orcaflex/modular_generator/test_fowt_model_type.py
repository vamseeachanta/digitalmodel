# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Tests for the floating offshore wind turbine (fowt) model type.

No licensed solver is used. The checks are: schema validation, the router's
unit and convention handling against independent hand calculations, OrcaFlex
key names against Orcina's public K03 and H01 examples held in the
repository, and a golden comparison for the IEA 15 MW example spec.
"""
from __future__ import annotations

import copy
import math
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.floating_wind.iea15_orcaflex_spec import (
    _expand,
    build_spec,
    hull_length,
    load_sources,
    thrust_at,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import (
    FowtRouter,
    buoy_attitude,
    full_hydrostatic_stiffness,
    vessel_rotation,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

ROOT = Path(__file__).resolve().parents[4]
EXAMPLES = ROOT / "docs" / "domains" / "orcaflex" / "examples"
SPEC_FILE = EXAMPLES / "fowt" / "iea15-volturnus-s" / "spec.yml"
GOLDEN = Path(__file__).parent / "goldens" / "fowt_iea15_lc0"
K03 = EXAMPLES / "modular" / "K03" / "K03 15MW semi-sub FOWT" / "includes"
H01 = EXAMPLES / "modular" / "H01" / "H01 Chinese lantern" / "includes"


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def spec_dict(sources):
    basis, data = sources
    return build_spec(basis, data)


def _flexible(spec_dict: dict) -> dict:
    """Synthetic flexible variant: two tower sections and a lumped RNA.

    Section values are illustrative for routing tests only; they are not
    reference-design values.
    """
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["representation"] = "flexible"
    fowt["hull"]["mass"] = 17838.0
    fowt["hull"]["centre_of_mass"] = [0.0, 0.0, -14.4]
    fowt["tower"] = {"base_elevation": 15.0, "target_segment_length": 5.0, "sections": [
        {"length": 60.0, "outer_diameter": 10.0, "mass_per_length": 20.0,
         "bending_stiffness": 1.0e9, "axial_stiffness": 1.0e8, "torsional_stiffness": 7.0e8,
         "poisson_ratio": 0.3},
        {"length": 69.386, "outer_diameter": 8.0, "mass_per_length": 10.0,
         "bending_stiffness": 5.0e8, "axial_stiffness": 6.0e7, "torsional_stiffness": 3.5e8,
         "poisson_ratio": 0.25}]}
    fowt["rna"] = {"mass": 945.77, "centre_of_mass_from_tower_top": [-7.32, -0.1, 4.6],
                   "inertia_about_centre_of_mass": [[4.0e5, 0, 0], [0, 3.4e5, 0],
                                                    [0, 0, 3.4e5]]}
    fowt["rotor_thrust"] = {"thrust": 2457.2, "hub_position": [-12.0313, 0.0, 150.0],
                            "shaft_tilt": 6.0}
    return fowt


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


# -------------------------------------------------- conversions and physics
def test_hydrostatic_matrix_adds_weight_moment_arm(spec_dict):
    spec = FowtSpec(**spec_dict["fowt"])
    k = full_hydrostatic_stiffness(spec)
    base = spec.hull.hydrostatic_stiffness_excluding_gravity
    arm = spec.hull.mass * spec.gravity * (-spec.hull.centre_of_mass[2])
    assert k[0][0] == base[0][0]
    assert k[1][1] - base[1][1] == pytest.approx(arm)
    assert k[2][2] - base[2][2] == pytest.approx(arm)
    # Independent comparator: Orcina's K03 model of the same platform carries
    # a roll/pitch entry 0.403e6 kN.m/rad above the WAMIT value; its own
    # m g (-zG) is 20287 te x 9.80665 x 2.0404 m = 0.406e6. The router's term
    # for the basis aggregation (zG = -1.575 m) is smaller, as expected.
    assert 0.30e6 < arm < 0.33e6


def test_k03_roll_term_consistent_with_convention():
    """K03's own numbers satisfy K = WAMIT(rho g) + m g (-zG) within 1 %."""
    data = yaml.safe_load((K03 / "04_vessel_types.yml").read_text(encoding="utf-8"))
    draught = data["VesselTypes"][0]["Draughts"][0]
    k = draught["HydrostaticStiffnessz, HydrostaticStiffnessRx, HydrostaticStiffnessRy"]
    wamit_roll = 218240.5 * 1.025 * 9.80665  # generator_data.yml .hst (4,4) x rho g
    # PyYAML reads K03's "20.28719e3" style numbers as strings.
    arm = float(draught["Mass"]) * 9.80665 * (-float(draught["CentreOfMass"][2]))
    assert float(k[1][1]) == pytest.approx(wamit_roll + arm, rel=0.01)
    assert float(k[0][0]) == pytest.approx(443.0486 * 1.025 * 9.80665, rel=0.001)


def test_heave_stiffness_against_published(spec_dict, sources):
    basis, _ = sources
    k33 = spec_dict["fowt"]["hull"]["hydrostatic_stiffness_excluding_gravity"][0][0]
    assert k33 * 1000.0 == pytest.approx(
        basis["published"]["hydrostatic_stiffness"]["value"][2], rel=0.02)


def test_infinite_frequency_added_mass_against_k03(spec_dict):
    entry = spec_dict["fowt"]["hull"]["radiation"][0]
    assert entry["period"] == 0.0
    data = yaml.safe_load((K03 / "04_vessel_types.yml").read_text(encoding="utf-8"))
    k03 = data["VesselTypes"][0]["Draughts"][0]["FrequencyDependentAddedMassAndDamping"][0]
    key = ("AddedMassMatrixX, AddedMassMatrixY, AddedMassMatrixZ, "
           "AddedMassMatrixRx, AddedMassMatrixRy, AddedMassMatrixRz")
    assert k03["AMDPeriodOrFrequency"] == 0
    for i in (0, 2, 4):
        assert entry["added_mass"][i][i] == pytest.approx(float(k03[key][i][i]), rel=0.03)


def test_emitted_radiation_matrices_are_symmetric(generated, sources):
    """Native matrices mirror the upper triangle, following the WAMIT importer."""
    basis, source_data = sources
    data = copy.deepcopy(source_data)
    original = copy.deepcopy(data)
    build_spec(basis, data)
    assert data == original
    raw = data["hydrodynamics"]["added_mass_infinite_frequency_nondimensional"]["value"]
    entries = generated["VesselTypes"][0]["Draughts"][0]["FrequencyDependentAddedMassAndDamping"]
    mass_key = ("AddedMassMatrixX, AddedMassMatrixY, AddedMassMatrixZ, "
                "AddedMassMatrixRx, AddedMassMatrixRy, AddedMassMatrixRz")
    damping_key = "DampingX, DampingY, DampingZ, DampingRx, DampingRy, DampingRz"
    assert entries[0]["AMDPeriodOrFrequency"] == 0
    for entry in entries:
        for key in (mass_key, damping_key):
            matrix = entry[key]
            assert matrix == [list(row) for row in zip(*matrix)]
    for i in range(6):
        for j in range(6):
            assert entries[0][mass_key][i][j] == pytest.approx(
                1.025 * raw[min(i, j)][max(i, j)])


def test_symmetry_completion_holds_for_full_matrices(sources):
    """A22=A11, A44=A55, A24=-A15 hold for the matrices held in full."""
    _, data = sources
    for key in ("added_mass_zero_frequency_nondimensional",
                "added_mass_infinite_frequency_nondimensional"):
        a = data["hydrodynamics"][key]["value"]
        assert a[1][1] == pytest.approx(a[0][0], rel=1e-3)
        assert a[3][3] == pytest.approx(a[4][4], rel=1e-3)
        assert a[1][3] == pytest.approx(-a[0][4], rel=1e-3)


def test_expand_places_entries():
    a, b = _expand([10.0, 1, 3, 5, 15, 6, 11, 33, 55, 115, 66])
    assert a[0][0] == a[1][1] == 1 and a[3][3] == a[4][4] == 5
    assert a[0][4] == a[4][0] == 15 and a[1][3] == a[3][1] == -15
    assert b[2][2] == 33 and b[5][5] == 66 and b[1][3] == -115


def test_damping_redimensionalised_with_frequency(spec_dict, sources):
    _, data = sources
    row = data["hydrodynamics"]["radiation_by_period_nondimensional"]["value"][10]
    entry = spec_dict["fowt"]["hull"]["radiation"][11]
    assert entry["period"] == row[0]
    omega = 2 * math.pi / row[0]
    assert entry["damping"][2][2] == pytest.approx(1.025 * omega * row[7])
    assert entry["added_mass"][2][2] == pytest.approx(1.025 * row[2])


def test_units_converted_to_te_and_kn(spec_dict, sources):
    basis, _ = sources
    hull = spec_dict["fowt"]["hull"]
    assert hull["mass"] == pytest.approx(basis["system"]["mass"]["value"] / 1000)
    lt = spec_dict["fowt"]["mooring_line_types"][0]
    assert lt["axial_stiffness"] == pytest.approx(3.27e6)
    assert lt["mass_per_length"] == pytest.approx(0.685)
    assert lt["minimum_breaking_load"] == pytest.approx(22286.0)


def test_anchor_height_measured_from_seabed(spec_dict):
    for line in spec_dict["fowt"]["mooring_lines"]:
        assert line["anchor_height_above_seabed"] == pytest.approx(0.0)


def test_hull_length_is_column_footprint(sources):
    basis, _ = sources
    assert hull_length(basis) == pytest.approx(51.75 * 1.5 + 12.5)


def test_thrust_interpolation(sources):
    _, data = sources
    rows = data["rotor_performance"]["value"]
    assert thrust_at(data, rows[5][0]) == pytest.approx(rows[5][3])
    mid = 0.5 * (rows[5][0] + rows[6][0])
    assert thrust_at(data, mid) == pytest.approx(0.5 * (rows[5][3] + rows[6][3]))
    with pytest.raises(ValueError):
        thrust_at(data, 30.0)


def test_thrust_vector_along_tilted_shaft(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["rotor_thrust"] = {"thrust": 1000.0, "hub_position": [-12.0, 0.0, 150.0],
                            "shaft_tilt": 6.0}
    routed = FowtRouter().route(FowtSpec(**fowt))
    hub = routed["buoys_6d"][0]["properties"]
    assert hub["Connection"] == "VolturnUS-S"
    force = hub["LocalAppliedLoads"][0]["Force"]
    assert force[0] == pytest.approx(1000 * math.cos(math.radians(6)))
    assert force[2] == pytest.approx(-1000 * math.sin(math.radians(6)))
    assert math.hypot(force[0], force[2]) == pytest.approx(1000.0)


def test_flexible_routes_tower_rna_and_hub(spec_dict):
    routed = FowtRouter().route(FowtSpec(**_flexible(spec_dict)))
    names = [b["name"] for b in routed["buoys_6d"]]
    assert names == ["RNA", "Hub load point"]
    tower = routed["lines"][0]
    assert tower["name"] == "Tower"
    sections = tower["properties"]["LineType, Length, TargetSegmentLength"]
    assert [s[0] for s in sections] == ["Tower section 2", "Tower section 1"]
    assert sum(s[1] for s in sections) == pytest.approx(129.386)
    rna = routed["buoys_6d"][0]["properties"]
    assert rna["InitialPosition"][2] == pytest.approx(144.386)
    assert rna["MomentsOfInertia"] == [4.0e5, 3.4e5, 3.4e5]
    hub = routed["buoys_6d"][1]["properties"]
    assert hub["Connection"] == "RNA"
    assert hub["InitialPosition"][2] == pytest.approx(150.0 - 144.386)
    # Flexible hull carries the platform alone, so the weight arm uses it.
    k = full_hydrostatic_stiffness(FowtSpec(**_flexible(spec_dict)))
    base = spec_dict["fowt"]["hull"]["hydrostatic_stiffness_excluding_gravity"]
    assert k[1][1] - base[1][1] == pytest.approx(17838.0 * 9.80665 * 14.4)


# ------------------------------------------------------ key names vs K03/H01
def _keys(path: Path, section: str) -> set[str]:
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    keys: set[str] = set()
    for obj in data[section]:
        keys |= set(obj)
    return keys


@pytest.fixture(scope="module")
def generated(spec_dict, tmp_path_factory):
    fowt = _flexible(spec_dict)
    data = copy.deepcopy(spec_dict)
    data["fowt"] = fowt
    out = tmp_path_factory.mktemp("fowt_flexible")
    ModularModelGenerator.from_spec(ProjectInputSpec(**data)).generate(out)
    return yaml.safe_load((out / "includes" / "20_generic_objects.yml").read_text(encoding="utf-8"))


def test_emitted_keys_exist_in_held_orcina_examples(generated):
    k03_vessel_types = _keys(K03 / "04_vessel_types.yml", "VesselTypes")
    k03_draught = set(yaml.safe_load((K03 / "04_vessel_types.yml").read_text(
        encoding="utf-8"))["VesselTypes"][0]["Draughts"][0])
    allowed = {
        "VesselTypes": k03_vessel_types,
        "Vessels": _keys(K03 / "06_vessels.yml", "Vessels"),
        "Lines": _keys(K03 / "07_lines.yml", "Lines"),
        "LineTypes": _keys(K03 / "05_line_types.yml", "LineTypes"),
        "6DBuoys": _keys(K03 / "08_buoys.yml", "6DBuoys")
        | _keys(H01 / "08_buoys.yml", "6DBuoys"),
    }
    for section, keys in allowed.items():
        for obj in generated[section]:
            extra = set(obj) - keys
            assert not extra, (section, obj["Name"], extra)
    for draught in generated["VesselTypes"][0]["Draughts"]:
        assert not set(draught) - k03_draught


def test_section_order_puts_vessels_before_buoys(generated):
    order = list(generated)
    assert order.index("VesselTypes") < order.index("Vessels") < order.index("6DBuoys")
    assert order.index("LineTypes") < order.index("Lines")


# ----------------------------------------------------------------- golden
@pytest.mark.parametrize("flexible", [False, True])
@pytest.mark.parametrize("methods", [
    ("generate", "generate"),
    ("generate_with_overrides", "generate_with_overrides"),
    ("generate", "generate_with_overrides"),
    ("generate_with_overrides", "generate"),
])
def test_repeated_generation_keeps_one_copy_of_each_object(spec_dict, tmp_path, flexible, methods):
    """Both entry points can reuse a generator without appending routed objects."""
    if flexible:
        data = copy.deepcopy(spec_dict)
        data["fowt"] = _flexible(spec_dict)
        generator = ModularModelGenerator.from_spec(ProjectInputSpec(**data))
    else:
        generator = ModularModelGenerator(SPEC_FILE)
    first, second = tmp_path / "first", tmp_path / "second"
    getattr(generator, methods[0])(first)
    getattr(generator, methods[1])(second)

    def contents(folder):
        return {p.relative_to(folder).as_posix(): p.read_bytes()
                for p in folder.rglob("*") if p.is_file()}

    assert contents(first) == contents(second)
    objects = yaml.safe_load((second / "includes" / "20_generic_objects.yml").read_text())
    assert len(objects["Vessels"]) == 1
    assert len(objects["Lines"]) == (4 if flexible else 3)
    assert len(objects.get("6DBuoys", [])) == (2 if flexible else 0)
    for collection in objects.values():
        names = [obj["Name"] for obj in collection]
        assert len(names) == len(set(names))


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


# ------------------------------------------------------- routing safeguards
MASS_KEY = ("AddedMassMatrixX, AddedMassMatrixY, AddedMassMatrixZ, "
            "AddedMassMatrixRx, AddedMassMatrixRy, AddedMassMatrixRz")
DAMPING_KEY = "DampingX, DampingY, DampingZ, DampingRx, DampingRy, DampingRz"


def _radiation(fowt):
    spec = fowt if isinstance(fowt, FowtSpec) else FowtSpec(**fowt)
    routed = FowtRouter().route(spec)
    return routed["vessel_types"][0]["properties"]["Draughts"][0][
        "FrequencyDependentAddedMassAndDamping"]


def test_emitted_radiation_is_exactly_symmetric_without_mutating_source(spec_dict):
    fowt = FowtSpec(**spec_dict["fowt"])
    original = fowt.model_dump()
    entries = _radiation(fowt)
    for source, emitted in zip(fowt.hull.radiation, entries, strict=True):
        for matrix, key in ((source.added_mass, MASS_KEY), (source.damping, DAMPING_KEY)):
            for i in range(6):
                assert emitted[key][i][i] == matrix[i][i]
                for j in range(i + 1, 6):
                    # Orcina WAMIT importer policy: upper triangle mirrored.
                    assert emitted[key][i][j] == matrix[i][j]
                    assert emitted[key][j][i] == matrix[i][j]
    assert fowt.model_dump() == original


@pytest.mark.parametrize("matrix_name,key", [("added_mass", MASS_KEY), ("damping", DAMPING_KEY)])
def test_radiation_symmetry_tolerance_accepts_boundary_and_rejects_excess(
        spec_dict, matrix_name, key):
    fowt = copy.deepcopy(spec_dict["fowt"])
    matrix = [[0.0] * 6 for _ in range(6)]
    for i in range(6):
        matrix[i][i] = 100.0
    matrix[0][1], matrix[1][0] = 2.0, 3.0
    fowt["hull"]["radiation"][0][matrix_name] = matrix
    emitted = _radiation(fowt)[0][key]
    assert emitted[0][1] == emitted[1][0] == 2.0
    matrix[1][0] = 3.01  # Exceeds 1 % of sqrt(diagonal_i * diagonal_j).
    with pytest.raises(ValueError, match="symmetry tolerance"):
        _radiation(fowt)


@pytest.mark.parametrize("matrix_name", ["added_mass", "damping"])
def test_radiation_zero_diagonal_does_not_hide_asymmetry(spec_dict, matrix_name):
    fowt = copy.deepcopy(spec_dict["fowt"])
    matrix = [[0.0] * 6 for _ in range(6)]
    matrix[0][1] = 1.0
    fowt["hull"]["radiation"][0][matrix_name] = matrix
    with pytest.raises(ValueError, match="symmetry tolerance"):
        _radiation(fowt)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("matrix_name", ["added_mass", "damping"])
def test_radiation_nonfinite_entries_rejected(spec_dict, matrix_name, value):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["radiation"][0][matrix_name][0][1] = value
    with pytest.raises(ValueError, match="finite"):
        _radiation(fowt)


def test_first_order_wave_load_rejected_without_sourced_raos(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["include_wave_load_first_order"] = True
    with pytest.raises(ValidationError, match="sourced load RAOs"):
        FowtSpec(**fowt)


@pytest.mark.parametrize("i,j", [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)])
def test_rna_asymmetric_inertia_rejected(spec_dict, i, j):
    fowt = _flexible(spec_dict)
    fowt["rna"]["inertia_about_centre_of_mass"][i][j] = -1.2e4
    with pytest.raises(ValidationError, match="symmetric"):
        FowtSpec(**fowt)


def test_rna_non_positive_definite_inertia_rejected(spec_dict):
    fowt = _flexible(spec_dict)
    inertia = fowt["rna"]["inertia_about_centre_of_mass"]
    inertia[0][2] = inertia[2][0] = 2.0 * max(inertia[0][0], inertia[2][2])
    with pytest.raises(ValidationError, match="positive definite"):
        FowtSpec(**fowt)


def _rotation_from_buoy_attitude(angles):
    a, b, c = (math.radians(x) for x in angles)
    rx = [[1, 0, 0], [0, math.cos(a), -math.sin(a)], [0, math.sin(a), math.cos(a)]]
    ry = [[math.cos(b), 0, math.sin(b)], [0, 1, 0], [-math.sin(b), 0, math.cos(b)]]
    rz = [[math.cos(c), -math.sin(c), 0], [math.sin(c), math.cos(c), 0], [0, 0, 1]]

    def mul(p, q):
        return [[sum(p[i][k] * q[k][j] for k in range(3)) for j in range(3)] for i in range(3)]
    return mul(mul(rx, ry), rz)


def _buoys(fowt):
    return {b["name"]: b["properties"]
            for b in FowtRouter().route(FowtSpec(**fowt))["buoys_6d"]}


@pytest.mark.parametrize("products", [
    {(0, 2): -1.2e4},
    {(0, 1): 3.0e3, (0, 2): -4.5e4, (1, 2): -7.0e2},
])
@pytest.mark.parametrize("tilted", [False, True])
def test_rna_products_of_inertia_carried_by_principal_axes_child(spec_dict, products, tilted):
    """Products are not dropped: the host carries mass and centre, a rigidly
    attached child carries the principal moments at the principal attitude, and
    the pair reproduces the full tensor about the centre of mass."""
    fowt = _flexible(spec_dict)
    rna = fowt["rna"]
    for (i, j), value in products.items():
        rna["inertia_about_centre_of_mass"][i][j] = value
        rna["inertia_about_centre_of_mass"][j][i] = value
    if tilted:
        fowt["hull"]["initial_orientation"] = [1.0, 3.5, 20.0]
    upright = copy.deepcopy(fowt)
    upright["rna"]["inertia_about_centre_of_mass"] = [
        [rna["inertia_about_centre_of_mass"][i][i] if i == j else 0.0 for j in range(3)]
        for i in range(3)]
    buoys, reference = _buoys(fowt), _buoys(upright)
    name = rna.get("name", "RNA")
    host = buoys[name]
    child = buoys[f"{name} inertia"]
    # Host: unchanged position, attitude, mass and centre; moments moved to the child.
    for key in ("InitialPosition", "InitialAttitude", "Mass", "CentreOfMass", "Connection"):
        assert host[key] == reference[name][key]
    assert host["MomentsOfInertia"] == [0, 0, 0]
    assert child["Connection"] == name
    assert child["InitialPosition"] == rna["centre_of_mass_from_tower_top"]
    assert child["Mass"] == 0 and child["CentreOfMass"] == [0, 0, 0]
    r = _rotation_from_buoy_attitude(child["InitialAttitude"])
    moments = child["MomentsOfInertia"]
    rebuilt = [[sum(r[i][k] * moments[k] * r[j][k] for k in range(3)) for j in range(3)]
               for i in range(3)]
    full = rna["inertia_about_centre_of_mass"]
    scale = max(full[i][i] for i in range(3))
    for i in range(3):
        for j in range(3):
            assert rebuilt[i][j] == pytest.approx(full[i][j], abs=1e-9 * scale)
    # Child axes stay close to the host axes (no axis permutation).
    assert all(abs(angle) < 45.0 for angle in child["InitialAttitude"])
    # Hub load point and tower connection are unaffected.
    for other in set(reference) - {name}:
        assert buoys[other] == reference[other]


def test_diagonal_rna_inertia_emits_no_child(spec_dict):
    buoys = _buoys(_flexible(spec_dict))
    assert not any(name.endswith(" inertia") for name in buoys)


def test_generated_source_note_preserves_inertia_qualification(spec_dict, sources):
    basis, _ = sources
    note = spec_dict["fowt"]["source_note"]
    assert f"Basis readiness {basis['qualification']['readiness']}" in note
    assert f"use for analysis {basis['qualification']['use_for_analysis']}" in note
    assert "System inertia assumes the RNA tensor is about tower top with CG tensor F-P" in note
    assert "Retaining F24:K24 as trusted endpoint is an additional assumption" in note
    assert "L24:Q24 remains the independent published comparator" in note
    assert "tensor-reference discrepancy remains unresolved" in note


@pytest.mark.parametrize("representation", ["rigid", "flexible"])
@pytest.mark.parametrize("method", ["generate", "generate_with_overrides"])
@pytest.mark.parametrize("from_file", [False, True])
def test_repeated_generation_preserves_bundle_and_domain_spec(
        spec_dict, tmp_path, representation, method, from_file):
    data = copy.deepcopy(spec_dict)
    if representation == "flexible":
        data["fowt"] = _flexible(spec_dict)
    source = ProjectInputSpec(**data)
    original = source.model_dump()
    if from_file:
        path = tmp_path / "spec.yml"
        path.write_text(yaml.safe_dump(data), encoding="utf-8")
        generator = ModularModelGenerator(path)
    else:
        generator = ModularModelGenerator.from_spec(source)
    outputs = []
    for name in ("first", "second"):
        out = tmp_path / name
        getattr(generator, method)(out)
        outputs.append({p.relative_to(out): p.read_bytes() for p in out.rglob("*") if p.is_file()})
    assert outputs[0] == outputs[1]
    assert source.model_dump() == original
    assert generator.spec.fowt == source.fowt
    assert len(generator.spec.generic.vessels) == 1
    assert len(generator.spec.generic.lines) == (4 if representation == "flexible" else 3)


def test_repeat_routing_reflects_domain_edits(spec_dict):
    generator = ModularModelGenerator.from_spec(ProjectInputSpec(**spec_dict))
    generator._run_routers()
    generator.spec.fowt.hull.initial_position[0] = 5.0
    generator._run_routers()
    assert len(generator.spec.generic.vessels) == 1
    assert generator.spec.generic.vessels[0].properties["InitialPosition"][0] == 5.0


# ------------------------------------------------------- finite inputs
def _set_path(data: dict, path: tuple, value: float) -> None:
    target = data
    for key in path[:-1]:
        target = target[key]
    target[path[-1]] = value


NONFINITE_PATHS = [
    ("hull", "mass"),
    ("hull", "centre_of_mass", 2),
    ("hull", "inertia_about_centre_of_mass", 0, 0),
    ("hull", "displaced_volume"),
    ("hull", "centre_of_buoyancy", 0),
    ("hull", "hydrostatic_stiffness_excluding_gravity", 1, 1),
    ("hull", "radiation", -1, "period"),
    ("hull", "radiation", 0, "added_mass", 2, 2),
    ("hull", "other_quadratic_damping", 0),
    ("mooring_line_types", 0, "mass_per_length"),
    ("mooring_lines", 0, "fairlead", 0),
    ("mooring_lines", 0, "anchor_xy", 1),
    ("mooring_lines", 0, "anchor_height_above_seabed"),
    ("mooring_lines", 0, "length"),
]
FLEXIBLE_NONFINITE_PATHS = [
    ("tower", "base_elevation"),
    ("tower", "sections", 0, "bending_stiffness"),
    ("tower", "sections", 0, "poisson_ratio"),
    ("rna", "mass"),
    ("rna", "centre_of_mass_from_tower_top", 0),
    ("rna", "inertia_about_centre_of_mass", 1, 1),
]


@pytest.mark.parametrize("value", [float("nan"), float("inf")])
@pytest.mark.parametrize("path", NONFINITE_PATHS)
def test_nonfinite_rigid_inputs_rejected(spec_dict, path, value):
    """NaN or infinity must never reach the generated model."""
    data = copy.deepcopy(spec_dict)
    _set_path(data["fowt"], path, value)
    with pytest.raises(ValidationError):
        FowtSpec(**data["fowt"])
    with pytest.raises(ValidationError):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("value", [float("nan"), -float("inf")])
@pytest.mark.parametrize("path", FLEXIBLE_NONFINITE_PATHS)
def test_nonfinite_flexible_inputs_rejected(spec_dict, path, value):
    fowt = _flexible(spec_dict)
    _set_path(fowt, path, value)
    with pytest.raises(ValidationError):
        FowtSpec(**fowt)


@pytest.mark.parametrize("value", [float("nan"), float("inf")])
def test_nonfinite_thrust_inputs_rejected(spec_dict, value):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["rotor_thrust"] = {"thrust": 1000.0, "shaft_tilt": 6.0,
                            "hub_position": [-10.0, 0.0, 150.0], "wind_speed": 10.0}
    FowtSpec(**fowt)  # finite reference accepted
    for key, index in (("thrust", None), ("shaft_tilt", None), ("wind_speed", None),
                       ("hub_position", 2)):
        probe = copy.deepcopy(fowt)
        if index is None:
            probe["rotor_thrust"][key] = value
        else:
            probe["rotor_thrust"][key][index] = value
        with pytest.raises(ValidationError):
            FowtSpec(**probe)
