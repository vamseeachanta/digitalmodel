"""fowt model type: unit and convention handling and OrcaFlex key names.

No licensed solver is used. Conversions are checked against independent hand
calculations and key names against Orcina's public K03 and H01 examples held
in the repository. Split from test_fowt_model_type.py.
"""
from __future__ import annotations

import copy
import math
from pathlib import Path

import pytest
import yaml

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
    full_hydrostatic_stiffness,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

from ._fowt_support import H01, K03, _flexible


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def spec_dict(sources):
    basis, data = sources
    return build_spec(basis, data)


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
