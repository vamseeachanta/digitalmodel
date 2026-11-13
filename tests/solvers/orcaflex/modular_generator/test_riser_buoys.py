# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Typed riser buoys (digitalmodel #3974).

The typed riser had no buoy object: a ``type: buoy`` line end referenced a
name that no typed builder created, and links could not attach to a buoy.
These tests cover the new ``riser.buoys`` lumped 6D buoys, their validation,
the buoy-supported riser example and its golden. Data names are compared with
the public example "A02 Lazy S simple". No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/riser_buoy_supported/spec.yml"
NATIVE_BUOYS = REPO / (
    "docs/domains/orcaflex/examples/modular/A02/A02 Lazy S simple/includes/08_buoys.yml"
)
GOLDEN = Path(__file__).parent / "goldens/typed/riser_buoy_supported"
CONNECTION_KEY = (
    "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
    "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"
)


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _generate(data: dict, out: Path) -> Path:
    ModularModelGenerator.from_spec(ProjectInputSpec(**data)).generate(out)
    return out / "includes"


def _load(path: Path) -> dict:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def test_example_generates_buoy_lines_and_tethers(example, tmp_path):
    includes = _generate(example, tmp_path / "out")
    buoys = _load(includes / "06_riser_buoys.yml")["6DBuoys"]
    assert [b["Name"] for b in buoys] == ["SupportBuoy"]
    buoy = buoys[0]
    assert buoy["BuoyType"] == "Lumped buoy"
    assert buoy["Mass"] == 150 and buoy["Volume"] == 400
    assert buoy["InitialPosition"] == [-200, 0, -100]

    lines = {line["Name"]: line for line in _load(includes / "07_riser_lines.yml")["Lines"]}
    assert lines["SCR"][CONNECTION_KEY][0][0] == "SupportBuoy"
    assert lines["Jumper"][CONNECTION_KEY][1][0] == "SupportBuoy"

    links = _load(includes / "08_riser_links.yml")["Links"]
    assert [link["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"][0][0]
            for link in links] == ["SupportBuoy", "SupportBuoy"]

    master = (tmp_path / "out" / "master.yml").read_text(encoding="utf-8")
    assert master.index("06_riser_buoys.yml") < master.index("07_riser_lines.yml")


def test_buoy_data_names_match_native_lumped_buoy(example, tmp_path):
    includes = _generate(example, tmp_path / "out")
    emitted = _load(includes / "06_riser_buoys.yml")["6DBuoys"][0]
    native = _load(NATIVE_BUOYS)["6DBuoys"][0]
    assert native["BuoyType"] == "Lumped buoy"
    assert set(emitted) <= set(native)


def test_buoy_net_upthrust_matches_hand_value(example):
    """Comparator: 2549.7 kN recorded in the example header."""
    buoy = ProjectInputSpec(**example).riser.buoys[0]
    upthrust = (1.025 * buoy.volume - buoy.mass) * 9.80665
    assert upthrust == pytest.approx(2549.7, abs=0.05)


def test_line_end_on_undefined_buoy_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["lines"][0]["end_a"]["name"] = "MissingBuoy"
    with pytest.raises(ValidationError, match="not in riser.buoys"):
        ProjectInputSpec(**data)


def test_link_to_undefined_buoy_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["links"][0]["connections"][0]["object_name"] = "MissingBuoy"
    with pytest.raises(ValidationError, match="unknown object"):
        ProjectInputSpec(**data)


def test_duplicate_or_clashing_buoy_names_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["buoys"].append(copy.deepcopy(data["riser"]["buoys"][0]))
    with pytest.raises(ValidationError, match="Duplicate buoy names"):
        ProjectInputSpec(**data)
    data = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8").replace("SupportBuoy", "SCR"))
    with pytest.raises(ValidationError, match="clash"):
        ProjectInputSpec(**data)


def test_buoy_connection_must_be_builtin_or_vessel(example):
    data = copy.deepcopy(example)
    data["riser"]["buoys"][0]["connection"] = "SomewhereElse"
    with pytest.raises(ValidationError, match="connection"):
        ProjectInputSpec(**data)
    data["riser"]["buoys"][0]["connection"] = "FPSO"
    assert ProjectInputSpec(**data).riser.buoys[0].connection == "FPSO"


@pytest.mark.parametrize(
    "field", ["moments_of_inertia", "drag_area", "drag_coefficient", "added_mass_coefficient"],
)
def test_buoy_hydrodynamic_and_inertia_inputs_required(example, field):
    data = copy.deepcopy(example)
    del data["riser"]["buoys"][0][field]
    with pytest.raises(ValidationError, match=field):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf")])
@pytest.mark.parametrize("field", ["mass", "volume", "height"])
def test_buoy_scalar_inputs_must_be_finite(example, field, value):
    data = copy.deepcopy(example)
    data["riser"]["buoys"][0][field] = value
    with pytest.raises(ValidationError, match=field):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf")])
@pytest.mark.parametrize("component", range(3))
@pytest.mark.parametrize("field", [
    "position", "attitude", "moments_of_inertia", "centre_of_mass",
    "centre_of_volume", "drag_area", "drag_coefficient", "added_mass_coefficient",
])
def test_buoy_vector_inputs_must_be_finite(example, field, component, value):
    data = copy.deepcopy(example)
    buoy = data["riser"]["buoys"][0]
    buoy.setdefault(field, [0.0, 0.0, 0.0])[component] = value
    with pytest.raises(ValidationError, match=field):
        ProjectInputSpec(**data)


def test_riser_without_buoys_emits_no_buoy_include(example, tmp_path):
    data = copy.deepcopy(example)
    del data["riser"]["buoys"]
    del data["riser"]["links"]
    data["riser"]["lines"] = [data["riser"]["lines"][1]]
    data["riser"]["lines"][0]["end_b"] = {"type": "anchor", "position": [-200, 0, 0]}
    includes = _generate(data, tmp_path / "out")
    assert not (includes / "06_riser_buoys.yml").exists()


def test_example_matches_golden(example, tmp_path):
    includes = _generate(example, tmp_path / "out")
    golden_files = sorted(p.name for p in GOLDEN.glob("*.yml"))
    assert golden_files == sorted(p.name for p in includes.glob("*.yml"))
    for name in golden_files:
        assert _load(includes / name) == _load(GOLDEN / name), name


def test_example_header_hand_checks(example):
    """Recompute the example header values from the spec inputs.

    The buoy is treated as a solid uniform cylinder 9.21 m x 6 m; the SCR
    catenary is inextensible with the schema default contents density. These
    are arithmetic checks of the header, not a licensed statics result.
    """
    import math

    project = ProjectInputSpec(**example)
    g, rho = 9.80665, project.environment.water.density
    spec = project.riser
    buoy = spec.buoys[0]

    def endpoint(connection):
        # This illustrative geometry has level, unrotated object axes.
        if connection.type.value == "buoy":
            assert buoy.attitude == [0, 0, 0]
            origin = buoy.position
        elif connection.type.value == "vessel":
            assert spec.vessel.orientation == [0, 0, 0]
            origin = spec.vessel.position
        else:
            assert connection.type.value == "anchor"
            origin = [0, 0, -project.environment.water.depth]
        return [a + b for a, b in zip(origin, connection.position)]

    def submerged_weight(line):
        line_type = next(lt for lt in spec.line_types if lt.name == line.sections[0].line_type)
        assert all(s.line_type == line_type.name for s in line.sections)
        return (line_type.mass_per_length
                + line.contents.density * math.pi * line_type.inner_diameter**2 / 4
                - rho * math.pi * line_type.outer_diameter**2 / 4) * g

    upthrust = (rho * buoy.volume - buoy.mass) * g
    assert upthrust == pytest.approx(2549.7, abs=0.05)
    r, h = 9.21 / 2, buoy.height
    assert math.pi * r**2 * h == pytest.approx(buoy.volume, rel=1e-3)
    assert math.pi * r**2 * h == pytest.approx(399.7, abs=0.05)
    ixx, iyy, izz = buoy.moments_of_inertia
    assert ixx == iyy == pytest.approx(buoy.mass * (3 * r**2 + h**2) / 12, abs=0.05)
    assert izz == pytest.approx(buoy.mass * r**2 / 2, abs=0.05)
    assert buoy.drag_area == pytest.approx([2 * r * h, 2 * r * h, math.pi * r**2], abs=0.05)

    scr = next(line for line in spec.lines if line.name == "SCR")
    scr_type = next(lt for lt in spec.line_types if lt.name == scr.sections[0].line_type)
    od, idia = scr_type.outer_diameter, scr_type.inner_diameter
    area = math.pi * (od**2 - idia**2) / 4
    second_moment = math.pi * (od**4 - idia**4) / 64
    youngs_modulus = 210e6  # kN/m2; public material basis cited in the header.
    assert scr_type.poisson_ratio == 0.3
    assert scr_type.mass_per_length == pytest.approx(7.85 * area, abs=0.00005)
    assert scr_type.bending_stiffness == pytest.approx(youngs_modulus * second_moment, abs=0.5)
    assert scr_type.axial_stiffness == pytest.approx(youngs_modulus * area, abs=500)
    assert scr_type.torsional_stiffness == pytest.approx(
        youngs_modulus * second_moment / (1 + scr_type.poisson_ratio), abs=0.5
    )
    w = submerged_weight(scr)
    assert w == pytest.approx(1.314, abs=0.0005)
    scr_top, scr_anchor = endpoint(scr.end_a), endpoint(scr.end_b)
    height = scr_top[2] - scr_anchor[2]
    assert height == pytest.approx(397.0, abs=0.05)
    horizontal = 150.0  # Authored horizontal tension assumption, kN.
    a = horizontal / w
    suspended = math.sqrt(height**2 + 2 * height * a)
    vertical = w * suspended
    span = a * math.asinh(suspended / a)
    length = sum(s.length for s in scr.sections)
    assert suspended == pytest.approx(498.3, abs=0.05)
    assert vertical == pytest.approx(654.6, abs=0.05)
    assert math.degrees(math.atan2(horizontal, vertical)) == pytest.approx(12.9, abs=0.05)
    assert span == pytest.approx(248.8, abs=0.05)
    assert length - suspended == pytest.approx(801.7, abs=0.05)
    assert length - suspended + span == pytest.approx(1050.6, abs=0.05)
    assert math.dist(scr_top[:2], scr_anchor[:2]) == pytest.approx(
        length - suspended + span, abs=0.05
    )

    jumper = next(line for line in spec.lines if line.name == "Jumper")
    jumper_length = sum(s.length for s in jumper.sections)
    jumper_top, jumper_buoy = endpoint(jumper.end_a), endpoint(jumper.end_b)
    assert jumper_length == 250
    assert jumper_top == [-40, 0, -10]
    assert jumper_buoy == [-200, 0, -97]
    jumper_w = submerged_weight(jumper)
    assert jumper_w == pytest.approx(1.305, abs=0.0005)
    dx = math.dist(jumper_top[:2], jumper_buoy[:2])
    dz = jumper_top[2] - jumper_buoy[2]
    # Free catenary: sqrt(L^2 - dz^2) = 2 a sinh(dx / (2 a)), H = w a.
    target = math.sqrt(jumper_length**2 - dz**2)
    assert target > dx > 0
    low, high = dx / 100, jumper_length**2 / (target - dx)
    for _ in range(100):
        mid = (low + high) / 2
        if 2 * mid * math.sinh(dx / (2 * mid)) > target:
            low = mid
        else:
            high = mid
    jumper_a = (low + high) / 2
    assert 2 * jumper_a * math.sinh(dx / (2 * jumper_a)) == pytest.approx(target)
    jumper_horizontal = jumper_w * jumper_a
    jumper_vertical = jumper_horizontal * math.sinh(
        dx / (2 * jumper_a) - math.atanh(dz / jumper_length)
    )
    assert jumper_horizontal == pytest.approx(66.5, abs=0.05)
    assert jumper_vertical == pytest.approx(101.2, abs=0.05)

    tether_total = upthrust - vertical - jumper_vertical
    tether_each = tether_total / len(spec.links)
    assert tether_total == pytest.approx(1793.9, abs=0.05)
    assert tether_each == pytest.approx(896.9, abs=0.05)
    lengths = []
    for link in spec.links:
        top, bottom = link.connections
        assert top.object_name == buoy.name and bottom.object_name == "Anchored"
        top_xyz = [buoy.position[0] + top.x, buoy.position[1] + top.y, buoy.position[2] + top.z]
        anchor_xyz = [bottom.x, bottom.y, -project.environment.water.depth + bottom.z]
        geometric_length = math.dist(top_xyz, anchor_xyz)
        lengths.append(geometric_length)
        assert geometric_length == pytest.approx(397.0, abs=0.05)
        assert link.stiffness == pytest.approx(0.78e8 * 0.1**2)
        assert link.unstretched_length == pytest.approx(
            geometric_length / (1 + tether_each / link.stiffness), abs=0.0005
        )
    net_horizontal = horizontal - jumper_horizontal
    lateral_stiffness = sum(tether_each / length for length in lengths)
    shift = net_horizontal / lateral_stiffness
    assert net_horizontal == pytest.approx(83.5, abs=0.05)
    assert lateral_stiffness == pytest.approx(4.5, abs=0.05)
    assert shift == pytest.approx(18, abs=0.5)
    assert shift**2 / (2 * lengths[0]) == pytest.approx(0.4, abs=0.05)


@pytest.mark.parametrize("path", [
    ("riser", "line_types", 0, "bending_stiffness"),
    ("riser", "line_types", 0, "axial_stiffness"),
    ("riser", "line_types", 0, "torsional_stiffness"),
    ("riser", "line_types", 1, "mass_per_length"),
    ("riser", "lines", 0, "end_a", "position", 2),
    ("riser", "lines", 0, "end_b", "position", 2),
    ("riser", "lines", 1, "end_a", "position", 2),
    ("riser", "lines", 1, "end_b", "position", 2),
    ("riser", "vessel", "position", 2),
    ("riser", "buoys", 0, "position", 2),
    ("riser", "links", 0, "connections", 1, "z"),
])
def test_header_checks_detect_changed_properties_or_elevations(example, path):
    """Every changed input invalidates at least one stated header figure."""
    data = copy.deepcopy(example)
    parent = data
    for key in path[:-1]:
        parent = parent[key]
    parent[path[-1]] *= 1.1
    if parent[path[-1]] == 0:
        parent[path[-1]] = 10
    with pytest.raises(AssertionError):
        test_example_header_hand_checks(data)
