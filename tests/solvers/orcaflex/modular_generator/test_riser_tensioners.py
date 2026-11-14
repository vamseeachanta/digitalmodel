"""Typed top tensioners for top-tensioned and drilling risers (#3976).

The typed riser had no tensioner, so a top-tensioned riser existed only as a
generic pass-through. ``riser.tensioners`` now emits linear spring/damper
links from the vessel to the free riser top, with unstretched lengths that
give the target vertical tension in the generated geometry. Native data names
are compared with the public examples "H03 Floating and stowed lines" (linear
spring/damper) and "B01 Drilling riser". No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
import math
import re
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.riser_links_builder import (
    RiserLinksBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    ProjectInputSpec,
    RiserTensioner,
)

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/library/typed/riser_top_tensioned/spec.yml"
NATIVE_LINKS = REPO / (
    "docs/domains/orcaflex/examples/modular/H03/H03 Floating and stowed lines/includes/11_links.yml"
)
GOLDEN = Path(__file__).parent / "goldens/typed/riser_top_tensioned"
CONN = "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _links(data: dict) -> list[dict]:
    spec = ProjectInputSpec(**data)
    return RiserLinksBuilder(spec, BuilderContext()).build()["Links"]


def test_tensioner_links_geometry_and_unstretched_length(example):
    links = _links(example)
    assert [link["Name"] for link in links] == [f"Tensioner_{i}" for i in range(1, 5)]
    for i, link in enumerate(links):
        vessel_end, riser_end = link[CONN]
        angle = math.radians(45 + 90 * i)
        assert vessel_end[0] == "TLP"
        assert vessel_end[1] == pytest.approx(3 * math.cos(angle), abs=1e-6)
        assert vessel_end[2] == pytest.approx(3 * math.sin(angle), abs=1e-6)
        assert vessel_end[3] == 20
        assert riser_end == ["TTR", 0, 0, 0, "End A"]
        # Hand value in the example header: L0 = 8.544 - 267.0 / 50 = 3.204 m.
        assert link["UnstretchedLength"] == pytest.approx(3.204, abs=0.001)
        assert link["Stiffness"] == 50
        assert link["LinearSpring"] == "Yes" and link["LinearDamper"] == "Yes"
        assert link["DampingConstant"] == 0


def test_target_vertical_tension_sums_to_total(example):
    spec = ProjectInputSpec(**example)
    units = RiserLinksBuilder.tensioner_geometry(spec.riser.tensioners[0], spec.riser)
    vertical = sum(u["tension"] * (u["z"] - 12) / u["length"] for u in units)
    assert vertical == pytest.approx(1000.0)


def test_total_tension_schema_explains_generated_geometry_target():
    """Expose the target and equilibrium distinction in the public schema."""
    description = RiserTensioner.model_json_schema()["properties"]["total_tension"][
        "description"
    ]
    assert "Target total vertical link tension in the generated geometry (kN)" in description
    assert "static value" in description
    assert "vertical tensioner stiffness" in description
    assert "top rise" in description


def test_vessel_heading_rotates_vessel_points(example):
    data = copy.deepcopy(example)
    data["riser"]["vessel"]["orientation"] = [0, 0, 90]
    data["riser"]["lines"][0]["end_a"]["position"] = [10, 0, 12]
    data["riser"]["tensioners"][0]["start_angle"] = 0
    first = _links(data)[0][CONN][0]
    # Riser top at global (10, 0) is (0, -10) in axes of a vessel heading 90 deg.
    assert first[1] == pytest.approx(3.0, abs=1e-6)
    assert first[2] == pytest.approx(-10.0, abs=1e-6)


def test_damping_emits_linear_damper(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["damping"] = 5
    link = _links(data)[0]
    assert link["LinearDamper"] == "Yes" and link["DampingConstant"] == 5


def test_spring_damper_link_data_names_match_native(example):
    data = copy.deepcopy(example)
    data["riser"]["links"] = [{
        "name": "Spring1", "link_type": "spring_damper",
        "connections": [
            {"object_name": "TLP", "x": 0, "y": 0, "z": 30},
            {"object_name": "TTR", "x": 0, "y": 0, "z": 0, "z_relative_to": "End A"},
        ],
        "unstretched_length": 5, "stiffness": 100,
    }]
    user_link = _links(data)[0]
    assert user_link["LinearSpring"] == "Yes" and user_link["LinearDamper"] == "Yes"
    assert user_link["DampingConstant"] == 0
    native = next(
        link for link in yaml.safe_load(NATIVE_LINKS.read_text(encoding="utf-8"))["Links"]
        # PyYAML (YAML 1.1) reads the native unquoted Yes as True.
        if link.get("LinkType") == "Spring/damper"
        and link.get("LinearSpring") in ("Yes", True)
        and link.get("DampingConstant") == 0
    )
    for link in (user_link, *_links(example)):
        assert link["LinearDamper"] in ("Yes", True)
        assert native["LinearDamper"] in ("Yes", True)
        assert link["DampingConstant"] == native["DampingConstant"]
    keys = set(user_link) - {"LinkType"}
    assert keys <= set(native)
    tensioner_keys = set(_links(example)[0]) - {"LinkType"}
    assert tensioner_keys <= set(native)


def test_tensioner_requires_free_end_a(example):
    data = copy.deepcopy(example)
    data["riser"]["lines"][0]["end_a"] = {"type": "vessel", "name": "TLP", "position": [0, 0, 12]}
    with pytest.raises(ValidationError, match="must be 'free'"):
        ProjectInputSpec(**data)


def test_tensioner_unknown_line_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["line"] = "Missing"
    with pytest.raises(ValidationError, match="unknown line"):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("arc_length", [0.4, 1.0])
def test_interior_tensioner_attachment_rejected(example, arc_length):
    """Only End A is supported, including on-node interior positions.

    At 0.4 m the 1 m top mesh snaps the attachment to node zero, which
    invalidates prestress computed from an exact 0.4 m vertical offset.
    """
    example["riser"]["tensioners"][0]["arc_length"] = arc_length
    with pytest.raises(ValidationError, match="supports End A only"):
        ProjectInputSpec(**example)


@pytest.mark.parametrize("link_name", ["Tensioner_1", "tensioner_1", "TENSIONER_4"])
def test_explicit_link_cannot_duplicate_generated_name(example, link_name):
    example["riser"]["links"] = [{
        "name": link_name, "link_type": "tether",
        "connections": [
            {"object_name": "Anchored", "x": 0, "y": 0, "z": 0},
            {"object_name": "TTR", "x": 0, "y": 0, "z": 0},
        ],
        "unstretched_length": 5, "stiffness": 100,
    }]
    with pytest.raises(ValidationError, match="Duplicate link or tensioner names"):
        ProjectInputSpec(**example)


def test_generated_link_names_are_case_insensitive(example):
    second = copy.deepcopy(example["riser"]["tensioners"][0])
    second["name"] = "tensioner"
    example["riser"]["tensioners"].append(second)
    with pytest.raises(ValidationError, match="Duplicate link or tensioner names"):
        ProjectInputSpec(**example)


def test_explicit_link_may_share_tensioner_prefix(example):
    example["riser"]["links"] = [{
        "name": "Tensioner", "link_type": "tether",
        "connections": [
            {"object_name": "Anchored", "x": 0, "y": 0, "z": 0},
            {"object_name": "TTR", "x": 0, "y": 0, "z": 0},
        ],
        "unstretched_length": 5, "stiffness": 100,
    }]
    names = [link["Name"] for link in _links(example)]
    assert names == ["Tensioner", *[f"Tensioner_{i}" for i in range(1, 5)]]


def test_vessel_point_below_riser_top_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["vessel_z"] = 10
    with pytest.raises(ValueError, match="must be above"):
        _links(data)


def test_too_soft_tensioner_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["stiffness"] = 20  # 267 kN / 20 kN/m > 8.544 m
    with pytest.raises(ValueError, match="too low"):
        _links(data)


FLOAT_FIELDS = [
    "total_tension", "stiffness", "damping", "radius", "vessel_z", "start_angle", "arc_length",
]


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("field", FLOAT_FIELDS)
def test_non_finite_tensioner_input_rejected(example, field, value):
    """NaN or infinite inputs would emit NaN coordinates and unstretched lengths."""
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0][field] = value
    with pytest.raises(ValidationError, match="finite number"):
        ProjectInputSpec(**data)


def test_yaml_nan_vessel_z_rejected():
    """The YAML ``.nan`` spelling reaches the schema as a float NaN."""
    text = EXAMPLE.read_text(encoding="utf-8")
    text, n = re.subn(r"(?m)^(\s*vessel_z:\s*)\S+", r"\g<1>.nan", text)
    assert n == 1
    with pytest.raises(ValidationError, match="finite number"):
        ProjectInputSpec(**yaml.safe_load(text))


@pytest.mark.parametrize(
    "path, index",
    [(("vessel", "position"), 0), (("vessel", "position"), 2),
     (("lines", 0, "end_a", "position"), 1), (("lines", 0, "end_a", "position"), 2)],
)
@pytest.mark.parametrize("value", [math.nan, math.inf])
def test_non_finite_tensioner_geometry_rejected(example, path, index, value):
    """Positions outside the tensioner schema must not yield non-finite links."""
    data = copy.deepcopy(example)
    target = data["riser"]
    for key in path:
        target = target[key]
    target[index] = value
    with pytest.raises(ValueError, match="non-finite"):
        _links(data)


def test_example_matches_golden(example, tmp_path):
    out = tmp_path / "out"
    ModularModelGenerator.from_spec(ProjectInputSpec(**example)).generate(out)
    includes = out / "includes"
    golden_files = sorted(p.name for p in GOLDEN.glob("*.yml"))
    assert golden_files == sorted(p.name for p in includes.glob("*.yml"))
    for name in golden_files:
        got = yaml.safe_load((includes / name).read_text(encoding="utf-8"))
        want = yaml.safe_load((GOLDEN / name).read_text(encoding="utf-8"))
        assert got == want, name


def test_example_material_stiffnesses_from_public_elastic_constants(example):
    """Recompute the stiffnesses using the material source cited in the header."""
    line_type = ProjectInputSpec(**example).riser.line_types[0]
    od, idia = line_type.outer_diameter, line_type.inner_diameter
    # SteelConstruction.info, Steel material properties, Other properties:
    # https://steelconstruction.info/topics/design/steel-material-properties/
    youngs_modulus, poisson_ratio = 210e6, 0.3  # kN/m2, dimensionless
    area = math.pi * (od**2 - idia**2) / 4
    inertia = math.pi * (od**4 - idia**4) / 64
    assert line_type.poisson_ratio == poisson_ratio
    assert line_type.axial_stiffness == pytest.approx(youngs_modulus * area, abs=0.5)
    assert line_type.bending_stiffness == pytest.approx(youngs_modulus * inertia, abs=0.5)
    shear_modulus = youngs_modulus / (2 * (1 + poisson_ratio))
    assert line_type.torsional_stiffness == pytest.approx(shear_modulus * 2 * inertia, abs=0.5)


def test_example_header_equilibrium_hand_check(example):
    """Recompute the example header's static estimate from the spec inputs.

    Effective-tension stretch of the riser balanced against the linearised
    tensioner vertical stiffness (axial plus geometric term). Arithmetic of
    the header only; it is not a licensed statics result.
    """
    g, rho = 9.80665, example["environment"]["water"]["density"]
    spec = ProjectInputSpec(**example).riser
    line_type, line = spec.line_types[0], spec.lines[0]
    tensioner = spec.tensioners[0]
    od, idia = line_type.outer_diameter, line_type.inner_diameter
    dry_weight = (line_type.mass_per_length
                  + line.contents.density * math.pi * idia**2 / 4) * g
    wet_weight = dry_weight - rho * math.pi * od**2 / 4 * g
    length = line.get_total_length()
    # The vertical line spans z = -500 to +12 m; only 500 m displaces seawater.
    dry_length = line.end_a.position[2]
    wet_length = length - dry_length
    assert (wet_length, dry_length) == (500, 12)
    weight = wet_weight * wet_length + dry_weight * dry_length
    assert wet_weight == pytest.approx(1.314, abs=0.001)
    assert dry_weight == pytest.approx(1.903, abs=0.001)
    assert weight == pytest.approx(679.7, abs=0.1)
    header_weight = re.search(
        r"Identity: top - bottom effective tension = W = ([\d.]+) kN",
        EXAMPLE.read_text(encoding="utf-8"),
    )
    assert header_weight is not None
    assert float(header_weight[1]) == pytest.approx(weight, abs=0.1)

    dz = tensioner.vessel_z - line.end_a.position[2]
    link = math.hypot(tensioner.radius, dz)
    unit = tensioner.total_tension / tensioner.count * link / dz
    k_vertical = tensioner.count * (
        tensioner.stiffness * (dz / link) ** 2 + unit / link * (tensioner.radius / link) ** 2
    )
    assert k_vertical == pytest.approx(190.8, abs=0.1)
    k_riser = line_type.axial_stiffness / length
    assert k_riser == pytest.approx(8107, abs=1)
    # Integrate the piecewise weight below the top to obtain the mean tension
    # loss. Stretch uses T_top minus that loss, at fixed initial wet/dry lengths.
    mean_loss = (dry_weight * (dry_length**2 / 2 + dry_length * wet_length)
                 + wet_weight * wet_length**2 / 2) / length
    t0 = tensioner.total_tension
    rise = (t0 - mean_loss) / (k_riser + k_vertical)
    top = t0 - k_vertical * rise
    assert rise == pytest.approx(0.079, abs=0.001)
    assert top == pytest.approx(984.9, abs=0.1)
    assert top - weight == pytest.approx(305.2, abs=0.1)
