"""J-lay tower installation (#3978).

The generator supported S-lay and floating installation only. ``equipment.jlay``
pins the pipe to the vessel at the tower top and seeds End B on the seabed at
the inextensible-catenary span for the hang-off angle. Comparators are the hand
values recorded in the example header. No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
import math
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder
from digitalmodel.solvers.orcaflex.modular_generator.builders.linetype_builder import LineTypeBuilder
from digitalmodel.solvers.orcaflex.modular_generator.builders.vardata_builder import VarDataBuilder
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/pipeline_jlay/spec.yml"
GOLDEN = Path(__file__).parent / "goldens/typed/pipeline_jlay"
CONN = (
    "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
    "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"
)


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _builder(data: dict) -> LinesBuilder:
    return LinesBuilder(ProjectInputSpec(**data), BuilderContext())


def _pipeline(data: dict) -> dict:
    return next(line for line in _builder(data).build()["Lines"] if line["Name"] == "Pipeline")


def test_seed_matches_hand_catenary(example):
    # Hand balance: Q = 0.6805*1000 + 1.5398*30; H = Q/(csc(8)-1).
    # Ttop = H + Q = 844.2 kN; adding wet/dry spans and L-s gives 2263.6 m.
    # Classical Mechanics, online edition accessed 2026-11-13, section 18.3,
    # equations 18.3.2-18.3.9, applied on each side of the waterline.
    seed = _builder(example).jlay_seed()
    assert seed["weight"] == pytest.approx(0.6805, abs=1e-4)
    assert seed["dry_weight"] == pytest.approx(1.5398, abs=1e-4)
    assert seed["a"] == pytest.approx(172.65, abs=0.01)
    assert seed["suspended"] == pytest.approx(1190.18, abs=0.01)
    assert seed["suspended_span"] == pytest.approx(453.82, abs=0.01)
    assert seed["span"] == pytest.approx(2263.637, abs=0.001)
    assert seed["top_tension"] == pytest.approx(844.2, abs=0.1)
    assert seed["horizontal_tension"] == pytest.approx(117.5, abs=0.1)


def test_end_a_pinned_at_tower_top_and_end_b_aft(example):
    line = _pipeline(example)
    end_a, end_b = line[CONN]
    assert end_a[:4] == ["LayVessel", -20, 0, 30]
    assert end_a[4:6] == [180, 172]
    assert end_b[0] == "Anchored"
    assert end_b[1] == pytest.approx(-20 - 2263.637, abs=0.01)
    assert end_b[3] == pytest.approx(0.3299 / 2)
    assert line["ConnectionxBendingStiffness, ConnectionyBendingStiffness"] == [[0, None], [0, None]]


def test_vessel_heading_rotates_lay_direction(example):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"]["orientation"] = [0, 0, 90]
    end_a, end_b = _pipeline(data)[CONN]
    # Heading 90 deg: vessel -x points to global -y; the hang-off at vessel
    # (-20, 0) is at global (0, -20).
    assert end_a[1:3] == [-20, 0]  # vessel axes
    assert end_b[1] == pytest.approx(0.0, abs=1e-6)
    assert end_b[2] == pytest.approx(-20 - 2263.637, abs=0.01)


@pytest.mark.parametrize(
    ("change", "message"),
    [
        (lambda d: d["equipment"].pop("vessel"), "requires a vessel"),
        (lambda d: d["equipment"].update(stinger={"radius": 100, "sections": [{"length": 10, "bend_radius": 100}]}),
         "cannot both be defined"),
        (lambda d: d["equipment"].update(tensioner={"capacity_kn": 2000, "tension_value": 800}),
         "tensioner cannot also be defined"),
    ],
)
def test_invalid_jlay_equipment_rejected(example, change, message):
    data = copy.deepcopy(example)
    change(data)
    with pytest.raises(ValidationError, match=message):
        ProjectInputSpec(**data)


def test_pipe_too_short_for_grounded_remainder_rejected(example):
    data = copy.deepcopy(example)
    data["pipeline"]["segments"][0]["length"] = 1000
    with pytest.raises(ValueError, match="grounded remainder"):
        _builder(data).jlay_seed()


def test_tower_angle_bounds(example):
    for angle in (0, 45):
        data = copy.deepcopy(example)
        data["equipment"]["jlay"]["tower_angle"] = angle
        with pytest.raises(ValidationError):
            ProjectInputSpec(**data)


def _numeric_yaml(value):
    """Compare rebuilt numbers by tolerance while keeping structural checks."""
    if isinstance(value, dict):
        return {key: _numeric_yaml(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_numeric_yaml(item) for item in value]
    if isinstance(value, float):
        return pytest.approx(value, rel=1e-10, abs=1e-10)
    return value


def test_example_matches_golden(example, tmp_path):
    out = tmp_path / "out"
    ModularModelGenerator.from_spec(ProjectInputSpec(**example)).generate(out)
    includes = out / "includes"
    golden_files = sorted(p.name for p in GOLDEN.glob("*.yml"))
    assert golden_files == sorted(p.name for p in includes.glob("*.yml"))
    for name in golden_files:
        got = yaml.safe_load((includes / name).read_text(encoding="utf-8"))
        want = yaml.safe_load((GOLDEN / name).read_text(encoding="utf-8"))
        assert got == _numeric_yaml(want), (
            f"{name}: rebuild with python -m "
            "digitalmodel.solvers.orcaflex.modular_generator generate "
            "--input docs/domains/orcaflex/examples/modular/typed/pipeline_jlay/spec.yml "
            "--output <output>"
        )


def test_pipeline_contents_stated_empty_as_seeded(example):
    """The seed takes the pipe empty, so the emitted line states empty contents.

    Without these data the loaded model would take the solver's default
    contents, which the offline seed cannot confirm.
    """
    line = _pipeline(example)
    assert line["ContentsMethod"] == "Uniform"
    assert line["ContentsDensity"] == 0


@pytest.mark.parametrize("suffix", ["CWC80", "CWC120", None])
@pytest.mark.parametrize("reverse", [False, True])
def test_seed_uses_only_emitted_coating_layers(example, suffix, reverse):
    data = copy.deepcopy(example)
    variants = [
        {"name": "CWC80", "thickness": 0.08, "density": 3.04},
        {"name": "CWC120", "thickness": 0.12, "density": 3.04},
    ]
    data["pipeline"]["coatings"]["weight"] = variants[::-1] if reverse else variants
    if suffix:
        data["pipeline"]["segments"][0]["type"] = f"X65+coating+{suffix}"
    spec = ProjectInputSpec(**data)
    context = BuilderContext()
    coatings = VarDataBuilder(spec, context).build()["VariableData"]["Coatingsorlinings"]
    line_type = LineTypeBuilder(spec, context).build()["LineTypes"][0]
    layers = next(c["LayerThickness, LayerMaterialDensity"] for c in coatings
                  if c["Name"] == line_type["CoatingThickness"])
    od = line_type["OD"]
    mass = line_type["MaterialDensity"] * math.pi / 4 * (od**2 - line_type["ID"]**2)
    for thickness, density in layers:
        outer = od + 2 * thickness
        mass += density * math.pi / 4 * (outer**2 - od**2)
        od = outer
    expected_weight = (mass - spec.environment.water.density * math.pi / 4 * od**2) * 9.80665
    seed = _builder(data).jlay_seed()
    assert seed["outer_diameter"] == pytest.approx(od)
    assert seed["weight"] == pytest.approx(expected_weight)
    assert seed["dry_weight"] == pytest.approx(mass * 9.80665)
    assert seed["top_tension"] == pytest.approx(
        seed["horizontal_tension"] + expected_weight * 1000 + seed["dry_weight"] * 30
    )
    assert _pipeline(data)[CONN][1][3] == pytest.approx(od / 2)
    if suffix == "CWC80":
        assert seed["outer_diameter"] == pytest.approx(0.4899)
        assert seed["weight"] == pytest.approx(2.7162, abs=1e-4)


def test_unused_lightweight_coating_does_not_reject_seed(example):
    data = copy.deepcopy(example)
    data["pipeline"]["coatings"]["weight"] = [
        {"name": "lightweight", "thickness": 0.5, "density": 0.5},
    ]
    assert _builder(data).jlay_seed() == _builder(example).jlay_seed()
    assert _pipeline(data) == _pipeline(example)


@pytest.mark.parametrize("second_suffix", ["CWC80", "CWC120", None])
def test_seed_requires_uniform_segment_coatings(example, second_suffix):
    data = copy.deepcopy(example)
    data["pipeline"]["coatings"]["weight"] = [
        {"name": "CWC80", "thickness": 0.08, "density": 3.04},
        {"name": "CWC120", "thickness": 0.12, "density": 3.04},
    ]
    data["pipeline"]["segments"] = [
        {"type": "X65+first+CWC80", "length": 1500, "segment_length": 5},
        {"type": f"X65+second+{second_suffix}" if second_suffix else "X65_12in",
         "length": 1500, "segment_length": 5},
    ]
    if second_suffix == "CWC80":
        assert _builder(data).jlay_seed()["weight"] == pytest.approx(2.7162, abs=1e-4)
    else:
        with pytest.raises(ValueError, match="uniform segment coatings"):
            _builder(data).jlay_seed()
        with pytest.raises(ValueError, match="uniform segment coatings"):
            _pipeline(data)


def test_seed_rejects_unresolved_coating_reference(example):
    data = copy.deepcopy(example)
    data["pipeline"]["segments"][0]["type"] = "X65+coating+missing"
    with pytest.raises(ValueError, match="unresolved coating"):
        _builder(data).jlay_seed()


@pytest.mark.parametrize("slope", [-30, 0.1, 30])
def test_jlay_rejects_sloping_seabed(example, slope):
    data = copy.deepcopy(example)
    data["environment"]["seabed"]["slope"] = slope
    with pytest.raises(ValueError, match="flat seabed"):
        _builder(data).jlay_seed()
    with pytest.raises(ValueError, match="flat seabed"):
        _pipeline(data)


@pytest.mark.parametrize("component", range(3), ids=["x", "y", "z"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf], ids=["nan", "inf", "negative-inf"])
def test_tower_top_rejects_nonfinite_components(example, component, value):
    data = copy.deepcopy(example)
    data["equipment"]["jlay"]["tower_top"][component] = value
    with pytest.raises(ValidationError, match="finite"):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("elevation", [-30, 0])
def test_submerged_hangoff_retains_uniform_wet_catenary(example, elevation):
    data = copy.deepcopy(example)
    data["equipment"]["jlay"]["tower_top"][2] = elevation
    seed = _builder(data).jlay_seed()
    phi = math.radians(82)
    a = (1000 + elevation) / (1 / math.cos(phi) - 1)
    assert seed["a"] == pytest.approx(a)
    assert seed["suspended"] == pytest.approx(a * math.tan(phi))
    assert seed["suspended_span"] == pytest.approx(a * math.asinh(math.tan(phi)))
    assert seed["top_tension"] == pytest.approx(seed["weight"] * (a + 1000 + elevation))


def test_wet_dry_seed_closes_length_and_departure_angle(example):
    seed = _builder(example).jlay_seed()
    horizontal = seed["horizontal_tension"]
    wet_vertical = math.sqrt((horizontal + seed["weight"] * 1000)**2 - horizontal**2)
    top_vertical = math.sqrt(seed["top_tension"]**2 - horizontal**2)
    wet_length = wet_vertical / seed["weight"]
    dry_length = (top_vertical - wet_vertical) / seed["dry_weight"]
    assert seed["suspended"] == pytest.approx(wet_length + dry_length)
    assert math.degrees(math.atan2(horizontal, top_vertical)) == pytest.approx(8)
    assert seed["span"] == pytest.approx(seed["suspended_span"] + 3000 - wet_length - dry_length)


@pytest.mark.parametrize("volume", [2.0, 4.0])
@pytest.mark.parametrize("zoned", [False, True])
def test_jlay_rejects_buoyancy_modules(example, volume, zoned):
    data = copy.deepcopy(example)
    modules = {"spacing": 50, "properties": {"mass": 0.05, "volume": volume}}
    if zoned:
        modules.update(start_arc_length=100, end_arc_length=500)
    data["equipment"]["buoyancy_modules"] = modules
    with pytest.raises(ValidationError, match="J-lay.*buoyancy modules"):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("anchor", [-500, 0, 500])
def test_jlay_rejects_slay_anchor_override(example, anchor):
    data = copy.deepcopy(example)
    data["equipment"]["slay_end_b_x"] = anchor
    with pytest.raises(ValidationError, match="J-lay.*slay_end_b_x"):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("field", ["position", "orientation"])
@pytest.mark.parametrize("component", range(3), ids=["x", "y", "z"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf], ids=["nan", "inf", "negative-inf"])
def test_jlay_rejects_nonfinite_vessel_geometry(example, field, component, value):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"].setdefault(field, [0, 0, 0])[component] = value
    with pytest.raises(ValueError, match="finite"):
        _builder(data).jlay_seed()
    with pytest.raises(ValueError, match="finite"):
        _pipeline(data)


@pytest.mark.parametrize("field", ["position", "orientation"])
def test_generation_rejects_nonfinite_vessel_geometry(example, tmp_path, field):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"].setdefault(field, [0, 0, 0])[2] = math.nan
    generator = ModularModelGenerator.from_spec(ProjectInputSpec(**data))
    with pytest.raises(ValueError, match="finite"):
        generator.generate(tmp_path / "output")
    assert not (tmp_path / "output").exists()


def test_jlay_rejects_overflowed_height(example):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"]["position"][2] = 1.3e308
    data["equipment"]["jlay"]["tower_top"][2] = 1.3e308
    with pytest.raises(ValueError, match="finite"):
        _builder(data).jlay_seed()


def test_jlay_rejects_overflowed_global_endpoint(example):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"]["position"] = [1.3e308, 1.3e308, 0]
    data["equipment"]["vessel"]["orientation"] = [0, 0, 45]
    data["equipment"]["jlay"]["tower_top"][0] = 1.3e308
    with pytest.raises(ValueError, match="finite"):
        _pipeline(data)


def test_finite_vessel_translation_preserves_seed(example):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"]["position"] = [100, 200, 5]
    seed = _builder(data).jlay_seed()
    end_b = _pipeline(data)[CONN][1]
    assert seed["height"] == 1035
    assert end_b[1] == pytest.approx(100 - 20 - seed["span"])
    assert end_b[2] == 200
