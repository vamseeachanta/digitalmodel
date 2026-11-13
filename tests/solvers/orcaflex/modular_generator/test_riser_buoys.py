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
    assert upthrust == pytest.approx(2549.7, abs=0.1)


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


# Steel elastic constants behind the SCR line-type stiffness in the example.
# They are authored assumptions stated in the example header, not sourced values.
STEEL_E_KPA = 207e6
STEEL_POISSON = 0.3
STEEL_DENSITY = 7.85  # te/m3
ASSUMED_SCR_HORIZONTAL_KN = 150.0  # stated as assumed in the example header


def _jumper_catenary(w: float, span: float, drop: float, length: float) -> tuple[float, float]:
    """Inextensible catenary between two fixed ends, the lower end ``drop`` below.

    Returns (horizontal tension, vertical load the line applies at the lower end,
    positive downwards). Pure arithmetic, solved by bisection on the catenary
    parameter a = H / w from sqrt(L^2 - v^2) = 2 a sinh(span / 2a).
    """
    import math

    target = math.sqrt(length**2 - drop**2)
    lo, hi = 1e-3, 1e6
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        if 2 * mid * math.sinh(span / (2 * mid)) > target:
            lo = mid
        else:
            hi = mid
    a = 0.5 * (lo + hi)
    # Catenary parameter at the lower end, measured from the vertex.
    u_low = span / (2 * a) - math.atanh(drop / length)
    return w * a, w * a * math.sinh(u_low)


def _header_figures(data: dict) -> dict[str, float]:
    """Recompute every header figure of the example from the spec inputs only.

    Geometry (hang-off height, jumper ends, tether length, anchor distance),
    the buoy cylinder, line weights, stiffness and the jumper catenary are all
    derived from ``data``; no load is copied from the header. The only
    non-spec inputs are the stated assumptions above (steel E, Poisson ratio,
    density and the SCR horizontal tension).
    """
    import math

    g = 9.80665
    spec = ProjectInputSpec(**data)
    riser = spec.riser
    rho = spec.environment.water.density
    seabed_z = -spec.environment.water.depth
    buoy = riser.buoys[0]
    bx, by, bz = buoy.position
    types = {lt.name: lt for lt in riser.line_types}
    lines = {line.name: line for line in riser.lines}

    f: dict[str, float] = {}
    f["upthrust"] = (rho * buoy.volume - buoy.mass) * g
    f["buoy_diameter"] = 2 * math.sqrt(buoy.volume / (math.pi * buoy.height))
    # The header rounds the diameter to 9.21 m and derives the inertia from it.
    r = round(f["buoy_diameter"], 2) / 2
    f["cylinder_volume"] = math.pi * r**2 * buoy.height
    f["ixx"] = buoy.mass * (3 * r**2 + buoy.height**2) / 12
    f["izz"] = buoy.mass * r**2 / 2
    f["side_area"] = 2 * r * buoy.height
    f["end_area"] = math.pi * r**2

    scr = lines["SCR"]
    scr_type = types[scr.sections[0].line_type]
    od, idia = scr_type.outer_diameter, scr_type.inner_diameter
    area = math.pi * (od**2 - idia**2) / 4
    second_moment = math.pi * (od**4 - idia**4) / 64
    f["scr_mass"] = STEEL_DENSITY * area
    f["scr_ei"] = STEEL_E_KPA * second_moment
    f["scr_ea"] = STEEL_E_KPA * area
    f["scr_gj"] = STEEL_E_KPA / (2 * (1 + STEEL_POISSON)) * 2 * second_moment
    w_scr = (scr_type.mass_per_length + scr.contents.density * math.pi * idia**2 / 4
             - rho * math.pi * od**2 / 4) * g
    f["scr_w"] = w_scr
    height = bz + scr.end_a.position[2] - seabed_z
    f["scr_height"] = height
    horizontal = ASSUMED_SCR_HORIZONTAL_KN
    a = horizontal / w_scr
    suspended = math.sqrt(height**2 + 2 * height * a)
    length = sum(s.length for s in scr.sections)
    f["scr_suspended"] = suspended
    f["scr_vertical"] = w_scr * suspended
    f["scr_angle"] = math.degrees(math.atan2(horizontal, w_scr * suspended))
    f["scr_span"] = a * math.asinh(suspended / a)
    f["scr_grounded"] = length - suspended
    f["scr_anchor_distance"] = length - suspended + f["scr_span"]
    f["scr_anchor_offset"] = (bx + scr.end_a.position[0]) - scr.end_b.position[0]

    jumper = lines["Jumper"]
    j_type = types[jumper.sections[0].line_type]
    vessel = riser.vessel.position
    top = [vessel[k] + jumper.end_a.position[k] for k in range(3)]
    bottom = [buoy.position[k] + jumper.end_b.position[k] for k in range(3)]
    w_j = (j_type.mass_per_length
           + jumper.contents.density * math.pi * j_type.inner_diameter**2 / 4
           - rho * math.pi * j_type.outer_diameter**2 / 4) * g
    j_span = math.hypot(bottom[0] - top[0], bottom[1] - top[1])
    j_h, j_v = _jumper_catenary(w_j, j_span, top[2] - bottom[2],
                                sum(s.length for s in jumper.sections))
    f["jumper_w"] = w_j
    f["jumper_h"] = j_h
    f["jumper_vertical"] = j_v

    links = riser.links
    tether_sum = f["upthrust"] - f["scr_vertical"] - j_v
    f["tether_sum"] = tether_sum
    f["tether_each"] = tether_sum / len(links)
    link = links[0]
    buoy_end, anchor_end = link.connections
    f["tether_ea"] = 0.78e8 * 0.1**2
    tether_length = math.dist(
        (bx + buoy_end.x, by + buoy_end.y, bz + buoy_end.z),
        (anchor_end.x, anchor_end.y, seabed_z + anchor_end.z),
    )
    f["tether_length"] = tether_length
    f["tether_unstretched"] = tether_length / (1 + f["tether_each"] / link.stiffness)
    f["buoy_net_horizontal"] = horizontal - j_h
    lateral_stiffness = tether_sum / tether_length
    f["tether_lateral_stiffness"] = lateral_stiffness
    offset = f["buoy_net_horizontal"] / lateral_stiffness
    f["buoy_offset"] = offset
    f["buoy_drop"] = tether_length - math.sqrt(tether_length**2 - offset**2)
    return f


# (figure, text in the example header, tolerance). Tolerances are half a unit
# of the last digit written in the header; "about" figures carry their own.
HEADER_FIGURES = [
    ("upthrust", "2549.7", 0.05),
    ("buoy_diameter", "9.21", 0.005),
    ("cylinder_volume", "399.7", 0.05),
    ("ixx", "1245.2", 0.05),
    ("izz", "1590.5", 0.05),
    ("side_area", "55.3", 0.05),
    ("end_area", "66.6", 0.05),
    ("scr_mass", "0.1552", 0.00005),
    ("scr_ei", "31,709", 0.5),
    ("scr_ea", "4.091e6", 500),
    ("scr_gj", "24,392", 0.5),
    ("scr_w", "1.314", 0.0005),
    ("scr_height", "397", 0.5),
    ("scr_suspended", "498.3", 0.05),
    ("scr_vertical", "654.6", 0.05),
    ("scr_angle", "12.9", 0.05),
    ("scr_span", "248.8", 0.05),
    ("scr_grounded", "801.7", 0.05),
    ("scr_anchor_distance", "1050.6", 0.05),
    ("jumper_w", "1.305", 0.0005),
    ("jumper_h", "66.5", 0.05),
    ("jumper_vertical", "101.2", 0.05),
    ("tether_sum", "1793.9", 0.05),
    ("tether_each", "896.9", 0.05),
    ("tether_ea", "780,000", 0.5),
    ("tether_length", "397", 0.5),
    ("tether_unstretched", "396.544", 0.0005),
    ("buoy_net_horizontal", "83.5", 0.05),
    ("tether_lateral_stiffness", "about 4.5", 0.05),
    ("buoy_offset", "about 18.5", 0.05),
    ("buoy_drop", "about 0.4", 0.05),
]


def _header_text() -> str:
    lines = EXAMPLE.read_text(encoding="utf-8").splitlines()
    return " ".join(line.lstrip("# ").strip() for line in lines if line.startswith("#"))


@pytest.mark.parametrize(("key", "text", "tol"), HEADER_FIGURES, ids=[h[0] for h in HEADER_FIGURES])
def test_example_header_figure_reproduced_from_spec(example, key, text, tol):
    """Each header figure is recomputed from the spec inputs within its rounding.

    These are arithmetic checks of the example header (inextensible catenaries,
    solid uniform cylinder buoy), not a licensed statics result.
    """
    assert text in _header_text(), f"header no longer states {text!r} for {key}"
    stated = float(text.removeprefix("about ").replace(",", ""))
    assert _header_figures(example)[key] == pytest.approx(stated, abs=tol)


def test_example_spec_values_match_hand_checks(example):
    """Spec inputs that the header derives agree with the recomputed values."""
    figures = _header_figures(example)
    spec = ProjectInputSpec(**example).riser
    buoy = spec.buoys[0]
    assert buoy.moments_of_inertia == pytest.approx(
        [figures["ixx"], figures["ixx"], figures["izz"]], abs=0.05)
    assert buoy.drag_area == pytest.approx(
        [figures["side_area"], figures["side_area"], figures["end_area"]], abs=0.05)
    scr_type = next(lt for lt in spec.line_types if lt.name == "scr_10in")
    assert scr_type.mass_per_length == pytest.approx(figures["scr_mass"], abs=0.00005)
    assert scr_type.bending_stiffness == pytest.approx(figures["scr_ei"], abs=0.5)
    assert scr_type.axial_stiffness == pytest.approx(figures["scr_ea"], abs=500)
    assert scr_type.torsional_stiffness == pytest.approx(figures["scr_gj"], abs=0.5)
    assert scr_type.poisson_ratio == STEEL_POISSON
    assert figures["scr_anchor_offset"] == pytest.approx(figures["scr_anchor_distance"], abs=0.05)
    for link in spec.links:
        assert link.stiffness == pytest.approx(figures["tether_ea"])
        assert link.unstretched_length == pytest.approx(figures["tether_unstretched"], abs=5e-4)


@pytest.mark.parametrize(
    ("path", "factor", "changed"),
    [
        (("line_types", 1, "mass_per_length"), 1.1,
         ["jumper_w", "jumper_vertical", "tether_each"]),
        (("lines", 1, "sections", 0, "length"), 1.05,
         ["jumper_h", "jumper_vertical", "tether_each"]),
        (("lines", 1, "end_a", "position", 2), 2.0, ["jumper_vertical", "tether_each"]),
        (("buoys", 0, "position", 2), 1.1, ["scr_height", "scr_vertical", "tether_length"]),
        (("buoys", 0, "mass"), 1.1, ["upthrust", "tether_each"]),
    ],
    ids=["jumper_mass", "jumper_length", "jumper_top_z", "buoy_z", "buoy_mass"],
)
def test_hand_checks_follow_spec_inputs(example, path, factor, changed):
    """Changing a spec input changes the dependent recomputed header figures.

    Guards against a hand check that copies a header load instead of deriving it.
    """
    base = _header_figures(example)
    data = copy.deepcopy(example)
    node = data["riser"]
    for key in path[:-1]:
        node = node[key]
    node[path[-1]] = node[path[-1]] * factor
    mutated = _header_figures(data)
    for key in changed:
        assert mutated[key] != pytest.approx(base[key], rel=1e-4), key


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), float("-inf")])
@pytest.mark.parametrize(
    "field",
    ["position", "attitude", "moments_of_inertia", "centre_of_mass", "centre_of_volume",
     "drag_area", "drag_coefficient", "added_mass_coefficient"],
)
def test_buoy_vector_inputs_must_be_finite(example, field, bad):
    data = copy.deepcopy(example)
    data["riser"]["buoys"][0][field] = [bad, 1.0, 1.0]
    with pytest.raises(ValidationError, match=field):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("bad", [float("nan"), float("inf")])
@pytest.mark.parametrize("field", ["mass", "volume", "height"])
def test_buoy_scalar_inputs_must_be_finite(example, field, bad):
    data = copy.deepcopy(example)
    data["riser"]["buoys"][0][field] = bad
    with pytest.raises(ValidationError, match=field):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("bad", [float("nan"), float("inf")])
def test_buoy_vector_validators_reject_non_finite_directly(bad):
    """The vector validators themselves reject non-finite components."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema.riser import RiserBuoy

    with pytest.raises(ValueError, match="finite"):
        RiserBuoy.validate_vector3([bad, 1.0, 1.0])
    with pytest.raises(ValueError, match="finite"):
        RiserBuoy.validate_non_negative([bad, 1.0, 1.0])
