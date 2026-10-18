# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the generic steel-riser basis and its solver-free calibration inputs."""

import importlib.util
import json
import math
import re
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml
from scipy.optimize import brentq

from digitalmodel.orcaflex.riser_config import RiserPipeProperties
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
)
from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / "docs/domains/orcaflex/library/campaigns/generic-scr-slwr"


def load_basis():
    """Read the authoritative parameter records."""
    return yaml.safe_load((CAMPAIGN / "basis.yml").read_text())


def load_check():
    """Load the analytical reproducer without any solver interface."""
    path = ROOT / "scripts/orcaflex/check_generic_scr_slwr.py"
    spec = importlib.util.spec_from_file_location("generic_static_check", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_basis_has_provenance_for_every_parameter():
    """Missing sources and unexplained assumptions prevent basis qualification."""
    basis = load_basis()
    assert basis["readiness"] == "prepared; not qualified for design or outward use"
    assert basis["host"]["redistribution_status"] == "NO_VESSEL_TYPE_DATA_COMMITTED"
    for name, record in basis["parameters"].items():
        assert record["unit"], name
        assert record["source"] in basis["sources"], name
        assert record["status"] in ("SOURCED", "ASSUMED"), name
        if record["status"] == "ASSUMED":
            assert record["reason"], name
    assert all(c["code"] and c["title"] for c in basis["criteria"])
    assert all(c["limit"] is None for c in basis["criteria"])


@pytest.mark.parametrize("kind", ["scr", "slwr"])
def test_specs_match_basis_and_static_geometry(kind):
    """Pipe, contents, buoyancy, endpoints and section lengths share one basis."""
    basis = load_basis()
    p = {name: r["value"] for name, r in basis["parameters"].items()}
    spec = yaml.safe_load((CAMPAIGN / kind / "spec.yml").read_text())
    reference = load_check().calculate(basis)[kind]
    bottom = reference["seabed_offset_sweep"]["selected_bottom_length_m"]
    pipe = RiserPipeProperties(**{k: p[k] for k in (
        "outer_diameter", "wall_thickness", "coating_thickness",
        "coating_density", "contents_density", "corrosion_allowance",
        "seawater_density",
    )})
    assert spec["environment"]["water"]["depth"] == p["water_depth"]
    assert spec["environment"]["water"]["density"] == p["seawater_density"] / 1000
    assert spec["environment"]["current"]["speed"] == p["static_current_speed"]
    assert spec["environment"]["waves"]["height"] == p["extreme_hs"]
    assert spec["environment"]["waves"]["period"] == p["extreme_tp"]
    host = spec["riser"]["vessel"]
    assert host["type_name"] == basis["host"]["type_name"]
    assert host["length"] == p["host_length"]
    assert host["primary_motion"] == host["superimposed_motion"] == "None"
    line = spec["riser"]["lines"][0]
    assert line["contents"]["density"] == p["contents_density"] / 1000
    assert line["end_a"]["position"] == [0, 0, -p["hangoff_depth"]]
    assert line["end_a"]["declination"] == 180 - p[kind + "_angle"]
    assert line["end_a"]["bending_stiffness"] == p["joint_stiffness"]
    assert line["end_b"]["position"][0] == pytest.approx(
        reference["oo"]["touchdown_horizontal_distance_m"] + bottom
    )
    assert line["end_b"]["position"][2] == pipe.total_od / 2
    assert sum(s["length"] for s in line["sections"]) == pytest.approx(
        reference["oo"]["suspended_length_m"] + bottom
    )
    bare = spec["riser"]["line_types"][0]
    assert bare["outer_diameter"] == pipe.total_od
    assert bare["inner_diameter"] == pipe.inner_diameter
    assert bare["mass_per_length"] == pytest.approx(
        (pipe.steel_mass_per_m + pipe.coating_mass_per_m) / 1000
    )
    assert bare["axial_stiffness"] == pytest.approx(pipe.axial_stiffness / 1000)
    assert bare["bending_stiffness"] == pytest.approx(pipe.bending_stiffness / 1000)
    if kind == "slwr":
        floats = spec["riser"]["line_types"][1]
        volume = reference["buoyancy"]["volume_per_m_m2"]
        assert floats["outer_diameter"] == pytest.approx(reference["buoyancy"]["effective_od_m"])
        assert floats["mass_per_length"] == pytest.approx(
            bare["mass_per_length"] + volume * p["module_density"] / 1000
        )
        assert line["sections"][1]["length"] == pytest.approx(reference["sections_m"][1])


@pytest.mark.parametrize("kind", ["scr", "slwr"])
def test_models_validate_and_reproduce(kind, tmp_path):
    """Fresh and previously loaded processes reproduce the model's YAML values."""
    model = CAMPAIGN / kind / "modular"
    result = OrcaFlexYAMLValidator().validate_file(model / "master.yml")
    assert result.valid, result.errors
    assert not PostGenerationValidator().validate_directory(model)
    environment = yaml.safe_load((model / "includes/03_environment.yml").read_text())
    train = environment["Environment"]["WaveTrains"][0]
    assert train["WaveTp"] == load_basis()["parameters"]["extreme_tp"]["value"]
    assert "WaveTz" not in train
    generated = tmp_path / kind
    command = (
        "import sys; "
        "from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator; "
        "ModularModelGenerator(sys.argv[1]).generate(sys.argv[2])"
    )
    subprocess.run([sys.executable, "-c", command,
                    str(CAMPAIGN / kind / "spec.yml"), str(generated)], check=True)
    expected = {p.relative_to(model) for p in model.rglob("*.yml")}
    assert expected == {p.relative_to(generated) for p in generated.rglob("*.yml")}
    loaded = tmp_path / (kind + "-loaded")
    # A prior import may register PyYAML's incompatible null spelling globally.
    prior_import = (
        "import yaml; "
        "yaml.add_representer(type(None), lambda dumper, value: "
        "dumper.represent_scalar('tag:yaml.org,2002:null', 'null')); "
    )
    subprocess.run([sys.executable, "-c", prior_import + command,
                    str(CAMPAIGN / kind / "spec.yml"), str(loaded)], check=True)
    assert expected == {p.relative_to(loaded) for p in loaded.rglob("*.yml")}
    for path in expected:
        retained = (model / path).read_bytes()
        for directory in (generated, loaded):
            raw = (directory / path).read_bytes()
            assert yaml.safe_load(retained) == yaml.safe_load(raw), path
            assert not re.search(rb"(?<!\w)null(?!\w)", raw), path
            if path == Path("master.yml"):
                # The master is written directly, without a YAML dumper.
                # The EOF hook adds its missing final newline.
                canonical = raw if raw.endswith(b"\n") else raw + b"\n"
                assert retained == canonical


def assert_reference_equal(actual, expected):
    """Keep structure and non-float leaves exact; tolerate float roundoff."""
    if isinstance(expected, float):
        assert isinstance(actual, float)
        # The floor is about 20 * (100 * 2 * eps), the percentage-point
        # cancellation bound for the difference leaves. With rel=1e-12,
        # relative tolerance governs above magnitude 1 (abs / rel).
        assert actual == pytest.approx(expected, rel=1e-12, abs=1e-12)
        return
    assert type(actual) is type(expected)
    if isinstance(expected, dict):
        assert {(type(key), key) for key in actual} == {
            (type(key), key) for key in expected
        }
        for key in expected:
            assert_reference_equal(actual[key], expected[key])
    elif isinstance(expected, (list, tuple)):
        assert len(actual) == len(expected)
        for actual_item, expected_item in zip(actual, expected):
            assert_reference_equal(actual_item, expected_item)
    else:
        assert actual == expected


def test_reference_comparison_preserves_structure_and_float_precision():
    """Only last-bit float differences may pass the reference comparison."""
    expected = {"result": [8.003624637684743, {"status": "converged", "count": 2}]}
    actual = {"result": [8.00362463768474, {"status": "converged", "count": 2}]}
    assert_reference_equal(actual, expected)
    observed_pairs = (
        ("hangoff_angle_deg", 8.00362463768474, 8.003624637684743),
        ("top_tension_kN", 1988.952981550418, 1988.9529815504172),
        ("top_tension_difference_pct", 0.0072850312434757925, 0.0072850312434313835),
    )
    for key, retained, reproduced in observed_pairs:
        assert_reference_equal({key: reproduced}, {key: retained})
        with pytest.raises(AssertionError):
            assert_reference_equal({key: retained * (1 + 1e-9)}, {key: retained})
    difference_pct = observed_pairs[-1][1]
    with pytest.raises(AssertionError):
        assert_reference_equal(
            {"top_tension_difference_pct": difference_pct + 1e-7},
            {"top_tension_difference_pct": difference_pct},
        )
    for changed in (
        {"result": [expected["result"][0] * (1 + 1e-9), expected["result"][1]]},
        {"result": [actual["result"][0], {"status": "changed", "count": 2}]},
        {"result": [actual["result"][0], {"status": "converged", "count": 3}]},
        {"changed": actual["result"]},
        {"result": actual["result"][:1]},
        {"result": [actual["result"][0], {"status": "converged", "count": 2.0}]},
        {"result": tuple(actual["result"])},
    ):
        with pytest.raises(AssertionError):
            assert_reference_equal(changed, expected)
    with pytest.raises(AssertionError):
        assert_reference_equal({True: "same"}, {1: "same"})
    with pytest.raises(AssertionError):
        assert_reference_equal(2, 2.0)


def test_analytical_reference_reproduces_from_basis():
    """Both kernel results and model digests are checked against retained bytes."""
    expected = json.loads((CAMPAIGN / "static-reference.json").read_text())
    assert_reference_equal(load_check().reference(CAMPAIGN), expected)
    for kind in ("scr", "slwr"):
        result = expected["models"][kind]
        assert result["oo"]["top_tension_kN"] > 0
        assert result["general"]["top_tension_kN"] > 0
        assert result["top_tension_difference_pct"] <= 2
        assert result["general"]["converged"]


def test_flooded_weight_and_module_net_uplift():
    """The operating/flooded distinction and upward buoyed weight are explicit."""
    result = load_check().calculate(load_basis())
    assert result["slwr"]["buoyancy"]["net_weight_N_per_m"] < 0
    assert result["flooded_weight_N_per_m"] > result["operating_weight_N_per_m"]


def hand_grounded_length(height, length, span):
    """Solve flat-seabed closure independently of the preparation helper.

    Negative reserve describes loss of the assumed touchdown solution, rather
    than a physical negative length of pipe on the seabed.
    """
    def closure(radius):
        suspended = math.sqrt(height * (height + 2 * radius))
        horizontal = radius * math.acosh(1 + height / radius)
        return horizontal - suspended - (span - length)

    radius = brentq(closure, height * 1e-9, height * 1e6)
    return length - math.sqrt(height * (height + 2 * radius))


@pytest.mark.parametrize("offset_pct", [-10, -5, 0, 5, 10])
def test_scr_retains_grounded_line_across_offsets(offset_pct):
    """Committed geometry shall retain touchdown throughout the offset sweep."""
    basis = load_basis()
    p = {name: record["value"] for name, record in basis["parameters"].items()}
    nominal = load_check().calculate(basis)["scr"]["oo"]
    line = yaml.safe_load((CAMPAIGN / "scr/spec.yml").read_text())["riser"]["lines"][0]
    height = (p["water_depth"] - p["hangoff_depth"]
              - (p["outer_diameter"] + 2 * p["coating_thickness"]) / 2)
    offset = offset_pct * p["water_depth"] / 100
    length = sum(section["length"] for section in line["sections"])
    reserve = hand_grounded_length(height, length, line["end_b"]["position"][0] - offset)
    assert reserve > 0, f"No grounded reserve at {offset_pct}%: {reserve:.1f} m"
    old_reserve = hand_grounded_length(
        height, nominal["suspended_length_m"] + p["bottom_length"],
        nominal["touchdown_horizontal_distance_m"] + p["bottom_length"] - offset,
    )
    assert (old_reserve > 0) == (offset_pct >= 0)


def test_scr_seabed_design_matches_independent_closure():
    """The selected length shall cover the derived requirement and margin."""
    basis = load_basis()
    p = {name: record["value"] for name, record in basis["parameters"].items()}
    result = load_check().calculate(basis)
    nominal = result["scr"]["oo"]
    design = result["scr"]["seabed_offset_sweep"]
    height = (p["water_depth"] - p["hangoff_depth"]
              - (p["outer_diameter"] + 2 * p["coating_thickness"]) / 2)
    selected = design["selected_bottom_length_m"]
    required = design["required_bottom_length_m"]
    assert required > p["bottom_length"]
    assert required < selected <= design["requested_bottom_length_m"]
    assert selected <= design["friction_bottom_limit_m"]
    for key, bottom in (("original", p["bottom_length"]), ("selected", selected)):
        length = nominal["suspended_length_m"] + bottom
        anchor = nominal["touchdown_horizontal_distance_m"] + bottom
        for state in design[key]:
            offset = state["offset_pct"] * p["water_depth"] / 100
            expected = hand_grounded_length(height, length, anchor - offset)
            assert state["grounded_length_m"] == pytest.approx(expected, abs=1e-7)
            assert state["touchdown_exists"] == (expected > 0)
            if key == "selected":
                assert expected >= design["minimum_grounded_length_m"] - 1e-7
    old_length = nominal["suspended_length_m"] + p["bottom_length"]
    old_anchor = nominal["touchdown_horizontal_distance_m"] + p["bottom_length"]
    limit = design["original_liftoff_far_offset_m"]
    assert hand_grounded_length(height, old_length, old_anchor + limit) == pytest.approx(0, abs=1e-7)
    assert design["original_liftoff_far_offset_pct"] == pytest.approx(limit / p["water_depth"] * 100)


def test_scr_regeneration_is_deterministic_and_preserves_lazy_wave(tmp_path):
    """A changed SCR margin shall regenerate only SCR geometry, reproducibly."""
    campaign = tmp_path / "campaign"
    shutil.copytree(CAMPAIGN, campaign)
    lazy = {p.relative_to(campaign): p.read_bytes() for p in (campaign / "slwr").rglob("*") if p.is_file()}
    basis = load_basis()
    basis["parameters"]["scr_seabed_margin"]["value"] += basis["parameters"]["mesh_coarse"]["value"]
    (campaign / "basis.yml").write_text(yaml.safe_dump(basis, sort_keys=False))
    check = load_check()
    check.regenerate_scr(campaign)
    first = {p.relative_to(campaign): p.read_bytes() for p in campaign.rglob("*") if p.is_file()}
    check.regenerate_scr(campaign)
    second = {p.relative_to(campaign): p.read_bytes() for p in campaign.rglob("*") if p.is_file()}
    assert first == second
    assert all(second[path] == raw for path, raw in lazy.items())
    line = yaml.safe_load((campaign / "scr/spec.yml").read_text())["riser"]["lines"][0]
    result = check.calculate(basis)["scr"]
    assert line["sections"][-1]["length"] == result["seabed_offset_sweep"]["selected_bottom_length_m"]
    assert line["end_b"]["position"][0] == pytest.approx(
        result["oo"]["touchdown_horizontal_distance_m"] + line["sections"][-1]["length"]
    )


def hand_lazy_state(basis, line, types, offset_pct):
    """Integrate force directions independently and solve both endpoint closures."""
    from scipy.integrate import quad
    from scipy.optimize import root

    p = {name: record["value"] for name, record in basis["parameters"].items()}
    weights = [(t["mass_per_length"] * 1000
                + p["contents_density"] * math.pi * t["inner_diameter"] ** 2 / 4
                - p["seawater_density"] * math.pi * t["outer_diameter"] ** 2 / 4)
               * p["gravity"] for t in types]
    lengths = [section["length"] for section in line["sections"]]
    height = p["water_depth"] - p["hangoff_depth"] - types[0]["outer_diameter"] / 2
    span = line["end_b"]["position"][0] - offset_pct * p["water_depth"] / 100

    def integrate(log_h, third):
        horizontal = math.exp(log_h)
        x = y = vertical = 0.0
        sag = None
        for index, (length, weight) in enumerate([
            (third, weights[0]), (lengths[1], weights[1]), (lengths[0], weights[0]),
        ]):
            def dy(arc):
                return (vertical + weight * arc) / math.hypot(horizontal, vertical + weight * arc)

            if index == 2:
                sag = y + quad(dy, 0, -vertical / weight)[0]
            x += quad(lambda arc: horizontal / math.hypot(horizontal, vertical + weight * arc), 0, length)[0]
            y += quad(dy, 0, length)[0]
            vertical += weight * length
        return x, y, horizontal, vertical, sag

    def closure(unknowns):
        x, y, *_ = integrate(*unknowns)
        grounded = sum(lengths) - lengths[0] - lengths[1] - unknowns[1]
        return x + grounded - span, y - height

    vertical = weights[0] * (lengths[0] + lengths[2]) + weights[1] * lengths[1]
    guess = vertical * math.tan(math.radians(180 - line["end_a"]["declination"]))
    solved = root(closure, [math.log(guess), lengths[2]], tol=1e-10)
    assert solved.success, solved.message
    assert max(abs(v) for v in closure(solved.x)) < 1e-7
    x, y, horizontal, vertical, sag = integrate(*solved.x)
    return {
        "grounded_length_m": sum(lengths) - lengths[0] - lengths[1] - solved.x[1],
        "horizontal_tension_kN": horizontal / 1000,
        "top_tension_kN": math.hypot(horizontal, vertical) / 1000,
        "sag_above_seabed_m": sag,
    }


def lazy_offsets():
    """Select the complete envelope from the retained basis."""
    p = load_basis()["parameters"]
    return sorted({0, *[sign * p[name]["value"] for sign in (-1, 1)
                        for name in ("intact_offset_pct", "damaged_offset_pct")]})


@pytest.mark.parametrize("offset_pct", lazy_offsets())
def test_slwr_retains_grounded_reserve_across_offsets(offset_pct):
    """Committed lazy-wave geometry shall retain the assumed reserve."""
    basis = load_basis()
    riser = yaml.safe_load((CAMPAIGN / "slwr/spec.yml").read_text())["riser"]
    state = hand_lazy_state(basis, riser["lines"][0], riser["line_types"], offset_pct)
    margin = basis["parameters"]["slwr_seabed_margin"]["value"]
    assert state["grounded_length_m"] >= margin - 1e-7


def test_slwr_seabed_design_matches_independent_closure():
    """The three-section sweep shall close and round the selected bottom upward."""
    basis = load_basis()
    result = load_check().calculate(basis)["slwr"]
    design = result["seabed_offset_sweep"]
    riser = yaml.safe_load((CAMPAIGN / "slwr/spec.yml").read_text())["riser"]
    line = riser["lines"][0]
    selected = design["selected_bottom_length_m"]
    margin = basis["parameters"]["slwr_seabed_margin"]["value"]
    assert design["required_bottom_length_m"] + margin <= selected
    assert selected < design["required_bottom_length_m"] + margin + 0.1
    assert [s["offset_pct"] for s in design["selected"]] == lazy_offsets()
    assert line["sections"][-1]["length"] == selected
    for state in design["selected"]:
        independent = hand_lazy_state(basis, line, riser["line_types"], state["offset_pct"])
        for name, value in independent.items():
            assert state[name] == pytest.approx(value, abs=1e-7)
    old_reserves = [12.2, 16.3, 20.0, 23.4, 26.5]
    for state, expected in zip(design["original"], old_reserves):
        assert state["grounded_length_m"] == pytest.approx(expected, abs=0.05)


def test_slwr_regeneration_is_deterministic_and_preserves_scr(tmp_path):
    """A changed lazy-wave reserve shall reproduce references and preserve SCR bytes."""
    campaign = tmp_path / "campaign"
    shutil.copytree(CAMPAIGN, campaign)
    scr = {p.relative_to(campaign): p.read_bytes() for p in (campaign / "scr").rglob("*") if p.is_file()}
    basis = load_basis()
    reserve_increment_m = 3.0
    basis["parameters"]["slwr_seabed_margin"]["value"] += reserve_increment_m
    (campaign / "basis.yml").write_text(yaml.safe_dump(basis, sort_keys=False))
    check = load_check()
    builds = []
    for _ in range(2):
        check.regenerate_slwr(campaign)
        (campaign / "static-reference.json").write_text(json.dumps(check.reference(campaign), indent=2) + "\n")
        builds.append({p.relative_to(campaign): p.read_bytes() for p in campaign.rglob("*") if p.is_file()})
    assert builds[0] == builds[1]
    assert all(builds[1][path] == raw for path, raw in scr.items())
    line = yaml.safe_load((campaign / "slwr/spec.yml").read_text())["riser"]["lines"][0]
    design = check.calculate(basis)["slwr"]["seabed_offset_sweep"]
    assert line["sections"][-1]["length"] == design["selected_bottom_length_m"]
    assert line["end_b"]["position"][0] == pytest.approx(design["selected_anchor_x_m"])


def hand_scr_state(basis, offset_m):
    """Close the committed SCR independently and apply full adverse friction."""
    p = {name: record["value"] for name, record in basis["parameters"].items()}
    riser = yaml.safe_load((CAMPAIGN / "scr/spec.yml").read_text())["riser"]
    line, bare = riser["lines"][0], riser["line_types"][0]
    height = p["water_depth"] - p["hangoff_depth"] - bare["outer_diameter"] / 2
    length = sum(section["length"] for section in line["sections"])
    grounded = hand_grounded_length(height, length, line["end_b"]["position"][0] - offset_m)
    suspended = length - grounded
    radius = (suspended ** 2 - height ** 2) / (2 * height)
    weight = (bare["mass_per_length"] * 1000
              + line["contents"]["density"] * 1000 * math.pi * bare["inner_diameter"] ** 2 / 4
              - p["seawater_density"] * math.pi * bare["outer_diameter"] ** 2 / 4) * p["gravity"]
    return {
        "grounded_length_m": grounded,
        "horizontal_tension_kN": weight * radius / 1000,
        "top_tension_kN": weight * (radius + height) / 1000,
        "minimum_effective_tension_kN": weight * (radius - 0.5 * grounded) / 1000,
    }


@pytest.mark.parametrize("offset_m", range(-150, 151, 25))
def test_scr_full_friction_tension_at_every_sweep_step(offset_m):
    """Full adverse axial friction shall leave positive tension at every point."""
    basis = load_basis()
    state = hand_scr_state(basis, offset_m)
    assert state["minimum_effective_tension_kN"] > 0
    margin = basis["parameters"]["minimum_static_tension"]["value"]
    assert margin > 0
    assert state["minimum_effective_tension_kN"] >= margin
    assert state["grounded_length_m"] > 0


def test_friction_basis_and_smallest_scr_change():
    """The largest feasible bottom on the existing grid changes the least pipe."""
    basis = load_basis()
    p = {name: record["value"] for name, record in basis["parameters"].items()}
    check = load_check()
    result = check.calculate(basis)
    design = result["scr"]["seabed_offset_sweep"]
    assert basis["seabed_axial_friction"]["statics_application_status"] == "ASSUMED"
    assert p["seabed_axial_friction"] == 0.5
    for kind in ("scr", "slwr"):
        types = yaml.safe_load((CAMPAIGN / kind / "modular/includes/05_riser_line_types.yml").read_text())
        for line_type in types["LineTypes"]:
            assert line_type["SeabedLateralFrictionCoefficient"] == p["seabed_axial_friction"]
            assert line_type["SeabedAxialFrictionCoefficient"] is None
    states = design["static_sweep"]
    assert [s["offset_m"] for s in states] == list(range(-150, 151, 25))
    assert [s["offset_pct"] for s in design["selected"]] == [-10, -5, 0, 5, 10]
    for state in states:
        independent = hand_scr_state(basis, state["offset_m"])
        for key, value in independent.items():
            assert state[key] == pytest.approx(value, abs=1e-7)
    selected = design["selected_bottom_length_m"]
    assert selected <= design["requested_bottom_length_m"]
    assert design["minimum_grounded_length_m"] > 0
    assert design["minimum_effective_tension_kN"] >= p["minimum_static_tension"]
    # Extending bottom and anchor together leaves the suspended solution fixed.
    next_tension = min(s["minimum_effective_tension_kN"] for s in states)
    next_tension -= p["seabed_axial_friction"] * result["operating_weight_N_per_m"] * 0.1 / 1000
    assert selected + 0.1 > design["requested_bottom_length_m"] or next_tension < p["minimum_static_tension"]
    nominal = hand_scr_state(basis, 0)
    assert nominal["top_tension_kN"] == pytest.approx(result["scr"]["oo"]["top_tension_kN"])
    for key in ("top_tension_kN", "horizontal_tension_kN", "minimum_effective_tension_kN"):
        assert all(a[key] > b[key] for a, b in zip(states, states[1:]))
    assert all(a["grounded_length_m"] < b["grounded_length_m"] for a, b in zip(states, states[1:]))
    invalid = load_basis()
    invalid["parameters"]["minimum_static_tension"]["value"] = 1000
    with pytest.raises(ValueError, match="No tension-only SCR"):
        check.calculate(invalid)


def test_slwr_full_friction_tension_and_reference():
    """The retained lazy-wave bottom shall also pass the adverse-friction sweep."""
    basis = load_basis()
    p = {name: record["value"] for name, record in basis["parameters"].items()}
    result = load_check().calculate(basis)
    states = result["slwr"]["seabed_offset_sweep"]["static_sweep"]
    assert [s["offset_m"] for s in states] == list(range(-150, 151, 25))
    riser = yaml.safe_load((CAMPAIGN / "slwr/spec.yml").read_text())["riser"]
    for state in states:
        independent = hand_lazy_state(basis, riser["lines"][0], riser["line_types"], state["offset_pct"])
        tension = (independent["horizontal_tension_kN"] - p["seabed_axial_friction"]
                   * result["operating_weight_N_per_m"] * independent["grounded_length_m"] / 1000)
        assert state["minimum_effective_tension_kN"] == pytest.approx(tension, abs=1e-7)
        assert tension >= p["minimum_static_tension"]
        assert independent["grounded_length_m"] > 0


def test_scr_friction_regeneration_preserves_other_bytes(tmp_path):
    """Two tension-margin builds shall preserve every unaffected model byte."""
    campaign = tmp_path / "campaign"
    shutil.copytree(CAMPAIGN, campaign)
    before = {p.relative_to(campaign): p.read_bytes() for p in campaign.rglob("*") if p.is_file()}
    basis = load_basis()
    basis["parameters"]["minimum_static_tension"]["value"] += 1
    (campaign / "basis.yml").write_text(yaml.safe_dump(basis, sort_keys=False))
    check = load_check()
    builds = []
    for _ in range(2):
        check.regenerate_scr(campaign)
        (campaign / "static-reference.json").write_text(json.dumps(check.reference(campaign), indent=2) + "\n")
        builds.append({p.relative_to(campaign): p.read_bytes() for p in campaign.rglob("*") if p.is_file()})
    assert builds[0] == builds[1]
    affected = {Path(name) for name in ("basis.yml", "static-reference.json", "scr/spec.yml",
                                      "scr/modular/includes/07_riser_lines.yml")}
    assert all(builds[1][path] == raw for path, raw in before.items() if path not in affected)
    for name in ("scr/spec.yml", "scr/modular/includes/07_riser_lines.yml"):
        old = yaml.safe_load(before[Path(name)])
        new = yaml.safe_load(builds[1][Path(name)])
        if name.endswith("spec.yml"):
            a, b = old["riser"]["lines"][0], new["riser"]["lines"][0]
            a["end_b"]["position"][0] = b["end_b"]["position"][0]
            a["sections"][-1]["length"] = b["sections"][-1]["length"]
        else:
            a, b = old["Lines"][0], new["Lines"][0]
            connection = next(key for key in a if key.startswith("Connection, ConnectionX"))
            a[connection][1][1] = b[connection][1][1]
            a["LineType, Length, TargetSegmentLength"][-1][1] = b["LineType, Length, TargetSegmentLength"][-1][1]
        assert old == new
