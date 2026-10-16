# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Load-form and fixed-solid clearance checks for every S-lay campaign case."""

import importlib.util
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder

ROOT = Path(__file__).resolve().parents[3]
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"
TABLE = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text(encoding="utf-8"))
CASES = [case["id"] for case in TABLE["cases"]]
requires_orcfxapi = pytest.mark.skipif(
    importlib.util.find_spec("OrcFxAPI") is None,
    reason="OrcFxAPI is not installed",
)


def _case_spec(case_id):
    module_spec = importlib.util.spec_from_file_location(
        "slay_preparation", ROOT / "scripts/orcaflex/prepare_slay_campaign.py")
    driver = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(driver)
    case = next(case for case in TABLE["cases"] if case["id"] == case_id)
    return driver.case_spec(TABLE["basis"], case)


def _resolve_case(folder):
    """Read includes in master order, relative to their containing file."""
    sections = {}

    def read(path):
        assert path.is_file(), path
        data = yaml.safe_load(path.read_text(encoding="utf-8"))
        if isinstance(data, list):
            for directive in data:
                assert set(directive) == {"includefile"}
                read(path.parent / directive["includefile"])
        else:
            assert isinstance(data, dict), path
            assert not sections.keys() & data.keys(), path
            sections.update(data)

    read(folder / "master.yml")
    assert sections
    return sections


def _assert_native_form(actual, native, *, ordered=True, columns=None):
    """Check native keys, vector dimensions, positional rows and scalar forms.

    Native tables may omit trailing default columns. End-specific tables use
    the corresponding end row; other tables repeat the saved row forms.
    Object mappings need not follow a save's property order, but draught
    selectors retain the ordered check used by the original contract.
    """
    if isinstance(native, dict):
        assert isinstance(actual, dict)
        assert actual.keys() <= native.keys()
        if ordered:
            assert list(actual) == [key for key in native if key in actual]
        for key, value in actual.items():
            _assert_native_form(value, native[key], ordered=ordered,
                                columns=key.split(", ") if ", " in key else None)
    elif isinstance(native, list):
        assert isinstance(actual, list)
        if not native:
            assert actual == []
        elif isinstance(native[0], dict):
            for value in actual:
                _assert_native_form(value, native[0], ordered=ordered)
        elif isinstance(native[0], list):
            end_table = columns and columns[0].startswith("Connection")
            if end_table:
                assert len(actual) == len(native)
            for index, row in enumerate(actual):
                assert isinstance(row, list)
                reference = native[index] if end_table else next(
                    (saved for saved in native if len(saved) == len(row)), None)
                assert reference is not None
                if not reference:
                    assert row == []
                    continue
                width = len(columns) if columns else len(reference)
                assert len(reference) <= len(row) <= width
                for cell_index, value in enumerate(row):
                    saved = reference[cell_index] if cell_index < len(reference) else None
                    _assert_native_form(value, saved, ordered=ordered)
        else:
            assert len(actual) == len(native)
            for value, saved in zip(actual, native):
                _assert_native_form(value, saved, ordered=ordered)
    else:
        assert not isinstance(actual, (dict, list))
        if isinstance(native, bool):
            assert isinstance(actual, bool) or actual in ("Yes", "No")
            return  # Native saves use unquoted Yes/No; PyYAML resolves booleans.
        if native is None:
            return  # A saved default does not establish a property's scalar type.
        if isinstance(native, str):
            try:
                float(native)  # YAML saves also use numeric strings and Infinity.
            except ValueError:
                assert isinstance(actual, str)
                return
        assert isinstance(actual, (int, float)) and not isinstance(actual, bool)


@pytest.mark.parametrize("case_id", CASES)
def test_every_case_has_native_named_draught_mappings(tmp_path, case_id):
    """Native saves use named mappings inside the Draughts collection."""
    reference = ROOT / "docs/domains/orcaflex/examples/raw/D03/D03 Lay on tower.yml"
    native = yaml.safe_load(reference.read_text(encoding="utf-8"))["VesselTypes"][0]
    native_draught = native["Draughts"][0]
    spec = _case_spec(case_id)
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    for folder in (CAMPAIGN / "models" / case_id, tmp_path):
        sections = _resolve_case(folder)
        assert sections["VesselTypes"]
        for vessel_type in sections["VesselTypes"]:
            assert vessel_type["Draughts"]
            for draught in vessel_type["Draughts"]:
                assert isinstance(draught, dict)
                assert list(draught) == list(native_draught)[:5]
                _assert_native_form(draught, native_draught)
            _assert_native_form(vessel_type, native)


@pytest.mark.parametrize("case_id", CASES)
def test_wave_calculation_method_belongs_to_lines_only(tmp_path, case_id):
    """The native vessel form has no wave calculation selector."""
    ModularModelGenerator.from_spec(_case_spec(case_id)).generate(tmp_path)
    for folder in (CAMPAIGN / "models" / case_id, tmp_path):
        sections = _resolve_case(folder)
        assert all("WaveCalculationMethod" not in item for item in sections["Vessels"])
        assert all(item["WaveCalculationMethod"] == "Specified by environment"
                   for item in sections["Lines"])


@pytest.mark.parametrize("case_id", CASES)
def test_free_end_stiffness_uses_empty_native_row(tmp_path, case_id):
    """A free end refuses values; the anchored end explicitly releases rotation."""
    key = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"
    reference = ROOT / "docs/domains/orcaflex/examples/raw/D03/D03 Lay on tower.yml"
    native_lines = yaml.safe_load(reference.read_text(encoding="utf-8"))["Lines"]
    native_free = next(item for item in native_lines if key in item and item[key][0] == [])
    ModularModelGenerator.from_spec(_case_spec(case_id)).generate(tmp_path)
    for folder in (CAMPAIGN / "models" / case_id, tmp_path):
        line = _resolve_case(folder)["Lines"][0]
        assert line[key][0] == native_free[key][0]
        assert line[key] == native_free[key] == [[], [0, None]]
        assert line["IncludeTorsion"] == "No"
        text = (folder / "includes/07_lines.yml").read_text(encoding="utf-8")
        assert f"  {key}:\n  - []\n  - [0, ~]\n" in text


@pytest.mark.parametrize("actual,native", [
    ([1, 2], [1, 2, 3]),
    (["wrong", 0, 0], [0, 0, 0]),
    ([["name", "wrong", 5]], [["type", 10, 1]]),
    ([["name", 0]], [["type", 10, 1]]),
])
def test_native_form_rejects_wrong_vector_and_table_values(actual, native):
    """A scalar-only comparison must not accept malformed native rows."""
    with pytest.raises(AssertionError):
        _assert_native_form(actual, native)


def _native_object_sections():
    """Use the pipelay save, with explicit supplements for absent native forms."""
    native = yaml.safe_load((ROOT / "docs/domains/orcaflex/examples/raw/E08/basecase.yml")
                            .read_text(encoding="utf-8"))
    tower = yaml.safe_load((ROOT / "docs/domains/orcaflex/examples/raw/D03/D03 Lay on tower.yml")
                           .read_text(encoding="utf-8"))
    # The older save uses Orientation; InitialHeading is the accepted scalar
    # spelling for heading in the generated models. Compare its numeric form.
    native["Vessels"][0]["InitialHeading"] = native["Vessels"][0]["Orientation"][2]
    native["SupportTypes"] = [dict(item for support in native["SupportTypes"]
                                   for item in support.items())]
    # The tower save supplies the two-column free-end form and winches.
    key = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"
    free_line = next(line for line in tower["Lines"] if key in line and line[key][0] == [])
    native["Lines"][0][key] = free_line[key]
    native["Winches"] = tower["Winches"]
    return native


@pytest.mark.parametrize("case_id", CASES)
def test_every_case_has_native_object_section_forms(tmp_path, case_id):
    """Compare every emitted object property, including nested and table forms."""
    native = _native_object_sections()
    ModularModelGenerator.from_spec(_case_spec(case_id)).generate(tmp_path)
    for folder in (CAMPAIGN / "models" / case_id, tmp_path):
        sections = _resolve_case(folder)
        for section in ("Vessels", "Lines", "SupportTypes", "Winches"):
            assert sections[section]
            _assert_native_form(sections[section], native[section], ordered=False)


def _local_point(point, origin, orientation):
    """Undo successive local x, y, z rotations to test a fixed shape."""
    x, y, z = (value - offset for value, offset in zip(point, origin))
    rx, ry, rz = map(math.radians, orientation)
    x, y = math.cos(rz) * x + math.sin(rz) * y, -math.sin(rz) * x + math.cos(rz) * y
    x, z = math.cos(ry) * x - math.sin(ry) * z, math.sin(ry) * x + math.cos(ry) * z
    y, z = math.cos(rx) * y + math.sin(rx) * z, -math.sin(rx) * y + math.cos(rx) * z
    return x, y, z


def _solid_may_contain(shape, point):
    """Test blocks exactly; conservatively enclose revolved plates by cylinders.

    The full cylinder includes every angular sector of a curved plate. A point
    outside this envelope is outside the solid without assuming sector limits.
    """
    x, y, z = _local_point(point, shape["Origin"], shape["Orientation"])
    if shape["Shape"] == "Block":
        return all(0 <= coordinate <= size for coordinate, size in zip((x, y, z), shape["Size"]))
    assert shape["Shape"] == "Curved plate", "Extend the clearance check for this shape"
    profile = shape["ProfileDistanceAlongAxis, ProfileDiameter"]
    return (min(row[0] for row in profile) <= z <= max(row[0] for row in profile)
            and math.hypot(x, y) <= max(row[1] for row in profile) / 2)


def _pipe_seed_points(sections, spec):
    line = sections["Lines"][0]
    connections = next(value for key, value in line.items() if key.startswith("Connection, "))
    points = {"End A": connections[0][1:4]}
    vessel = sections["Vessels"][0]
    systems = {system["SupportCoordinateSystemName"]: system
               for system in vessel["SupportCoordinateSystems"]}
    # These cases declare a level, unrotated hull and support coordinate system.
    assert vessel["InitialHeading"] == 0
    stinger = spec.equipment.stinger
    offsets = [stinger.rollers[0].z_offset] * 12 + [roller.z_offset for roller in stinger.rollers]
    for index, (support, offset) in enumerate(zip(vessel["Supports"], offsets)):
        system = systems[support["SupportCoordinateSystem"]]
        assert system["SupportCoordinateSystemOrientation"] == [0, 0, 0]
        local = list(support["SupportPosition"])
        local[2] -= offset
        points[f"roller {index}"] = [sum(values) for values in zip(
            vessel["InitialPosition"], system["SupportCoordinateSystemPos"], local)]
    assert len(points) == len(vessel["Supports"]) + 1
    return points


@pytest.mark.parametrize("case_id", CASES)
def test_pipe_seed_positions_clear_unrequested_fixed_solids(tmp_path, case_id):
    """Rollers supply intended contact; implicit fixed ramps must not trap seeds."""
    spec = _case_spec(case_id)
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    for folder in (CAMPAIGN / "models" / case_id, tmp_path):
        sections = _resolve_case(folder)
        fixed = [shape for shape in sections.get("Shapes", [])
                 if shape["Connection"] == "Fixed" and shape["ShapeType"] == "Elastic solid"]
        trapped = [(name, shape["Name"]) for name, point in _pipe_seed_points(sections, spec).items()
                   for shape in fixed if _solid_may_contain(shape, point)]
        assert not trapped, trapped


@pytest.mark.solver
@requires_orcfxapi
@pytest.mark.parametrize("case_id", CASES)
def test_native_load_and_statics_of_every_generated_case(case_id, record_property):
    """Load each retained master and require converged whole-system statics."""
    import OrcFxAPI

    model = OrcFxAPI.Model(str(CAMPAIGN / "models" / case_id / "master.yml"))
    model.CalculateStatics()
    assert model.state == OrcFxAPI.ModelState.InStaticState

    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import (
        assert_static_acceptance, end_a_support_reaction)
    spec = _case_spec(case_id)
    line = model[spec.pipeline.name]
    sections = _resolve_case(CAMPAIGN / "models" / case_id)
    vessel = model[spec.equipment.vessel.name]
    support_extra = OrcFxAPI.oeSupport(SupportIndex=0)
    # Discover the native resultant rather than assuming a component is normal force.
    force_names = [name for name in vessel.vars(objectExtra=support_extra)
                   if "support" in name.lower() and "force" in name.lower()
                   and not {"x", "y", "z"}.intersection(name.lower().split())]
    assert len(force_names) == 1, "A unique native support resultant is required"
    forces = [vessel.StaticResult(force_names[0], OrcFxAPI.oeSupport(SupportIndex=index))
              for index in range(len(sections["Vessels"][0]["Supports"]))]
    firing_count = math.ceil(spec.equipment.stinger.firing_line_length /
                             spec.equipment.stinger.firing_line_spacing)
    active = [roller.arc_length for roller, force in zip(
        spec.equipment.stinger.rollers, forces[firing_count:]) if force > 0]
    assert active, "Stinger contact readback is required"
    lift_off = max(active)
    arcs, (x, z, moments, tensions) = _aligned_static_graphs(
        line.RangeGraph("X"), line.RangeGraph("Z"),
        line.RangeGraph("Bend Moment"), line.RangeGraph("Effective Tension"))
    od = spec.pipeline.dimensions.outer_diameter
    wall = spec.pipeline.dimensions.wall_thickness
    section = sections["LineTypes"][0]
    ei = section["E"] * math.pi / 64 * (od**4 - (od - 2 * wall)**4)
    strains = [abs(moment) / ei * od / 2 for moment in moments]
    coated_radius = od / 2 + spec.pipeline.coatings.corrosion.thickness
    coated_radius += sum(coating.thickness for coating in spec.pipeline.coatings.weight)
    # Supports remain at the declared pose in these statics cases. End A
    # travel changes the material station at each physical support contact.
    assert vessel.IncludedInStatics == "None"
    support_index = firing_count + next(
        i for i, roller in enumerate(spec.equipment.stinger.rollers)
        if roller.arc_length == lift_off)
    contact = _pipe_seed_points(sections, spec)[f"roller {support_index}"]
    lift_index = _contact_node_index(x, z, contact)
    grounded = [i for i, height in enumerate(z)
                if i > lift_index and height <= -spec.environment.water.depth + coated_radius]
    assert grounded, "Grounded pipe readback is required"
    touchdown = grounded[0]
    assert 0 < lift_index < touchdown
    departure = math.degrees(math.atan2(z[lift_index - 1] - z[lift_index + 1],
                                       abs(x[lift_index + 1] - x[lift_index - 1])))
    sag_indices = set(_sag_bend_indices(x, z, lift_index, touchdown))
    # Include overbend beyond the last support, excluding measured sag curvature.
    overbend = [value for i, value in enumerate(strains[:touchdown + 1])
                if i not in sag_indices]
    sag_bend = [strains[i] for i in sag_indices]
    assert overbend and sag_bend
    reference = next(row["stiff_reference"] for row in TABLE["cases"] if row["id"] == case_id)
    reference = {key: reference[key] for key in
                 ("lift_off_arc", "departure_angle", "touchdown_x", "sag_bend_strain")}
    points = _pipe_seed_points(sections, spec)
    support_x = [points[f"roller {i}"][0] for i in range(len(forces))]
    result = assert_static_acceptance(
        reference=reference,
        wire_length=model[spec.equipment.tensioner.name].StaticResult("Length"),
        end_a_reaction=end_a_support_reaction(x[0], support_x, forces),
        last_support_force=forces[-1],
        grounded_tension=[tensions[i] for i in grounded], overbend_strain=max(overbend),
        lift_off_arc=lift_off, departure_angle=departure,
        touchdown_x=x[touchdown], sag_bend_strain=max(sag_bend),
        diameter=od, radius=spec.equipment.stinger.radius)
    import json
    record_property("static_qa", json.dumps(result, allow_nan=False))
    assert result["counts"]["failed"] == 0
    assert result["counts"]["not_evaluated"] == 3
    assert not result["fully_accepted"]


def test_sag_bend_excludes_the_positive_curvature_at_lift_off():
    assert _sag_bend_indices([0, -5, -10, -15, -20, -25],
                             [0, -1, -3, -6, -7, -7.5], 1, 5) == [3, 4]


def _sag_bend_indices(x, z, lift_off, touchdown):
    """Select decreasing departure slope, excluding residual overbend curvature."""
    slopes = [math.atan2(z[i] - z[i + 1], abs(x[i + 1] - x[i]))
              for i in range(len(x) - 1)]
    return [i for i in range(max(1, lift_off + 1), touchdown)
            if slopes[i] < slopes[i - 1]]


def test_lift_off_uses_physical_contact_after_end_a_travel():
    # End travel shifts the pipe material stations relative to fixed supports.
    arcs = [0, 5, 110, 115, 120, 125]
    x = [-26.1, -31.1, -135, -140, -144.9, -150]
    z = [0, 0, -8, -9, -10, -12]
    index = _contact_node_index(x, z, [-144.9, 0, -10])
    assert index == 4
    assert arcs[index] != 60 + 55


def test_static_graphs_require_common_material_stations():
    from types import SimpleNamespace
    position = SimpleNamespace(X=[0, 5, 10], Mean=[0, -5, -10])
    shifted = SimpleNamespace(X=[0, 4, 10], Mean=[1, 2, 3])
    with pytest.raises(ValueError, match="stations"):
        _aligned_static_graphs(position, shifted)
    aligned = SimpleNamespace(X=[0, 5, 10], Mean=[1, 2, 3])
    assert _aligned_static_graphs(position, aligned) == (
        [0, 5, 10], [[0, -5, -10], [1, 2, 3]])


def _contact_node_index(x, z, contact):
    """Locate the line node nearest a fixed support's pipe-axis contact."""
    return min(range(len(x)), key=lambda i: (x[i] - contact[0])**2 + (z[i] - contact[2])**2)


def _aligned_static_graphs(*graphs):
    """Reject combining readbacks evaluated at different material stations."""
    arcs = list(graphs[0].X)
    values = []
    for graph in graphs:
        stations, readings = list(graph.X), list(graph.Mean)
        if (len(stations) != len(arcs) or len(readings) != len(arcs)
                or any(not math.isclose(a, b, abs_tol=1e-9, rel_tol=0)
                       for a, b in zip(arcs, stations))):
            raise ValueError("Static graphs must use common material stations")
        values.append(readings)
    return arcs, values
