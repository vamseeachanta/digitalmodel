# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Load-form and fixed-solid clearance checks for every S-lay campaign case."""

import importlib.util
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

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
    key = ("ConnectionxBendingStiffness, ConnectionyBendingStiffness, "
           "ConnectionTwistingStiffness, ConnectionDamping")
    reference = ROOT / "docs/domains/orcaflex/examples/raw/D03/D03 Lay on tower.yml"
    native_lines = yaml.safe_load(reference.read_text(encoding="utf-8"))["Lines"]
    native_free = next(item for item in native_lines if key in item and item[key][0] == [])
    ModularModelGenerator.from_spec(_case_spec(case_id)).generate(tmp_path)
    for folder in (CAMPAIGN / "models" / case_id, tmp_path):
        line = _resolve_case(folder)["Lines"][0]
        assert line[key][0] == native_free[key][0]
        assert line[key] == [[], [0, 0, 0, 0]]
        text = (folder / "includes/07_lines.yml").read_text(encoding="utf-8")
        assert f"  {key}:\n  - []\n  - [0, 0, 0, 0]\n" in text


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
    # The pipelay save has neither torsional end columns nor winches. The
    # tower save supplies both, including the empty row for its free upper end.
    key = ("ConnectionxBendingStiffness, ConnectionyBendingStiffness, "
           "ConnectionTwistingStiffness, ConnectionDamping")
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
    for index, (support, roller) in enumerate(zip(vessel["Supports"], spec.equipment.stinger.rollers)):
        system = systems[support["SupportCoordinateSystem"]]
        assert system["SupportCoordinateSystemOrientation"] == [0, 0, 0]
        local = list(support["SupportPosition"])
        local[2] -= roller.z_offset
        points[f"roller {index}"] = [sum(values) for values in zip(
            vessel["InitialPosition"], system["SupportCoordinateSystemPos"], local)]
    assert len(points) == len(spec.equipment.stinger.rollers) + 1
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
def test_native_load_and_statics_of_every_generated_case(case_id):
    """Load each retained master and require converged whole-system statics."""
    import OrcFxAPI

    model = OrcFxAPI.Model(str(CAMPAIGN / "models" / case_id / "master.yml"))
    model.CalculateStatics()
    assert model.state == OrcFxAPI.ModelState.InStaticState
