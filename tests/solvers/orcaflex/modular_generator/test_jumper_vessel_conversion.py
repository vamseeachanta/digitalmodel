"""Vessel conversion contracts for #2001, using declared illustrative inputs.

Independent hand calculation for installation.yml: with zero vessel rotation,
origin (100, 200, 0) plus local End A (-10, 0, -20) = (90, 200, -20)
and local End B (10, 0, -20) = (110, 200, -20). No solver answer is claimed.
The generator must preserve local connection coordinates and the separately
supplied global spline. All numerical fixture values are declared inputs;
exact equality checks serialization, not an engineering acceptance tolerance.
"""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.marine_ops.installation.jumper_installation_schema import (
    JumperInstallationSpec,
)
from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO_ROOT = Path(__file__).resolve().parents[4]
SPEC_DIR = REPO_ROOT / "docs/domains/orcaflex/subsea/jumper/installation"
EXAMPLE = REPO_ROOT / "examples/workflows/jumper-vessel/installation.yml"
CONFIGS = ["gom_tieback_mf_plet", "gom_tieback_plet_plem"]
CONNECTION_KEY = (
    "Connection, ConnectionX, ConnectionY, ConnectionZ, "
    "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
    "ConnectionReleaseStage, ConnectionzRelativeTo"
)
SPLINE_KEY = "SplineControlPointX, SplineControlPointY, SplineControlPointZ"


@pytest.fixture
def domain():
    data = yaml.safe_load((SPEC_DIR / CONFIGS[0] / "spec.yml").read_text())
    data.update(yaml.safe_load(EXAMPLE.read_text()))
    return data


def convert(domain, tmp_path):
    path = tmp_path / "domain.yml"
    path.write_text(yaml.safe_dump(domain, sort_keys=False))
    return build_modular_spec(str(path))


def test_installation_round_trips_through_domain_schema(domain):
    parsed = JumperInstallationSpec.from_dict(domain)
    assert parsed.installation.model_dump(exclude_none=True) == domain["installation"]


@pytest.mark.parametrize("config", CONFIGS)
def test_vessel_and_connections_survive_generation(config, tmp_path):
    domain = yaml.safe_load((SPEC_DIR / config / "spec.yml").read_text())
    installation = yaml.safe_load(EXAMPLE.read_text())["installation"]
    domain["installation"] = installation
    data = convert(domain, tmp_path)
    model = ProjectInputSpec(**data)
    out = tmp_path / "model"
    ModularModelGenerator.from_spec(model).generate(out)
    objects = yaml.safe_load((out / "includes/20_generic_objects.yml").read_text())
    vessel = objects["Vessels"][0]
    assert vessel["Name"] == "support"
    assert vessel["VesselType"] == "support_type"
    assert vessel["InitialPosition"] == [100.0, 200.0, 0.0]
    assert vessel["Orientation"] == [0.0, 0.0, 0.0]
    assert vessel["SuperimposedMotion"] == "None"
    assert objects["VesselTypes"][0]["Length"] == 100.0
    assert objects["VesselTypes"][0]["RAOWaveUnit"] == "amplitude"
    line = objects["Lines"][0]
    assert line[CONNECTION_KEY] == [
        ["support", -10.0, 0.0, -20.0, 0.0, 0.0, 0.0, None, None],
        ["support", 10.0, 0.0, -20.0, 0.0, 180.0, 0.0, None, None],
    ]
    assert line[SPLINE_KEY] == installation["spline_control_points"]
    # Independent vector addition from this module's docstring.
    assert line[SPLINE_KEY][0] == [90.0, 200.0, -20.0]
    assert line[SPLINE_KEY][-1] == [110.0, 200.0, -20.0]
    master = yaml.safe_load((out / "master.yml").read_text())
    assert any(row["includefile"] == "includes/20_generic_objects.yml" for row in master)


def test_supplied_definitions_are_not_hardcoded(domain, tmp_path):
    install = domain["installation"]
    install["vessel_types"][0]["name"] = "custom_type"
    install["vessels"][0]["name"] = "custom_support"
    install["vessels"][0]["vessel_type"] = "custom_type"
    for end in ("end_a", "end_b"):
        install[end]["connection"] = "custom_support"
    data = convert(domain, tmp_path)
    assert data["generic"]["vessels"] == install["vessels"]
    assert data["generic"]["vessel_types"] == install["vessel_types"]
    assert data["generic"]["lines"][0]["properties"][CONNECTION_KEY][0][0] == "custom_support"


def test_one_end_can_remain_fixed(domain, tmp_path):
    domain["installation"]["end_b"]["connection"] = "Fixed"
    data = convert(domain, tmp_path)
    assert data["generic"]["lines"][0]["properties"][CONNECTION_KEY][1][0] == "Fixed"


def test_in_place_stage_retains_fixed_ends_and_spline(domain, tmp_path):
    domain["installation"] = {
        "stage": "in_place",
        "vessels": domain["installation"]["vessels"],
        "vessel_types": domain["installation"]["vessel_types"],
    }
    data = convert(domain, tmp_path)
    domain.pop("installation")
    original = convert(domain, tmp_path)
    assert data["generic"]["vessels"]
    assert data["generic"]["lines"] == original["generic"]["lines"]


@pytest.mark.parametrize("field", ["end_a", "end_b", "spline_control_points"])
def test_suspended_stage_requires_explicit_geometry(domain, tmp_path, field):
    domain["installation"].pop(field)
    with pytest.raises((ValueError, ValidationError), match=field):
        convert(domain, tmp_path)


@pytest.mark.parametrize("field", ["vessels", "vessel_types"])
def test_suspended_stage_requires_vessel_definitions(domain, tmp_path, field):
    domain["installation"][field] = []
    with pytest.raises((ValueError, ValidationError), match="vessel"):
        convert(domain, tmp_path)


@pytest.mark.parametrize("field", ["name", "vessel_type", "initial_position"])
def test_vessel_core_fields_are_required(domain, tmp_path, field):
    domain["installation"]["vessels"][0].pop(field)
    with pytest.raises((ValueError, ValidationError), match=field):
        convert(domain, tmp_path)


def test_unknown_attachment_rejected(domain, tmp_path):
    domain["installation"]["end_a"]["connection"] = "missing"
    with pytest.raises((ValueError, ValidationError), match="connection"):
        convert(domain, tmp_path)


def test_unresolved_vessel_type_rejected(domain, tmp_path):
    domain["installation"]["vessels"][0]["vessel_type"] = "missing"
    with pytest.raises((ValueError, ValidationError), match="vessel_type"):
        convert(domain, tmp_path)


@pytest.mark.parametrize("field", ["vessels", "vessel_types"])
def test_duplicate_object_names_rejected(domain, tmp_path, field):
    entries = domain["installation"][field]
    entries.append(deepcopy(entries[0]))
    with pytest.raises((ValueError, ValidationError), match="duplicate"):
        convert(domain, tmp_path)


def test_suspended_stage_cannot_silently_keep_two_fixed_ends(domain, tmp_path):
    for end in ("end_a", "end_b"):
        domain["installation"][end]["connection"] = "Fixed"
    with pytest.raises((ValueError, ValidationError), match="suspended"):
        convert(domain, tmp_path)


def test_unknown_stage_rejected(domain, tmp_path):
    domain["installation"]["stage"] = "unknown"
    with pytest.raises((ValueError, ValidationError), match="stage"):
        convert(domain, tmp_path)


@pytest.mark.parametrize("value", [float("nan"), float("inf")])
def test_nonfinite_attachment_rejected(domain, tmp_path, value):
    domain["installation"]["end_a"]["position"][0] = value
    with pytest.raises((ValueError, ValidationError), match="finite"):
        convert(domain, tmp_path)


def test_in_place_stage_rejects_ignored_lift_geometry(domain, tmp_path):
    domain["installation"]["stage"] = "in_place"
    with pytest.raises((ValueError, ValidationError), match="in_place"):
        convert(domain, tmp_path)


@pytest.mark.parametrize("field", ["Orientation", "PrimaryMotion", "SuperimposedMotion"])
def test_motion_choices_are_explicit(domain, tmp_path, field):
    domain["installation"]["vessels"][0]["properties"].pop(field)
    with pytest.raises((ValueError, ValidationError), match=field):
        convert(domain, tmp_path)


@pytest.mark.parametrize("orientation", [
    None, [], [0.0, 0.0], [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, float("nan")], [0.0, float("inf"), 0.0],
    [float("-inf"), 0.0, 0.0], [None, 0.0, 0.0],
    ["invalid", 0.0, 0.0], ["0", 0.0, 0.0], [True, 0.0, 0.0], "0, 0, 0",
])
def test_invalid_orientation_rejected_by_schema_and_converter(
    domain, tmp_path, orientation,
):
    domain["installation"]["vessels"][0]["properties"]["Orientation"] = orientation
    with pytest.raises(ValidationError, match="Orientation"):
        JumperInstallationSpec.from_dict(domain)
    with pytest.raises(ValidationError, match="Orientation"):
        convert(domain, tmp_path)


@pytest.mark.parametrize("field,value", [
    (field, value)
    for field in ("PrimaryMotion", "SuperimposedMotion")
    for value in (None, "", "unknown", 1, False, [], {})
] + [
    ("PrimaryMotion", "RAOs + harmonics"),
    ("SuperimposedMotion", "Calculated (6 DOF)"),
])
def test_invalid_motion_rejected_by_schema_and_converter(domain, tmp_path, field, value):
    domain["installation"]["vessels"][0]["properties"][field] = value
    with pytest.raises(ValidationError, match=field):
        JumperInstallationSpec.from_dict(domain)
    with pytest.raises(ValidationError, match=field):
        convert(domain, tmp_path)


@pytest.mark.parametrize("field,value", [
    ("PrimaryMotion", value) for value in (
        "None", "Prescribed", "Calculated (3 DOF)", "Calculated (6 DOF)",
        "Time history", "Externally calculated",
    )
] + [
    ("SuperimposedMotion", value)
    for value in ("None", "RAOs + harmonics", "Time history")
])
def test_supported_motion_and_orientation_survive_generation(domain, tmp_path, field, value):
    properties = domain["installation"]["vessels"][0]["properties"]
    properties[field] = value
    properties["Orientation"] = [15.0, -2.0, 1.0]  # Declared angles (degrees).
    parsed = JumperInstallationSpec.from_dict(domain)
    assert parsed.installation.vessels[0].properties == properties
    data = convert(domain, tmp_path)
    out = tmp_path / "motion_choices"
    ModularModelGenerator.from_spec(ProjectInputSpec(**data)).generate(out)
    objects = yaml.safe_load((out / "includes/20_generic_objects.yml").read_text())
    vessel = objects["Vessels"][0]
    for key, expected in properties.items():
        assert vessel[key] == expected


def test_nested_rao_data_survive_generation(domain, tmp_path):
    """Serialization oracle: existing repository template, not a motion solution."""
    template = yaml.safe_load(
        (REPO_ROOT / "docs/domains/orcaflex/library/templates/drilling_riser/spec.yml").read_text()
    )
    properties = template["generic"]["vessel_types"][0]["properties"]
    domain["installation"]["vessel_types"][0]["properties"] = deepcopy(properties)
    data = convert(domain, tmp_path)
    out = tmp_path / "with_raos"
    ModularModelGenerator.from_spec(ProjectInputSpec(**data)).generate(out)
    objects = yaml.safe_load((out / "includes/20_generic_objects.yml").read_text())
    assert objects["VesselTypes"][0]["Draughts"] == properties["Draughts"]


def test_worked_example_cli(tmp_path):
    import os
    import subprocess
    import sys

    env = dict(os.environ, PYTHONPATH=str(REPO_ROOT / "src"))
    result = subprocess.run(
        [sys.executable, str(EXAMPLE.with_name("convert.py")),
         str(SPEC_DIR / CONFIGS[0] / "spec.yml"), str(EXAMPLE), str(tmp_path / "output")],
        env=env, capture_output=True, text=True, check=True,
    )
    assert "NOT_EVALUATED" in result.stdout
    assert (tmp_path / "output/master.yml").is_file()


@pytest.mark.parametrize("field", ["position", "spline_control_points"])
def test_coordinates_must_be_xyz_triples(domain, tmp_path, field):
    if field == "position":
        domain["installation"]["end_a"][field].pop()
    else:
        domain["installation"][field][0].pop()
    with pytest.raises((ValueError, ValidationError)):
        convert(domain, tmp_path)


def test_short_spline_rejected(domain, tmp_path):
    domain["installation"]["spline_control_points"].pop()
    with pytest.raises((ValueError, ValidationError), match="spline_control_points"):
        convert(domain, tmp_path)


def test_reserved_vessel_name_rejected(domain, tmp_path):
    domain["installation"]["vessels"][0]["name"] = "Fixed"
    with pytest.raises((ValueError, ValidationError), match="reserved"):
        convert(domain, tmp_path)
