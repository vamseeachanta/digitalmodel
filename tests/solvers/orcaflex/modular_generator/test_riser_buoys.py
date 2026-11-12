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
