"""Mooring line attachments: clump weights and submerged buoys (#3975).

``MooringLine`` had no attachments, so semi-taut and buoy-supported moorings
could not carry clump weights or submerged buoys. These tests cover the schema
(``MooringSystem.clump_types``, ``MooringLine.attachments``), the router output
and its golden. Native data names are those of the public example
"A01 Lazy wave riser". Generation of the example through the typed route needs
the generic host vessel from #3972; the routed output is generated here through
a generic-only specification instead. No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator.routers.mooring_router import (
    MooringRouter,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema.generic import GenericModel
from digitalmodel.solvers.orcaflex.modular_generator.schema.mooring import MooringSystem

REPO = Path(__file__).resolve().parents[5]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/mooring_with_attachments/spec.yml"
NATIVE = REPO / "docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/includes"
GOLDEN = Path(__file__).parents[1] / "goldens/typed/mooring_with_attachments/router_output.yml"
ATTACH_KEY = "AttachmentType, Attachmentx, Attachmenty, Attachmentz, AttachmentzRelativeTo"


@pytest.fixture
def mooring() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))["mooring"]


def _route(data: dict) -> dict:
    return MooringRouter().route(MooringSystem(**data))


def test_router_emits_clump_types_and_sorted_attachments(mooring):
    out = _route(mooring)
    assert [ct["name"] for ct in out["clump_types"]] == ["Clump20te", "SubBuoy"]
    for line in out["lines"]:
        rows = line["properties"][ATTACH_KEY]
        # Sorted by arc length from the anchor, which is End A.
        assert rows == [
            ["Clump20te", 0, 0, 250, "End A"],
            ["SubBuoy", 0, 0, 300, "End A"],
        ]
        assert line["properties"]["EndAConnection"] == "Fixed"


def test_router_output_is_valid_generic_model(mooring):
    model = GenericModel(**_route(mooring))
    assert [ct.name for ct in model.clump_types] == ["Clump20te", "SubBuoy"]


def test_data_names_match_native_example(mooring):
    native_lines = yaml.safe_load((NATIVE / "07_lines.yml").read_text(encoding="utf-8"))["Lines"]
    assert any(ATTACH_KEY in line for line in native_lines)
    native_clump = yaml.safe_load(
        (NATIVE / "01_general.yml").read_text(encoding="utf-8")
    )["ClumpTypes"][0]
    emitted = _route(mooring)["clump_types"][0]["properties"]
    assert set(emitted) <= set(native_clump)


# Standard acceleration of gravity, exact (NIST CODATA,
# https://physics.nist.gov/cgi-bin/cuu/Value?gn, retrieved 2026-11-13).
G_N = 9.80665


def test_net_weight_and_upthrust_match_hand_values():
    """Comparators recorded in the example header: 170.5 kN and 202.3 kN.

    Sea density is the example's environment input, which equals the
    density of the public example "A01 Lazy wave riser"; g is G_N.
    """
    spec = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    native_env = yaml.safe_load((NATIVE / "03_environment.yml").read_text(encoding="utf-8"))
    rho = spec["environment"]["water"]["density"]
    assert rho == native_env["Environment"]["Density"]
    types = {ct["name"]: ct for ct in spec["mooring"]["clump_types"]}
    clump, buoy = types["Clump20te"], types["SubBuoy"]
    assert (clump["mass"] - rho * clump["volume"]) * G_N == pytest.approx(170.5, abs=0.1)
    assert (rho * buoy["volume"] - buoy["mass"]) * G_N == pytest.approx(202.3, abs=0.1)


def test_coefficients_are_the_cited_example_values(mooring):
    """Cd and Ca equal the (normal, axial) cells of the public example's
    clump type "Module", the source cited in the example header."""
    native = yaml.safe_load((NATIVE / "01_general.yml").read_text(encoding="utf-8"))
    (module,) = [ct for ct in native["ClumpTypes"] if ct["Name"] == "Module"]
    for ct in mooring["clump_types"]:
        assert ct["drag_coefficient"] == [module["Cd"][0], module["Cd"][2]]
        assert ct["added_mass_coefficient"] == [module["Ca"][0], module["Ca"][2]]


def test_unknown_clump_type_rejected(mooring):
    data = copy.deepcopy(mooring)
    data["lines"][0]["attachments"][0]["clump_type"] = "Missing"
    with pytest.raises(ValidationError, match="unknown clump_type"):
        MooringSystem(**data)


def test_attachment_beyond_line_length_rejected(mooring):
    data = copy.deepcopy(mooring)
    data["lines"][0]["attachments"][0]["arc_length"] = 900.5
    with pytest.raises(ValidationError, match="beyond the line length"):
        MooringSystem(**data)


def test_duplicate_clump_type_names_rejected(mooring):
    data = copy.deepcopy(mooring)
    data["clump_types"].append(copy.deepcopy(data["clump_types"][0]))
    with pytest.raises(ValidationError, match="Duplicate clump type names"):
        MooringSystem(**data)


def test_lines_without_attachments_unchanged(mooring):
    data = copy.deepcopy(mooring)
    for line in data["lines"]:
        line.pop("attachments")
    data.pop("clump_types")
    out = _route(data)
    assert "clump_types" not in out
    assert all(ATTACH_KEY not in line["properties"] for line in out["lines"])


def test_router_output_matches_golden(mooring):
    """Golden covers the clump types and lines this change adds; line-type
    defaults are covered by test_mooring_router.py (see #3981, #3982)."""
    out = _route(mooring)
    golden = yaml.safe_load(GOLDEN.read_text(encoding="utf-8"))
    assert {k: out[k] for k in ("clump_types", "lines", "winches") if k in out} == golden


def test_router_output_generates_through_generic_route(tmp_path):
    """Generator-level check that does not need #3972.

    The routed mooring is placed in a generic-only specification together
    with the example's host vessel. ClumpTypes must be written before the
    Lines that reference them, with the native key order of the public
    example "A01 Lazy wave riser".
    """
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    spec = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    generic = spec.pop("generic")
    generic.update(_route(spec.pop("mooring")))
    spec["generic"] = generic
    out = tmp_path / "out"
    ModularModelGenerator.from_spec(ProjectInputSpec(**spec)).generate(out)
    objects = yaml.safe_load((out / "includes/20_generic_objects.yml").read_text(encoding="utf-8"))
    keys = list(objects)
    assert keys.index("ClumpTypes") < keys.index("Lines")
    native_clump = yaml.safe_load(
        (NATIVE / "01_general.yml").read_text(encoding="utf-8")
    )["ClumpTypes"][0]
    native_order = [k for k in native_clump if k != "Name"]
    for clump in objects["ClumpTypes"]:
        emitted = [k for k in clump if k != "Name"]
        assert emitted == [k for k in native_order if k in emitted]
    assert [c["Name"] for c in objects["ClumpTypes"]] == ["Clump20te", "SubBuoy"]
    for line in objects["Lines"]:
        assert line[ATTACH_KEY] == [
            ["Clump20te", 0, 0, 250, "End A"],
            ["SubBuoy", 0, 0, 300, "End A"],
        ]
        assert line["EndAConnection"] == "Fixed" and line["EndBConnection"] == "FPSO"
