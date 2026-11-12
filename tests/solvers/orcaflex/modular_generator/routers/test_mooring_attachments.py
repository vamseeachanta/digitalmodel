"""Mooring line attachments: clump weights and submerged buoys (#3975).

``MooringLine`` had no attachments, so semi-taut and buoy-supported moorings
could not carry clump weights or submerged buoys. These tests cover the schema
(``MooringSystem.clump_types``, ``MooringLine.attachments``), the router output
and its golden. Native data names are those of the public example
"A01 Lazy wave riser". Full-model generation of the example needs the generic
host vessel from #3972 and is not exercised here. No OrcaFlex licence is used.
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


def test_net_weight_and_upthrust_match_hand_values(mooring):
    """Comparators recorded in the example header: 170.5 kN and 202.3 kN."""
    types = {ct["name"]: ct for ct in mooring["clump_types"]}
    clump, buoy = types["Clump20te"], types["SubBuoy"]
    assert (clump["mass"] - 1.025 * clump["volume"]) * 9.80665 == pytest.approx(170.5, abs=0.1)
    assert (1.025 * buoy["volume"] - buoy["mass"]) * 9.80665 == pytest.approx(202.3, abs=0.1)


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
