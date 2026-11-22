# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Contract tests for pending sweep descriptors; no solver dispatch is imported."""

import ast
from copy import deepcopy
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
CATALOG = ROOT / "docs/registry/sweeps.yaml"
FAMILY_IDS = ["operability-grid", "frequency-mesh", "roll-damping", "coupled-roll"]


def validate(catalog):
    """Import at call time so every contract can demonstrate the missing feature."""
    from digitalmodel.workflows.sweep_catalog import validate_catalog

    return validate_catalog(catalog)


def descriptor():
    """Return a complete synthetic proposal that still cannot claim readiness."""
    return {
        "schema_version": 1,
        "families": [
            {
                "id": "synthetic",
                "status": "pending",
                "workflow": "synthetic-workflow",
                "vessel_id": "synthetic-body",
                "base_input": "synthetic.yaml",
                "axes": [{"name": "heading", "unit": "deg"}],
                "variants": {"source": "factorial"},
                "grid": {"parameters": [{"name": "heading", "values": [0, 90]}]},
                "batch_size": 2,
                "outputs": ["Per-case response summaries"],
                "consumer": "Synthetic envelope calculation",
                "prerequisites": [
                    {"issue": 1557, "reason": "Execution contract review"}
                ],
                "unresolved": {},
            }
        ],
    }


def test_shipped_catalog_is_ordered_pending_and_lf():
    raw = CATALOG.read_bytes()
    assert b"\r" not in raw
    assert raw.endswith(b"\n")
    catalog = yaml.safe_load(raw)
    assert [family["id"] for family in catalog["families"]] == FAMILY_IDS
    assert catalog["schema_version"] == 1
    assert validate(catalog) == {
        "pending_family_ids": FAMILY_IDS,
        "prerequisite_counts": dict(zip(FAMILY_IDS, [2, 2, 1, 1])),
    }
    for family in catalog["families"]:
        assert family["status"] == "pending"
        assert set(family["unresolved"]) == {
            "vessel_id",
            "base_input",
            "grid",
            "batch_size",
        }
        assert all(family[field] is None for field in family["unresolved"])


def test_shipped_axes_and_tracking_issues():
    families = yaml.safe_load(CATALOG.read_bytes())["families"]
    assert [[axis["name"] for axis in family["axes"]] for family in families] == [
        ["heading", "hs", "tp"],
        ["frequency", "mesh_density"],
        ["damping_ratio"],
        ["roll_period", "fill_level"],
    ]
    assert [
        [item["issue"] for item in family["prerequisites"]] for family in families
    ] == [[1554, 1556], [612, 1555], [1538], [1528]]


def test_complete_proposal_stays_pending_and_validation_does_not_mutate():
    catalog = descriptor()
    before = deepcopy(catalog)
    expected = {
        "pending_family_ids": ["synthetic"],
        "prerequisite_counts": {"synthetic": 1},
    }
    assert validate(catalog) == expected
    assert validate(catalog) == expected
    assert catalog == before


@pytest.mark.parametrize("source", ["factorial", "range", "csv", "yaml_matrix"])
def test_variant_vocabulary_matches_existing_dispatch_source(source):
    tree = ast.parse(
        (ROOT / "src/digitalmodel/workflows/parametric_run.py").read_text()
    )
    loader = next(
        node
        for node in tree.body
        if isinstance(node, ast.FunctionDef) and node.name == "_load_cases"
    )
    actual = {
        node.comparators[0].value
        for node in ast.walk(loader)
        if isinstance(node, ast.Compare)
        and isinstance(node.left, ast.Name)
        and node.left.id == "source"
    }
    assert actual == {"factorial", "range", "csv", "yaml_matrix"}
    catalog = descriptor()
    catalog["families"][0]["variants"]["source"] = source
    assert validate(catalog)["pending_family_ids"] == ["synthetic"]


@pytest.mark.parametrize(
    "value",
    [None, [], {}, {"schema_version": 1}, {"schema_version": 1, "families": []}],
)
def test_missing_catalog_structure_is_rejected(value):
    with pytest.raises(ValueError):
        validate(value)


@pytest.mark.parametrize("version", [True, "1", 1.0, 2, None])
def test_unknown_or_coerced_schema_is_rejected(version):
    catalog = descriptor()
    catalog["schema_version"] = version
    with pytest.raises(ValueError, match="schema_version"):
        validate(catalog)


def test_duplicate_family_is_rejected():
    catalog = descriptor()
    catalog["families"] *= 2
    with pytest.raises(ValueError, match="duplicate"):
        validate(catalog)


@pytest.mark.parametrize("status", ["ready", "approved", None, True])
def test_readiness_claim_is_rejected(status):
    catalog = descriptor()
    catalog["families"][0]["status"] = status
    with pytest.raises(ValueError, match="pending"):
        validate(catalog)


@pytest.mark.parametrize("field", list(descriptor()["families"][0]))
def test_missing_family_field_is_rejected(field):
    catalog = descriptor()
    del catalog["families"][0][field]
    with pytest.raises(ValueError):
        validate(catalog)


@pytest.mark.parametrize(
    "field,value",
    [
        ("id", " "),
        ("workflow", 7),
        ("vessel_id", []),
        ("base_input", ""),
        ("axes", []),
        ("axes", [{"name": "heading", "unit": ""}]),
        ("axes", [{"name": "heading", "unit": "deg"}] * 2),
        ("axes", [{"name": "heading", "unit": "deg", "extra": 1}]),
        ("variants", {"source": "unknown"}),
        ("variants", {"source": []}),
        ("variants", {"source": "factorial", "extra": 1}),
        ("variants", "factorial"),
        ("grid", []),
        ("grid", {}),
        ("batch_size", 0),
        ("batch_size", -1),
        ("batch_size", True),
        ("batch_size", 1.5),
        ("batch_size", "2"),
        ("outputs", []),
        ("outputs", [""]),
        ("outputs", "summary"),
        ("consumer", " "),
        ("prerequisites", []),
        ("prerequisites", [{"issue": True, "reason": "Review"}]),
        ("prerequisites", [{"issue": 0, "reason": "Review"}]),
        ("prerequisites", [{"issue": 1557, "reason": ""}]),
        ("prerequisites", [{"issue": 1557, "reason": "Review"}] * 2),
        ("prerequisites", [{"issue": 1557, "reason": "Review", "extra": 1}]),
        ("unresolved", []),
    ],
)
def test_malformed_family_is_rejected_without_mutation(field, value):
    catalog = descriptor()
    catalog["families"][0][field] = value
    before = deepcopy(catalog)
    with pytest.raises(ValueError):
        validate(catalog)
    assert catalog == before


@pytest.mark.parametrize("field", ["vessel_id", "base_input", "grid", "batch_size"])
def test_unknown_value_requires_reason(field):
    catalog = descriptor()
    family = catalog["families"][0]
    family[field] = None
    with pytest.raises(ValueError, match="unresolved"):
        validate(catalog)
    family["unresolved"][field] = "Awaiting reviewed input"
    assert validate(catalog)["pending_family_ids"] == ["synthetic"]
    family["unresolved"][field] = " "
    with pytest.raises(ValueError):
        validate(catalog)


@pytest.mark.parametrize("where", ["catalog", "family", "unresolved"])
def test_unknown_fields_are_rejected(where):
    catalog = descriptor()
    targets = {
        "catalog": catalog,
        "family": catalog["families"][0],
        "unresolved": catalog["families"][0]["unresolved"],
    }
    targets[where]["extra"] = "Unrecognized field"
    with pytest.raises(ValueError):
        validate(catalog)


def test_resolved_value_cannot_keep_an_unresolved_reason():
    catalog = descriptor()
    catalog["families"][0]["unresolved"]["batch_size"] = "Stale reason"
    with pytest.raises(ValueError, match="unresolved"):
        validate(catalog)
