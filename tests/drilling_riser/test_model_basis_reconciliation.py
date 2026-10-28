"""Synthetic C.6 evidence and independent arithmetic; no design values.

Source: the synthetic example defined here. Elevations span 10 to 0 m,
density is 8 kg/m^3, and the supplied diameter and stiffness are 2 m and
30 N m^2. Basis alternatives are chosen solely to exercise subtraction:
2 - 2.5 = -0.5 m; 30 - 20 = 10 N m^2; abs(-0.5) = 0.5 m.
Zero, unchanged and missing evidence cases are mathematical boundary cases.
"""

import hashlib
import json
import math
from dataclasses import replace

import pytest

from digitalmodel.drilling_riser.model_data_appendix import (
    ComponentSnapshot,
    build_appendix,
)


MODEL_BYTES = b"synthetic reconciliation evidence"
DIGEST = hashlib.sha256(MODEL_BYTES).hexdigest()


def component(**changes):
    """Return the explicit model side of the synthetic example above."""
    properties = {"stress_od_m": 2.0, "ei_n_m2": 30.0}
    properties.update(changes)
    return ComponentSnapshot(
        "upper", 10.0, 0.0, DIGEST, "synthetic model elevations",
        properties, {name: "synthetic model input" for name in properties},
    )


def basis(diameter=2.5, stiffness=20.0):
    """Return separately labelled basis entries, keyed by exact identities."""
    return {"upper": {
        "stress_od_m": {"value": diameter, "units": "m",
                        "source": "synthetic diameter basis"},
        "ei_n_m2": {"value": stiffness, "units": "N m^2",
                    "source": "synthetic stiffness basis"},
    }}


def build(design_basis, item=None):
    """Use only the synthetic inputs declared in the module example."""
    return build_appendix(
        MODEL_BYTES, [component() if item is None else item],
        water_density_kg_m3=8.0,
        water_density_source="synthetic density input",
        design_basis=design_basis,
    )


def rows(result):
    """Index the returned rows without deriving any expected quantity."""
    return {(row["component_id"], row["property_id"]): row
            for row in result["tables"]["C.6"]["rows"]}


def test_altered_basis_detected():
    unchanged = rows(build(basis(diameter=2.0, stiffness=30.0)))
    altered = rows(build(basis()))
    for name in ("stress_od_m", "ei_n_m2"):
        assert unchanged["upper", name]["absolute_difference"] == 0.0
        assert unchanged["upper", name]["signed_difference"] == 0.0
        assert unchanged["upper", name]["status"] == "CALCULATED"
        assert altered["upper", name]["absolute_difference"] > 0.0
        assert altered["upper", name]["status"] == "CALCULATED"


def test_signed_difference_by_hand():
    """Model minus basis: 2-2.5=-0.5; 30-20=10; absolute 0.5 and 10."""
    table = rows(build(basis()))
    diameter, stiffness = table["upper", "stress_od_m"], table["upper", "ei_n_m2"]
    assert diameter["model_value"] == 2.0
    assert diameter["basis_value"] == 2.5
    assert diameter["units"] == "m"
    assert diameter["signed_difference"] == -0.5
    assert diameter["absolute_difference"] == 0.5
    assert diameter["model_source"] == "synthetic model input"
    assert diameter["basis_source"] == "synthetic diameter basis"
    assert stiffness["signed_difference"] == 10.0
    assert stiffness["absolute_difference"] == 10.0
    assert stiffness["units"] == "N m^2"


def test_zero_basis_uses_subtraction_without_division():
    """30-0=30 N m^2; an actual zero is evidence, not an absent value."""
    row = rows(build(basis(stiffness=0.0)))["upper", "ei_n_m2"]
    assert row["basis_value"] == 0.0
    assert row["signed_difference"] == 30.0
    assert row["absolute_difference"] == 30.0
    assert row["status"] == "CALCULATED"


def test_zero_model_and_basis_are_valid_evidence():
    """Stiffness 0-0=0 N m^2 is distinct from an unknown difference."""
    row = rows(build(basis(stiffness=0.0), component(ei_n_m2=0.0)))["upper", "ei_n_m2"]
    assert row["model_value"] == 0.0
    assert row["basis_value"] == 0.0
    assert row["signed_difference"] == 0.0
    assert row["absolute_difference"] == 0.0
    assert row["status"] == "CALCULATED"
    assert row["missing_evidence"] == []


@pytest.mark.parametrize("design_basis", [{}, {"upper": {}}, basis(diameter=None)])
def test_missing_basis_not_evaluated(design_basis):
    row = rows(build(design_basis))["upper", "stress_od_m"]
    assert row["model_value"] == 2.0
    assert row["basis_value"] is None
    assert row["signed_difference"] is None
    assert row["absolute_difference"] is None
    assert row["status"] == "NOT_EVALUATED"
    assert row["missing_evidence"]


def test_missing_model_value_is_not_evaluated():
    row = rows(build(basis(), component(stress_od_m=None)))["upper", "stress_od_m"]
    assert row["model_value"] is None
    assert row["basis_value"] == 2.5
    assert row["signed_difference"] is None
    assert row["absolute_difference"] is None
    assert row["status"] == "NOT_EVALUATED"
    assert "model_value" in row["missing_evidence"]


def test_missing_components_are_not_matched_by_position():
    supplied = {"lower": basis()["upper"]}
    table = rows(build(supplied))
    model_only = table["upper", "stress_od_m"]
    basis_only = table["lower", "stress_od_m"]
    assert model_only["basis_source"] is None
    assert model_only["basis_value"] is None
    assert "basis_component" in model_only["missing_evidence"]
    assert basis_only["model_source"] is None
    assert basis_only["model_value"] is None
    assert basis_only["basis_value"] == 2.5
    assert "model_component" in basis_only["missing_evidence"]
    for row in (model_only, basis_only):
        assert row["signed_difference"] is None
        assert row["absolute_difference"] is None
        assert row["status"] == "NOT_EVALUATED"


def test_property_identity_cannot_be_matched_by_units():
    """Hydrodynamic diameter is distinct from stress diameter despite metres."""
    supplied = {"upper": {"hydrodynamic_diameter_m": basis()["upper"]["stress_od_m"]}}
    table = rows(build(supplied))
    assert table["upper", "stress_od_m"]["basis_value"] is None
    row = table["upper", "hydrodynamic_diameter_m"]
    assert row["basis_value"] == 2.5
    assert row["model_value"] is None
    assert row["status"] == "NOT_EVALUATED"
    assert row["signed_difference"] is None


@pytest.mark.parametrize("units", ["mm", "N", "", None])
def test_units_cannot_be_relabelled(units):
    supplied = basis()
    supplied["upper"]["stress_od_m"]["units"] = units
    with pytest.raises(ValueError, match="units"):
        build(supplied)


@pytest.mark.parametrize("name", ["units", "source", "value"])
def test_incomplete_basis_evidence_is_rejected(name):
    supplied = basis()
    del supplied["upper"]["stress_od_m"][name]
    with pytest.raises(ValueError, match="basis"):
        build(supplied)


@pytest.mark.parametrize("source", ["", " ", None, math.nan])
def test_invalid_basis_source_is_rejected(source):
    supplied = basis()
    supplied["upper"]["stress_od_m"]["source"] = source
    with pytest.raises(ValueError, match="source"):
        build(supplied)


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, True, "2", -1.0])
def test_invalid_basis_number_is_rejected(value):
    with pytest.raises(ValueError, match="stress_od_m"):
        build(basis(diameter=value))


@pytest.mark.parametrize("supplied", [
    {"": basis()["upper"]},
    {"upper": {"Stress_OD": basis()["upper"]["stress_od_m"]}},
    {"upper": None},
    {"upper": {"stress_od_m": None}},
    [],
])
def test_invalid_basis_identity_or_structure_is_rejected(supplied):
    with pytest.raises(ValueError, match="basis"):
        build(supplied)


def test_model_digest_retained():
    result = build(basis())
    assert result["model_sha256"] == DIGEST
    assert DIGEST in result["tables"]["C.6"]["caption"]
    assert result["model_extraction_status"] == "NOT_EVALUATED"
    with pytest.raises(ValueError, match="digest"):
        build(basis(), replace(component(), model_sha256="mismatched synthetic digest"))


def test_strict_json_and_input_detachment():
    item, supplied = component(), basis()
    result = build(supplied, item)
    before = json.loads(json.dumps(result, allow_nan=False))
    # Use the unchanged example values to mutate both caller-owned mappings.
    item.properties["stress_od_m"] = 2.5
    item.sources["stress_od_m"] = "changed model source"
    supplied["upper"]["stress_od_m"]["value"] = 2.0
    supplied["upper"]["stress_od_m"]["source"] = "changed basis source"
    supplied.clear()
    assert result == before
    rows(result)["upper", "ei_n_m2"]["model_value"] = 20.0
    assert item.properties["ei_n_m2"] == 30.0
    missing = build({})
    assert json.loads(json.dumps(missing, allow_nan=False)) == missing


def test_output_does_not_mutate_basis_or_other_tables():
    """Use the example's unchanged diameter to change output evidence only."""
    supplied = basis()
    result = build(supplied)
    rows(result)["upper", "stress_od_m"]["basis_value"] = 2.0
    rows(result)["upper", "stress_od_m"]["model_source"] = "changed output source"
    assert supplied["upper"]["stress_od_m"]["value"] == 2.5
    assert result["tables"]["C.1"]["rows"][0]["sources"]["stress_od_m"] == (
        "synthetic model input"
    )


def test_row_order_is_independent_of_basis_mapping_order():
    """Two identical components meet at 0 m; the lower spans 0 to -10 m."""
    upper = component()
    lower = replace(upper, component_id="lower", top_elevation_m=0.0,
                    bottom_elevation_m=-10.0)
    supplied = {"lower": basis()["upper"], "upper": basis()["upper"]}
    result = build_appendix(
        MODEL_BYTES, [lower, upper], water_density_kg_m3=8.0,
        water_density_source="synthetic density input", design_basis=supplied,
    )
    actual = result["tables"]["C.6"]["rows"]
    assert list(dict.fromkeys(row["component_id"] for row in actual)) == [
        "upper", "lower",
    ]
    assert rows(result)["lower", "ei_n_m2"]["signed_difference"] == 10.0


def test_optional_reconciliation_preserves_existing_tables():
    previous = build(None)
    reconciled = build(basis())
    assert set(previous["tables"]) == {"C.1", "C.2", "C.3"}
    for name, table in previous["tables"].items():
        assert reconciled["tables"][name] == table
    assert any("C.6" in item for item in previous["not_evaluated"])
    assert not any("C.6" in item for item in reconciled["not_evaluated"])
    for marker in ("C.4", "C.5", "authenticity", "figure"):
        assert any(marker in item for item in reconciled["not_evaluated"])


def test_signed_wet_mass_and_difference_overflow():
    """Signed wet masses -2 and -2.5 differ by +0.5 kg/m.

    Opposite finite extremes +/-1e308 overflow subtraction and are rejected.
    """
    supplied = {"upper": {"wet_mass_kg_m": {
        "value": -2.5, "units": "kg/m", "source": "synthetic wet mass basis",
    }}}
    row = rows(build(supplied, component(wet_mass_kg_m=-2.0)))["upper", "wet_mass_kg_m"]
    assert row["signed_difference"] == 0.5
    assert row["absolute_difference"] == 0.5
    supplied["upper"]["wet_mass_kg_m"]["value"] = -1e308
    with pytest.raises(ValueError, match="finite"):
        build(supplied, component(wet_mass_kg_m=1e308))
