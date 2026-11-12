"""G4 evidence tests without package initialization or licensed API imports.

Source: diffraction/docs/agent-pack.md, revision 4, section 5, AQG r0
section 8 matrix. Numerical fixtures are synthetic independent hand calculations;
they are arithmetic examples, not qualified engineering data or acceptance limits.
"""

import importlib.util
import json
from pathlib import Path
import sys

import pytest


SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/hydrodynamics/diffraction/g4_acceptance.py"
)


@pytest.fixture(scope="module")
def g4():
    """Load only the numerical module; no diffraction package initializer."""
    spec = importlib.util.spec_from_file_location("g4_acceptance_test", SOURCE)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    try:
        spec.loader.exec_module(module)
        yield module
    finally:
        sys.modules.pop(spec.name, None)


def mass_evidence(g4, **changes):
    """Hand example: rho V = 900 kg, |1000 - 900| / 1000 = 0.1."""
    values = dict(
        mass_kg=1000.0,
        displacement_m3=0.9,
        water_density_kg_m3=1000.0,
        relative_tolerance=0.1,
        criterion_source="Synthetic hand example in this test, not an AQG limit",
        mass_source="Synthetic mass table",
        displacement_source="Synthetic independent volume calculation",
        density_source="Synthetic environment declaration",
    )
    return g4.FreeFlotationMassEvidence(**(values | changes))


@pytest.mark.parametrize("screen", [None, "PASS", "WARNING", "FAIL", "SKIPPED"])
def test_mesh_screen_never_establishes_g4(g4, screen):
    record = g4.assess_g4(mesh_screen_status=screen)
    assert record["status"] == "NOT_RUN"
    assert record["mesh_screen_status"] == screen
    assert record["assessment_scope"] == "AQG_G4_evidence"
    assert all(c["status"] == "NOT_RUN" for c in record["checks"].values())
    assert all(c["applicability"] == "NOT_ASSESSED" for c in record["checks"].values())


def test_every_documented_subcheck_remains_visible(g4):
    """Inventory independently transcribed from the cited revision 4 matrix."""
    expected = {
        "G4.1a",
        "G4.1b",
        "G4.2a",
        "G4.2b",
        "G4.3a",
        "G4.3b",
        "G4.3c",
        "G4.4a",
        "G4.4b",
        "G4.4c",
        "G4.4d",
        "G4.4e",
        "G4.5a",
        "G4.5b",
        "G4.5c",
        "G4.5d",
        "G4.6a",
        "G4.6b",
        "G4.6c",
        "G4.7a",
        "G4.7b",
        "G4.8a",
        "G4.8b",
        "G4.8c",
        "G4.8d",
        "G4.8e",
    }
    checks = g4.assess_g4()["checks"]
    assert checks.keys() == expected
    for check in checks.values():
        assert check["requirement_source"]
        assert check["required_evidence"]
        assert check["evidence"] == {}
        assert check["criterion"] is None
        assert check["reason"]


def test_mass_uses_input_mass_and_inclusive_limit(g4):
    """100 / 1000 = 0.1 passes; the legacy 100 / 900 would fail 0.1."""
    record = g4.assess_g4(mass_balance=mass_evidence(g4))
    check = record["checks"]["G4.1a"]
    assert check["status"] == "PASS"
    assert check["applicability"] == "APPLICABLE"
    assert check["measurement"] == {"value": pytest.approx(0.1), "unit": "1"}
    assert check["criterion"]["limit"] == 0.1
    assert check["criterion"]["unit"] == "1"
    assert check["evidence"]["displaced_mass_kg"] == 900.0
    assert check["evidence"]["mass_kg"] == 1000.0
    assert check["evidence"]["displacement_source"]
    assert record["status"] == "NOT_RUN"
    assert record["checks"]["G4.1b"]["status"] == "NOT_RUN"


def test_failed_mass_cannot_be_hidden_by_mesh_pass(g4):
    """|1000 - 800| / 1000 = 0.2 > supplied 0.1, irrespective of mesh."""
    record = g4.assess_g4(
        mesh_screen_status="PASS",
        mass_balance=mass_evidence(g4, displacement_m3=0.8),
    )
    assert record["checks"]["G4.1a"]["measurement"]["value"] == pytest.approx(0.2)
    assert record["checks"]["G4.1a"]["status"] == "FAIL"
    assert record["status"] == "FAIL"


def test_excess_buoyancy_uses_absolute_residual(g4):
    """rho V = 1100 kg; |1000 - 1100| / 1000 = 0.1, inclusive."""
    record = g4.assess_g4(mass_balance=mass_evidence(g4, displacement_m3=1.1))
    assert record["checks"]["G4.1a"]["status"] == "PASS"
    assert record["checks"]["G4.1a"]["measurement"]["value"] == pytest.approx(0.1)


def test_untyped_mass_evidence_rejected(g4):
    with pytest.raises(ValueError, match="mass_balance"):
        g4.assess_g4(mass_balance={"status": "PASS"})


def test_no_default_acceptance_limit_or_source(g4):
    with pytest.raises(TypeError, match="relative_tolerance"):
        g4.FreeFlotationMassEvidence(
            mass_kg=1000.0,
            displacement_m3=0.9,
            water_density_kg_m3=1000.0,
        )


@pytest.mark.parametrize("volume,expected", [(1.0, "PASS"), (0.9, "FAIL")])
def test_zero_tolerance_requires_equality(g4, volume, expected):
    """rho V = 1000 or 900 kg against 1000 kg; zero admits equality only."""
    record = g4.assess_g4(
        mass_balance=mass_evidence(
            g4,
            displacement_m3=volume,
            relative_tolerance=0.0,
        )
    )
    assert record["checks"]["G4.1a"]["status"] == expected


@pytest.mark.parametrize(
    "field",
    [
        "mass_kg",
        "displacement_m3",
        "water_density_kg_m3",
        "relative_tolerance",
    ],
)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1.0, True, "1"])
def test_invalid_numeric_evidence_rejected(g4, field, value):
    with pytest.raises(ValueError, match=field):
        mass_evidence(g4, **{field: value})


@pytest.mark.parametrize("field", ["mass_kg", "displacement_m3", "water_density_kg_m3"])
def test_zero_physical_inputs_rejected(g4, field):
    with pytest.raises(ValueError, match=field):
        mass_evidence(g4, **{field: 0.0})


@pytest.mark.parametrize(
    "field",
    [
        "criterion_source",
        "mass_source",
        "displacement_source",
        "density_source",
    ],
)
@pytest.mark.parametrize("value", ["", "  ", None])
def test_missing_source_rejected(g4, field, value):
    with pytest.raises(ValueError, match=field):
        mass_evidence(g4, **{field: value})


def test_nonfinite_derived_mass_rejected(g4):
    with pytest.raises(ValueError, match="displaced mass"):
        g4.assess_g4(
            mass_balance=mass_evidence(
                g4,
                displacement_m3=1e300,
                water_density_kg_m3=1e300,
            )
        )


def test_nonfinite_residual_rejected(g4):
    with pytest.raises(ValueError, match="relative residual"):
        g4.assess_g4(
            mass_balance=mass_evidence(
                g4,
                mass_kg=1e-300,
                displacement_m3=1e300,
                water_density_kg_m3=1.0,
            )
        )


def test_underflowed_displacement_rejected(g4):
    with pytest.raises(ValueError, match="displaced mass"):
        g4.assess_g4(
            mass_balance=mass_evidence(
                g4,
                displacement_m3=1e-300,
                water_density_kg_m3=1e-300,
            )
        )


def test_records_are_independent_and_json_serializable(g4):
    record = g4.assess_g4(mass_balance=mass_evidence(g4))
    assert json.loads(json.dumps(record, allow_nan=False)) == record
    record["checks"]["G4.7b"]["status"] = "PASS"
    assert g4.assess_g4()["checks"]["G4.7b"]["status"] == "NOT_RUN"


def test_unknown_screen_status_rejected(g4):
    with pytest.raises(ValueError, match="mesh_screen_status"):
        g4.assess_g4(mesh_screen_status="APPROVED")
