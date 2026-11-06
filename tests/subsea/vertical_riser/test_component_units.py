"""Offline checks of the vertical-riser component's imperial-to-SI boundary.

Reference: NIST SP 811, Appendix B.8, entries for inch, pound-force, and
pound-force per square inch:
https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8

The reference gives 1 lbf = 4.448222 N, 1 inch = 2.54e-2 m, and
1 psi = 6.894757e3 Pa. The expected modulus uses force divided by area.
All property values below are synthetic test inputs, not material data.
"""

import ast
from pathlib import Path

import pytest

from digitalmodel.units import Q_


# Independent dimensional arithmetic: psi = lbf/in**2, kPa = kN/m**2.
LBF_TO_N = 4.448222
INCH_TO_M = 2.54e-2
PSI_TO_KPA = LBF_TO_N / INCH_TO_M**2 / 1e3
# The rounded NIST force entry has seven significant figures; 2e-7 bounds
# its rounding relative to the unit registry's more precise definitions.
REFERENCE_REL = 2e-7
# Existing stiffness/strength factors differ from NIST by less than 2 ppm.
LEGACY_REL = 2e-6


@pytest.fixture(scope="module")
def component():
    """Construct the source class without spreadsheet or builder imports."""
    source = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py"
    )
    tree = ast.parse(source.read_text(encoding="utf-8"), filename=str(source))
    definition = next(
        node for node in tree.body
        if isinstance(node, ast.ClassDef) and node.name == "VerticalRiser"
    )
    # Execute the actual class and real unit helper only. No model builder or
    # licensed binding is imported, and no spreadsheet is read.
    isolated = ast.Module(body=[definition], type_ignores=[])
    namespace = {"Q_": Q_}
    exec(compile(isolated, str(source), "exec"), namespace)
    return namespace["VerticalRiser"]({})


def property_dictionary(modulus_psi):
    """Return synthetic areas, inertias, stiffnesses and material properties."""
    return {
        "A": 2.0, "Ai": 3.0, "Ao": 5.0,
        "I": 7.0, "Ii": 11.0, "Io": 18.0,
        "J": 14.0, "Ji": 22.0, "Jo": 36.0,
        "EI": 70.0, "EA": 20.0, "GJ": 140.0,
        "E": modulus_psi, "SMYS": 40.0, "SMUS": 60.0,
        "MassPerUnitLength": 4.0, "PoissonRatio": 0.3,
        "geometry": {"synthetic": True},
    }


@pytest.mark.parametrize("modulus_psi", [0.0, 1.0, 1e3, 10e6, 30e6])
def test_component_modulus_units(component, modulus_psi):
    """Return E in kPa (kN/m squared), using independent SI arithmetic."""
    properties = property_dictionary(modulus_psi)
    converted = component.update_property_dictionary_to_si_units(properties)

    # PipeSizing passes Material.E through unchanged. The vertical-riser base
    # configuration declares E in psi; the SI builder consumes kN and m.
    expected_kpa = modulus_psi * PSI_TO_KPA
    assert converted["E"] == pytest.approx(expected_kpa, rel=REFERENCE_REL)
    assert isinstance(converted["E"], float)
    assert converted is properties  # Preserve the existing in-place API.


def test_elastic_modulus_converts_linearly(component):
    """Tripling an input modulus must triple the converted modulus."""
    modulus_psi = 10e6  # Synthetic reference value; no material claim.
    scale = 3.0  # Chosen proportionality probe.
    first = component.update_property_dictionary_to_si_units(
        property_dictionary(modulus_psi)
    )
    scaled = component.update_property_dictionary_to_si_units(
        property_dictionary(scale * modulus_psi)
    )

    assert scaled["E"] == pytest.approx(scale * first["E"], rel=REFERENCE_REL)
    assert first["E"] == pytest.approx(
        modulus_psi * PSI_TO_KPA, rel=REFERENCE_REL
    )


def test_other_component_properties_keep_existing_behavior(component):
    """Characterize neighboring conversions without changing their scope."""
    properties = property_dictionary(10e6)
    original = properties.copy()
    converted = component.update_property_dictionary_to_si_units(properties)

    for key in ("A", "Ai", "Ao"):
        assert converted[key] == pytest.approx(original[key] * INCH_TO_M**2)
    for key in ("I", "Ii", "Io", "J", "Ji", "Jo"):
        assert converted[key] == pytest.approx(original[key] * INCH_TO_M**4)
    for key, length_power in (("EI", 4), ("EA", 2), ("GJ", 4)):
        assert converted[key] == pytest.approx(
            original[key] * PSI_TO_KPA * INCH_TO_M**length_power,
            rel=LEGACY_REL,
        )
    assert converted["SMYS"] == pytest.approx(
        original["SMYS"] * PSI_TO_KPA, rel=LEGACY_REL
    )
    # SMUS is currently converted twice because the method aliases its input.
    # This characterizes existing behavior, not a dimensionally correct result;
    # correcting that separate strength defect is outside the modulus change.
    assert converted["SMUS"] == pytest.approx(
        original["SMUS"] * PSI_TO_KPA**2, rel=2 * LEGACY_REL
    )
    for key in ("MassPerUnitLength", "PoissonRatio", "geometry"):
        assert converted[key] == original[key]
    assert converted.keys() == original.keys()
