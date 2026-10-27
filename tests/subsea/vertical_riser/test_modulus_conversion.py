# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline conversion regressions; minimal extras: pytest, pint and PyYAML.

Inputs are synthetic arithmetic probes, not engineering material data.
Load the production method's AST to avoid unrelated model/file-reader imports;
the method body and its Pint quantity constructor are used unchanged.
"""

import ast
from pathlib import Path

import pytest
import yaml

from digitalmodel.units import Q_


ROOT = next(parent for parent in Path(__file__).parents if (parent / "src").is_dir())
COMPONENTS = Path("src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py")
CONFIG = ROOT / "src/digitalmodel/base_configs/modules/vertical_riser/vertical_riser.yml"

# NIST SP 811, Appendix B.8, entries "inch" and "pound-force":
# https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8
# 1 in = 0.0254 m; 1 lbf = 4.448222 N (published rounded value).
# Hand derivation: psi = lbf/in**2; kN/m**2 = N/m**2 / 1000.
METRES_PER_INCH = 0.0254
NEWTONS_PER_POUND_FORCE = 4.448222
NEWTONS_PER_KILONEWTON = 1000
PSI_TO_KPA = NEWTONS_PER_POUND_FORCE / METRES_PER_INCH**2 / NEWTONS_PER_KILONEWTON
# The existing stress factor is about two parts per million below this hand
# calculation. Allow three parts per million, without changing that factor.
STRESS_REL_TOL = 3e-6


@pytest.fixture(scope="module")
def convert():
    """Execute the actual method with real Pint and no model preparation."""
    tree = ast.parse((ROOT / COMPONENTS).read_text(encoding="utf-8"))
    riser = next(
        node for node in tree.body
        if isinstance(node, ast.ClassDef) and node.name == "VerticalRiser"
    )
    method = next(
        node for node in riser.body
        if isinstance(node, ast.FunctionDef)
        and node.name == "update_property_dictionary_to_si_units"
    )
    namespace = {"Q_": Q_}
    exec(compile(ast.Module(body=[method], type_ignores=[]), str(COMPONENTS), "exec"), namespace)
    conversion = namespace[method.name]
    return lambda properties: conversion(None, properties)


def properties(modulus_psi):
    """Use unit-valued geometry/rigidities and a synthetic yield strength."""
    values = dict.fromkeys(
        ("A", "Ai", "Ao", "I", "Io", "Ii", "J", "Jo", "Ji", "EI", "EA", "GJ"),
        1.0,
    )
    values.update(E=modulus_psi, SMYS=50000.0, SMUS=60000.0, label="synthetic")
    return values


# Unit psi, kilo-psi and a synthetic steel-scale modulus exercise the same law.
@pytest.mark.parametrize("source_e", [1.0, 1000.0, 30000000.0])
def test_modulus_conversion_is_linear(convert, source_e):
    """Doubling a source modulus must double the converted modulus."""
    single = convert(properties(source_e))
    double = convert(properties(2 * source_e))
    assert double["E"] == pytest.approx(2 * single["E"])


@pytest.mark.parametrize("source_e", [1000.0, 30000000.0])
def test_modulus_and_strength_share_declared_stress_units(convert, source_e):
    """Compare E and yield strength independently against psi -> kPa."""
    # Caller get_fea_properties passes PipeSizing's E and SMYS through directly.
    units = yaml.safe_load(CONFIG.read_text(encoding="utf-8"))["default"]["units"]
    assert units["E"] == units["YieldStrength"] == "psi"
    # OrcaflexModelComponents.GetGeneral selects SI; lineTypes forwards E/SMYS.
    # The SI stress convention kN/m**2 = kPa is documented by Pipeline's
    # youngs_modulus in solvers/orcaflex/modular_generator/schema/pipeline.py.
    source = properties(source_e)
    expected_e = source["E"] * PSI_TO_KPA
    expected_yield = source["SMYS"] * PSI_TO_KPA
    converted = convert(source)
    assert converted["E"] == pytest.approx(expected_e, rel=STRESS_REL_TOL)
    assert converted["SMYS"] == pytest.approx(expected_yield, rel=STRESS_REL_TOL)


def test_other_property_conversions_are_preserved(convert):
    """Retain geometric/rigidity dimensions, extra fields and in-place updates."""
    source = properties(1000.0)
    converted = convert(source)
    assert converted is source
    for key in ("A", "Ai", "Ao"):
        assert converted[key] == pytest.approx(METRES_PER_INCH**2)
    for key in ("I", "Io", "Ii", "J", "Jo", "Ji"):
        assert converted[key] == pytest.approx(METRES_PER_INCH**4)
    # Unit-valued source rigidities: psi*in**2 -> kN and psi*in**4 -> kN*m**2.
    assert converted["EA"] == pytest.approx(
        PSI_TO_KPA * METRES_PER_INCH**2, rel=STRESS_REL_TOL
    )
    for key in ("EI", "GJ"):
        assert converted[key] == pytest.approx(
            PSI_TO_KPA * METRES_PER_INCH**4, rel=STRESS_REL_TOL
        )
    assert converted["label"] == "synthetic"
