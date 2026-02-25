# ABOUTME: Integration tests for unit-checked plate buckling calculations.
# ABOUTME: Verifies TrackedQuantity inputs, auto-conversion, error handling, and provenance.

"""Integration tests for @unit_checked decorators on ElasticBucklingCalculator.

Tests that TrackedQuantity inputs produce identical numerical results to raw
floats, that unit conversions happen automatically (e.g. psi→Pa, mm→m), that
dimensionality mismatches raise errors, and that provenance chains are
preserved through calculations.
"""

import math
import sys
from pathlib import Path

import pytest
from pint.errors import DimensionalityError

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from assetutilities.units import TrackedQuantity, get_registry
from digitalmodel.infrastructure.calculations.plate_buckling import (
    ElasticBucklingCalculator,
    PlateEdgeCondition,
)


@pytest.fixture
def calc():
    return ElasticBucklingCalculator()


@pytest.fixture
def steel_params_raw():
    """Raw float parameters in SI units."""
    return {
        "youngs_modulus": 210e9,
        "poisson_ratio": 0.3,
        "thickness": 0.012,
        "breadth": 0.8,
        "length": 3.0,
    }


@pytest.fixture
def steel_params_tracked(steel_params_raw):
    """TrackedQuantity parameters in SI units."""
    return {
        "youngs_modulus": TrackedQuantity(steel_params_raw["youngs_modulus"], "Pa", source="test"),
        "poisson_ratio": steel_params_raw["poisson_ratio"],
        "thickness": TrackedQuantity(steel_params_raw["thickness"], "m", source="test"),
        "breadth": TrackedQuantity(steel_params_raw["breadth"], "m", source="test"),
        "length": TrackedQuantity(steel_params_raw["length"], "m", source="test"),
    }


# ---------------------------------------------------------------------------
# 1. TrackedQuantity inputs produce same results as raw floats
# ---------------------------------------------------------------------------


class TestTrackedMatchesRaw:
    """TrackedQuantity inputs must yield identical numerical results to raw floats."""

    def test_base_factor_matches(self, calc, steel_params_raw, steel_params_tracked):
        raw_result = calc.calculate_base_factor(
            steel_params_raw["youngs_modulus"],
            steel_params_raw["poisson_ratio"],
            steel_params_raw["thickness"],
            steel_params_raw["breadth"],
        )
        tracked_result = calc.calculate_base_factor(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
        )
        # base_factor has no _return, so tracked still returns float
        assert raw_result == pytest.approx(tracked_result, rel=1e-10)

    def test_longitudinal_stress_matches(self, calc, steel_params_raw, steel_params_tracked):
        raw_result = calc.calculate_longitudinal_buckling_stress(
            steel_params_raw["youngs_modulus"],
            steel_params_raw["poisson_ratio"],
            steel_params_raw["thickness"],
            steel_params_raw["breadth"],
            steel_params_raw["length"],
        )
        tracked_result = calc.calculate_longitudinal_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert raw_result == pytest.approx(tracked_result.magnitude, rel=1e-10)

    def test_transverse_stress_matches(self, calc, steel_params_raw, steel_params_tracked):
        raw_result = calc.calculate_transverse_buckling_stress(
            steel_params_raw["youngs_modulus"],
            steel_params_raw["poisson_ratio"],
            steel_params_raw["thickness"],
            steel_params_raw["breadth"],
            steel_params_raw["length"],
        )
        tracked_result = calc.calculate_transverse_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert raw_result == pytest.approx(tracked_result.magnitude, rel=1e-10)

    def test_shear_stress_matches(self, calc, steel_params_raw, steel_params_tracked):
        raw_result = calc.calculate_shear_buckling_stress(
            steel_params_raw["youngs_modulus"],
            steel_params_raw["poisson_ratio"],
            steel_params_raw["thickness"],
            steel_params_raw["breadth"],
            steel_params_raw["length"],
        )
        tracked_result = calc.calculate_shear_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert raw_result == pytest.approx(tracked_result.magnitude, rel=1e-10)


# ---------------------------------------------------------------------------
# 2. Auto-conversion from compatible units (psi→Pa, mm→m)
# ---------------------------------------------------------------------------


class TestAutoConversion:
    """Inputs in compatible but non-SI units auto-convert to the expected unit."""

    def test_psi_converts_to_pa(self, calc):
        """Young's modulus in psi auto-converts to Pa."""
        E_pa = 210e9
        E_psi = E_pa / 6894.757293168
        result_pa = calc.calculate_longitudinal_buckling_stress(
            TrackedQuantity(E_pa, "Pa", source="test"),
            0.3,
            TrackedQuantity(0.012, "m", source="test"),
            TrackedQuantity(0.8, "m", source="test"),
            TrackedQuantity(3.0, "m", source="test"),
        )
        result_psi = calc.calculate_longitudinal_buckling_stress(
            TrackedQuantity(E_psi, "psi", source="test"),
            0.3,
            TrackedQuantity(0.012, "m", source="test"),
            TrackedQuantity(0.8, "m", source="test"),
            TrackedQuantity(3.0, "m", source="test"),
        )
        assert result_pa.magnitude == pytest.approx(result_psi.magnitude, rel=1e-6)

    def test_mm_converts_to_m(self, calc):
        """Thickness in mm auto-converts to m."""
        result_m = calc.calculate_longitudinal_buckling_stress(
            TrackedQuantity(210e9, "Pa", source="test"),
            0.3,
            TrackedQuantity(0.012, "m", source="test"),
            TrackedQuantity(0.8, "m", source="test"),
            TrackedQuantity(3.0, "m", source="test"),
        )
        result_mm = calc.calculate_longitudinal_buckling_stress(
            TrackedQuantity(210e9, "Pa", source="test"),
            0.3,
            TrackedQuantity(12.0, "mm", source="test"),
            TrackedQuantity(800.0, "mm", source="test"),
            TrackedQuantity(3000.0, "mm", source="test"),
        )
        assert result_m.magnitude == pytest.approx(result_mm.magnitude, rel=1e-6)


# ---------------------------------------------------------------------------
# 3. Unit mismatch raises DimensionalityError
# ---------------------------------------------------------------------------


class TestDimensionalityError:
    """Passing a unit with wrong dimensions raises DimensionalityError."""

    def test_meters_where_pa_expected(self, calc):
        """Passing meters for youngs_modulus (expects Pa) raises DimensionalityError."""
        with pytest.raises(DimensionalityError):
            calc.calculate_longitudinal_buckling_stress(
                TrackedQuantity(210e9, "m", source="test"),
                0.3,
                TrackedQuantity(0.012, "m", source="test"),
                TrackedQuantity(0.8, "m", source="test"),
                TrackedQuantity(3.0, "m", source="test"),
            )

    def test_pa_where_meters_expected(self, calc):
        """Passing Pa for thickness (expects m) raises DimensionalityError."""
        with pytest.raises(DimensionalityError):
            calc.calculate_longitudinal_buckling_stress(
                TrackedQuantity(210e9, "Pa", source="test"),
                0.3,
                TrackedQuantity(0.012, "Pa", source="test"),
                TrackedQuantity(0.8, "m", source="test"),
                TrackedQuantity(3.0, "m", source="test"),
            )


# ---------------------------------------------------------------------------
# 4. Provenance chain is preserved through calculations
# ---------------------------------------------------------------------------


class TestProvenance:
    """Output TrackedQuantity carries provenance from all tracked inputs."""

    def test_output_has_provenance(self, calc, steel_params_tracked):
        result = calc.calculate_longitudinal_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert isinstance(result, TrackedQuantity)
        assert len(result.provenance) > 0

    def test_provenance_includes_input_creation(self, calc, steel_params_tracked):
        result = calc.calculate_longitudinal_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        operations = [p.operation for p in result.provenance]
        # Should include "created" entries from input TrackedQuantities
        assert "created" in operations

    def test_provenance_includes_function_source(self, calc, steel_params_tracked):
        result = calc.calculate_longitudinal_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        sources = [p.source for p in result.provenance]
        # The final "created" entry should reference the qualified function name
        assert any("calculate_longitudinal_buckling_stress" in s for s in sources if s)

    def test_conversion_recorded_in_provenance(self, calc):
        """When auto-converting mm→m, the conversion appears in provenance."""
        result = calc.calculate_longitudinal_buckling_stress(
            TrackedQuantity(210e9, "Pa", source="test"),
            0.3,
            TrackedQuantity(12.0, "mm", source="test"),
            TrackedQuantity(800.0, "mm", source="test"),
            TrackedQuantity(3000.0, "mm", source="test"),
        )
        operations = [p.operation for p in result.provenance]
        assert "converted" in operations

    def test_base_factor_no_return_tracking(self, calc, steel_params_tracked):
        """calculate_base_factor has no _return, so it returns a plain float
        even when tracked inputs are provided (dimensionless ratio)."""
        result = calc.calculate_base_factor(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
        )
        # Should be a plain float, not a TrackedQuantity
        assert isinstance(result, float)


# ---------------------------------------------------------------------------
# 5. Output unit is correct
# ---------------------------------------------------------------------------


class TestOutputUnit:
    """Methods with _return='Pa' produce TrackedQuantity with pascal units."""

    def test_longitudinal_returns_pa(self, calc, steel_params_tracked):
        result = calc.calculate_longitudinal_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert isinstance(result, TrackedQuantity)
        assert str(result.units) == "pascal"

    def test_transverse_returns_pa(self, calc, steel_params_tracked):
        result = calc.calculate_transverse_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert isinstance(result, TrackedQuantity)
        assert str(result.units) == "pascal"

    def test_shear_returns_pa(self, calc, steel_params_tracked):
        result = calc.calculate_shear_buckling_stress(
            steel_params_tracked["youngs_modulus"],
            steel_params_tracked["poisson_ratio"],
            steel_params_tracked["thickness"],
            steel_params_tracked["breadth"],
            steel_params_tracked["length"],
        )
        assert isinstance(result, TrackedQuantity)
        assert str(result.units) == "pascal"
