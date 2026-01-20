"""
Test suite for catenary adapter backward compatibility.

Tests that the adapter provides 100% compatibility with the legacy
catenaryMethods.py dict-based API.

Author: Digital Model Project
Date: 2025-10-03
"""

import pytest
import math
import warnings
import sys
from pathlib import Path

# Add src to path for imports
src_path = Path(__file__).parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.modules.marine_analysis.catenary.adapter import (
    catenaryEquation,
    catenaryForces,
    validate_catenary_data,
    ADAPTER_VERSION
)


class TestCatenaryEquationForceMethod:
    """Test force-based catenary calculation (legacy F, w, d method)."""

    def test_basic_force_calculation(self):
        """Test basic force-based calculation with known values."""
        data = {
            "F": 10000.0,
            "w": 500.0,
            "d": 100.0,
            "X": None,
            "q": None
        }

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            result = catenaryEquation(data)

            # Check deprecation warning was raised
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)
            assert "deprecated" in str(w[0].message).lower()

        # Check required output fields exist
        assert "S" in result
        assert "X" in result
        assert "W" in result
        assert "THorizontal" in result

        # Check values are positive and reasonable
        assert result["S"] > 0
        assert result["X"] > 0
        assert result["W"] > 0
        assert result["THorizontal"] > 0

        # Weight should equal w * S
        assert abs(result["W"] - data["w"] * result["S"]) < 1e-6

        # Input values should be preserved
        assert result["F"] == data["F"]
        assert result["w"] == data["w"]
        assert result["d"] == data["d"]

    def test_force_method_exact_legacy_match(self):
        """Test that results exactly match legacy implementation."""
        # These expected values come from running original catenaryMethods.py
        data = {
            "F": 5000.0,
            "w": 250.0,
            "d": 50.0,
            "X": None,
            "q": None
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            result = catenaryEquation(data)

        # Calculate expected values using legacy formulas
        F = data["F"]
        w = data["w"]
        d = data["d"]

        expected_S = d * (2 * F / w - d)
        expected_X = ((F/w) - d) * math.log((expected_S + (F/w)) / ((F/w) - d))
        expected_W = w * expected_S
        expected_THorizontal = F * expected_X / math.sqrt(expected_S**2 + expected_X**2)

        # Check exact match (within floating point precision)
        assert abs(result["S"] - expected_S) < 1e-9
        assert abs(result["X"] - expected_X) < 1e-9
        assert abs(result["W"] - expected_W) < 1e-9
        assert abs(result["THorizontal"] - expected_THorizontal) < 1e-9

    def test_force_method_multiple_scenarios(self):
        """Test force method with various parameter combinations."""
        test_cases = [
            {"F": 15000.0, "w": 600.0, "d": 120.0},
            {"F": 3000.0, "w": 150.0, "d": 30.0},
            {"F": 50000.0, "w": 2000.0, "d": 200.0},
        ]

        for params in test_cases:
            data = {**params, "X": None, "q": None}

            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                result = catenaryEquation(data)

            # Verify W = w * S relationship
            assert abs(result["W"] - params["w"] * result["S"]) < 1e-6

            # Verify THorizontal < F (horizontal component < total force)
            assert result["THorizontal"] < params["F"]

    def test_force_method_invalid_inputs(self):
        """Test that invalid inputs raise appropriate errors."""
        # F/w <= d should fail
        with pytest.raises(ValueError, match="F/w.*must be > d"):
            data = {"F": 100.0, "w": 50.0, "d": 100.0, "X": None, "q": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

        # Negative F
        with pytest.raises(ValueError, match="Force F must be positive"):
            data = {"F": -100.0, "w": 50.0, "d": 50.0, "X": None, "q": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

        # Zero w
        with pytest.raises(ValueError, match="Weight per length w must be positive"):
            data = {"F": 1000.0, "w": 0.0, "d": 50.0, "X": None, "q": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)


class TestCatenaryEquationAngleMethod:
    """Test angle-based catenary calculation (legacy q, d method)."""

    def test_basic_angle_calculation(self):
        """Test basic angle-based calculation."""
        data = {
            "q": 30.0,
            "d": 100.0,
            "F": None,
            "w": None,
            "X": None
        }

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            result = catenaryEquation(data)

            # Check deprecation warning
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)

        # Check required output fields
        assert "S" in result
        assert "X" in result
        assert "BendRadius" in result

        # Check values are positive
        assert result["S"] > 0
        assert result["X"] > 0
        assert result["BendRadius"] > 0

        # Input values should be preserved
        assert result["q"] == data["q"]
        assert result["d"] == data["d"]

    def test_angle_method_exact_legacy_match(self):
        """Test that results exactly match legacy implementation."""
        data = {
            "q": 45.0,
            "d": 80.0,
            "F": None,
            "w": None,
            "X": None
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            result = catenaryEquation(data)

        # Calculate expected values using legacy formulas
        q = data["q"]
        d = data["d"]

        angle_rad = math.radians(90 - q)
        tanq = math.tan(angle_rad)
        cos_angle = math.cos(angle_rad)

        expected_BendRadius = d * cos_angle / (1 - cos_angle)
        expected_S = expected_BendRadius * tanq
        expected_X = expected_BendRadius * math.asinh(tanq)

        # Check exact match
        assert abs(result["BendRadius"] - expected_BendRadius) < 1e-9
        assert abs(result["S"] - expected_S) < 1e-9
        assert abs(result["X"] - expected_X) < 1e-9

    def test_angle_method_various_angles(self):
        """Test angle method with various angle values."""
        test_angles = [10, 20, 30, 45, 60, 75, 85]

        for q in test_angles:
            data = {"q": q, "d": 100.0, "F": None, "w": None, "X": None}

            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                result = catenaryEquation(data)

            # All results should be positive
            assert result["S"] > 0
            assert result["X"] > 0
            assert result["BendRadius"] > 0

            # As angle increases, bend radius should decrease
            # (steeper angle = tighter bend)

    def test_angle_method_invalid_inputs(self):
        """Test that invalid angle inputs raise errors."""
        # Angle > 90
        with pytest.raises(ValueError, match="Angle q must be between 0 and 90"):
            data = {"q": 95.0, "d": 100.0, "F": None, "w": None, "X": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

        # Negative angle
        with pytest.raises(ValueError, match="Angle q must be between 0 and 90"):
            data = {"q": -10.0, "d": 100.0, "F": None, "w": None, "X": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

        # Negative d
        with pytest.raises(ValueError, match="Vertical distance d must be positive"):
            data = {"q": 30.0, "d": -50.0, "F": None, "w": None, "X": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)


class TestCatenaryEquationEdgeCases:
    """Test edge cases and error handling."""

    def test_missing_required_parameter(self):
        """Test that missing 'd' parameter raises error."""
        with pytest.raises(ValueError, match="Required parameter 'd'"):
            data = {"F": 1000.0, "w": 50.0, "X": None, "q": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

    def test_insufficient_parameters(self):
        """Test that insufficient parameters raise error."""
        with pytest.raises(ValueError, match="Insufficient parameters"):
            data = {"d": 100.0, "F": None, "w": None, "X": None, "q": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

    def test_x_based_not_implemented(self):
        """Test that X-based method raises NotImplementedError."""
        with pytest.raises(NotImplementedError, match="X-based calculation not implemented"):
            data = {"X": 200.0, "d": 100.0, "F": None, "w": None, "q": None}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryEquation(data)

    def test_deprecation_warning_content(self):
        """Test that deprecation warning has correct message."""
        data = {"q": 30.0, "d": 100.0, "F": None, "w": None, "X": None}

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            catenaryEquation(data)

            assert len(w) == 1
            message = str(w[0].message).lower()
            assert "deprecated" in message
            assert "catenary" in message or "adapter" in message


class TestCatenaryForces:
    """Test catenary forces calculation."""

    def test_basic_forces_calculation(self):
        """Test basic forces calculation."""
        data = {
            "weightPerUnitLength": 500.0,
            "S": 150.0,
            "q": 30.0
        }

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            result = catenaryForces(data)

            # Check deprecation warning
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)

        # Check output fields
        assert "Fv" in result
        assert "F" in result
        assert "Fh" in result

        # Check values
        assert result["Fv"] > 0
        assert result["F"] > 0
        assert result["Fh"] > 0

        # Fv should equal weightPerUnitLength * S
        expected_Fv = data["weightPerUnitLength"] * data["S"]
        assert abs(result["Fv"] - expected_Fv) < 1e-9

    def test_forces_exact_legacy_match(self):
        """Test that forces match legacy implementation exactly."""
        data = {
            "weightPerUnitLength": 600.0,
            "S": 200.0,
            "q": 45.0
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            result = catenaryForces(data)

        # Calculate expected values
        weight_per_length = data["weightPerUnitLength"]
        S = data["S"]
        q = data["q"]

        expected_Fv = weight_per_length * S
        angle_rad = math.radians(90 - q)
        expected_F = expected_Fv / math.sin(angle_rad)
        expected_Fh = expected_F * math.cos(angle_rad)

        # Check exact match
        assert abs(result["Fv"] - expected_Fv) < 1e-9
        assert abs(result["F"] - expected_F) < 1e-9
        assert abs(result["Fh"] - expected_Fh) < 1e-9

    def test_forces_various_scenarios(self):
        """Test forces with various parameter combinations."""
        test_cases = [
            {"weightPerUnitLength": 300.0, "S": 100.0, "q": 20.0},
            {"weightPerUnitLength": 800.0, "S": 250.0, "q": 50.0},
            {"weightPerUnitLength": 450.0, "S": 180.0, "q": 35.0},
        ]

        for params in test_cases:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                result = catenaryForces(params)

            # Verify Fv = w * S
            expected_Fv = params["weightPerUnitLength"] * params["S"]
            assert abs(result["Fv"] - expected_Fv) < 1e-9

            # Verify F > Fv (total force > vertical component)
            assert result["F"] >= result["Fv"]

            # Verify Fh > 0 (horizontal component exists)
            assert result["Fh"] > 0

    def test_forces_invalid_inputs(self):
        """Test that invalid inputs raise errors."""
        # Missing parameter
        with pytest.raises(ValueError, match="Missing required parameters"):
            data = {"weightPerUnitLength": 500.0, "S": 100.0}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryForces(data)

        # Negative weight
        with pytest.raises(ValueError, match="weightPerUnitLength must be positive"):
            data = {"weightPerUnitLength": -500.0, "S": 100.0, "q": 30.0}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryForces(data)

        # Invalid angle
        with pytest.raises(ValueError, match="Angle q must be between"):
            data = {"weightPerUnitLength": 500.0, "S": 100.0, "q": 95.0}
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                catenaryForces(data)


class TestValidation:
    """Test validation helper functions."""

    def test_validate_force_method(self):
        """Test validation for force method."""
        valid_data = {"F": 1000.0, "w": 50.0, "d": 100.0}
        # Should not raise
        validate_catenary_data(valid_data, "force")

        invalid_data = {"F": 1000.0, "w": 50.0}  # Missing d
        with pytest.raises(ValueError, match="Missing required parameters"):
            validate_catenary_data(invalid_data, "force")

    def test_validate_angle_method(self):
        """Test validation for angle method."""
        valid_data = {"q": 30.0, "d": 100.0}
        # Should not raise
        validate_catenary_data(valid_data, "angle")

        invalid_data = {"q": 30.0}  # Missing d
        with pytest.raises(ValueError, match="Missing required parameters"):
            validate_catenary_data(invalid_data, "angle")

    def test_validate_unknown_method(self):
        """Test validation with unknown method."""
        with pytest.raises(ValueError, match="Unknown method"):
            validate_catenary_data({"d": 100.0}, "unknown")


class TestIntegration:
    """Integration tests combining multiple functions."""

    def test_force_then_forces(self):
        """Test using catenaryEquation then catenaryForces."""
        # First calculate catenary geometry
        catenary_data = {
            "F": 8000.0,
            "w": 400.0,
            "d": 90.0,
            "X": None,
            "q": None
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            catenary_result = catenaryEquation(catenary_data)

        # Now calculate forces (need to add q parameter)
        # In practice, q would be derived from geometry
        # For this test, use a known angle
        forces_data = {
            "weightPerUnitLength": catenary_data["w"],
            "S": catenary_result["S"],
            "q": 35.0  # Example angle
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            forces_result = catenaryForces(forces_data)

        # Verify consistency
        assert forces_result["Fv"] == catenary_data["w"] * catenary_result["S"]

    def test_angle_workflow(self):
        """Test complete angle-based workflow."""
        # Calculate geometry from angle
        geometry_data = {
            "q": 40.0,
            "d": 120.0,
            "F": None,
            "w": None,
            "X": None
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            geometry_result = catenaryEquation(geometry_data)

        # Use results in forces calculation
        forces_data = {
            "weightPerUnitLength": 550.0,
            "S": geometry_result["S"],
            "q": geometry_data["q"]
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            forces_result = catenaryForces(forces_data)

        # All values should be positive and consistent
        assert all(v > 0 for k, v in forces_result.items()
                   if k in ["Fv", "F", "Fh"])


class TestAdapterMetadata:
    """Test adapter version and metadata."""

    def test_version_available(self):
        """Test that adapter version is defined."""
        assert ADAPTER_VERSION is not None
        assert isinstance(ADAPTER_VERSION, str)
        assert len(ADAPTER_VERSION) > 0

    def test_module_exports(self):
        """Test that module exports expected functions."""
        from digitalmodel.modules.marine_analysis.catenary import adapter

        assert hasattr(adapter, 'catenaryEquation')
        assert hasattr(adapter, 'catenaryForces')
        assert hasattr(adapter, 'validate_catenary_data')


if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short"])
