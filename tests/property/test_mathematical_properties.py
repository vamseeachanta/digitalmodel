"""
Property-based tests for mathematical operations in DigitalModel.

Uses Hypothesis to generate test cases and verify mathematical properties
that should hold for all valid inputs, following practices from Dropbox
and other companies that rely heavily on property-based testing.
"""
import pytest
import numpy as np
import pandas as pd
from hypothesis import given, strategies as st, settings, assume, example
from hypothesis.extra.numpy import arrays
from hypothesis.extra.pandas import data_frames, columns
import math
from typing import List, Union
from pathlib import Path
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

# Custom strategies for domain-specific data
@st.composite
def engineering_float(draw, min_value=-1e6, max_value=1e6, allow_nan=False, allow_infinity=False):
    """Generate engineering-realistic floating point numbers."""
    return draw(st.floats(
        min_value=min_value,
        max_value=max_value,
        allow_nan=allow_nan,
        allow_infinity=allow_infinity,
        width=64
    ))


@st.composite
def positive_engineering_float(draw, min_value=1e-10, max_value=1e6):
    """Generate positive engineering-realistic floating point numbers."""
    return draw(st.floats(min_value=min_value, max_value=max_value, width=64))


@st.composite
def engineering_array(draw, min_size=1, max_size=1000):
    """Generate arrays of engineering-realistic values."""
    size = draw(st.integers(min_value=min_size, max_value=max_size))
    return draw(arrays(
        dtype=np.float64,
        shape=(size,),
        elements=engineering_float()
    ))


@st.composite
def pressure_values(draw):
    """Generate realistic pressure values (Pa)."""
    return draw(st.floats(min_value=0, max_value=1e8, width=64))  # 0 to 100 MPa


@st.composite
def temperature_values(draw):
    """Generate realistic temperature values (Celsius)."""
    return draw(st.floats(min_value=-273.15, max_value=2000, width=64))


@st.composite
def coordinate_values(draw):
    """Generate realistic coordinate values (meters)."""
    return draw(st.floats(min_value=-1e6, max_value=1e6, width=64))


class TestBasicMathematicalProperties:
    """Test fundamental mathematical properties."""

    @given(x=engineering_float(), y=engineering_float())
    @settings(max_examples=200)
    def test_addition_commutativity(self, x, y):
        """Test that addition is commutative: x + y == y + x"""
        assume(not (math.isnan(x) or math.isnan(y)))
        assume(not (math.isinf(x) or math.isinf(y)))

        result1 = x + y
        result2 = y + x

        assert abs(result1 - result2) < 1e-10, f"Addition not commutative: {x} + {y} != {y} + {x}"

    @given(x=engineering_float(), y=engineering_float(), z=engineering_float())
    @settings(max_examples=150)
    def test_addition_associativity(self, x, y, z):
        """Test that addition is associative: (x + y) + z == x + (y + z)"""
        assume(not any(math.isnan(v) or math.isinf(v) for v in [x, y, z]))

        result1 = (x + y) + z
        result2 = x + (y + z)

        # Allow for floating point precision errors
        assert abs(result1 - result2) < 1e-10, f"Addition not associative: ({x} + {y}) + {z} != {x} + ({y} + {z})"

    @given(x=positive_engineering_float(), y=positive_engineering_float())
    @settings(max_examples=200)
    def test_multiplication_commutativity(self, x, y):
        """Test that multiplication is commutative for positive numbers."""
        result1 = x * y
        result2 = y * x

        assert abs(result1 - result2) < max(result1, result2) * 1e-12, \
               f"Multiplication not commutative: {x} * {y} != {y} * {x}"

    @given(data=engineering_array(min_size=2, max_size=100))
    @settings(max_examples=100)
    def test_array_sum_properties(self, data):
        """Test properties of array summation."""
        assume(not np.any(np.isnan(data)))
        assume(not np.any(np.isinf(data)))

        # Sum should be deterministic
        sum1 = np.sum(data)
        sum2 = np.sum(data)
        assert sum1 == sum2, "Array sum not deterministic"

        # Sum of reversed array should be the same
        sum_reversed = np.sum(data[::-1])
        assert abs(sum1 - sum_reversed) < abs(sum1) * 1e-12, "Sum depends on order"

        # Sum should equal cumulative sum final value
        cumsum_final = np.cumsum(data)[-1]
        assert abs(sum1 - cumsum_final) < abs(sum1) * 1e-12, "Sum != cumulative sum final"


class TestStatisticalProperties:
    """Test statistical calculation properties."""

    @given(data=engineering_array(min_size=3, max_size=1000))
    @settings(max_examples=100)
    def test_mean_properties(self, data):
        """Test properties of mean calculation."""
        assume(not np.any(np.isnan(data)))
        assume(not np.any(np.isinf(data)))

        mean_val = np.mean(data)

        # Mean should be between min and max
        assert np.min(data) <= mean_val <= np.max(data), \
               f"Mean {mean_val} not between min {np.min(data)} and max {np.max(data)}"

        # Mean of constant array should equal the constant
        if np.allclose(data, data[0]):
            assert abs(mean_val - data[0]) < 1e-12, f"Mean of constant array {data[0]} is {mean_val}"

        # Mean should be scale-invariant for linear transforms
        scaled_data = data * 2.0
        scaled_mean = np.mean(scaled_data)
        assert abs(scaled_mean - (mean_val * 2.0)) < abs(mean_val) * 1e-12, \
               "Mean not scale-invariant"

    @given(data=engineering_array(min_size=2, max_size=500))
    @settings(max_examples=100)
    def test_variance_properties(self, data):
        """Test properties of variance calculation."""
        assume(not np.any(np.isnan(data)))
        assume(not np.any(np.isinf(data)))
        assume(np.std(data) > 1e-10)  # Avoid constant arrays

        variance = np.var(data)
        std_dev = np.std(data)

        # Variance should be non-negative
        assert variance >= 0, f"Variance {variance} is negative"

        # Standard deviation should be square root of variance
        assert abs(std_dev - math.sqrt(variance)) < 1e-12, \
               f"Std dev {std_dev} != sqrt(variance) {math.sqrt(variance)}"

        # Variance of scaled data should scale by factor squared
        scaled_data = data * 3.0
        scaled_variance = np.var(scaled_data)
        expected_variance = variance * 9.0
        assert abs(scaled_variance - expected_variance) < abs(expected_variance) * 1e-10, \
               "Variance not properly scaled"

    @given(data=engineering_array(min_size=10, max_size=200))
    @settings(max_examples=50)
    def test_percentile_properties(self, data):
        """Test properties of percentile calculations."""
        assume(not np.any(np.isnan(data)))
        assume(not np.any(np.isinf(data)))

        # Sort data for easier reasoning
        sorted_data = np.sort(data)

        p25 = np.percentile(sorted_data, 25)
        p50 = np.percentile(sorted_data, 50)  # median
        p75 = np.percentile(sorted_data, 75)

        # Percentiles should be in order
        assert p25 <= p50 <= p75, f"Percentiles not ordered: {p25}, {p50}, {p75}"

        # Percentiles should be within data range
        min_val, max_val = np.min(sorted_data), np.max(sorted_data)
        assert min_val <= p25 <= max_val, f"25th percentile {p25} outside range [{min_val}, {max_val}]"
        assert min_val <= p50 <= max_val, f"50th percentile {p50} outside range [{min_val}, {max_val}]"
        assert min_val <= p75 <= max_val, f"75th percentile {p75} outside range [{min_val}, {max_val}]"

        # Median should be the middle value for odd-length arrays
        if len(sorted_data) % 2 == 1:
            middle_index = len(sorted_data) // 2
            expected_median = sorted_data[middle_index]
            assert abs(p50 - expected_median) < 1e-12, f"Median {p50} != middle value {expected_median}"


class TestEngineeringCalculationProperties:
    """Test properties specific to engineering calculations."""

    @given(
        pressure=pressure_values(),
        area=positive_engineering_float(min_value=1e-6, max_value=1e6)
    )
    @settings(max_examples=200)
    def test_force_calculation_properties(self, pressure, area):
        """Test force = pressure * area properties."""
        assume(not (math.isnan(pressure) or math.isnan(area)))
        assume(pressure >= 0)  # Pressure should be non-negative
        assume(area > 0)       # Area should be positive

        force = pressure * area

        # Force should be non-negative when pressure and area are positive
        assert force >= 0, f"Force {force} is negative with pressure {pressure} and area {area}"

        # Force should scale linearly with pressure
        double_pressure_force = (pressure * 2) * area
        assert abs(double_pressure_force - (force * 2)) < force * 1e-12, \
               "Force doesn't scale linearly with pressure"

        # Force should scale linearly with area
        double_area_force = pressure * (area * 2)
        assert abs(double_area_force - (force * 2)) < force * 1e-12, \
               "Force doesn't scale linearly with area"

    @given(
        coordinates=st.lists(
            st.tuples(coordinate_values(), coordinate_values()),
            min_size=2,
            max_size=100
        )
    )
    @settings(max_examples=50)
    def test_distance_calculation_properties(self, coordinates):
        """Test distance calculation properties."""
        assume(all(not (math.isnan(x) or math.isnan(y)) for x, y in coordinates))

        def distance(p1, p2):
            """Calculate Euclidean distance between two points."""
            return math.sqrt((p1[0] - p2[0])**2 + (p1[1] - p2[1])**2)

        # Test triangle inequality for any three points
        if len(coordinates) >= 3:
            p1, p2, p3 = coordinates[0], coordinates[1], coordinates[2]

            d12 = distance(p1, p2)
            d23 = distance(p2, p3)
            d13 = distance(p1, p3)

            # Triangle inequality: d13 <= d12 + d23
            assert d13 <= d12 + d23 + 1e-10, \
                   f"Triangle inequality violated: {d13} > {d12} + {d23}"

        # Test symmetry: distance(p1, p2) == distance(p2, p1)
        if len(coordinates) >= 2:
            p1, p2 = coordinates[0], coordinates[1]
            d12 = distance(p1, p2)
            d21 = distance(p2, p1)
            assert abs(d12 - d21) < 1e-12, f"Distance not symmetric: {d12} != {d21}"

        # Test identity: distance(p, p) == 0
        if coordinates:
            p = coordinates[0]
            d = distance(p, p)
            assert abs(d) < 1e-12, f"Distance from point to itself is {d}, not 0"

    @given(
        temperatures=st.lists(
            temperature_values(),
            min_size=2,
            max_size=50
        )
    )
    @settings(max_examples=100)
    def test_temperature_conversion_properties(self, temperatures):
        """Test temperature conversion properties."""
        assume(all(not math.isnan(t) for t in temperatures))

        def celsius_to_kelvin(celsius):
            """Convert Celsius to Kelvin."""
            return celsius + 273.15

        def kelvin_to_celsius(kelvin):
            """Convert Kelvin to Celsius."""
            return kelvin - 273.15

        for temp_c in temperatures:
            # Conversion should be reversible
            temp_k = celsius_to_kelvin(temp_c)
            temp_c_back = kelvin_to_celsius(temp_k)

            assert abs(temp_c - temp_c_back) < 1e-12, \
                   f"Temperature conversion not reversible: {temp_c} -> {temp_k} -> {temp_c_back}"

            # Kelvin should always be positive for valid Celsius temperatures
            if temp_c >= -273.15:  # Above absolute zero
                assert temp_k >= 0, f"Kelvin temperature {temp_k} is negative for Celsius {temp_c}"

            # Temperature difference should be preserved
            if len(temperatures) > 1:
                temp2_c = temperatures[1]
                if not math.isnan(temp2_c):
                    temp2_k = celsius_to_kelvin(temp2_c)
                    diff_c = temp_c - temp2_c
                    diff_k = temp_k - temp2_k

                    assert abs(diff_c - diff_k) < 1e-12, \
                           f"Temperature difference not preserved: C: {diff_c}, K: {diff_k}"


class TestArrayAndMatrixProperties:
    """Test properties of array and matrix operations."""

    @given(
        array1=engineering_array(min_size=5, max_size=100),
        array2=engineering_array(min_size=5, max_size=100)
    )
    @settings(max_examples=50)
    def test_dot_product_properties(self, array1, array2):
        """Test dot product properties."""
        assume(len(array1) == len(array2))
        assume(not np.any(np.isnan(array1)))
        assume(not np.any(np.isnan(array2)))
        assume(not np.any(np.isinf(array1)))
        assume(not np.any(np.isinf(array2)))

        dot1 = np.dot(array1, array2)
        dot2 = np.dot(array2, array1)

        # Dot product should be commutative
        assert abs(dot1 - dot2) < max(abs(dot1), abs(dot2)) * 1e-12, \
               f"Dot product not commutative: {dot1} != {dot2}"

        # Dot product with zero vector should be zero
        zero_array = np.zeros(len(array1))
        dot_zero = np.dot(array1, zero_array)
        assert abs(dot_zero) < 1e-12, f"Dot product with zero vector is {dot_zero}, not 0"

        # Dot product with itself should be sum of squares
        dot_self = np.dot(array1, array1)
        sum_squares = np.sum(array1 ** 2)
        assert abs(dot_self - sum_squares) < max(abs(dot_self), abs(sum_squares)) * 1e-12, \
               f"Dot product with self {dot_self} != sum of squares {sum_squares}"

    @given(
        size=st.integers(min_value=2, max_value=20),
        factor=positive_engineering_float(min_value=0.1, max_value=10.0)
    )
    @settings(max_examples=50)
    def test_matrix_scaling_properties(self, size, factor):
        """Test matrix scaling properties."""
        # Create a simple matrix
        matrix = np.random.randn(size, size)
        assume(not np.any(np.isnan(matrix)))
        assume(not np.any(np.isinf(matrix)))

        scaled_matrix = matrix * factor

        # Determinant should scale by factor^n for n×n matrix
        det_original = np.linalg.det(matrix)
        det_scaled = np.linalg.det(scaled_matrix)

        if abs(det_original) > 1e-10:  # Avoid singular matrices
            expected_det = det_original * (factor ** size)
            relative_error = abs(det_scaled - expected_det) / abs(expected_det)
            assert relative_error < 1e-10, \
                   f"Determinant scaling incorrect: {det_scaled} != {expected_det}"

        # Eigenvalue scaling
        try:
            eigenvals_original = np.linalg.eigvals(matrix)
            eigenvals_scaled = np.linalg.eigvals(scaled_matrix)

            if not np.any(np.isnan(eigenvals_original)) and not np.any(np.isnan(eigenvals_scaled)):
                # Sort eigenvalues for comparison
                eigenvals_original_sorted = np.sort(eigenvals_original)
                eigenvals_scaled_sorted = np.sort(eigenvals_scaled)
                expected_eigenvals = eigenvals_original_sorted * factor

                max_error = np.max(np.abs(eigenvals_scaled_sorted - expected_eigenvals))
                max_val = np.max(np.abs(expected_eigenvals))

                if max_val > 1e-10:
                    relative_error = max_error / max_val
                    assert relative_error < 1e-8, \
                           f"Eigenvalue scaling incorrect: max relative error {relative_error}"

        except np.linalg.LinAlgError:
            # Skip if eigenvalue computation fails
            pass


# Example of using custom strategies for specific domain testing
class TestCustomDomainProperties:
    """Test properties using custom strategies for specific domains."""

    @given(
        depths=st.lists(
            positive_engineering_float(min_value=0.1, max_value=11000),  # Ocean depths
            min_size=1,
            max_size=50
        )
    )
    @settings(max_examples=100)
    def test_hydrostatic_pressure_properties(self, depths):
        """Test hydrostatic pressure calculation properties."""
        water_density = 1025.0  # kg/m³ (seawater)
        gravity = 9.81  # m/s²

        def hydrostatic_pressure(depth):
            """Calculate hydrostatic pressure at given depth."""
            return water_density * gravity * depth

        for depth in depths:
            pressure = hydrostatic_pressure(depth)

            # Pressure should be positive
            assert pressure > 0, f"Pressure {pressure} is not positive for depth {depth}"

            # Pressure should increase linearly with depth
            double_depth_pressure = hydrostatic_pressure(depth * 2)
            expected = pressure * 2
            assert abs(double_depth_pressure - expected) < expected * 1e-12, \
                   f"Pressure doesn't scale linearly with depth"

            # Pressure at zero depth should be zero
            zero_pressure = hydrostatic_pressure(0)
            assert abs(zero_pressure) < 1e-12, f"Pressure at zero depth is {zero_pressure}, not 0"


if __name__ == "__main__":
    # Run some property tests as examples
    import hypothesis

    # Set up hypothesis for more verbose output
    hypothesis.settings.register_profile("dev", max_examples=50, verbosity=hypothesis.Verbosity.verbose)
    hypothesis.settings.load_profile("dev")

    # Run a simple property test
    @given(x=st.floats(min_value=-100, max_value=100), y=st.floats(min_value=-100, max_value=100))
    def test_addition_commutative_demo(x, y):
        assume(not (math.isnan(x) or math.isnan(y)))
        assert x + y == y + x

    print("Running property-based test demo...")
    test_addition_commutative_demo()
    print("Property test passed!")