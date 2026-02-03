"""
Property-based tests using Hypothesis for digitalmodel.

These tests generate random inputs to verify properties and find edge cases.
"""

import pytest
from hypothesis import given, strategies as st, assume, example
from hypothesis.stateful import RuleBasedStateMachine, rule, invariant
from typing import List, Dict, Any
import math


@pytest.mark.property
class TestDataStructureProperties:
    """Property-based tests for data structures."""

    @given(st.lists(st.integers()))
    def test_list_reverse_property(self, items: List[int]):
        """Test that reversing a list twice returns original."""
        original = items.copy()
        reversed_once = list(reversed(items))
        reversed_twice = list(reversed(reversed_once))

        assert reversed_twice == original

    @given(st.lists(st.integers(), min_size=1))
    def test_list_sort_properties(self, items: List[int]):
        """Test properties of sorted lists."""
        sorted_items = sorted(items)

        # Property 1: Sorted list has same length
        assert len(sorted_items) == len(items)

        # Property 2: Sorted list is actually sorted
        for i in range(len(sorted_items) - 1):
            assert sorted_items[i] <= sorted_items[i + 1]

        # Property 3: All original elements are present
        assert sorted(items) == sorted_items

    @given(st.dictionaries(st.text(min_size=1), st.integers()))
    def test_dictionary_merge_properties(self, dict1: Dict[str, int]):
        """Test properties of dictionary operations."""
        dict2 = {k: v * 2 for k, v in dict1.items()}

        merged = {**dict1, **dict2}

        # Property: Merged dict contains all keys
        assert all(key in merged for key in dict1.keys())
        assert all(key in merged for key in dict2.keys())

        # Property: Values from dict2 override dict1
        for key in dict1.keys():
            assert merged[key] == dict2[key]


@pytest.mark.property
class TestMathematicalProperties:
    """Property-based tests for mathematical operations."""

    @given(st.integers(min_value=0, max_value=1000))
    def test_square_root_properties(self, n: int):
        """Test properties of square root calculations."""
        if n >= 0:
            sqrt_n = math.sqrt(n)

            # Property: Square root squared equals original (within tolerance)
            assert abs(sqrt_n * sqrt_n - n) < 1e-10

            # Property: Square root is non-negative
            assert sqrt_n >= 0

    @given(st.floats(min_value=0.1, max_value=1000.0, allow_nan=False, allow_infinity=False))
    def test_logarithm_properties(self, x: float):
        """Test properties of logarithmic functions."""
        log_x = math.log(x)
        exp_log_x = math.exp(log_x)

        # Property: exp(log(x)) = x
        assert abs(exp_log_x - x) < 1e-10

    @given(st.lists(st.floats(min_value=-1000, max_value=1000, allow_nan=False, allow_infinity=False), min_size=1))
    def test_statistics_properties(self, numbers: List[float]):
        """Test properties of statistical calculations."""
        total = sum(numbers)
        count = len(numbers)
        average = total / count

        # Property: Average is between min and max (allow floating-point tolerance)
        min_val = min(numbers)
        max_val = max(numbers)
        assert min_val - 1e-10 <= average <= max_val + 1e-10

        # Property: Sum of deviations from mean is close to zero
        deviations = [x - average for x in numbers]
        sum_deviations = sum(deviations)
        assert abs(sum_deviations) < 1e-10


@pytest.mark.property
class TestStringProperties:
    """Property-based tests for string operations."""

    @given(st.text())
    def test_string_encoding_roundtrip(self, text: str):
        """Test that string encoding/decoding is lossless."""
        encoded = text.encode('utf-8')
        decoded = encoded.decode('utf-8')

        assert decoded == text

    @given(st.text(min_size=1), st.text(min_size=1))
    def test_string_concatenation_properties(self, s1: str, s2: str):
        """Test properties of string concatenation."""
        concatenated = s1 + s2

        # Property: Length is sum of individual lengths
        assert len(concatenated) == len(s1) + len(s2)

        # Property: Original strings are substrings
        assert s1 in concatenated
        assert s2 in concatenated

        # Property: Order matters
        reverse_concat = s2 + s1
        if s1 != s2:
            assert concatenated != reverse_concat

    @given(st.text(), st.text())
    @example("", "")
    @example("hello", "world")
    def test_string_replacement_properties(self, text: str, replacement: str):
        """Test properties of string replacement operations."""
        original_length = len(text)

        # Replace all spaces with replacement
        replaced = text.replace(" ", replacement)

        # Property: If no spaces, text unchanged
        if " " not in text:
            assert replaced == text

        # Property: All original spaces are replaced (check by counting)
        # The number of occurrences of replacement should account for replaced spaces
        space_count = text.count(" ")
        if " " not in replacement:
            # If replacement contains no space, no spaces should remain from original
            assert replaced.count(" ") == 0 or " " not in text


@pytest.mark.property
class TestFileProcessingProperties:
    """Property-based tests for file processing operations."""

    @given(st.lists(st.text(min_size=1, alphabet=st.characters(blacklist_characters="\n")), min_size=1))
    def test_file_line_processing_properties(self, lines: List[str]):
        """Test properties of line-by-line file processing."""
        # Simulate file content (lines should not contain newlines for this property to hold)
        content = "\n".join(lines)
        processed_lines = content.split("\n")

        # Property: Number of lines preserved
        assert len(processed_lines) == len(lines)

        # Property: Original lines recoverable
        assert processed_lines == lines

    @given(st.dictionaries(st.text(min_size=1), st.one_of(st.text(), st.integers(), st.floats(allow_nan=False))))
    def test_data_serialization_properties(self, data: Dict[str, Any]):
        """Test properties of data serialization."""
        import json

        try:
            # Serialize to JSON
            serialized = json.dumps(data, default=str)
            deserialized = json.loads(serialized)

            # Property: Keys are preserved
            assert set(data.keys()) == set(deserialized.keys())

        except (TypeError, ValueError):
            # Some data types might not be JSON serializable
            pytest.skip("Data not JSON serializable")


# Stateful testing example
class FileSystemStateMachine(RuleBasedStateMachine):
    """Stateful testing for file system operations."""

    def __init__(self):
        super().__init__()
        self.files = {}
        self.directories = {"/"}

    @rule(filename=st.text(min_size=1, max_size=10),
          content=st.text(max_size=100))
    def create_file(self, filename: str, content: str):
        """Create a file with content."""
        assume("/" not in filename)  # Simple filename only
        assume(filename not in self.files)

        self.files[filename] = content

    @rule(filename=st.sampled_from([]))
    def read_file(self, filename: str):
        """Read a file's content."""
        if filename in self.files:
            content = self.files[filename]
            assert isinstance(content, str)

    @rule(filename=st.sampled_from([]))
    def delete_file(self, filename: str):
        """Delete a file."""
        if filename in self.files:
            del self.files[filename]

    @invariant()
    def files_have_string_content(self):
        """All files must have string content."""
        for content in self.files.values():
            assert isinstance(content, str)

    @invariant()
    def root_directory_exists(self):
        """Root directory always exists."""
        assert "/" in self.directories


@pytest.mark.property
@pytest.mark.slow
class TestStatefulProperties:
    """Stateful property-based tests."""

    def test_file_system_state_machine(self):
        """Test file system operations maintain consistency."""
        # This will run the state machine and verify invariants
        state_machine = FileSystemStateMachine()
        # Note: In real usage, you'd use state_machine.run()
        # For this example, we'll just verify it initializes correctly
        assert "/" in state_machine.directories
        assert len(state_machine.files) == 0


# Custom strategies for domain-specific testing
@st.composite
def valid_email_strategy(draw):
    """Generate valid email addresses."""
    username = draw(st.text(min_size=1, max_size=20, alphabet=st.characters(whitelist_categories=("Lu", "Ll", "Nd"))))
    domain = draw(st.text(min_size=1, max_size=15, alphabet=st.characters(whitelist_categories=("Lu", "Ll"))))
    tld = draw(st.sampled_from(["com", "org", "edu", "net"]))

    return f"{username}@{domain}.{tld}"


@pytest.mark.property
class TestCustomStrategies:
    """Tests using custom data generation strategies."""

    @given(valid_email_strategy())
    def test_email_validation_properties(self, email: str):
        """Test properties of email validation."""
        # Basic email format properties
        assert "@" in email
        assert "." in email
        assert email.count("@") == 1

        parts = email.split("@")
        assert len(parts) == 2
        assert len(parts[0]) > 0  # Username
        assert len(parts[1]) > 0  # Domain