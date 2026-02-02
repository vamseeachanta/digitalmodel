"""
Tests for BEMRosetta core exceptions.

TDD: Tests written first to define expected exception behavior.
"""

import pytest
from pathlib import Path


class TestBEMRosettaError:
    """Tests for base BEMRosettaError exception."""

    def test_basic_initialization(self):
        """Test basic exception initialization with message only."""
        from digitalmodel.bemrosetta.core.exceptions import BEMRosettaError

        error = BEMRosettaError("Test error message")

        assert str(error).endswith("Test error message") or "Test error message" in str(error)
        assert error.message == "Test error message"
        assert error.error_code == "BEMROSETTA_ERROR"
        assert error.context == {}
        assert error.suggestions == []

    def test_full_initialization(self):
        """Test exception with all optional parameters."""
        from digitalmodel.bemrosetta.core.exceptions import BEMRosettaError

        error = BEMRosettaError(
            message="Full test error",
            error_code="CUSTOM_CODE",
            context={"file": "test.lis", "line": 42},
            suggestions=["Check file format", "Verify syntax"]
        )

        assert error.message == "Full test error"
        assert error.error_code == "CUSTOM_CODE"
        assert error.context == {"file": "test.lis", "line": 42}
        assert error.suggestions == ["Check file format", "Verify syntax"]

    def test_string_representation(self):
        """Test string representation includes all components."""
        from digitalmodel.bemrosetta.core.exceptions import BEMRosettaError

        error = BEMRosettaError(
            message="Detailed error",
            error_code="TEST_CODE",
            context={"key": "value"},
            suggestions=["Do this"]
        )

        str_repr = str(error)
        assert "TEST_CODE" in str_repr
        assert "Detailed error" in str_repr
        assert "key" in str_repr or "value" in str_repr
        assert "Do this" in str_repr

    def test_to_dict_serialization(self):
        """Test conversion to dictionary for serialization."""
        from digitalmodel.bemrosetta.core.exceptions import BEMRosettaError

        error = BEMRosettaError(
            message="Serialization test",
            error_code="SERIAL_CODE",
            context={"data": 123},
            suggestions=["Suggestion 1"]
        )

        result = error.to_dict()

        assert result["message"] == "Serialization test"
        assert result["error_code"] == "SERIAL_CODE"
        assert result["context"] == {"data": 123}
        assert result["suggestions"] == ["Suggestion 1"]
        assert result["type"] == "BEMRosettaError"

    def test_is_exception_subclass(self):
        """Test that BEMRosettaError is a proper Exception subclass."""
        from digitalmodel.bemrosetta.core.exceptions import BEMRosettaError

        assert issubclass(BEMRosettaError, Exception)

        with pytest.raises(BEMRosettaError):
            raise BEMRosettaError("Test raise")


class TestParserError:
    """Tests for ParserError exception."""

    def test_basic_initialization(self):
        """Test basic ParserError initialization."""
        from digitalmodel.bemrosetta.core.exceptions import ParserError

        error = ParserError("Failed to parse file")

        assert "Failed to parse file" in str(error)
        assert error.error_code == "PARSER_ERROR"

    def test_with_file_path(self):
        """Test ParserError with file_path parameter."""
        from digitalmodel.bemrosetta.core.exceptions import ParserError

        error = ParserError(
            message="Invalid file format",
            file_path=Path("/data/test.lis")
        )

        assert error.context.get("file_path") == Path("/data/test.lis")

    def test_with_line_number(self):
        """Test ParserError with line_number parameter."""
        from digitalmodel.bemrosetta.core.exceptions import ParserError

        error = ParserError(
            message="Syntax error",
            file_path=Path("/data/test.lis"),
            line_number=42
        )

        assert error.context.get("line_number") == 42

    def test_is_bemrosetta_error_subclass(self):
        """Test that ParserError inherits from BEMRosettaError."""
        from digitalmodel.bemrosetta.core.exceptions import (
            BEMRosettaError,
            ParserError
        )

        assert issubclass(ParserError, BEMRosettaError)

    def test_has_suggestions(self):
        """Test that ParserError includes helpful suggestions."""
        from digitalmodel.bemrosetta.core.exceptions import ParserError

        error = ParserError("Parse failed")

        assert len(error.suggestions) > 0


class TestConverterError:
    """Tests for ConverterError exception."""

    def test_basic_initialization(self):
        """Test basic ConverterError initialization."""
        from digitalmodel.bemrosetta.core.exceptions import ConverterError

        error = ConverterError("Conversion failed")

        assert "Conversion failed" in str(error)
        assert error.error_code == "CONVERTER_ERROR"

    def test_with_source_format(self):
        """Test ConverterError with source_format parameter."""
        from digitalmodel.bemrosetta.core.exceptions import ConverterError

        error = ConverterError(
            message="Format not supported",
            source_format="AQWA"
        )

        assert error.context.get("source_format") == "AQWA"

    def test_with_target_format(self):
        """Test ConverterError with target_format parameter."""
        from digitalmodel.bemrosetta.core.exceptions import ConverterError

        error = ConverterError(
            message="Cannot convert to target",
            source_format="WAMIT",
            target_format="OrcaFlex"
        )

        assert error.context.get("source_format") == "WAMIT"
        assert error.context.get("target_format") == "OrcaFlex"

    def test_is_bemrosetta_error_subclass(self):
        """Test that ConverterError inherits from BEMRosettaError."""
        from digitalmodel.bemrosetta.core.exceptions import (
            BEMRosettaError,
            ConverterError
        )

        assert issubclass(ConverterError, BEMRosettaError)


class TestValidationError:
    """Tests for ValidationError exception."""

    def test_basic_initialization(self):
        """Test basic ValidationError initialization."""
        from digitalmodel.bemrosetta.core.exceptions import ValidationError

        error = ValidationError("Validation failed")

        assert "Validation failed" in str(error)
        assert error.error_code == "VALIDATION_ERROR"

    def test_with_validation_errors_list(self):
        """Test ValidationError with list of specific errors."""
        from digitalmodel.bemrosetta.core.exceptions import ValidationError

        validation_errors = [
            "Missing required field: vessel_name",
            "Invalid frequency range"
        ]
        error = ValidationError(
            message="Multiple validation failures",
            validation_errors=validation_errors
        )

        assert error.context.get("validation_errors") == validation_errors

    def test_with_field_name(self):
        """Test ValidationError with specific field name."""
        from digitalmodel.bemrosetta.core.exceptions import ValidationError

        error = ValidationError(
            message="Invalid field value",
            field="water_depth"
        )

        assert error.context.get("field") == "water_depth"

    def test_is_bemrosetta_error_subclass(self):
        """Test that ValidationError inherits from BEMRosettaError."""
        from digitalmodel.bemrosetta.core.exceptions import (
            BEMRosettaError,
            ValidationError
        )

        assert issubclass(ValidationError, BEMRosettaError)


class TestMeshError:
    """Tests for MeshError exception."""

    def test_basic_initialization(self):
        """Test basic MeshError initialization."""
        from digitalmodel.bemrosetta.core.exceptions import MeshError

        error = MeshError("Mesh processing failed")

        assert "Mesh processing failed" in str(error)
        assert error.error_code == "MESH_ERROR"

    def test_with_mesh_file(self):
        """Test MeshError with mesh_file parameter."""
        from digitalmodel.bemrosetta.core.exceptions import MeshError

        error = MeshError(
            message="Invalid mesh format",
            mesh_file=Path("/meshes/hull.gdf")
        )

        assert error.context.get("mesh_file") == Path("/meshes/hull.gdf")

    def test_with_element_count(self):
        """Test MeshError with element_count parameter."""
        from digitalmodel.bemrosetta.core.exceptions import MeshError

        error = MeshError(
            message="Too many elements",
            element_count=50000
        )

        assert error.context.get("element_count") == 50000

    def test_is_bemrosetta_error_subclass(self):
        """Test that MeshError inherits from BEMRosettaError."""
        from digitalmodel.bemrosetta.core.exceptions import (
            BEMRosettaError,
            MeshError
        )

        assert issubclass(MeshError, BEMRosettaError)


class TestExecutableNotFoundError:
    """Tests for ExecutableNotFoundError exception."""

    def test_basic_initialization(self):
        """Test basic ExecutableNotFoundError initialization."""
        from digitalmodel.bemrosetta.core.exceptions import ExecutableNotFoundError

        error = ExecutableNotFoundError("BEMRosetta_cl.exe not found")

        assert "BEMRosetta_cl.exe not found" in str(error)
        assert error.error_code == "EXECUTABLE_NOT_FOUND"

    def test_with_executable_path(self):
        """Test ExecutableNotFoundError with executable_path parameter."""
        from digitalmodel.bemrosetta.core.exceptions import ExecutableNotFoundError

        error = ExecutableNotFoundError(
            message="Executable not found",
            executable_path=Path("D:/software/BEMRosetta/BEMRosetta_cl.exe")
        )

        assert error.context.get("executable_path") == Path("D:/software/BEMRosetta/BEMRosetta_cl.exe")

    def test_has_suggestions(self):
        """Test that ExecutableNotFoundError includes helpful suggestions."""
        from digitalmodel.bemrosetta.core.exceptions import ExecutableNotFoundError

        error = ExecutableNotFoundError("Not found")

        assert len(error.suggestions) > 0
        # Should include installation hints
        suggestions_text = " ".join(error.suggestions).lower()
        assert "install" in suggestions_text or "path" in suggestions_text or "download" in suggestions_text

    def test_is_bemrosetta_error_subclass(self):
        """Test that ExecutableNotFoundError inherits from BEMRosettaError."""
        from digitalmodel.bemrosetta.core.exceptions import (
            BEMRosettaError,
            ExecutableNotFoundError
        )

        assert issubclass(ExecutableNotFoundError, BEMRosettaError)


class TestExceptionRaising:
    """Test proper exception raising in different scenarios."""

    def test_raise_and_catch_hierarchy(self):
        """Test exception can be caught at different hierarchy levels."""
        from digitalmodel.bemrosetta.core.exceptions import (
            BEMRosettaError,
            ParserError
        )

        # Should be catchable as both ParserError and BEMRosettaError
        with pytest.raises(BEMRosettaError):
            raise ParserError("Test")

        with pytest.raises(ParserError):
            raise ParserError("Test")

    def test_exception_chaining(self):
        """Test exception chaining with __cause__."""
        from digitalmodel.bemrosetta.core.exceptions import ParserError

        original = ValueError("Original error")

        try:
            try:
                raise original
            except ValueError as e:
                raise ParserError("Wrapped error") from e
        except ParserError as pe:
            assert pe.__cause__ is original
