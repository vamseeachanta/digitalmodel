"""
Tests for BEMRosetta core interfaces.

TDD: Tests written first to verify abstract base class contracts.
"""

import pytest
from abc import ABC
from pathlib import Path
from typing import Any


class TestParserInterface:
    """Tests for ParserInterface abstract base class."""

    def test_is_abstract_class(self):
        """Test that ParserInterface is an abstract base class."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ParserInterface

        assert issubclass(ParserInterface, ABC)

    def test_cannot_instantiate_directly(self):
        """Test that ParserInterface cannot be instantiated directly."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ParserInterface

        with pytest.raises(TypeError, match="abstract"):
            ParserInterface()

    def test_requires_parse_method(self):
        """Test that subclasses must implement parse method."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ParserInterface

        class IncompleteParser(ParserInterface):
            @property
            def supported_extensions(self):
                return [".lis"]

            def can_parse(self, file_path):
                return True

        with pytest.raises(TypeError, match="abstract"):
            IncompleteParser()

    def test_requires_can_parse_method(self):
        """Test that subclasses must implement can_parse method."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ParserInterface

        class IncompleteParser(ParserInterface):
            @property
            def supported_extensions(self):
                return [".lis"]

            def parse(self, file_path):
                return {}

        with pytest.raises(TypeError, match="abstract"):
            IncompleteParser()

    def test_requires_supported_extensions_property(self):
        """Test that subclasses must implement supported_extensions property."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ParserInterface

        class IncompleteParser(ParserInterface):
            def parse(self, file_path):
                return {}

            def can_parse(self, file_path):
                return True

        with pytest.raises(TypeError, match="abstract"):
            IncompleteParser()

    def test_complete_implementation_can_instantiate(self):
        """Test that a complete implementation can be instantiated."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ParserInterface

        class CompleteParser(ParserInterface):
            @property
            def supported_extensions(self) -> list[str]:
                return [".lis", ".LIS"]

            def parse(self, file_path: Path | str) -> Any:
                return {"parsed": True}

            def can_parse(self, file_path: Path | str) -> bool:
                return str(file_path).endswith(".lis")

        parser = CompleteParser()
        assert parser.supported_extensions == [".lis", ".LIS"]
        assert parser.can_parse("test.lis") is True
        assert parser.parse("test.lis") == {"parsed": True}


class TestConverterInterface:
    """Tests for ConverterInterface abstract base class."""

    def test_is_abstract_class(self):
        """Test that ConverterInterface is an abstract base class."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ConverterInterface

        assert issubclass(ConverterInterface, ABC)

    def test_cannot_instantiate_directly(self):
        """Test that ConverterInterface cannot be instantiated directly."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ConverterInterface

        with pytest.raises(TypeError, match="abstract"):
            ConverterInterface()

    def test_requires_convert_method(self):
        """Test that subclasses must implement convert method."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ConverterInterface

        class IncompleteConverter(ConverterInterface):
            @property
            def output_format(self):
                return "yaml"

            def validate_input(self, data):
                return []

        with pytest.raises(TypeError, match="abstract"):
            IncompleteConverter()

    def test_requires_validate_input_method(self):
        """Test that subclasses must implement validate_input method."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ConverterInterface

        class IncompleteConverter(ConverterInterface):
            @property
            def output_format(self):
                return "yaml"

            def convert(self, data, output_dir):
                return Path(output_dir) / "output.yml"

        with pytest.raises(TypeError, match="abstract"):
            IncompleteConverter()

    def test_requires_output_format_property(self):
        """Test that subclasses must implement output_format property."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ConverterInterface

        class IncompleteConverter(ConverterInterface):
            def convert(self, data, output_dir):
                return Path(output_dir) / "output.yml"

            def validate_input(self, data):
                return []

        with pytest.raises(TypeError, match="abstract"):
            IncompleteConverter()

    def test_complete_implementation_can_instantiate(self):
        """Test that a complete implementation can be instantiated."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ConverterInterface

        class CompleteConverter(ConverterInterface):
            @property
            def output_format(self) -> str:
                return "OrcaFlex YAML"

            def convert(self, data: Any, output_dir: Path | str) -> Path:
                return Path(output_dir) / "output.yml"

            def validate_input(self, data: Any) -> list[str]:
                return []

        converter = CompleteConverter()
        assert converter.output_format == "OrcaFlex YAML"
        assert converter.validate_input({}) == []
        assert converter.convert({}, "/tmp") == Path("/tmp/output.yml")


class TestMeshHandlerInterface:
    """Tests for MeshHandlerInterface abstract base class."""

    def test_is_abstract_class(self):
        """Test that MeshHandlerInterface is an abstract base class."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        assert issubclass(MeshHandlerInterface, ABC)

    def test_cannot_instantiate_directly(self):
        """Test that MeshHandlerInterface cannot be instantiated directly."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        with pytest.raises(TypeError, match="abstract"):
            MeshHandlerInterface()

    def test_requires_read_method(self):
        """Test that subclasses must implement read method."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        class IncompleteMeshHandler(MeshHandlerInterface):
            @property
            def format_name(self):
                return "GDF"

            @property
            def file_extension(self):
                return ".gdf"

            def write(self, mesh, file_path):
                pass

        with pytest.raises(TypeError, match="abstract"):
            IncompleteMeshHandler()

    def test_requires_write_method(self):
        """Test that subclasses must implement write method."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        class IncompleteMeshHandler(MeshHandlerInterface):
            @property
            def format_name(self):
                return "GDF"

            @property
            def file_extension(self):
                return ".gdf"

            def read(self, file_path):
                return {}

        with pytest.raises(TypeError, match="abstract"):
            IncompleteMeshHandler()

    def test_requires_format_name_property(self):
        """Test that subclasses must implement format_name property."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        class IncompleteMeshHandler(MeshHandlerInterface):
            @property
            def file_extension(self):
                return ".gdf"

            def read(self, file_path):
                return {}

            def write(self, mesh, file_path):
                pass

        with pytest.raises(TypeError, match="abstract"):
            IncompleteMeshHandler()

    def test_requires_file_extension_property(self):
        """Test that subclasses must implement file_extension property."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        class IncompleteMeshHandler(MeshHandlerInterface):
            @property
            def format_name(self):
                return "GDF"

            def read(self, file_path):
                return {}

            def write(self, mesh, file_path):
                pass

        with pytest.raises(TypeError, match="abstract"):
            IncompleteMeshHandler()

    def test_complete_implementation_can_instantiate(self):
        """Test that a complete implementation can be instantiated."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import MeshHandlerInterface

        class CompleteMeshHandler(MeshHandlerInterface):
            @property
            def format_name(self) -> str:
                return "GDF"

            @property
            def file_extension(self) -> str:
                return ".gdf"

            def read(self, file_path: Path | str) -> Any:
                return {"vertices": [], "panels": []}

            def write(self, mesh: Any, file_path: Path | str) -> None:
                pass

        handler = CompleteMeshHandler()
        assert handler.format_name == "GDF"
        assert handler.file_extension == ".gdf"
        assert handler.read("test.gdf") == {"vertices": [], "panels": []}


class TestValidatorInterface:
    """Tests for ValidatorInterface abstract base class."""

    def test_is_abstract_class(self):
        """Test that ValidatorInterface is an abstract base class."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidatorInterface

        assert issubclass(ValidatorInterface, ABC)

    def test_cannot_instantiate_directly(self):
        """Test that ValidatorInterface cannot be instantiated directly."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidatorInterface

        with pytest.raises(TypeError, match="abstract"):
            ValidatorInterface()

    def test_complete_implementation_can_instantiate(self):
        """Test that a complete implementation can be instantiated."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import (
            ValidatorInterface,
            ValidationReport
        )

        class CompleteValidator(ValidatorInterface):
            def validate(self, data: Any) -> ValidationReport:
                report = ValidationReport()
                if not data:
                    report.add_error("Data is empty")
                return report

            def is_valid(self, data: Any) -> bool:
                return bool(data)

        validator = CompleteValidator()
        assert validator.is_valid({"data": 1}) is True
        assert validator.is_valid({}) is False


class TestValidationReport:
    """Tests for ValidationReport container class."""

    def test_default_initialization(self):
        """Test ValidationReport initializes with valid state."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidationReport

        report = ValidationReport()

        assert report.is_valid is True
        assert report.errors == []
        assert report.warnings == []
        assert report.info == []
        assert report.metrics == {}

    def test_add_error_marks_invalid(self):
        """Test that adding an error marks report as invalid."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidationReport

        report = ValidationReport()
        report.add_error("Something went wrong")

        assert report.is_valid is False
        assert "Something went wrong" in report.errors

    def test_add_warning_keeps_valid(self):
        """Test that adding a warning does not mark report as invalid."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidationReport

        report = ValidationReport()
        report.add_warning("Consider reviewing this")

        assert report.is_valid is True
        assert "Consider reviewing this" in report.warnings

    def test_add_info(self):
        """Test adding informational messages."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidationReport

        report = ValidationReport()
        report.add_info("Processing completed")

        assert "Processing completed" in report.info

    def test_set_metric(self):
        """Test setting validation metrics."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidationReport

        report = ValidationReport()
        report.set_metric("panel_count", 1000)
        report.set_metric("symmetry_error", 0.001)

        assert report.metrics["panel_count"] == 1000
        assert report.metrics["symmetry_error"] == 0.001

    def test_repr(self):
        """Test string representation."""
        from digitalmodel.hydrodynamics.bemrosetta.core.interfaces import ValidationReport

        report = ValidationReport()
        assert "VALID" in repr(report)

        report.add_error("Error")
        assert "INVALID" in repr(report)
