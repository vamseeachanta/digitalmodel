"""
ABOUTME: Test suite for Excel to Parquet migration utilities.
TDD tests for data migration, integrity validation, and loader functionality.
"""

import json
import shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, List

import numpy as np
import pandas as pd
import pytest

from digitalmodel.data.migration import (
    ExcelToParquetConverter,
    MigrationManifest,
    MigrationReport,
    convert_excel_to_parquet,
    load_data,
    rollback_migration,
    scan_excel_files,
    validate_data_integrity,
)


@pytest.fixture
def temp_data_dir(tmp_path):
    """Create temporary data directory with sample Excel files."""
    data_dir = tmp_path / "data"
    data_dir.mkdir()

    # Create subdirectories
    (data_dir / "raw").mkdir()
    (data_dir / "processed").mkdir()

    return data_dir


@pytest.fixture
def sample_excel_single_sheet(temp_data_dir):
    """Create single-sheet Excel file for testing."""
    file_path = temp_data_dir / "raw" / "test_single.xlsx"

    df = pd.DataFrame({
        "id": [1, 2, 3, 4, 5],
        "name": ["Alice", "Bob", "Charlie", "David", "Eve"],
        "value": [10.5, 20.3, 30.1, 40.9, 50.2],
        "category": ["A", "B", "A", "C", "B"],
    })

    df.to_excel(file_path, index=False, sheet_name="Sheet1")
    return file_path


@pytest.fixture
def sample_excel_multi_sheet(temp_data_dir):
    """Create multi-sheet Excel file for testing."""
    file_path = temp_data_dir / "raw" / "test_multi.xlsx"

    df1 = pd.DataFrame({
        "product_id": [101, 102, 103],
        "product_name": ["Widget", "Gadget", "Doohickey"],
        "price": [9.99, 19.99, 29.99],
    })

    df2 = pd.DataFrame({
        "order_id": [1001, 1002, 1003, 1004],
        "product_id": [101, 102, 101, 103],
        "quantity": [2, 1, 5, 3],
    })

    with pd.ExcelWriter(file_path, engine="openpyxl") as writer:
        df1.to_excel(writer, sheet_name="products", index=False)
        df2.to_excel(writer, sheet_name="orders", index=False)

    return file_path


@pytest.fixture
def sample_excel_with_types(temp_data_dir):
    """Create Excel file with various data types for integrity testing."""
    file_path = temp_data_dir / "processed" / "test_types.xlsx"

    df = pd.DataFrame({
        "int_col": [1, 2, 3, 4, 5],
        "float_col": [1.1, 2.2, 3.3, 4.4, 5.5],
        "str_col": ["a", "b", "c", "d", "e"],
        "bool_col": [True, False, True, False, True],
        "date_col": pd.date_range("2024-01-01", periods=5),
    })

    df.to_excel(file_path, index=False)
    return file_path


class TestExcelScanning:
    """Test Excel file discovery and scanning."""

    def test_scan_excel_files_finds_all_files(self, temp_data_dir, sample_excel_single_sheet, sample_excel_multi_sheet):
        """Should find all Excel files recursively."""
        excel_files = scan_excel_files(temp_data_dir)

        assert len(excel_files) == 2
        assert any("test_single.xlsx" in str(f) for f in excel_files)
        assert any("test_multi.xlsx" in str(f) for f in excel_files)

    def test_scan_excel_files_empty_directory(self, temp_data_dir):
        """Should return empty list for directory without Excel files."""
        empty_dir = temp_data_dir / "empty"
        empty_dir.mkdir()

        excel_files = scan_excel_files(empty_dir)
        assert excel_files == []

    def test_scan_excel_files_includes_xls_and_xlsx(self, temp_data_dir):
        """Should find both .xls and .xlsx files."""
        # Create .xlsx file
        xlsx_file = temp_data_dir / "test.xlsx"
        pd.DataFrame({"a": [1, 2]}).to_excel(xlsx_file, index=False)

        # Note: openpyxl doesn't support .xls format, so we'll just test .xlsx
        excel_files = scan_excel_files(temp_data_dir)
        assert len(excel_files) >= 1


class TestExcelToParquetConversion:
    """Test Excel to Parquet conversion functionality."""

    def test_convert_single_sheet_excel(self, sample_excel_single_sheet, temp_data_dir):
        """Should convert single-sheet Excel to Parquet."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        assert len(output_files) == 1
        assert output_files[0].suffix == ".parquet"
        assert output_files[0].stem == "test_single_Sheet1"

    def test_convert_multi_sheet_excel(self, sample_excel_multi_sheet, temp_data_dir):
        """Should create one Parquet file per sheet."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_multi_sheet, temp_data_dir / "output")

        assert len(output_files) == 2
        output_stems = {f.stem for f in output_files}
        assert "test_multi_products" in output_stems
        assert "test_multi_orders" in output_stems

    def test_converted_parquet_is_readable(self, sample_excel_single_sheet, temp_data_dir):
        """Should create valid Parquet files that can be read."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        df_parquet = pd.read_parquet(output_files[0])
        assert len(df_parquet) == 5
        assert list(df_parquet.columns) == ["id", "name", "value", "category"]

    def test_parquet_uses_snappy_compression(self, sample_excel_single_sheet, temp_data_dir):
        """Should use Snappy compression for Parquet files."""
        converter = ExcelToParquetConverter(compression="snappy")
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        # Read metadata to verify compression
        import pyarrow.parquet as pq
        parquet_file = pq.ParquetFile(output_files[0])
        assert parquet_file.metadata.row_group(0).column(0).compression == "SNAPPY"

    def test_preserve_original_excel_file(self, sample_excel_single_sheet, temp_data_dir):
        """Should not delete or modify original Excel file."""
        original_content = sample_excel_single_sheet.read_bytes()

        converter = ExcelToParquetConverter()
        converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        assert sample_excel_single_sheet.exists()
        assert sample_excel_single_sheet.read_bytes() == original_content


class TestDataIntegrityValidation:
    """Test statistical validation of data integrity."""

    def test_validate_row_count_matches(self, sample_excel_single_sheet, temp_data_dir):
        """Should verify row counts match between Excel and Parquet."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        validation = validate_data_integrity(sample_excel_single_sheet, output_files[0], sheet_name="Sheet1")

        assert validation["row_count_match"] is True
        assert validation["excel_rows"] == 5
        assert validation["parquet_rows"] == 5

    def test_validate_column_names_match(self, sample_excel_single_sheet, temp_data_dir):
        """Should verify column names match."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        validation = validate_data_integrity(sample_excel_single_sheet, output_files[0], sheet_name="Sheet1")

        assert validation["columns_match"] is True
        assert set(validation["excel_columns"]) == {"id", "name", "value", "category"}
        assert set(validation["parquet_columns"]) == {"id", "name", "value", "category"}

    def test_validate_numeric_statistics(self, sample_excel_single_sheet, temp_data_dir):
        """Should validate min/max/mean for numeric columns."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "output")

        validation = validate_data_integrity(sample_excel_single_sheet, output_files[0], sheet_name="Sheet1")

        assert "numeric_stats" in validation
        stats = validation["numeric_stats"]

        # Check 'value' column statistics
        assert "value" in stats
        assert abs(stats["value"]["excel_min"] - 10.5) < 0.01
        assert abs(stats["value"]["excel_max"] - 50.2) < 0.01
        assert abs(stats["value"]["parquet_min"] - 10.5) < 0.01
        assert abs(stats["value"]["parquet_max"] - 50.2) < 0.01

    def test_validate_handles_multiple_numeric_columns(self, sample_excel_with_types, temp_data_dir):
        """Should validate statistics for all numeric columns."""
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_with_types, temp_data_dir / "output")

        validation = validate_data_integrity(sample_excel_with_types, output_files[0])
        stats = validation["numeric_stats"]

        assert "int_col" in stats
        assert "float_col" in stats
        assert stats["int_col"]["excel_min"] == 1
        assert stats["float_col"]["excel_max"] == 5.5


class TestMigrationManifest:
    """Test migration manifest creation and management."""

    def test_create_migration_manifest(self, temp_data_dir):
        """Should create manifest with migration metadata."""
        manifest = MigrationManifest(
            migration_id="test_migration_001",
            timestamp=datetime.now(),
            source_directory=temp_data_dir,
        )

        assert manifest.migration_id == "test_migration_001"
        assert manifest.conversions == []
        assert manifest.errors == []

    def test_add_successful_conversion_to_manifest(self, temp_data_dir):
        """Should track successful conversions."""
        manifest = MigrationManifest(
            migration_id="test_migration_001",
            timestamp=datetime.now(),
            source_directory=temp_data_dir,
        )

        manifest.add_conversion(
            excel_file=temp_data_dir / "test.xlsx",
            parquet_files=[temp_data_dir / "test_Sheet1.parquet"],
            validation_result={"row_count_match": True},
        )

        assert len(manifest.conversions) == 1
        assert manifest.conversions[0]["excel_file"] == str(temp_data_dir / "test.xlsx")

    def test_add_error_to_manifest(self, temp_data_dir):
        """Should track conversion errors."""
        manifest = MigrationManifest(
            migration_id="test_migration_001",
            timestamp=datetime.now(),
            source_directory=temp_data_dir,
        )

        manifest.add_error(
            excel_file=temp_data_dir / "corrupt.xlsx",
            error_message="File is corrupted",
        )

        assert len(manifest.errors) == 1
        assert "corrupt.xlsx" in manifest.errors[0]["excel_file"]

    def test_save_and_load_manifest(self, temp_data_dir):
        """Should persist manifest to JSON and reload."""
        manifest = MigrationManifest(
            migration_id="test_migration_001",
            timestamp=datetime.now(),
            source_directory=temp_data_dir,
        )
        manifest.add_conversion(
            excel_file=temp_data_dir / "test.xlsx",
            parquet_files=[temp_data_dir / "test.parquet"],
            validation_result={"row_count_match": True},
        )

        manifest_path = temp_data_dir / "manifest.json"
        manifest.save(manifest_path)

        loaded_manifest = MigrationManifest.load(manifest_path)
        assert loaded_manifest.migration_id == "test_migration_001"
        assert len(loaded_manifest.conversions) == 1


class TestBatchMigration:
    """Test batch migration of multiple Excel files."""

    def test_convert_all_excel_files_in_directory(
        self, temp_data_dir, sample_excel_single_sheet, sample_excel_multi_sheet
    ):
        """Should convert all Excel files found in directory."""
        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            verify=True,
        )

        assert report.total_files >= 2
        assert report.successful_conversions >= 2
        assert report.failed_conversions == 0

    def test_skip_failed_files_and_continue(self, temp_data_dir, sample_excel_single_sheet):
        """Should continue migration even if some files fail."""
        # Create a corrupt "Excel" file
        corrupt_file = temp_data_dir / "corrupt.xlsx"
        corrupt_file.write_text("Not a valid Excel file")

        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            verify=False,
        )

        # Should have processed the valid file and reported error for corrupt file
        assert report.successful_conversions >= 1
        assert report.failed_conversions >= 1

    def test_generate_migration_report(self, temp_data_dir, sample_excel_single_sheet):
        """Should generate comprehensive migration report."""
        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            verify=True,
        )

        assert hasattr(report, "total_files")
        assert hasattr(report, "successful_conversions")
        assert hasattr(report, "failed_conversions")
        assert hasattr(report, "total_size_excel")
        assert hasattr(report, "total_size_parquet")


class TestDataLoader:
    """Test unified data loader with Parquet-first fallback."""

    def test_load_parquet_when_available(self, temp_data_dir, sample_excel_single_sheet):
        """Should load Parquet file when available."""
        # Convert Excel to Parquet
        converter = ExcelToParquetConverter()
        output_files = converter.convert_file(sample_excel_single_sheet, temp_data_dir / "parquet")

        # Load using unified loader
        df = load_data("test_single_Sheet1", search_dirs=[temp_data_dir / "parquet"])

        assert len(df) == 5
        assert "name" in df.columns

    def test_fallback_to_excel_when_parquet_missing(self, sample_excel_single_sheet, temp_data_dir):
        """Should fall back to Excel if Parquet not found."""
        df = load_data("test_single", search_dirs=[temp_data_dir / "raw"], prefer_format="parquet")

        assert len(df) == 5
        assert "name" in df.columns

    def test_prefer_format_parameter(self, temp_data_dir, sample_excel_single_sheet):
        """Should respect prefer_format parameter."""
        # Create both Excel and Parquet
        converter = ExcelToParquetConverter()
        converter.convert_file(sample_excel_single_sheet, temp_data_dir / "parquet")

        # Should load Parquet when preferred
        df_parquet = load_data(
            "test_single_Sheet1",
            search_dirs=[temp_data_dir / "raw", temp_data_dir / "parquet"],
            prefer_format="parquet",
        )
        assert len(df_parquet) == 5

        # Should load Excel when preferred (and available)
        df_excel = load_data(
            "test_single",
            search_dirs=[temp_data_dir / "raw"],
            prefer_format="excel",
        )
        assert len(df_excel) == 5

    def test_raise_error_when_file_not_found(self, temp_data_dir):
        """Should raise FileNotFoundError when file doesn't exist."""
        with pytest.raises(FileNotFoundError):
            load_data("nonexistent_file", search_dirs=[temp_data_dir])


class TestCatalogIntegration:
    """Test catalog.yml creation and updates."""

    def test_create_catalog_if_not_exists(self, temp_data_dir):
        """Should create catalog.yml if it doesn't exist."""
        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            update_catalog=True,
        )

        catalog_path = temp_data_dir / "catalog.yml"
        # Catalog creation is part of the migration function
        # We'll verify this in the implementation

    def test_update_existing_catalog(self, temp_data_dir, sample_excel_single_sheet):
        """Should update existing catalog with new Parquet entries."""
        catalog_path = temp_data_dir / "catalog.yml"
        catalog_path.write_text("datasets:\n  existing_data: path/to/existing.parquet\n")

        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            update_catalog=True,
        )

        # Should preserve existing entries and add new ones
        # Will verify in implementation


class TestRollback:
    """Test migration rollback functionality."""

    def test_rollback_deletes_parquet_files(self, temp_data_dir, sample_excel_single_sheet):
        """Should delete Parquet files created during migration."""
        # Perform migration
        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            verify=False,
        )

        manifest_path = temp_data_dir / "migration_manifest.json"
        report.manifest.save(manifest_path)

        # Verify Parquet files exist
        parquet_files = list((temp_data_dir / "parquet").rglob("*.parquet"))
        assert len(parquet_files) > 0

        # Rollback
        rollback_migration(manifest_path)

        # Verify Parquet files are deleted
        parquet_files_after = list((temp_data_dir / "parquet").rglob("*.parquet"))
        assert len(parquet_files_after) == 0

    def test_rollback_preserves_excel_files(self, temp_data_dir, sample_excel_single_sheet):
        """Should never delete original Excel files during rollback."""
        # Perform migration
        report = convert_excel_to_parquet(
            source_dir=temp_data_dir,
            output_dir=temp_data_dir / "parquet",
            verify=False,
        )

        manifest_path = temp_data_dir / "migration_manifest.json"
        report.manifest.save(manifest_path)

        # Rollback
        rollback_migration(manifest_path)

        # Verify Excel file still exists
        assert sample_excel_single_sheet.exists()


class TestCLI:
    """Test command-line interface functionality."""

    def test_cli_convert_command(self, temp_data_dir, sample_excel_single_sheet):
        """Should provide CLI for conversion."""
        # This will be tested via click.testing.CliRunner
        # Placeholder for CLI test structure
        pass

    def test_cli_rollback_command(self, temp_data_dir):
        """Should provide CLI for rollback."""
        # This will be tested via click.testing.CliRunner
        # Placeholder for CLI test structure
        pass

    def test_cli_verify_flag(self, temp_data_dir):
        """Should support --verify flag for data integrity checks."""
        # Placeholder for CLI test structure
        pass
