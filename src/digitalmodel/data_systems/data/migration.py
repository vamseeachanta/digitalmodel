"""
ABOUTME: Excel to Parquet migration utilities for digitalmodel project.
Provides batch conversion, data integrity validation, and unified data loading.
"""

import json
import shutil
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import yaml
from loguru import logger


class MigrationManifest:
    """Tracks migration operations for rollback and audit purposes."""

    def __init__(
        self,
        migration_id: str,
        timestamp: datetime,
        source_directory: Path,
    ):
        """
        Initialize migration manifest.

        Args:
            migration_id: Unique identifier for this migration
            timestamp: When migration started
            source_directory: Root directory being migrated
        """
        self.migration_id = migration_id
        self.timestamp = timestamp
        self.source_directory = Path(source_directory)
        self.conversions: List[Dict[str, Any]] = []
        self.errors: List[Dict[str, Any]] = []

    def add_conversion(
        self,
        excel_file: Path,
        parquet_files: List[Path],
        validation_result: Dict[str, Any],
    ) -> None:
        """
        Record successful conversion.

        Args:
            excel_file: Source Excel file
            parquet_files: Generated Parquet files
            validation_result: Data integrity validation results
        """
        self.conversions.append({
            "excel_file": str(excel_file),
            "parquet_files": [str(f) for f in parquet_files],
            "validation": validation_result,
            "timestamp": datetime.now().isoformat(),
        })

    def add_error(self, excel_file: Path, error_message: str) -> None:
        """
        Record conversion error.

        Args:
            excel_file: Source Excel file that failed
            error_message: Error description
        """
        self.errors.append({
            "excel_file": str(excel_file),
            "error": error_message,
            "timestamp": datetime.now().isoformat(),
        })

    def save(self, manifest_path: Path) -> None:
        """
        Save manifest to JSON file.

        Args:
            manifest_path: Where to save the manifest
        """
        manifest_data = {
            "migration_id": self.migration_id,
            "timestamp": self.timestamp.isoformat(),
            "source_directory": str(self.source_directory),
            "conversions": self.conversions,
            "errors": self.errors,
        }

        manifest_path.parent.mkdir(parents=True, exist_ok=True)
        with open(manifest_path, "w") as f:
            json.dump(manifest_data, f, indent=2)

        logger.info(f"Saved migration manifest to {manifest_path}")

    @classmethod
    def load(cls, manifest_path: Path) -> "MigrationManifest":
        """
        Load manifest from JSON file.

        Args:
            manifest_path: Path to manifest file

        Returns:
            Loaded MigrationManifest instance
        """
        with open(manifest_path, "r") as f:
            data = json.load(f)

        manifest = cls(
            migration_id=data["migration_id"],
            timestamp=datetime.fromisoformat(data["timestamp"]),
            source_directory=Path(data["source_directory"]),
        )
        manifest.conversions = data["conversions"]
        manifest.errors = data["errors"]

        return manifest


@dataclass
class MigrationReport:
    """Summary report of migration operations."""

    total_files: int = 0
    successful_conversions: int = 0
    failed_conversions: int = 0
    total_size_excel: int = 0  # bytes
    total_size_parquet: int = 0  # bytes
    manifest: Optional[MigrationManifest] = None
    errors: List[Dict[str, str]] = field(default_factory=list)

    @property
    def compression_ratio(self) -> float:
        """Calculate compression ratio (Excel size / Parquet size)."""
        if self.total_size_parquet == 0:
            return 0.0
        return self.total_size_excel / self.total_size_parquet

    def __str__(self) -> str:
        """Human-readable report summary."""
        return f"""
Migration Report
================
Total Files: {self.total_files}
Successful: {self.successful_conversions}
Failed: {self.failed_conversions}
Excel Size: {self.total_size_excel / (1024**2):.2f} MB
Parquet Size: {self.total_size_parquet / (1024**2):.2f} MB
Compression Ratio: {self.compression_ratio:.2f}x
        """.strip()


class ExcelToParquetConverter:
    """Converts Excel files to Parquet format with validation."""

    def __init__(self, compression: str = "snappy"):
        """
        Initialize converter.

        Args:
            compression: Parquet compression algorithm (snappy, gzip, brotli, etc.)
        """
        self.compression = compression

    def convert_file(
        self,
        excel_file: Path,
        output_dir: Path,
        sheet_name: Optional[Union[str, int, List]] = None,
    ) -> List[Path]:
        """
        Convert Excel file to Parquet format(s).

        For multi-sheet Excel files, creates one Parquet file per sheet.

        Args:
            excel_file: Path to Excel file
            output_dir: Directory for output Parquet files
            sheet_name: Specific sheet(s) to convert, None for all sheets

        Returns:
            List of created Parquet file paths

        Raises:
            ValueError: If Excel file cannot be read
        """
        excel_file = Path(excel_file)
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Read Excel file to get all sheets
        try:
            excel_data = pd.ExcelFile(excel_file)
        except Exception as e:
            raise ValueError(f"Cannot read Excel file {excel_file}: {e}")

        # Determine which sheets to convert
        if sheet_name is None:
            sheets_to_convert = excel_data.sheet_names
        elif isinstance(sheet_name, (list, tuple)):
            sheets_to_convert = sheet_name
        else:
            sheets_to_convert = [sheet_name]

        output_files = []
        base_name = excel_file.stem

        for sheet in sheets_to_convert:
            try:
                # Read sheet data
                df = pd.read_excel(excel_file, sheet_name=sheet)

                # Generate output filename: originalname_sheetname.parquet
                output_filename = f"{base_name}_{sheet}.parquet"
                output_path = output_dir / output_filename

                # Write to Parquet with compression
                df.to_parquet(
                    output_path,
                    engine="pyarrow",
                    compression=self.compression,
                    index=False,
                )

                output_files.append(output_path)
                logger.debug(f"Converted {excel_file}[{sheet}] -> {output_path}")

            except Exception as e:
                logger.error(f"Failed to convert sheet '{sheet}' from {excel_file}: {e}")
                raise

        return output_files


def validate_data_integrity(
    excel_file: Path,
    parquet_file: Path,
    sheet_name: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Validate data integrity between Excel and Parquet files.

    Performs statistical validation:
    - Row count comparison
    - Column name comparison
    - Min/max/mean for numeric columns

    Args:
        excel_file: Source Excel file
        parquet_file: Converted Parquet file
        sheet_name: Excel sheet name (required for multi-sheet files)

    Returns:
        Dictionary with validation results
    """
    validation = {
        "row_count_match": False,
        "columns_match": False,
        "numeric_stats": {},
    }

    try:
        # Read both files
        # If sheet_name is None, read first sheet (default pandas behavior with sheet_name=0)
        # Otherwise read specified sheet
        if sheet_name is None:
            df_excel = pd.read_excel(excel_file, sheet_name=0)
        else:
            df_excel = pd.read_excel(excel_file, sheet_name=sheet_name)
        df_parquet = pd.read_parquet(parquet_file)

        # Validate row counts
        excel_rows = len(df_excel)
        parquet_rows = len(df_parquet)
        validation["excel_rows"] = excel_rows
        validation["parquet_rows"] = parquet_rows
        validation["row_count_match"] = excel_rows == parquet_rows

        # Validate column names
        excel_columns = list(df_excel.columns)
        parquet_columns = list(df_parquet.columns)
        validation["excel_columns"] = excel_columns
        validation["parquet_columns"] = parquet_columns
        validation["columns_match"] = excel_columns == parquet_columns

        # Validate numeric statistics
        numeric_cols = df_excel.select_dtypes(include=["number"]).columns

        for col in numeric_cols:
            if col in df_parquet.columns:
                validation["numeric_stats"][col] = {
                    "excel_min": float(df_excel[col].min()),
                    "excel_max": float(df_excel[col].max()),
                    "excel_mean": float(df_excel[col].mean()),
                    "parquet_min": float(df_parquet[col].min()),
                    "parquet_max": float(df_parquet[col].max()),
                    "parquet_mean": float(df_parquet[col].mean()),
                }

                # Check if statistics match (within tolerance for floating point)
                stats_match = (
                    abs(validation["numeric_stats"][col]["excel_min"] -
                        validation["numeric_stats"][col]["parquet_min"]) < 1e-6
                    and abs(validation["numeric_stats"][col]["excel_max"] -
                            validation["numeric_stats"][col]["parquet_max"]) < 1e-6
                )
                validation["numeric_stats"][col]["match"] = stats_match

        validation["success"] = (
            validation["row_count_match"] and validation["columns_match"]
        )

    except Exception as e:
        validation["error"] = str(e)
        validation["success"] = False

    return validation


def scan_excel_files(directory: Path) -> List[Path]:
    """
    Recursively scan directory for Excel files.

    Args:
        directory: Root directory to scan

    Returns:
        List of Excel file paths (.xlsx and .xls)
    """
    directory = Path(directory)
    excel_files = []

    # Find .xlsx files
    excel_files.extend(directory.rglob("*.xlsx"))

    # Find .xls files (older format)
    excel_files.extend(directory.rglob("*.xls"))

    return sorted(excel_files)


def convert_excel_to_parquet(
    source_dir: Path,
    output_dir: Optional[Path] = None,
    verify: bool = True,
    update_catalog: bool = False,
) -> MigrationReport:
    """
    Batch convert all Excel files in directory to Parquet.

    Args:
        source_dir: Directory containing Excel files
        output_dir: Directory for Parquet output (default: source_dir/parquet)
        verify: Whether to perform data integrity validation
        update_catalog: Whether to update catalog.yml

    Returns:
        MigrationReport with conversion statistics
    """
    source_dir = Path(source_dir)

    if output_dir is None:
        output_dir = source_dir / "parquet"

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Initialize manifest and report
    migration_id = f"migration_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    manifest = MigrationManifest(
        migration_id=migration_id,
        timestamp=datetime.now(),
        source_directory=source_dir,
    )

    report = MigrationReport(manifest=manifest)

    # Find all Excel files
    excel_files = scan_excel_files(source_dir)
    report.total_files = len(excel_files)

    logger.info(f"Found {len(excel_files)} Excel files in {source_dir}")

    # Convert each file
    converter = ExcelToParquetConverter(compression="snappy")

    for excel_file in excel_files:
        try:
            logger.info(f"Converting {excel_file.name}...")

            # Calculate Excel file size
            excel_size = excel_file.stat().st_size
            report.total_size_excel += excel_size

            # Convert to Parquet
            parquet_files = converter.convert_file(excel_file, output_dir)

            # Calculate Parquet file sizes
            for pf in parquet_files:
                report.total_size_parquet += pf.stat().st_size

            # Validate if requested
            validation_results = {}
            if verify:
                # For multi-sheet files, validate each sheet
                excel_data = pd.ExcelFile(excel_file)
                for idx, sheet_name in enumerate(excel_data.sheet_names):
                    if idx < len(parquet_files):
                        validation = validate_data_integrity(
                            excel_file,
                            parquet_files[idx],
                            sheet_name=sheet_name,
                        )
                        validation_results[sheet_name] = validation

                        if not validation.get("success", False):
                            logger.warning(
                                f"Validation failed for {excel_file.name}[{sheet_name}]"
                            )

            # Record success
            manifest.add_conversion(excel_file, parquet_files, validation_results)
            report.successful_conversions += 1

            logger.success(f"✓ Converted {excel_file.name} -> {len(parquet_files)} Parquet file(s)")

        except Exception as e:
            logger.error(f"✗ Failed to convert {excel_file.name}: {e}")
            manifest.add_error(excel_file, str(e))
            report.failed_conversions += 1
            report.errors.append({
                "file": str(excel_file),
                "error": str(e),
            })

    # Save manifest
    manifest_path = output_dir / "migration_manifest.json"
    manifest.save(manifest_path)

    # Update catalog if requested
    if update_catalog:
        _update_catalog(source_dir, manifest)

    logger.info(f"\n{report}")

    return report


def _update_catalog(data_dir: Path, manifest: MigrationManifest) -> None:
    """
    Update catalog.yml with Parquet file entries.

    Args:
        data_dir: Root data directory
        manifest: Migration manifest with conversion records
    """
    catalog_path = data_dir / "catalog.yml"

    # Load existing catalog or create new one
    if catalog_path.exists():
        with open(catalog_path, "r") as f:
            catalog = yaml.safe_load(f) or {}
    else:
        catalog = {"datasets": {}}

    if "datasets" not in catalog:
        catalog["datasets"] = {}

    # Add Parquet entries
    for conversion in manifest.conversions:
        excel_file = Path(conversion["excel_file"])
        parquet_files = [Path(pf) for pf in conversion["parquet_files"]]

        for parquet_file in parquet_files:
            # Create relative path from data_dir
            try:
                rel_path = parquet_file.relative_to(data_dir)
            except ValueError:
                rel_path = parquet_file

            # Use stem as dataset name
            dataset_name = parquet_file.stem

            catalog["datasets"][dataset_name] = {
                "path": str(rel_path),
                "format": "parquet",
                "source_excel": str(excel_file.name),
                "migrated_at": conversion["timestamp"],
            }

    # Save updated catalog
    with open(catalog_path, "w") as f:
        yaml.dump(catalog, f, default_flow_style=False, sort_keys=False)

    logger.info(f"Updated catalog at {catalog_path}")


def load_data(
    name: str,
    search_dirs: Optional[List[Path]] = None,
    prefer_format: str = "parquet",
) -> pd.DataFrame:
    """
    Load data with Parquet-first fallback to Excel.

    Args:
        name: Dataset name (without extension)
        search_dirs: Directories to search (default: ['data/'])
        prefer_format: Preferred format ('parquet' or 'excel')

    Returns:
        Loaded DataFrame

    Raises:
        FileNotFoundError: If file not found in any format
    """
    if search_dirs is None:
        search_dirs = [Path("data")]

    search_dirs = [Path(d) for d in search_dirs]

    # Define search order based on preference
    if prefer_format == "parquet":
        extensions = [".parquet", ".xlsx", ".xls"]
    else:
        extensions = [".xlsx", ".xls", ".parquet"]

    # Search for file
    for directory in search_dirs:
        for ext in extensions:
            file_path = directory / f"{name}{ext}"

            if file_path.exists():
                logger.debug(f"Loading {file_path}")

                if ext == ".parquet":
                    return pd.read_parquet(file_path)
                else:
                    return pd.read_excel(file_path)

    # File not found
    raise FileNotFoundError(
        f"Dataset '{name}' not found in {search_dirs} "
        f"(searched formats: {extensions})"
    )


def rollback_migration(manifest_path: Path) -> None:
    """
    Rollback migration by deleting generated Parquet files.

    Preserves original Excel files.

    Args:
        manifest_path: Path to migration manifest JSON
    """
    manifest = MigrationManifest.load(manifest_path)

    logger.info(f"Rolling back migration {manifest.migration_id}")

    deleted_count = 0

    for conversion in manifest.conversions:
        parquet_files = [Path(pf) for pf in conversion["parquet_files"]]

        for parquet_file in parquet_files:
            if parquet_file.exists():
                parquet_file.unlink()
                deleted_count += 1
                logger.debug(f"Deleted {parquet_file}")

    logger.success(f"Rollback complete. Deleted {deleted_count} Parquet files.")
    logger.info("Original Excel files preserved.")
