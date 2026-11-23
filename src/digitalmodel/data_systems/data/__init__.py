"""
ABOUTME: Data management utilities for digitalmodel.
Provides Excel/Parquet migration and unified data loading.
"""

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

# Catalog imports (optional - requires jsonschema)
try:
    from digitalmodel.data.catalog import (
        DataCatalog,
        CatalogEntry,
        CatalogDiscovery,
        SchemaValidator,
        CatalogError,
        SchemaValidationError,
        create_master_catalog,
    )
    _CATALOG_AVAILABLE = True
except ImportError:
    _CATALOG_AVAILABLE = False

__all__ = [
    "ExcelToParquetConverter",
    "MigrationManifest",
    "MigrationReport",
    "convert_excel_to_parquet",
    "load_data",
    "rollback_migration",
    "scan_excel_files",
    "validate_data_integrity",
]

if _CATALOG_AVAILABLE:
    __all__.extend([
        "DataCatalog",
        "CatalogEntry",
        "CatalogDiscovery",
        "SchemaValidator",
        "CatalogError",
        "SchemaValidationError",
        "create_master_catalog",
    ])
