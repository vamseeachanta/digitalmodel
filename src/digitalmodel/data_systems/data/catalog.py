"""
ABOUTME: Data catalog system for managing datasets with metadata, versioning, and schema validation
Provides unified API for data discovery, validation, and loading across the digitalmodel project
"""

import hashlib
import json
import warnings
from dataclasses import dataclass, field, asdict
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any, Union

import pandas as pd
import yaml
from jsonschema import validate, ValidationError, Draft7Validator


# ============================================================================
# Exceptions
# ============================================================================


class CatalogError(Exception):
    """Base exception for catalog errors"""
    pass


class SchemaValidationError(Exception):
    """Raised when schema validation fails"""
    pass


# ============================================================================
# Data Models
# ============================================================================


@dataclass
class CatalogEntry:
    """Represents a single dataset entry in the catalog"""

    # Required fields
    name: str
    file: str
    format: str
    version: str
    description: str

    # Optional fields
    schema: Optional[str] = None
    source: Optional[str] = None
    tags: Optional[List[str]] = None
    units: Optional[str] = None
    engineering_standard: Optional[str] = None
    columns: Optional[List[str]] = None

    # Auto-generated metadata
    hash: Optional[str] = None
    created_at: Optional[str] = None
    last_modified: Optional[str] = None
    schema_version: str = "1.0.0"

    def __post_init__(self):
        """Initialize auto-generated fields"""
        if self.created_at is None:
            self.created_at = datetime.now().isoformat()
        if self.tags is None:
            self.tags = []

    def update_hash(self, base_path: Optional[Path] = None) -> str:
        """
        Calculate and update file hash

        Args:
            base_path: Base path for resolving relative file paths

        Returns:
            File hash
        """
        file_path = Path(self.file)

        # If base_path provided and file is relative, resolve it
        if base_path and not file_path.is_absolute():
            file_path = base_path / file_path

        if file_path.exists():
            sha256 = hashlib.sha256()
            with open(file_path, "rb") as f:
                for chunk in iter(lambda: f.read(4096), b""):
                    sha256.update(chunk)
            self.hash = sha256.hexdigest()
            self.last_modified = datetime.now().isoformat()
        return self.hash

    def to_dict(self) -> Dict[str, Any]:
        """Convert entry to dictionary"""
        data = asdict(self)
        # Remove None values
        return {k: v for k, v in data.items() if v is not None}

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CatalogEntry":
        """Create entry from dictionary"""
        # Filter to only known fields
        known_fields = {
            f.name for f in cls.__dataclass_fields__.values()
        }
        filtered_data = {k: v for k, v in data.items() if k in known_fields}
        return cls(**filtered_data)


# ============================================================================
# Schema Validation
# ============================================================================


class SchemaValidator:
    """Validates data against JSON schemas"""

    def __init__(self, schema_dir: Path, strict: bool = False):
        """
        Initialize schema validator

        Args:
            schema_dir: Directory containing JSON schemas
            strict: If True, raise on unknown fields. If False, warn only.
        """
        self.schema_dir = Path(schema_dir)
        self.strict = strict
        self._cache: Dict[str, Dict] = {}

    def load_schema(self, schema_name: str) -> Dict:
        """Load JSON schema from file"""
        if schema_name in self._cache:
            return self._cache[schema_name]

        schema_path = self.schema_dir / schema_name
        if not schema_path.exists():
            raise CatalogError(f"Schema not found: {schema_path}")

        with open(schema_path) as f:
            schema = json.load(f)

        self._cache[schema_name] = schema
        return schema

    def validate(self, data: Any, schema_name: str) -> bool:
        """
        Validate data against schema

        Args:
            data: Data to validate (dict or DataFrame)
            schema_name: Name of schema file

        Returns:
            True if valid

        Raises:
            SchemaValidationError: If validation fails in strict mode
        """
        schema = self.load_schema(schema_name)

        # Convert DataFrame to dict if needed
        if isinstance(data, pd.DataFrame):
            data = data.to_dict(orient="records")
            # Validate first row as sample
            if data:
                data = data[0]

        try:
            validate(instance=data, schema=schema)
        except ValidationError as e:
            if self.strict:
                raise SchemaValidationError(f"Validation failed: {e.message}") from e
            else:
                warnings.warn(f"Validation warning: {e.message}", UserWarning)

        # Check for unknown fields (not in schema)
        if isinstance(data, dict) and "properties" in schema:
            known_fields = set(schema["properties"].keys())
            data_fields = set(data.keys())
            unknown_fields = data_fields - known_fields

            if unknown_fields:
                msg = f"Unknown fields found: {unknown_fields}"
                if self.strict:
                    raise SchemaValidationError(msg)
                else:
                    warnings.warn(msg, UserWarning)

        return True


# ============================================================================
# Discovery
# ============================================================================


class CatalogDiscovery:
    """Auto-discovers data files and creates catalog entries"""

    # Supported file extensions and their formats
    FORMAT_MAP = {
        ".xlsx": "excel",
        ".xls": "excel",
        ".csv": "csv",
        ".parquet": "parquet",
        ".yml": "yaml",
        ".yaml": "yaml",
        ".json": "json",
    }

    # Exclusion patterns
    EXCLUDE_PATTERNS = [
        "__pycache__",
        ".git",
        "*.pyc",
        "*.tmp",
        "*_backup*",
        "*.swp",
        "*~",
    ]

    def __init__(self, data_dir: Path):
        """Initialize discovery with data directory"""
        self.data_dir = Path(data_dir)

    def discover(self) -> List[Path]:
        """
        Discover all data files in directory

        Returns:
            List of discovered file paths
        """
        discovered = []

        for pattern in self.FORMAT_MAP.keys():
            for file_path in self.data_dir.rglob(f"*{pattern}"):
                if self._should_include(file_path):
                    discovered.append(file_path)

        return discovered

    def _should_include(self, file_path: Path) -> bool:
        """Check if file should be included based on exclusion patterns"""
        file_str = str(file_path)

        for pattern in self.EXCLUDE_PATTERNS:
            # Handle simple glob patterns
            if pattern.startswith("*") and pattern.endswith("*"):
                # Pattern like *_backup*
                if pattern[1:-1] in file_str:
                    return False
            elif pattern.startswith("*."):
                # Pattern like *.pyc
                if file_path.suffix == pattern[1:]:
                    return False
            else:
                # Direct match like __pycache__
                if pattern in file_path.parts:
                    return False

        return True

    def infer_format(self, file_path: Path) -> str:
        """Infer format from file extension"""
        return self.FORMAT_MAP.get(file_path.suffix, "unknown")

    def create_entry(
        self,
        file_path: Path,
        base_path: Optional[Path] = None,
        **kwargs
    ) -> CatalogEntry:
        """
        Create catalog entry from discovered file

        Args:
            file_path: Path to file
            base_path: Base path for relative paths (default: data_dir parent)
            **kwargs: Additional entry fields

        Returns:
            CatalogEntry
        """
        if base_path is None:
            base_path = self.data_dir.parent

        # Generate name from filename (without extension)
        name = file_path.stem

        # Get relative path
        try:
            rel_path = file_path.relative_to(base_path)
        except ValueError:
            rel_path = file_path

        # Infer format
        format_type = self.infer_format(file_path)

        # Create entry
        entry = CatalogEntry(
            name=name,
            file=str(rel_path),
            format=format_type,
            version="1.0.0",  # Initial version
            description=f"Auto-discovered {format_type} file",
            **kwargs
        )

        # Calculate hash
        entry.update_hash()

        return entry


# ============================================================================
# Main Catalog
# ============================================================================


class DataCatalog:
    """Main data catalog for managing datasets"""

    def __init__(
        self,
        catalog_file: Path,
        base_path: Optional[Path] = None,
        schema_dir: Optional[Path] = None,
    ):
        """
        Initialize data catalog

        Args:
            catalog_file: Path to catalog YAML file
            base_path: Base path for resolving relative file paths
            schema_dir: Directory containing JSON schemas
        """
        self.catalog_file = Path(catalog_file)
        self.base_path = Path(base_path) if base_path else self.catalog_file.parent
        self.schema_dir = Path(schema_dir) if schema_dir else self.base_path / "schemas"
        self.datasets: Dict[str, CatalogEntry] = {}
        self.catalog_version = "1.0.0"

        # Initialize schema validator if schema dir exists
        self.validator = None
        if self.schema_dir.exists():
            self.validator = SchemaValidator(self.schema_dir, strict=False)

    def add_entry(self, entry: CatalogEntry) -> None:
        """Add entry to catalog"""
        self.datasets[entry.name] = entry

    def get_entry(self, name: str) -> CatalogEntry:
        """
        Get entry by name

        Args:
            name: Dataset name

        Returns:
            CatalogEntry

        Raises:
            CatalogError: If dataset not found
        """
        if name not in self.datasets:
            raise CatalogError(f"Dataset not found: {name}")
        return self.datasets[name]

    def list_datasets(self) -> List[str]:
        """List all dataset names"""
        return list(self.datasets.keys())

    def search(
        self,
        format: Optional[str] = None,
        tags: Optional[List[str]] = None,
        description: Optional[str] = None,
        **kwargs
    ) -> List[CatalogEntry]:
        """
        Search catalog for datasets matching criteria

        Args:
            format: Filter by format
            tags: Filter by tags (any match)
            description: Filter by description substring
            **kwargs: Additional field filters

        Returns:
            List of matching entries
        """
        results = []

        for entry in self.datasets.values():
            # Check format
            if format and entry.format != format:
                continue

            # Check tags
            if tags and not any(tag in entry.tags for tag in tags):
                continue

            # Check description
            if description and description.lower() not in entry.description.lower():
                continue

            # Check additional filters
            match = True
            for key, value in kwargs.items():
                if not hasattr(entry, key) or getattr(entry, key) != value:
                    match = False
                    break

            if match:
                results.append(entry)

        return results

    def save(self) -> None:
        """Save catalog to YAML file"""
        data = {
            "catalog_version": self.catalog_version,
            "datasets": {
                name: entry.to_dict()
                for name, entry in self.datasets.items()
            }
        }

        self.catalog_file.parent.mkdir(parents=True, exist_ok=True)
        with open(self.catalog_file, "w") as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=False)

    @classmethod
    def load_catalog(
        cls,
        catalog_file: Path,
        base_path: Optional[Path] = None,
        schema_dir: Optional[Path] = None,
    ) -> "DataCatalog":
        """
        Load catalog from YAML file

        Args:
            catalog_file: Path to catalog YAML
            base_path: Base path for resolving relative paths
            schema_dir: Schema directory

        Returns:
            DataCatalog instance
        """
        catalog = cls(catalog_file, base_path, schema_dir)

        if catalog_file.exists():
            with open(catalog_file) as f:
                data = yaml.safe_load(f)

            catalog.catalog_version = data.get("catalog_version", "1.0.0")

            for name, entry_data in data.get("datasets", {}).items():
                entry = CatalogEntry.from_dict(entry_data)
                catalog.add_entry(entry)

        return catalog

    def merge(self, other: "DataCatalog") -> None:
        """
        Merge another catalog into this one

        Args:
            other: Another DataCatalog to merge
        """
        for name, entry in other.datasets.items():
            if name in self.datasets:
                warnings.warn(
                    f"Duplicate dataset '{name}' in merge, keeping existing",
                    UserWarning
                )
            else:
                self.datasets[name] = entry

    def load(
        self,
        name: str,
        validate: bool = False,
        **load_kwargs
    ) -> Union[pd.DataFrame, Dict, Any]:
        """
        Load dataset by name

        Args:
            name: Dataset name
            validate: Whether to validate against schema
            **load_kwargs: Additional arguments for loader

        Returns:
            Loaded data (format depends on file type)
        """
        entry = self.get_entry(name)
        file_path = self.base_path / entry.file

        if not file_path.exists():
            raise CatalogError(f"Data file not found: {file_path}")

        # Load based on format
        data = self._load_by_format(entry.format, file_path, **load_kwargs)

        # Validate if requested
        if validate and entry.schema and self.validator:
            self.validator.validate(data, entry.schema)

        return data

    def _load_by_format(
        self,
        format: str,
        file_path: Path,
        **kwargs
    ) -> Union[pd.DataFrame, Dict, Any]:
        """Load data file based on format"""
        if format == "excel":
            return pd.read_excel(file_path, **kwargs)
        elif format == "csv":
            return pd.read_csv(file_path, **kwargs)
        elif format == "parquet":
            return pd.read_parquet(file_path, **kwargs)
        elif format == "yaml":
            with open(file_path) as f:
                return yaml.safe_load(f)
        elif format == "json":
            with open(file_path) as f:
                return json.load(f)
        else:
            raise CatalogError(f"Unsupported format: {format}")

    def check_version(self, name: str, auto_bump: bool = True) -> str:
        """
        Check if file hash has changed and update version

        Args:
            name: Dataset name
            auto_bump: Whether to auto-bump version on hash change

        Returns:
            Current version (possibly updated)
        """
        entry = self.get_entry(name)
        old_hash = entry.hash

        # Recalculate hash with base_path
        entry.update_hash(self.base_path)

        if old_hash != entry.hash and auto_bump:
            # Auto-bump version (date-based)
            now = datetime.now()
            entry.version = f"{now.year}.{now.month}"

        return entry.version


# ============================================================================
# Convenience Functions
# ============================================================================


def create_master_catalog(
    data_dir: Path,
    output_file: Optional[Path] = None,
    discover: bool = True,
) -> DataCatalog:
    """
    Create master catalog, optionally merging module catalogs

    Args:
        data_dir: Root data directory
        output_file: Output catalog file (default: data_dir/catalog.yml)
        discover: Whether to auto-discover files

    Returns:
        DataCatalog
    """
    if output_file is None:
        output_file = data_dir / "catalog.yml"

    catalog = DataCatalog(output_file, base_path=data_dir.parent)

    # Discover and add files
    if discover:
        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        for file_path in files:
            entry = discovery.create_entry(file_path, base_path=data_dir.parent)
            catalog.add_entry(entry)

    # Find and merge module catalogs
    for module_catalog_file in data_dir.rglob("catalog.yml"):
        if module_catalog_file != output_file:
            module_catalog = DataCatalog.load_catalog(
                module_catalog_file,
                base_path=data_dir.parent
            )
            catalog.merge(module_catalog)

    return catalog
