# ABOUTME: Integration tests for all 6 core components
# Tests config→database, validation→catalog, migration→provenance workflows

import pytest
from pathlib import Path
import tempfile
import pandas as pd
import numpy as np
from datetime import datetime

from digitalmodel.config.registry import ConfigRegistry
from digitalmodel.core.database_manager import DatabaseManager
from digitalmodel.validation.pipeline import (
    ValidationPipeline, RangeValidator, MatrixValidator,
    PhysicalPlausibilityValidator, TimeSeriesValidator
)
from digitalmodel.data.catalog import DataCatalog, CatalogEntry, CatalogDiscovery
from digitalmodel.core.provenance import DataProvenance, ProvenanceTracker, compute_hash
from digitalmodel.data.migration import (
    ExcelToParquetConverter, convert_excel_to_parquet, validate_data_integrity
)


# ============================================================================
# Test 1: Config Registry → Database Manager Integration
# ============================================================================

def test_config_to_database_integration(tmp_path):
    """Test config loading and database connection"""

    # Create test config
    config_dir = tmp_path / "base_configs" / "modules"
    config_dir.mkdir(parents=True)

    config_file = config_dir / "database.yml"
    config_file.write_text("""
default:
  server_type: postgresql
  server: localhost
  port: 5432
  database: testdb
  user: testuser
  password: testpass
  pool_size: 10
""")

    # Test config loading
    registry = ConfigRegistry(config_base_path=tmp_path / "base_configs")
    config = registry.get_config("database")

    assert config["default"]["server_type"] == "postgresql"
    assert config["default"]["pool_size"] == 10

    # Test database manager from config
    db_props = config["default"]
    manager = DatabaseManager.from_legacy_properties(db_props)

    assert manager.db_type == "postgresql"
    assert manager.pool_size == 10

    metrics = manager.get_metrics()
    assert metrics["pool_size"] == 10
    assert metrics["active_connections"] == 0


# ============================================================================
# Test 2: Data Loading → Validation → Catalog Integration
# ============================================================================

def test_data_validation_catalog_integration(tmp_path):
    """Test complete data workflow: load → validate → catalog"""

    # Create test data
    data_dir = tmp_path / "data"
    data_dir.mkdir()

    test_data = pd.DataFrame({
        "time": np.linspace(0, 10, 100),
        "force": np.random.uniform(100, 1000, 100),
        "displacement": np.random.uniform(-5, 5, 100)
    })

    data_file = data_dir / "test_data.csv"
    test_data.to_csv(data_file, index=False)

    # Step 1: Load data
    df = pd.read_csv(data_file)

    # Step 2: Validate with pipeline
    validators = [
        RangeValidator("force", min_value=0, max_value=2000),
        RangeValidator("displacement", min_value=-10, max_value=10),
        TimeSeriesValidator(
            "time", "force",
            detect_gaps=True,
            detect_outliers=True
        )
    ]

    pipeline = ValidationPipeline(validators, parallel=True)
    results = pipeline.execute({
        "force": df["force"].values,
        "displacement": df["displacement"].values,
        "time": df["time"].values
    })

    assert len(results) == 3
    assert all(r.passed for r in results)

    # Step 3: Add to catalog
    catalog = DataCatalog(
        catalog_file=tmp_path / "catalog.yml",
        base_path=tmp_path
    )

    entry = CatalogEntry(
        name="test_data",
        file=str(data_file.relative_to(tmp_path)),
        format="csv",
        version="1.0.0",
        description="Test time series data",
        tags=["test", "timeseries"]
    )

    entry.update_hash(base_path=tmp_path)
    catalog.add_entry(entry)
    catalog.save()

    # Verify catalog
    loaded_catalog = DataCatalog.load_catalog(tmp_path / "catalog.yml", base_path=tmp_path)
    assert "test_data" in loaded_catalog.list_datasets()

    loaded_data = loaded_catalog.load("test_data")
    assert len(loaded_data) == 100


# ============================================================================
# Test 3: Excel Migration → Provenance Tracking Integration
# ============================================================================

def test_migration_provenance_integration(tmp_path):
    """Test Excel→Parquet migration with provenance tracking"""

    # Create test Excel file
    excel_dir = tmp_path / "excel"
    excel_dir.mkdir()

    test_excel = excel_dir / "test_data.xlsx"
    df_original = pd.DataFrame({
        "id": range(100),
        "value": np.random.randn(100),
        "category": np.random.choice(["A", "B", "C"], 100)
    })

    df_original.to_excel(test_excel, index=False, sheet_name="Sheet1")

    # Initialize provenance tracker
    tracker = ProvenanceTracker()

    # Create provenance for source
    source_hash = compute_hash(test_excel)
    provenance = DataProvenance(
        source=str(test_excel),
        source_version="1.0.0",
        source_hash=source_hash,
        metadata={"format": "excel"}
    )

    # Perform migration
    parquet_dir = tmp_path / "parquet"
    converter = ExcelToParquetConverter(compression="snappy")
    parquet_files = converter.convert_file(test_excel, parquet_dir)

    assert len(parquet_files) == 1
    parquet_file = parquet_files[0]

    # Track transformation in provenance
    provenance.add_transformation(
        function_name="excel_to_parquet",
        parameters={"compression": "snappy"},
        output_metadata={
            "output_file": str(parquet_file),
            "format": "parquet"
        }
    )

    tracker.add_record("test_data_parquet", provenance)

    # Validate data integrity
    validation = validate_data_integrity(
        test_excel, parquet_file, sheet_name="Sheet1"
    )

    assert validation["success"]
    assert validation["row_count_match"]
    assert validation["columns_match"]

    # Verify provenance tracking
    retrieved = tracker.get_record("test_data_parquet")
    assert retrieved is not None
    assert len(retrieved.transformations) == 1
    assert retrieved.transformations[0].function_name == "excel_to_parquet"


# ============================================================================
# Test 4: End-to-End Workflow Integration
# ============================================================================

def test_end_to_end_workflow(tmp_path):
    """Test complete workflow: Config → Load → Validate → Migrate → Catalog → Provenance"""

    # Step 1: Config Registry
    config_dir = tmp_path / "base_configs" / "modules"
    config_dir.mkdir(parents=True)

    config_file = config_dir / "workflow.yml"
    config_file.write_text("""
default:
  data_dir: data
  validation:
    force_min: 0
    force_max: 10000
    tolerance: 0.001
""")

    registry = ConfigRegistry(config_base_path=tmp_path / "base_configs")
    config = registry.get_config("workflow")

    # Step 2: Create and load test data
    data_dir = tmp_path / config["default"]["data_dir"]
    data_dir.mkdir()

    excel_file = data_dir / "analysis_data.xlsx"
    df = pd.DataFrame({
        "time": np.linspace(0, 100, 50),
        "force": np.random.uniform(100, 5000, 50)
    })
    df.to_excel(excel_file, index=False, sheet_name="Results")

    # Step 3: Validate data
    validators = [
        RangeValidator(
            "force",
            min_value=config["default"]["validation"]["force_min"],
            max_value=config["default"]["validation"]["force_max"]
        ),
        TimeSeriesValidator("time", "force", detect_gaps=True)
    ]

    pipeline = ValidationPipeline(validators, parallel=False)
    results = pipeline.execute({"force": df["force"].values, "time": df["time"].values})

    assert all(r.passed for r in results)

    # Step 4: Migrate to Parquet
    tracker = ProvenanceTracker()

    source_prov = DataProvenance(
        source=str(excel_file),
        source_version="1.0.0",
        source_hash=compute_hash(excel_file)
    )

    parquet_dir = data_dir / "parquet"
    converter = ExcelToParquetConverter()
    parquet_files = converter.convert_file(excel_file, parquet_dir)

    source_prov.add_transformation(
        function_name="excel_to_parquet",
        parameters={"compression": "snappy"},
        output_metadata={"files": [str(f) for f in parquet_files]}
    )

    tracker.add_record("analysis_data", source_prov)

    # Step 5: Add to catalog
    catalog = DataCatalog(
        catalog_file=data_dir / "catalog.yml",
        base_path=tmp_path
    )

    for pf in parquet_files:
        entry = CatalogEntry(
            name=pf.stem,
            file=str(pf.relative_to(tmp_path)),
            format="parquet",
            version="1.0.0",
            description="Migrated analysis data"
        )
        entry.update_hash(base_path=tmp_path)
        catalog.add_entry(entry)

    catalog.save()

    # Step 6: Verify complete workflow
    # - Config loaded
    assert registry.has_config("workflow")

    # - Data migrated
    assert all(pf.exists() for pf in parquet_files)

    # - Validation passed
    assert all(r.passed for r in results)

    # - Provenance tracked
    prov = tracker.get_record("analysis_data")
    assert len(prov.transformations) == 1

    # - Catalog updated
    loaded_catalog = DataCatalog.load_catalog(data_dir / "catalog.yml", base_path=tmp_path)
    assert len(loaded_catalog.list_datasets()) == 1

    # - Data loadable
    loaded_df = loaded_catalog.load(parquet_files[0].stem)
    assert len(loaded_df) == 50
    assert list(loaded_df.columns) == ["time", "force"]


# ============================================================================
# Test 5: Matrix Validation Integration
# ============================================================================

def test_matrix_validation_integration():
    """Test matrix validation for hydrodynamic data"""

    # Create 6x6 added mass matrix (symmetric, positive definite)
    A = np.eye(6) * 1000  # Diagonal dominance
    A[0, 1] = A[1, 0] = 50  # Symmetry
    A[2, 3] = A[3, 2] = 30

    validators = [
        MatrixValidator(
            "added_mass",
            expected_shape=(6, 6),
            check_symmetric=True,
            check_positive_definite=True
        )
    ]

    pipeline = ValidationPipeline(validators)
    results = pipeline.execute({"added_mass": A})

    assert len(results) == 1
    assert results[0].passed
    assert results[0].summary["symmetric_check"] is True


# ============================================================================
# Test 6: Physical Plausibility Integration
# ============================================================================

def test_physical_plausibility_integration():
    """Test physical limits validation for engineering data"""

    # Realistic mooring line forces
    forces = np.array([50000, 75000, 100000, 125000])  # N (50-125 kN)

    validator = PhysicalPlausibilityValidator(
        "tension",
        physical_type="force",
        custom_limits={
            "min": 0,
            "max": 1e8,  # 100 MN
            "typical_max": 1e6  # 1 MN
        }
    )

    result = validator.validate({"tension": forces})

    assert result.passed
    # No warnings since forces are well within typical range


# ============================================================================
# Test 7: Performance Metrics
# ============================================================================

def test_performance_metrics(tmp_path):
    """Test performance characteristics of integrated system"""

    import time

    # Create larger dataset
    data_dir = tmp_path / "data"
    data_dir.mkdir()

    large_df = pd.DataFrame({
        "time": np.linspace(0, 1000, 10000),
        "force": np.random.uniform(0, 5000, 10000)
    })

    csv_file = data_dir / "large_data.csv"
    large_df.to_csv(csv_file, index=False)

    # Test validation throughput
    validator = RangeValidator("force", min_value=0, max_value=10000)

    start = time.time()
    result = validator.validate({"force": large_df["force"].values})
    validation_time = time.time() - start

    assert result.passed
    throughput = len(large_df) / validation_time

    # Should validate > 100k records/sec
    assert throughput > 100000, f"Validation too slow: {throughput:.0f} records/sec"

    # Test catalog loading speed
    catalog = DataCatalog(tmp_path / "catalog.yml", base_path=tmp_path)

    entry = CatalogEntry(
        name="large_data",
        file=str(csv_file.relative_to(tmp_path)),
        format="csv",
        version="1.0.0",
        description="Large dataset"
    )

    catalog.add_entry(entry)

    start = time.time()
    loaded_df = catalog.load("large_data")
    load_time = time.time() - start

    assert len(loaded_df) == 10000
    # Should load in < 1 second
    assert load_time < 1.0, f"Loading too slow: {load_time:.2f}s"


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
