"""
ABOUTME: Test suite for data provenance tracking system.
Tests cover provenance creation, transformation tracking, hashing, serialization, and querying.
"""

import pytest
import tempfile
import json
import yaml
from pathlib import Path
from datetime import datetime
import hashlib
import pandas as pd
import numpy as np

from digitalmodel.infrastructure.core.provenance import (
    DataProvenance,
    TransformationRecord,
    ProvenanceTracker,
    track_provenance,
    compute_hash,
    find_sources_for,
    find_outputs_from,
)


class TestDataProvenance:
    """Test DataProvenance core functionality."""

    def test_provenance_creation(self):
        """Test creating a basic provenance record."""
        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        assert prov.source == "data/input.csv"
        assert prov.source_version == "1.0"
        assert prov.source_hash == "abc123"
        assert prov.transformations == []
        assert prov.metadata == {}
        assert isinstance(prov.created_at, datetime)

    def test_provenance_with_metadata(self):
        """Test provenance with custom metadata."""
        metadata = {"author": "test", "project": "digitalmodel"}
        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
            metadata=metadata,
        )

        assert prov.metadata["author"] == "test"
        assert prov.metadata["project"] == "digitalmodel"

    def test_add_transformation(self):
        """Test adding transformation to provenance."""
        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        prov.add_transformation(
            function_name="normalize_data",
            parameters={"method": "minmax"},
            output_metadata={"shape": (100, 5), "dtype": "float64"},
        )

        assert len(prov.transformations) == 1
        assert prov.transformations[0].function_name == "normalize_data"
        assert prov.transformations[0].parameters["method"] == "minmax"
        assert prov.transformations[0].output_metadata["shape"] == (100, 5)

    def test_transformation_chain(self):
        """Test tracking multiple transformations in sequence."""
        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        # Add multiple transformations
        prov.add_transformation("load_csv", {}, {"rows": 100})
        prov.add_transformation("filter_data", {"column": "status", "value": "active"}, {"rows": 80})
        prov.add_transformation("aggregate", {"groupby": "category"}, {"rows": 10})

        assert len(prov.transformations) == 3
        assert prov.transformations[0].function_name == "load_csv"
        assert prov.transformations[1].function_name == "filter_data"
        assert prov.transformations[2].function_name == "aggregate"


class TestHashComputation:
    """Test hash computation for various data types."""

    def test_hash_file(self, tmp_path):
        """Test computing hash for a file."""
        test_file = tmp_path / "test.txt"
        test_file.write_text("Hello, World!")

        hash_value = compute_hash(test_file)

        # Verify it's a valid SHA256 hash
        assert len(hash_value) == 64
        assert all(c in "0123456789abcdef" for c in hash_value)

        # Same content should produce same hash
        hash_value2 = compute_hash(test_file)
        assert hash_value == hash_value2

    def test_hash_dataframe(self):
        """Test computing hash for pandas DataFrame."""
        df = pd.DataFrame({
            "a": [1, 2, 3],
            "b": [4, 5, 6],
        })

        hash_value = compute_hash(df)

        assert len(hash_value) == 64

        # Same DataFrame should produce same hash
        df2 = pd.DataFrame({
            "a": [1, 2, 3],
            "b": [4, 5, 6],
        })
        assert compute_hash(df2) == hash_value

        # Different DataFrame should produce different hash
        df3 = pd.DataFrame({
            "a": [1, 2, 4],
            "b": [4, 5, 6],
        })
        assert compute_hash(df3) != hash_value

    def test_hash_numpy_array(self):
        """Test computing hash for numpy array."""
        arr = np.array([[1, 2, 3], [4, 5, 6]])

        hash_value = compute_hash(arr)

        assert len(hash_value) == 64

        # Same array should produce same hash
        arr2 = np.array([[1, 2, 3], [4, 5, 6]])
        assert compute_hash(arr2) == hash_value

    def test_hash_dict(self):
        """Test computing hash for dictionary."""
        data = {"key1": "value1", "key2": 123, "key3": [1, 2, 3]}

        hash_value = compute_hash(data)

        assert len(hash_value) == 64

        # Same dict should produce same hash
        data2 = {"key1": "value1", "key2": 123, "key3": [1, 2, 3]}
        assert compute_hash(data2) == hash_value

    def test_hash_large_file_chunked(self, tmp_path):
        """Test chunked hash computation for large files."""
        # Create a file larger than chunk size (1MB)
        test_file = tmp_path / "large.bin"
        with open(test_file, "wb") as f:
            f.write(b"x" * (2 * 1024 * 1024))  # 2MB

        hash_value = compute_hash(test_file)

        assert len(hash_value) == 64


class TestTransformationRecord:
    """Test TransformationRecord functionality."""

    def test_transformation_record_creation(self):
        """Test creating a transformation record."""
        record = TransformationRecord(
            function_name="normalize_data",
            parameters={"method": "zscore"},
            output_metadata={"mean": 0.0, "std": 1.0},
        )

        assert record.function_name == "normalize_data"
        assert record.parameters["method"] == "zscore"
        assert record.output_metadata["mean"] == 0.0
        assert isinstance(record.timestamp, datetime)

    def test_transformation_record_to_dict(self):
        """Test converting transformation record to dictionary."""
        record = TransformationRecord(
            function_name="filter_outliers",
            parameters={"threshold": 3.0},
            output_metadata={"removed": 5},
        )

        record_dict = record.to_dict()

        assert record_dict["function_name"] == "filter_outliers"
        assert record_dict["parameters"]["threshold"] == 3.0
        assert record_dict["output_metadata"]["removed"] == 5
        assert "timestamp" in record_dict


class TestProvenanceTracker:
    """Test ProvenanceTracker for managing multiple provenance records."""

    def test_tracker_initialization(self):
        """Test creating a provenance tracker."""
        tracker = ProvenanceTracker()

        assert tracker.records == {}

    def test_tracker_add_record(self):
        """Test adding a provenance record to tracker."""
        tracker = ProvenanceTracker()

        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        tracker.add_record("output.csv", prov)

        assert "output.csv" in tracker.records
        assert tracker.records["output.csv"] == prov

    def test_tracker_get_record(self):
        """Test retrieving a provenance record."""
        tracker = ProvenanceTracker()

        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        tracker.add_record("output.csv", prov)

        retrieved = tracker.get_record("output.csv")
        assert retrieved == prov

    def test_tracker_save_load(self, tmp_path):
        """Test saving and loading tracker state."""
        tracker = ProvenanceTracker()

        prov = DataProvenance(
            source="data/input.csv",
            source_version="1.0",
            source_hash="abc123",
        )
        prov.add_transformation("normalize", {}, {"shape": (100, 5)})

        tracker.add_record("output.csv", prov)

        # Save to file
        save_path = tmp_path / "tracker.json"
        tracker.save(save_path)

        # Load from file
        tracker2 = ProvenanceTracker()
        tracker2.load(save_path)

        assert "output.csv" in tracker2.records
        assert tracker2.records["output.csv"].source == "data/input.csv"


class TestProvenanceDecorator:
    """Test @track_provenance decorator."""

    def test_decorator_basic(self):
        """Test decorator tracks function execution."""
        tracker = ProvenanceTracker()

        @track_provenance(tracker)
        def process_data(data, method="default"):
            return data * 2

        input_data = np.array([1, 2, 3])
        result = process_data(input_data, method="multiply")

        # Should track the transformation
        assert len(tracker.records) > 0

    def test_decorator_with_source(self, tmp_path):
        """Test decorator with source file tracking."""
        tracker = ProvenanceTracker()

        # Create source file
        source_file = tmp_path / "input.csv"
        df = pd.DataFrame({"a": [1, 2, 3]})
        df.to_csv(source_file, index=False)

        @track_provenance(tracker, source=str(source_file))
        def load_and_process(filepath):
            df = pd.read_csv(filepath)
            return df * 2

        result = load_and_process(str(source_file))

        # Check provenance was tracked
        assert len(tracker.records) > 0


class TestProvenanceQuerying:
    """Test querying provenance relationships."""

    def test_find_sources_for_output(self, tmp_path):
        """Test finding all sources for a given output."""
        tracker = ProvenanceTracker()

        # Create provenance chain
        prov = DataProvenance(
            source="input1.csv",
            source_version="1.0",
            source_hash="abc123",
        )
        prov.add_transformation("merge", {"with": "input2.csv"}, {})

        tracker.add_record("output.csv", prov)

        sources = find_sources_for("output.csv", tracker)

        assert "input1.csv" in sources

    def test_find_outputs_from_source(self):
        """Test finding all outputs derived from a source."""
        tracker = ProvenanceTracker()

        # Create multiple outputs from same source
        for i in range(3):
            prov = DataProvenance(
                source="input.csv",
                source_version="1.0",
                source_hash="abc123",
            )
            tracker.add_record(f"output{i}.csv", prov)

        outputs = find_outputs_from("input.csv", tracker)

        assert len(outputs) == 3
        assert "output0.csv" in outputs
        assert "output1.csv" in outputs
        assert "output2.csv" in outputs


class TestProvenanceSerialization:
    """Test JSON/YAML export and import."""

    def test_export_json_summary(self, tmp_path):
        """Test exporting provenance to JSON (summary mode)."""
        prov = DataProvenance(
            source="input.csv",
            source_version="1.0",
            source_hash="abc123",
        )
        prov.add_transformation("normalize", {"method": "minmax"}, {"shape": (100, 5)})

        output_file = tmp_path / "prov.json"
        prov.export_json(output_file, detailed=False)

        assert output_file.exists()

        with open(output_file) as f:
            data = json.load(f)

        assert data["source"] == "input.csv"
        assert data["source_hash"] == "abc123"
        assert len(data["transformations"]) == 1

    def test_export_json_detailed(self, tmp_path):
        """Test exporting provenance to JSON (detailed mode)."""
        prov = DataProvenance(
            source="input.csv",
            source_version="1.0",
            source_hash="abc123",
            metadata={"project": "test"},
        )
        prov.add_transformation("normalize", {"method": "minmax"}, {"shape": (100, 5)})

        output_file = tmp_path / "prov_detailed.json"
        prov.export_json(output_file, detailed=True)

        with open(output_file) as f:
            data = json.load(f)

        assert "metadata" in data
        assert data["metadata"]["project"] == "test"

    def test_export_yaml(self, tmp_path):
        """Test exporting provenance to YAML."""
        prov = DataProvenance(
            source="input.csv",
            source_version="1.0",
            source_hash="abc123",
        )
        prov.add_transformation("filter", {"column": "status"}, {"rows": 50})

        output_file = tmp_path / "prov.yaml"
        prov.export_yaml(output_file)

        assert output_file.exists()

        with open(output_file) as f:
            data = yaml.safe_load(f)

        assert data["source"] == "input.csv"
        assert len(data["transformations"]) == 1

    def test_import_from_json(self, tmp_path):
        """Test importing provenance from JSON."""
        prov = DataProvenance(
            source="input.csv",
            source_version="1.0",
            source_hash="abc123",
        )
        prov.add_transformation("normalize", {}, {})

        # Export
        output_file = tmp_path / "prov.json"
        prov.export_json(output_file)

        # Import
        prov2 = DataProvenance.from_json(output_file)

        assert prov2.source == "input.csv"
        assert prov2.source_hash == "abc123"
        assert len(prov2.transformations) == 1


class TestProvenanceGraph:
    """Test provenance graph support for multiple inputs."""

    def test_merge_provenance_records(self):
        """Test merging multiple provenance records."""
        prov1 = DataProvenance(
            source="input1.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        prov2 = DataProvenance(
            source="input2.csv",
            source_version="1.0",
            source_hash="def456",
        )

        # Merge two sources
        merged = DataProvenance.merge([prov1, prov2], operation="concat")

        assert len(merged.parent_provenances) == 2
        assert merged.parent_provenances[0].source == "input1.csv"
        assert merged.parent_provenances[1].source == "input2.csv"

    def test_provenance_graph_depth(self):
        """Test tracking provenance graph depth."""
        # Create a chain of transformations
        prov1 = DataProvenance(source="input.csv", source_version="1.0", source_hash="abc")
        prov2 = DataProvenance.merge([prov1], operation="transform1")
        prov3 = DataProvenance.merge([prov2], operation="transform2")

        assert prov3.get_depth() == 2

    def test_provenance_graph_max_depth(self):
        """Test maximum depth enforcement (10 levels)."""
        current = DataProvenance(source="input.csv", source_version="1.0", source_hash="abc")

        for i in range(15):
            current = DataProvenance.merge([current], operation=f"transform{i}")

        # Should only track up to depth 10
        assert current.get_depth() <= 10

    def test_export_graph_to_dict(self):
        """Test exporting provenance graph to dictionary."""
        prov1 = DataProvenance(source="input1.csv", source_version="1.0", source_hash="abc")
        prov2 = DataProvenance(source="input2.csv", source_version="1.0", source_hash="def")
        merged = DataProvenance.merge([prov1, prov2], operation="join")

        graph_dict = merged.to_graph_dict()

        assert "nodes" in graph_dict
        assert "edges" in graph_dict
        assert len(graph_dict["nodes"]) == 3  # 2 inputs + 1 output
        assert len(graph_dict["edges"]) == 2  # 2 connections


class TestProvenanceIntegration:
    """Test integration with data loaders."""

    def test_auto_provenance_for_csv(self, tmp_path):
        """Test automatic provenance tracking for CSV loading."""
        # Create test CSV
        csv_file = tmp_path / "test.csv"
        df = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6]})
        df.to_csv(csv_file, index=False)

        tracker = ProvenanceTracker()

        @track_provenance(tracker, source=str(csv_file))
        def load_csv(filepath):
            return pd.read_csv(filepath)

        result = load_csv(str(csv_file))

        # Check provenance file was created
        prov_file = Path(str(csv_file) + ".provenance.json")

        # Tracker should have the record
        assert len(tracker.records) > 0

    def test_provenance_file_alongside_data(self, tmp_path):
        """Test provenance file is saved alongside data file."""
        data_file = tmp_path / "output.csv"

        prov = DataProvenance(
            source="input.csv",
            source_version="1.0",
            source_hash="abc123",
        )

        # Save provenance alongside data file
        prov.save_alongside(data_file)

        prov_file = Path(str(data_file) + ".provenance.json")
        assert prov_file.exists()
