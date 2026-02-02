"""
ABOUTME: Data provenance tracking system for digitalmodel project.
Tracks data lineage, transformations, and integrity with SHA256 hashing.
"""

import hashlib
import json
import yaml
from dataclasses import dataclass, field, asdict
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Callable, Union
import pandas as pd
import numpy as np
from functools import wraps


@dataclass
class TransformationRecord:
    """Record of a single data transformation."""

    function_name: str
    parameters: Dict[str, Any]
    output_metadata: Dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        """Convert transformation record to dictionary."""
        return {
            "function_name": self.function_name,
            "parameters": self.parameters,
            "output_metadata": self.output_metadata,
            "timestamp": self.timestamp.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TransformationRecord":
        """Create transformation record from dictionary."""
        return cls(
            function_name=data["function_name"],
            parameters=data["parameters"],
            output_metadata=data["output_metadata"],
            timestamp=datetime.fromisoformat(data["timestamp"]),
        )


@dataclass
class DataProvenance:
    """
    Provenance record for tracking data lineage.

    Attributes:
        source: Original data source (file path or database)
        source_version: Version of the source data
        source_hash: SHA256 hash of source data
        created_at: Timestamp when provenance was created
        transformations: List of transformations applied
        metadata: Additional metadata
        parent_provenances: List of parent provenance records (for graphs)
    """

    source: str
    source_version: str
    source_hash: str
    created_at: datetime = field(default_factory=datetime.now)
    transformations: List[TransformationRecord] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    parent_provenances: List["DataProvenance"] = field(default_factory=list)

    def add_transformation(
        self,
        function_name: str,
        parameters: Dict[str, Any],
        output_metadata: Dict[str, Any],
    ) -> None:
        """Add a transformation to the provenance record."""
        record = TransformationRecord(
            function_name=function_name,
            parameters=parameters,
            output_metadata=output_metadata,
        )
        self.transformations.append(record)

    def to_dict(self, detailed: bool = True) -> Dict[str, Any]:
        """
        Convert provenance to dictionary.

        Args:
            detailed: Include full metadata and timestamps

        Returns:
            Dictionary representation
        """
        result = {
            "source": self.source,
            "source_version": self.source_version,
            "source_hash": self.source_hash,
            "transformations": [t.to_dict() for t in self.transformations],
        }

        if detailed:
            result["created_at"] = self.created_at.isoformat()
            result["metadata"] = self.metadata

            if self.parent_provenances:
                result["parent_provenances"] = [
                    p.to_dict(detailed=False) for p in self.parent_provenances
                ]

        return result

    def export_json(self, filepath: Union[str, Path], detailed: bool = True) -> None:
        """
        Export provenance to JSON file.

        Args:
            filepath: Output file path
            detailed: Include full details
        """
        filepath = Path(filepath)
        data = self.to_dict(detailed=detailed)

        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)

    def export_yaml(self, filepath: Union[str, Path]) -> None:
        """Export provenance to YAML file."""
        filepath = Path(filepath)
        data = self.to_dict(detailed=True)

        with open(filepath, "w") as f:
            yaml.dump(data, f, default_flow_style=False)

    @classmethod
    def from_json(cls, filepath: Union[str, Path]) -> "DataProvenance":
        """
        Import provenance from JSON file.

        Args:
            filepath: JSON file path

        Returns:
            DataProvenance instance
        """
        filepath = Path(filepath)

        with open(filepath) as f:
            data = json.load(f)

        return cls._from_dict(data)

    @classmethod
    def from_yaml(cls, filepath: Union[str, Path]) -> "DataProvenance":
        """Import provenance from YAML file."""
        filepath = Path(filepath)

        with open(filepath) as f:
            data = yaml.safe_load(f)

        return cls._from_dict(data)

    @classmethod
    def _from_dict(cls, data: Dict[str, Any]) -> "DataProvenance":
        """Create provenance from dictionary."""
        transformations = [
            TransformationRecord.from_dict(t) for t in data.get("transformations", [])
        ]

        parent_provenances = []
        if "parent_provenances" in data:
            parent_provenances = [
                cls._from_dict(p) for p in data["parent_provenances"]
            ]

        created_at = datetime.now()
        if "created_at" in data:
            created_at = datetime.fromisoformat(data["created_at"])

        return cls(
            source=data["source"],
            source_version=data["source_version"],
            source_hash=data["source_hash"],
            created_at=created_at,
            transformations=transformations,
            metadata=data.get("metadata", {}),
            parent_provenances=parent_provenances,
        )

    def save_alongside(self, data_filepath: Union[str, Path]) -> None:
        """
        Save provenance file alongside data file.

        Args:
            data_filepath: Path to data file (e.g., output.csv)
                          Provenance will be saved as output.csv.provenance.json
        """
        data_filepath = Path(data_filepath)
        prov_filepath = Path(str(data_filepath) + ".provenance.json")
        self.export_json(prov_filepath, detailed=True)

    @classmethod
    def merge(
        cls,
        provenances: List["DataProvenance"],
        operation: str,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> "DataProvenance":
        """
        Merge multiple provenance records into one (for graph support).

        Args:
            provenances: List of parent provenance records
            operation: Description of merge operation
            metadata: Additional metadata

        Returns:
            New provenance record with parents
        """
        if not provenances:
            raise ValueError("Cannot merge empty provenance list")

        # Create merged provenance
        merged = cls(
            source=f"merged({operation})",
            source_version="1.0",
            source_hash=_compute_merged_hash(provenances),
            metadata=metadata or {},
            parent_provenances=provenances,
        )

        return merged

    def get_depth(self) -> int:
        """
        Calculate depth of provenance graph.

        Returns:
            Maximum depth from this node to leaf nodes
        """
        if not self.parent_provenances:
            return 0

        max_parent_depth = max(p.get_depth() for p in self.parent_provenances)
        depth = max_parent_depth + 1

        # Enforce maximum depth of 10
        return min(depth, 10)

    def to_graph_dict(self) -> Dict[str, Any]:
        """
        Export provenance graph as dictionary for visualization.

        Returns:
            Dictionary with 'nodes' and 'edges' for graph visualization
        """
        nodes = []
        edges = []
        node_id_map = {}

        def add_node(prov: DataProvenance, node_id: int) -> int:
            """Recursively add nodes and edges."""
            if id(prov) in node_id_map:
                return node_id_map[id(prov)]

            node_id_map[id(prov)] = node_id

            nodes.append({
                "id": node_id,
                "source": prov.source,
                "hash": prov.source_hash[:8],
                "transformations": len(prov.transformations),
            })

            current_id = node_id
            next_id = node_id + 1

            for parent in prov.parent_provenances:
                parent_id = add_node(parent, next_id)
                edges.append({
                    "from": parent_id,
                    "to": current_id,
                })
                next_id = max(next_id, parent_id + 1)

            return current_id

        add_node(self, 0)

        return {
            "nodes": nodes,
            "edges": edges,
        }


def compute_hash(data: Any, chunk_size: int = 1024 * 1024) -> str:
    """
    Compute SHA256 hash for various data types.

    Supports:
        - File paths (chunked reading for large files)
        - pandas DataFrames
        - numpy arrays
        - dictionaries

    Args:
        data: Data to hash
        chunk_size: Chunk size for file reading (default: 1MB)

    Returns:
        SHA256 hash as hexadecimal string
    """
    hasher = hashlib.sha256()

    if isinstance(data, (str, Path)):
        # File path - chunked reading
        filepath = Path(data)
        with open(filepath, "rb") as f:
            while chunk := f.read(chunk_size):
                hasher.update(chunk)

    elif isinstance(data, pd.DataFrame):
        # DataFrame - convert to CSV bytes
        csv_bytes = data.to_csv(index=False).encode("utf-8")
        hasher.update(csv_bytes)

    elif isinstance(data, np.ndarray):
        # Numpy array - use tobytes
        hasher.update(data.tobytes())

    elif isinstance(data, dict):
        # Dictionary - convert to sorted JSON
        json_bytes = json.dumps(data, sort_keys=True).encode("utf-8")
        hasher.update(json_bytes)

    else:
        # Fallback - convert to string
        hasher.update(str(data).encode("utf-8"))

    return hasher.hexdigest()


def _compute_merged_hash(provenances: List[DataProvenance]) -> str:
    """Compute hash for merged provenance records."""
    combined = "".join(p.source_hash for p in provenances)
    return hashlib.sha256(combined.encode("utf-8")).hexdigest()


class ProvenanceTracker:
    """
    Tracker for managing multiple provenance records.

    Stores provenance records for different outputs and provides
    querying capabilities.
    """

    def __init__(self):
        """Initialize empty tracker."""
        self.records: Dict[str, DataProvenance] = {}

    def add_record(self, output_key: str, provenance: DataProvenance) -> None:
        """
        Add a provenance record.

        Args:
            output_key: Identifier for the output (e.g., file path)
            provenance: Provenance record
        """
        self.records[output_key] = provenance

    def get_record(self, output_key: str) -> Optional[DataProvenance]:
        """
        Retrieve a provenance record.

        Args:
            output_key: Identifier for the output

        Returns:
            Provenance record or None if not found
        """
        return self.records.get(output_key)

    def save(self, filepath: Union[str, Path]) -> None:
        """
        Save tracker state to JSON file.

        Args:
            filepath: Output file path
        """
        filepath = Path(filepath)

        data = {
            key: prov.to_dict(detailed=True)
            for key, prov in self.records.items()
        }

        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)

    def load(self, filepath: Union[str, Path]) -> None:
        """
        Load tracker state from JSON file.

        Args:
            filepath: Input file path
        """
        filepath = Path(filepath)

        with open(filepath) as f:
            data = json.load(f)

        self.records = {
            key: DataProvenance._from_dict(prov_dict)
            for key, prov_dict in data.items()
        }

    def find_sources(self, output_key: str) -> List[str]:
        """
        Find all sources for a given output.

        Args:
            output_key: Output identifier

        Returns:
            List of source identifiers
        """
        prov = self.get_record(output_key)
        if not prov:
            return []

        sources = [prov.source]

        # Recursively find parent sources
        for parent in prov.parent_provenances:
            sources.extend(self._get_all_sources(parent))

        return list(set(sources))

    def _get_all_sources(self, prov: DataProvenance) -> List[str]:
        """Recursively get all sources from provenance graph."""
        sources = [prov.source]

        for parent in prov.parent_provenances:
            sources.extend(self._get_all_sources(parent))

        return sources

    def find_outputs(self, source_key: str) -> List[str]:
        """
        Find all outputs derived from a source.

        Args:
            source_key: Source identifier

        Returns:
            List of output identifiers
        """
        outputs = []

        for output_key, prov in self.records.items():
            if self._has_source(prov, source_key):
                outputs.append(output_key)

        return outputs

    def _has_source(self, prov: DataProvenance, source_key: str) -> bool:
        """Check if provenance has a specific source."""
        if prov.source == source_key:
            return True

        return any(
            self._has_source(parent, source_key)
            for parent in prov.parent_provenances
        )


def find_sources_for(output_key: str, tracker: ProvenanceTracker) -> List[str]:
    """
    Find all sources for a given output.

    Args:
        output_key: Output identifier
        tracker: Provenance tracker

    Returns:
        List of source identifiers
    """
    return tracker.find_sources(output_key)


def find_outputs_from(source_key: str, tracker: ProvenanceTracker) -> List[str]:
    """
    Find all outputs derived from a source.

    Args:
        source_key: Source identifier
        tracker: Provenance tracker

    Returns:
        List of output identifiers
    """
    return tracker.find_outputs(source_key)


def track_provenance(
    tracker: ProvenanceTracker,
    source: Optional[str] = None,
    output_key: Optional[str] = None,
) -> Callable:
    """
    Decorator to automatically track provenance for functions.

    Args:
        tracker: Provenance tracker instance
        source: Source file/data identifier
        output_key: Output identifier (defaults to function result hash)

    Returns:
        Decorated function

    Example:
        @track_provenance(tracker, source="input.csv")
        def process_data(filepath):
            df = pd.read_csv(filepath)
            return df * 2
    """

    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            # Create provenance record
            source_path = source or (str(args[0]) if args else "unknown")

            # Compute source hash if source is a file
            source_hash = "unknown"
            if source and Path(source).exists():
                source_hash = compute_hash(source)

            prov = DataProvenance(
                source=source_path,
                source_version="1.0",
                source_hash=source_hash,
                metadata={
                    "function": func.__name__,
                    "module": func.__module__,
                },
            )

            # Execute function
            result = func(*args, **kwargs)

            # Track transformation
            output_metadata = {}
            if isinstance(result, pd.DataFrame):
                output_metadata = {
                    "type": "DataFrame",
                    "shape": result.shape,
                    "columns": list(result.columns),
                }
            elif isinstance(result, np.ndarray):
                output_metadata = {
                    "type": "ndarray",
                    "shape": result.shape,
                    "dtype": str(result.dtype),
                }

            prov.add_transformation(
                function_name=func.__name__,
                parameters={**kwargs},
                output_metadata=output_metadata,
            )

            # Add to tracker
            result_key = output_key or f"{func.__name__}_result"
            tracker.add_record(result_key, prov)

            return result

        return wrapper

    return decorator
