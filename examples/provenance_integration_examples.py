"""
ABOUTME: Integration examples for data provenance tracking system.
Shows how to integrate provenance tracking with OrcaFlex, YAML, and Excel loaders.
"""

import pandas as pd
import numpy as np
from pathlib import Path
from digitalmodel.core.provenance import (
    DataProvenance,
    ProvenanceTracker,
    track_provenance,
    compute_hash,
)


# Example 1: Basic provenance tracking with CSV
def example_basic_csv_provenance():
    """Example: Track provenance for CSV data loading and processing."""
    print("=" * 70)
    print("Example 1: Basic CSV Provenance Tracking")
    print("=" * 70)

    # Create sample data
    df = pd.DataFrame({
        "time": [0, 1, 2, 3, 4],
        "tension": [100, 120, 115, 130, 125],
        "angle": [45, 50, 48, 52, 49],
    })

    # Save to temporary file
    csv_path = Path("data/temp_input.csv")
    csv_path.parent.mkdir(exist_ok=True)
    df.to_csv(csv_path, index=False)

    # Create provenance record
    prov = DataProvenance(
        source=str(csv_path),
        source_version="1.0",
        source_hash=compute_hash(csv_path),
        metadata={"project": "mooring_analysis", "analyst": "John Doe"},
    )

    # Track transformations
    prov.add_transformation(
        function_name="load_csv",
        parameters={"filepath": str(csv_path)},
        output_metadata={"shape": df.shape, "columns": list(df.columns)},
    )

    # Apply transformation
    df_normalized = (df - df.min()) / (df.max() - df.min())

    prov.add_transformation(
        function_name="normalize",
        parameters={"method": "minmax"},
        output_metadata={"shape": df_normalized.shape},
    )

    # Save provenance alongside output
    output_path = Path("data/temp_normalized.csv")
    df_normalized.to_csv(output_path, index=False)
    prov.save_alongside(output_path)

    print(f"[OK] Created provenance for {csv_path}")
    print(f"[OK] Tracked {len(prov.transformations)} transformations")
    print(f"[OK] Saved provenance to {output_path}.provenance.json")
    print()


# Example 2: Using @track_provenance decorator
def example_decorator_usage():
    """Example: Use decorator for automatic provenance tracking."""
    print("=" * 70)
    print("Example 2: Decorator-Based Provenance Tracking")
    print("=" * 70)

    tracker = ProvenanceTracker()

    # Create source data
    source_path = Path("data/temp_source.csv")
    source_path.parent.mkdir(exist_ok=True)
    df = pd.DataFrame({"a": [1, 2, 3, 4, 5], "b": [10, 20, 30, 40, 50]})
    df.to_csv(source_path, index=False)

    @track_provenance(tracker, source=str(source_path), output_key="filtered_data")
    def filter_data(filepath, threshold=2):
        """Load and filter data."""
        df = pd.read_csv(filepath)
        return df[df["a"] > threshold]

    @track_provenance(tracker, source="filtered_data", output_key="aggregated_data")
    def aggregate_data(df):
        """Aggregate data."""
        return df.groupby("a").sum()

    # Execute pipeline
    filtered = filter_data(str(source_path), threshold=2)
    aggregated = aggregate_data(filtered)

    # Check tracked provenance
    print(f"[OK] Tracked {len(tracker.records)} provenance records")
    for key, prov in tracker.records.items():
        print(f"  - {key}: {len(prov.transformations)} transformations")

    # Save tracker state
    tracker.save("data/tracker_state.json")
    print("[OK] Saved tracker state to data/tracker_state.json")
    print()


# Example 3: Multi-source provenance graph
def example_multi_source_graph():
    """Example: Track provenance for data from multiple sources."""
    print("=" * 70)
    print("Example 3: Multi-Source Provenance Graph")
    print("=" * 70)

    # Create multiple source datasets
    df1 = pd.DataFrame({"id": [1, 2, 3], "value_a": [10, 20, 30]})
    df2 = pd.DataFrame({"id": [1, 2, 3], "value_b": [100, 200, 300]})

    source1_path = Path("data/temp_source1.csv")
    source2_path = Path("data/temp_source2.csv")
    source1_path.parent.mkdir(exist_ok=True)

    df1.to_csv(source1_path, index=False)
    df2.to_csv(source2_path, index=False)

    # Create provenance for each source
    prov1 = DataProvenance(
        source=str(source1_path),
        source_version="1.0",
        source_hash=compute_hash(source1_path),
    )

    prov2 = DataProvenance(
        source=str(source2_path),
        source_version="1.0",
        source_hash=compute_hash(source2_path),
    )

    # Merge datasets
    df_merged = pd.merge(df1, df2, on="id")

    # Create merged provenance
    merged_prov = DataProvenance.merge(
        [prov1, prov2],
        operation="join",
        metadata={"join_key": "id"},
    )

    merged_prov.add_transformation(
        function_name="merge",
        parameters={"on": "id", "how": "inner"},
        output_metadata={"shape": df_merged.shape},
    )

    # Export graph for visualization
    graph_dict = merged_prov.to_graph_dict()

    print(f"[OK] Created provenance graph with {len(graph_dict['nodes'])} nodes")
    print(f"[OK] Graph has {len(graph_dict['edges'])} edges")
    print(f"[OK] Provenance depth: {merged_prov.get_depth()}")
    print()


# Example 4: Integration with OrcaFlex-like workflow
def example_orcaflex_integration():
    """Example: Provenance tracking for OrcaFlex-like analysis workflow."""
    print("=" * 70)
    print("Example 4: OrcaFlex Integration Example")
    print("=" * 70)

    tracker = ProvenanceTracker()

    # Simulate OrcaFlex model file
    model_config = {
        "vessel": "FPSO_001",
        "mooring_lines": 8,
        "water_depth": 1500,
    }

    @track_provenance(tracker, source="orcaflex_model.yml", output_key="simulation_results")
    def run_orcaflex_simulation(config):
        """Simulate running OrcaFlex analysis."""
        # Simulated results
        results = pd.DataFrame({
            "time": np.arange(0, 100, 0.1),
            "tension_line_1": np.random.normal(1000, 100, 1000),
            "tension_line_2": np.random.normal(1200, 120, 1000),
        })
        return results

    @track_provenance(tracker, source="simulation_results", output_key="fatigue_analysis")
    def perform_fatigue_analysis(results):
        """Calculate fatigue damage from tension time series."""
        # Simplified fatigue calculation
        max_tension = results[["tension_line_1", "tension_line_2"]].max()
        return max_tension

    # Execute workflow
    sim_results = run_orcaflex_simulation(model_config)
    fatigue_results = perform_fatigue_analysis(sim_results)

    print(f"[OK] Executed OrcaFlex-like workflow")
    print(f"[OK] Tracked {len(tracker.records)} analysis steps")

    # Query provenance
    sources = tracker.find_sources("fatigue_analysis")
    print(f"[OK] Fatigue analysis sources: {sources}")

    # Save provenance
    tracker.save("data/orcaflex_provenance.json")
    print("[OK] Saved provenance to data/orcaflex_provenance.json")
    print()


# Example 5: Querying provenance relationships
def example_provenance_queries():
    """Example: Query provenance to find data lineage."""
    print("=" * 70)
    print("Example 5: Provenance Querying")
    print("=" * 70)

    tracker = ProvenanceTracker()

    # Create provenance chain
    source_file = "raw_data.csv"

    prov1 = DataProvenance(
        source=source_file,
        source_version="1.0",
        source_hash="abc123",
    )
    tracker.add_record("cleaned_data.csv", prov1)

    prov2 = DataProvenance(
        source="cleaned_data.csv",
        source_version="1.0",
        source_hash="def456",
    )
    tracker.add_record("analysis_output.csv", prov2)

    prov3 = DataProvenance(
        source="cleaned_data.csv",
        source_version="1.0",
        source_hash="def456",
    )
    tracker.add_record("visualization.png", prov3)

    # Query: Find all outputs from raw_data.csv
    outputs = tracker.find_outputs(source_file)
    print(f"[OK] Outputs derived from '{source_file}':")
    for output in outputs:
        print(f"  - {output}")

    # Query: Find sources for analysis_output.csv
    sources = tracker.find_sources("analysis_output.csv")
    print(f"[OK] Sources for 'analysis_output.csv': {sources}")
    print()


# Example 6: Export/Import provenance
def example_export_import():
    """Example: Export and import provenance records."""
    print("=" * 70)
    print("Example 6: Export/Import Provenance")
    print("=" * 70)

    # Create provenance
    prov = DataProvenance(
        source="input.csv",
        source_version="2.0",
        source_hash=compute_hash({"data": "sample"}),
        metadata={"project": "offshore_analysis"},
    )

    prov.add_transformation(
        function_name="filter_outliers",
        parameters={"threshold": 3.0},
        output_metadata={"removed_count": 5},
    )

    # Export to JSON (summary)
    json_summary_path = Path("data/provenance_summary.json")
    json_summary_path.parent.mkdir(exist_ok=True)
    prov.export_json(json_summary_path, detailed=False)
    print(f"[OK] Exported summary to {json_summary_path}")

    # Export to JSON (detailed)
    json_detailed_path = Path("data/provenance_detailed.json")
    prov.export_json(json_detailed_path, detailed=True)
    print(f"[OK] Exported detailed to {json_detailed_path}")

    # Export to YAML
    yaml_path = Path("data/provenance.yaml")
    prov.export_yaml(yaml_path)
    print(f"[OK] Exported to YAML at {yaml_path}")

    # Import from JSON
    imported_prov = DataProvenance.from_json(json_detailed_path)
    print(f"[OK] Imported provenance: {imported_prov.source}")
    print(f"  - Transformations: {len(imported_prov.transformations)}")
    print()


# Example 7: Hash computation for different data types
def example_hash_computation():
    """Example: Compute hashes for various data types."""
    print("=" * 70)
    print("Example 7: Hash Computation")
    print("=" * 70)

    # DataFrame hash
    df = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6]})
    df_hash = compute_hash(df)
    print(f"[OK] DataFrame hash: {df_hash[:16]}...")

    # Numpy array hash
    arr = np.array([[1, 2], [3, 4]])
    arr_hash = compute_hash(arr)
    print(f"[OK] Numpy array hash: {arr_hash[:16]}...")

    # Dictionary hash
    data_dict = {"key1": "value1", "key2": [1, 2, 3]}
    dict_hash = compute_hash(data_dict)
    print(f"[OK] Dictionary hash: {dict_hash[:16]}...")

    # File hash
    test_file = Path("data/test_hash.txt")
    test_file.parent.mkdir(exist_ok=True)
    test_file.write_text("Sample content for hashing")
    file_hash = compute_hash(test_file)
    print(f"[OK] File hash: {file_hash[:16]}...")
    print()


def run_all_examples():
    """Run all provenance integration examples."""
    print("\n" + "=" * 70)
    print("DATA PROVENANCE TRACKING - INTEGRATION EXAMPLES")
    print("=" * 70 + "\n")

    example_basic_csv_provenance()
    example_decorator_usage()
    example_multi_source_graph()
    example_orcaflex_integration()
    example_provenance_queries()
    example_export_import()
    example_hash_computation()

    print("=" * 70)
    print("All examples completed successfully!")
    print("=" * 70)


if __name__ == "__main__":
    run_all_examples()
