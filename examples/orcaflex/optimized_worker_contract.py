# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the optimized V2 contract with synthetic inputs and no solver.

Source: issue 2521 (2026), orcaflex_optimized_parallel_v2.py and
core/mock_artifacts.py. Expected counts are derived by hand: two requests with
no interface yield two failures; explicit opt-in yields two mock completions
and zero native completions. Saving both output types yields four artifacts.
The input text is a regression fixture, not an engineering model.
"""

import argparse
import json
import sys
from pathlib import Path
from unittest.mock import patch


def compact_summary(summary: dict) -> dict:
    """Retain completion counters without timing or absolute path details."""
    return {
        key: summary[key]
        for key in ("successful", "mock_successful", "failed")
    }


def run_example(directory: Path) -> dict:
    """Create a fresh synthetic case and read back its marked artifacts.

    Args:
        directory: New output directory; existing directories are refused.

    Returns:
        Independent refusal/regression counters and relative artifact paths.
    """
    # Block the binding before the first worker import, including when installed.
    # The example is run as a standalone process, never against a live model.
    with patch.dict(sys.modules, {"OrcFxAPI": None}):
        from digitalmodel.solvers.orcaflex.orcaflex_optimized_parallel_v2 import (
            ORCAFLEX_AVAILABLE,
            OrcaFlexOptimizedParallelAnalysis,
        )

        if ORCAFLEX_AVAILABLE:
            raise RuntimeError("Run the example in an isolated interpreter")
        directory.mkdir(parents=True, exist_ok=False)
        files = []
        for name in ("first", "second"):
            path = directory / f"{name}.dat"
            path.write_text("Synthetic regression input; not a native model.\n")
            files.append(str(path))

        # One thread is a demonstration resource choice, not a physical limit.
        analyzer = OrcaFlexOptimizedParallelAnalysis(num_threads=1, use_processes=False)
        config = {"output_dir": str(directory / "results"), "save_dat": True}
        refused = analyzer.process_files_parallel(files, config)
        regression = analyzer.process_files_parallel(
            files, dict(config, mock_mode=True)
        )

    if any(result["output_files"] for result in refused["results"]):
        raise RuntimeError("Unavailable execution reported an output")
    artifacts = []
    for result in regression["results"]:
        if result["status"] != "mock_success" or not result["is_mock"]:
            raise RuntimeError("Expected a marked regression completion")
        for output in result["output_files"]:
            path = Path(output)
            if not path.read_text().startswith("Mock simulation for "):
                raise RuntimeError("Regression artifact marker is missing")
            artifacts.append(path.relative_to(directory).as_posix())
    return {
        "refused": compact_summary(refused),
        "regression": compact_summary(regression),
        "artifacts": sorted(artifacts),
    }


def main() -> None:
    """Run the isolated example in a new directory supplied by the caller."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path, help="new example output directory")
    args = parser.parse_args()
    print("RESULT: " + json.dumps(run_example(args.directory), sort_keys=True))


if __name__ == "__main__":
    main()
