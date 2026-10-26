# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep omitted-shard reporting consistent with full-sweep triggers."""

import json
import os
from pathlib import Path
import subprocess
import sys


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/report_shard_scope.py"


def test_omitted_shard_summary_names_nightly_and_manual_coverage(tmp_path):
    """A scoped result must not promise that every main push runs all shards."""
    output = tmp_path / "outputs"
    summary = tmp_path / "summary.md"
    selected = {"include": [{"domain": "subsea"}]}
    full = {"include": [{"domain": "subsea"}, {"domain": "gis"}]}
    result = subprocess.run(
        [sys.executable, str(SCRIPT)],
        env={
            **os.environ,
            "SELECTED_MATRIX": json.dumps(selected),
            "FULL_MATRIX": json.dumps(full),
            "SCOPE_EVENT": "push",
            "GITHUB_OUTPUT": str(output),
            "GITHUB_STEP_SUMMARY": str(summary),
        },
        capture_output=True,
        text=True,
        check=True,
    )
    assert output.read_text().splitlines() == [
        "selected-count=1",
        "total-count=2",
        "skipped-count=1",
        "skipped-domains=gis",
        "scope-line=Shard scope: 1 of 2 shards dispatched (1 not run) for event 'push'.",
    ]
    text = " ".join(summary.read_text().split())
    assert "nightly and on manual dispatch" in text
    assert "every push" not in text
    assert "`.github/workflows/full-matrix-sweep.yml`" in text
    assert "- `tests-gis`" in text
    assert "Shards not run: tests-gis" in result.stdout
