# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regenerate every worked-example quantity and digest in the evidence note."""

import re
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def test_evidence_note_regenerates(tmp_path):
    script = ROOT / "scripts/pipelay/generate_evidence_note.py"
    output = tmp_path / "evidence-manifest.md"
    subprocess.run(
        [sys.executable, str(script), "--work-dir", str(tmp_path / "example"),
         "--output", str(output)],
        check=True, cwd=ROOT,
    )
    committed = ROOT / "src/digitalmodel/pipelay/docs/evidence-manifest.md"

    def metadata_independent(text):
        # Installed package versions differ between environments. No digest,
        # input, output, calculation or refusal is normalized by this check.
        return re.sub(r'"tool_revision": "[^"\n]*"',
                      '"tool_revision": "<distribution metadata>"', text)

    assert metadata_independent(output.read_text(encoding="utf-8")) == metadata_independent(
        committed.read_text(encoding="utf-8"))
