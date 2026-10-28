# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regenerate every worked-example quantity and digest in the evidence note."""

import json
import re
import runpy
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

    assert _metadata_independent(output.read_text(encoding="utf-8")) == _metadata_independent(
        committed.read_text(encoding="utf-8"))


def _metadata_independent(text):
    # Installed package versions differ between environments. No digest,
    # input, output, calculation or refusal is normalized by this check.
    return re.sub(r'"tool_revision": "[^"\n]*"',
                  '"tool_revision": "<distribution metadata>"', text)


def test_evidence_note_regenerates_with_windows_newlines(tmp_path, monkeypatch):
    """Default text writes translate LF to CRLF; explicit LF remains unchanged."""
    original_open = Path.open

    def windows_open(path, mode="r", buffering=-1, encoding=None, errors=None,
                     newline=None):
        if "b" not in mode and any(flag in mode for flag in "wax") and newline is None:
            newline = "\r\n"
        return original_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", windows_open)
    probe = tmp_path / "newline-probe.txt"
    probe.write_text("first\nsecond\n", encoding="utf-8")
    assert probe.read_bytes() == b"first\r\nsecond\r\n"

    script = ROOT / "scripts/pipelay/generate_evidence_note.py"
    build_note = runpy.run_path(str(script))["build_note"]
    generated = build_note(tmp_path / "example")
    committed = ROOT / "src/digitalmodel/pipelay/docs/evidence-manifest.md"
    assert _metadata_independent(generated) == _metadata_independent(
        committed.read_text(encoding="utf-8"))

    summary = tmp_path / "example/issued/screening.json"
    saved = summary.read_bytes()
    assert b"\n" in saved
    assert b"\r" not in saved
    assert saved == json.dumps(json.loads(saved), indent=2, allow_nan=False).encode("utf-8")
