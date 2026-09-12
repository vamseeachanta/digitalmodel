"""Offline attribution checks: only outputs changed by this run are captured."""

import os
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys.runner import ANSYSRunner, run_ansys


@pytest.mark.parametrize("mode", ["fresh", "unchanged", "overwritten"])
def test_csv_capture_requires_current_run_evidence(tmp_path, monkeypatch, mode):
    script = tmp_path / "model.inp"
    script.write_text("FINISH\n")
    output = tmp_path / "output"
    output.mkdir()
    digest = output / "model_result.csv"
    stale_result = output / "previous.rst"
    stale_result.write_bytes(b"previous run")
    if mode != "fresh":
        digest.write_text("stress,1.0\n")
        previous = digest.stat()
        original_stat = Path.stat

        def pinned_stat(path, *args, **kwargs):
            return previous if path == digest else original_stat(path, *args, **kwargs)

        # Pin all metadata, including POSIX ctime: only the content hash differs.
        monkeypatch.setattr(Path, "stat", pinned_stat)

    def simulate(*args, **kwargs):
        if mode != "unchanged":
            digest.write_text("stress,2.0\n")
            if mode == "overwritten":
                # Same size and restored mtime must not hide new CSV contents.
                os.utime(digest, ns=(previous.st_atime_ns, previous.st_mtime_ns))
        (output / "model.out").write_text("completed")
        return SimpleNamespace(returncode=0, stdout="", stderr="")

    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("fake"))
    monkeypatch.setattr("digitalmodel.ansys.runner.subprocess.run", simulate)
    result = run_ansys(script, output_dir=output)
    assert (digest in result.result_files) == (mode != "unchanged")
    assert stale_result not in result.result_files
    assert output / "model.out" in result.result_files
