"""C11 review round 2, findings 4-6: the scripts the cleanup made configurable.

4. An explicit ``ORCAWAVE_EXE`` that does not exist must stop the run, not
   fall back to whichever OrcaWave install autodetection finds -- that can be
   a different solver version.
5. A batch file that expands ``%DIGITALMODEL_PRIVATE_DATA%`` or
   ``%WORKSPACE_ROOT%`` must quote the path, or a folder with a space in its
   name becomes two arguments.
6. ``fix_working_model.py`` must reference the mesh at the path it wrote it.
"""

from __future__ import annotations

import importlib.util
import os
import re
import subprocess
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
BENCH = REPO / "docs" / "domains" / "orcawave" / "L01_aqwa_benchmark"
FIX_MODEL = (
    REPO / "scripts" / "python" / "digitalmodel" / "orcawave" / "fix_working_model.py"
)
CONFIGURED_VARS = ("DIGITALMODEL_PRIVATE_DATA", "WORKSPACE_ROOT")


def _load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


# -- finding 4 ---------------------------------------------------------------

_FINDERS = [
    ("run_orcawave_benchmark.py", "find_orcawave_exe"),
    ("run_orcawave_shell.py", "find_orcawave_executable"),
]


@pytest.mark.parametrize("script,func", _FINDERS)
def test_a_missing_configured_orcawave_is_an_error(monkeypatch, tmp_path, script, func):
    mod = _load(BENCH / script, f"_r2_{script[:-3]}")
    monkeypatch.setenv("ORCAWAVE_EXE", str(tmp_path / "absent" / "OrcaWave.exe"))
    with pytest.raises(FileNotFoundError, match="ORCAWAVE_EXE"):
        getattr(mod, func)()


@pytest.mark.parametrize("script,func", _FINDERS)
def test_an_existing_configured_orcawave_is_used(monkeypatch, tmp_path, script, func):
    mod = _load(BENCH / script, f"_r2_{script[:-3]}")
    exe = tmp_path / "OrcaWave.exe"
    exe.write_bytes(b"")
    monkeypatch.setenv("ORCAWAVE_EXE", str(exe))
    assert Path(getattr(mod, func)()) == exe


@pytest.mark.parametrize("script,func", _FINDERS)
def test_the_override_is_read_when_called_not_at_import(
    monkeypatch, tmp_path, script, func
):
    monkeypatch.delenv("ORCAWAVE_EXE", raising=False)
    mod = _load(BENCH / script, f"_r2_{script[:-3]}")
    monkeypatch.setenv("ORCAWAVE_EXE", str(tmp_path / "absent.exe"))
    with pytest.raises(FileNotFoundError):
        getattr(mod, func)()


# -- finding 5 ---------------------------------------------------------------


def _tracked_batch_files() -> list[str]:
    out = subprocess.run(
        ["git", "ls-files", "-z", "--", "*.bat", "*.cmd"],
        cwd=REPO,
        capture_output=True,
        check=True,
    )
    return [p for p in out.stdout.decode("utf-8", "surrogateescape").split("\0") if p]


def _unquoted_uses(text: str) -> list[str]:
    bad = []
    for line in text.splitlines():
        stripped = line.strip().lower()
        if stripped.startswith(("rem ", "::", "@rem ")):
            continue
        outside = re.sub(r'"[^"]*"', '""', line)
        if any(f"%{v}%" in outside for v in CONFIGURED_VARS):
            bad.append(line.strip())
    return bad


def test_the_unquoted_detector_itself():
    assert _unquoted_uses("aqwa.exe /nowind %DIGITALMODEL_PRIVATE_DATA%\\a")
    assert _unquoted_uses("cd /d %WORKSPACE_ROOT%")
    assert not _unquoted_uses('aqwa.exe /nowind "%DIGITALMODEL_PRIVATE_DATA%\\a"')
    assert not _unquoted_uses("if not defined DIGITALMODEL_PRIVATE_DATA (")


def test_configured_paths_in_batch_files_are_quoted():
    bad = {}
    for rel in _tracked_batch_files():
        text = (REPO / rel).read_text(encoding="utf-8", errors="replace")
        uses = _unquoted_uses(text)
        if uses:
            bad[rel] = uses[:2]
    assert not bad, bad


# -- finding 6 ---------------------------------------------------------------


def test_the_minimal_model_references_the_mesh_it_wrote(monkeypatch, tmp_path):
    mod = _load(FIX_MODEL, "_r2_fix_working_model")
    monkeypatch.chdir(tmp_path)
    written = mod.create_simple_gdf()
    mesh = Path(written)
    assert mesh.is_absolute()
    assert mesh.is_file()
    text = (tmp_path / "minimal_test_box.yml").read_text(encoding="utf-8")
    config = yaml.safe_load(text.split("---\n", 1)[1])
    configured = Path(config["Bodies"][0]["BodyMeshFileName"])
    assert configured.is_absolute()
    assert os.path.samefile(configured, mesh)
    assert configured.parent == tmp_path.resolve()
