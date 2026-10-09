"""Conversion diagnostics must fail closed and preserve existing evidence."""

import importlib.util
from pathlib import Path

import ezdxf
import pytest

SCRIPT = (
    Path(__file__).resolve().parents[3] / "scripts/hull_library/dwg_conversion_probe.py"
)


def load_probe():
    spec = importlib.util.spec_from_file_location("dwg_probe", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_malformed_dxf_reports_failure(tmp_path):
    path = tmp_path / "bad.dxf"
    path.write_text("not a drawing")
    report = load_probe().inspect_dxf(path)
    assert report["opens"] is False
    assert report["readiness"] == "blocked"
    assert report["error"]


def test_openable_dxf_is_not_automatically_qualified(tmp_path):
    path = tmp_path / "drawing.dxf"
    doc = ezdxf.new()
    doc.modelspace().add_lwpolyline([(0, 0), (4, 3)], dxfattribs={"layer": "SECTIONS"})
    doc.saveas(path)
    report = load_probe().inspect_dxf(path)
    assert report["opens"] is True
    assert report["readiness"] == "unverified"
    assert report["entities_by_type"]["LWPOLYLINE"] == 1
    assert report["entities_by_layer"]["SECTIONS"] == 1
    assert report["modelspace_bbox"]["maximum"] == [4, 3, 0]


def test_existing_output_is_preserved_before_converter_runs(tmp_path):
    source, output = tmp_path / "source.dwg", tmp_path / "result.dxf"
    source.write_bytes(b"AC1032source")
    output.write_bytes(b"existing evidence")
    with pytest.raises(FileExistsError):
        load_probe().convert(source, output, "nonexistent-converter")
    assert source.read_bytes() == b"AC1032source"
    assert output.read_bytes() == b"existing evidence"


def test_incomplete_inspection_makes_cli_fail(tmp_path, monkeypatch):
    probe = load_probe()
    path = tmp_path / "drawing.dxf"
    ezdxf.new().saveas(path)

    def fail_bounds(*args, **kwargs):
        raise ValueError("invalid geometry")

    monkeypatch.setattr(probe.bbox, "extents", fail_bounds)
    report = probe.inspect_dxf(path)
    assert report["readiness"] == "blocked"
    monkeypatch.setattr(
        probe, "convert", lambda *args: {"returncode": 0, "dxf": report}
    )
    monkeypatch.setattr("sys.argv", ["probe", "source.dwg", "output.dxf"])
    assert probe.main() == 1


def fake_converter(tmp_path, body):
    import os
    import sys

    if os.name == "nt":
        pytest.skip("POSIX executable fixture")
    script = tmp_path / "converter"
    script.write_text(f"#!{sys.executable}\n" + body)
    script.chmod(0o755)
    return script


def test_converter_timeout_retains_blocker_and_source(tmp_path):
    converter = fake_converter(tmp_path, "import time\ntime.sleep(2)\n")
    source = tmp_path / "input.dwg"
    source.write_bytes(b"AC1032source")
    report = load_probe().convert(
        source, tmp_path / "result.dxf", converter, timeout=0.01
    )
    assert report["returncode"] is None
    assert "TimeoutExpired" in report["error"]
    assert report["dxf"]["readiness"] == "blocked"
    assert source.read_bytes() == b"AC1032source"


def test_converter_failure_retains_stderr_and_clears_git_environment(
    tmp_path, monkeypatch
):
    converter = fake_converter(
        tmp_path,
        "import os,sys\nif '--version' in sys.argv:\n print('fixture 1')\nelse:\n print(str({k:v for k,v in os.environ.items() if k.startswith('GIT_')}))\n print('decode failed',file=sys.stderr)\n sys.exit(7)\n",
    )
    source = tmp_path / "input.dwg"
    source.write_bytes(b"AC1032source")
    monkeypatch.setenv("GIT_DIR", str(tmp_path / "caller-git"))
    report = load_probe().convert(source, tmp_path / "result.dxf", converter)
    assert report["returncode"] == 7
    assert report["stdout"].strip() == "{}"
    assert report["stderr"].strip() == "decode failed"
    assert report["dxf"]["readiness"] == "blocked"


def test_conversion_timeout_retains_partial_diagnostics(tmp_path):
    converter = fake_converter(
        tmp_path,
        "import sys,time\nif '--version' in sys.argv:\n print('fixture 1')\nelse:\n print('decode started',flush=True)\n time.sleep(2)\n",
    )
    source = tmp_path / "input.dwg"
    source.write_bytes(b"AC1032source")
    report = load_probe().convert(
        source, tmp_path / "result.dxf", converter, timeout=0.1
    )
    assert report["converter_version"] == "fixture 1"
    assert report["returncode"] is None
    assert "decode started" in report["stdout"]
    assert "TimeoutExpired" in report["error"]


def test_staged_digest_mismatch_stops_before_conversion(tmp_path, monkeypatch):
    probe = load_probe()
    source = tmp_path / "source.dwg"
    source.write_bytes(b"AC1032source")
    converter = fake_converter(tmp_path, "raise AssertionError('must not execute')\n")
    monkeypatch.setattr(
        probe.shutil,
        "copyfile",
        lambda source, target: Path(target).write_bytes(b"changed"),
    )
    with pytest.raises(RuntimeError, match="Staged source differs"):
        probe.convert(source, tmp_path / "result.dxf", converter)
    assert source.read_bytes() == b"AC1032source"
