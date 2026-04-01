"""Unit tests for solver smoke-test helpers."""
from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from types import SimpleNamespace

import openpyxl

SMOKE_TEST_PATH = Path(__file__).parents[2] / "solver" / "smoke_test.py"
_SPEC = importlib.util.spec_from_file_location("solver_smoke_test_module", SMOKE_TEST_PATH)
assert _SPEC is not None and _SPEC.loader is not None
smoke_test = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(smoke_test)


class _FakeDiffraction:
    def __init__(self, source_path: str, export_raises: bool = False) -> None:
        self.source_path = source_path
        self.state = "loaded"
        self.frequencyCount = 3
        self._export_raises = export_raises

    def Calculate(self) -> None:
        self.state = "solved"

    def SaveData(self, target: str) -> None:
        Path(target).write_text(f"saved from {self.source_path}\n", encoding="utf-8")

    def ExportResults(self, target: str) -> None:
        if self._export_raises:
            raise RuntimeError("native export unavailable")
        Path(target).write_text("native export\n", encoding="utf-8")

    def frequency(self, index: int) -> float:
        return [0.1, 0.2, 0.3][index]


def _install_fake_orcfxapi(monkeypatch, export_raises: bool = False) -> None:
    fake_module = SimpleNamespace(
        Diffraction=lambda source: _FakeDiffraction(source, export_raises=export_raises)
    )
    monkeypatch.setitem(sys.modules, "OrcFxAPI", fake_module)


def test_export_excel_artifact_uses_native_export(tmp_path: Path) -> None:
    diff = _FakeDiffraction("native")
    xlsx_path = tmp_path / "native.xlsx"

    exported = smoke_test._export_excel_artifact(diff, xlsx_path, "TEST")

    assert exported is True
    assert xlsx_path.read_text(encoding="utf-8") == "native export\n"


def test_export_excel_artifact_falls_back_to_workbook(tmp_path: Path) -> None:
    diff = _FakeDiffraction("fallback", export_raises=True)
    xlsx_path = tmp_path / "fallback.xlsx"

    exported = smoke_test._export_excel_artifact(diff, xlsx_path, "TEST")

    assert exported is True
    workbook = openpyxl.load_workbook(xlsx_path)
    sheet = workbook.active
    assert sheet.title == "Frequencies"
    assert sheet["A1"].value == "Index"
    assert sheet["B2"].value == 0.1


def test_run_l01_smoke_test_writes_expected_fixtures(
    tmp_path: Path,
    monkeypatch,
) -> None:
    source_owr = tmp_path / "source.owr"
    source_owr.write_text("seed result\n", encoding="utf-8")
    fixtures_dir = tmp_path / "fixtures"

    _install_fake_orcfxapi(monkeypatch)
    monkeypatch.setattr(smoke_test, "L01_OWR_SOURCE", source_owr)
    monkeypatch.setattr(smoke_test, "FIXTURES_DIR", fixtures_dir)

    assert smoke_test.run_l01_smoke_test() is True
    assert (fixtures_dir / "L01_001_ship_raos.owr").exists()
    assert (fixtures_dir / "L01_001_ship_raos.xlsx").exists()


def test_run_l00_smoke_test_fails_when_excel_artifact_missing(
    tmp_path: Path,
    monkeypatch,
) -> None:
    source_owd = tmp_path / "source.owd"
    source_owd.write_text("seed input\n", encoding="utf-8")
    fixtures_dir = tmp_path / "fixtures"

    _install_fake_orcfxapi(monkeypatch, export_raises=True)
    monkeypatch.setattr(smoke_test, "L00_OWD", source_owd)
    monkeypatch.setattr(smoke_test, "FIXTURES_DIR", fixtures_dir)
    monkeypatch.setattr(smoke_test, "_write_frequency_workbook", lambda *_args: False)

    assert smoke_test.run_l00_smoke_test() is False
    assert (fixtures_dir / "L00_test01.owr").exists()
    assert not (fixtures_dir / "L00_test01.xlsx").exists()


def test_promote_committed_artifacts_copies_available_files(
    tmp_path: Path,
    monkeypatch,
) -> None:
    fixtures_dir = tmp_path / "fixtures"
    queue_completed_dir = tmp_path / "queue" / "completed" / "20260401T021707Z-test01"
    benchmark_dir = tmp_path / "benchmark"

    queue_completed_dir.mkdir(parents=True)
    benchmark_dir.mkdir(parents=True)

    (queue_completed_dir / "test01.owr").write_text("l00 owr\n", encoding="utf-8")
    (benchmark_dir / "orcawave_001_ship_raos_rev2.owr").write_text("l01 owr\n", encoding="utf-8")
    (benchmark_dir / "orcawave_001_ship_raos_rev2.xlsx").write_text("l01 xlsx\n", encoding="utf-8")

    monkeypatch.setattr(smoke_test, "FIXTURES_DIR", fixtures_dir)
    monkeypatch.setattr(smoke_test, "QUEUE_COMPLETED_DIR", queue_completed_dir.parent)
    monkeypatch.setattr(smoke_test, "L01_BENCHMARK_DIR", benchmark_dir)

    promoted = smoke_test.promote_committed_artifacts()

    assert promoted == {
        "L00_test01.owr": True,
        "L00_test01.xlsx": False,
        "L01_001_ship_raos.owr": True,
        "L01_001_ship_raos.xlsx": True,
    }
    assert (fixtures_dir / "L00_test01.owr").read_text(encoding="utf-8") == "l00 owr\n"
    assert (fixtures_dir / "L01_001_ship_raos.owr").read_text(encoding="utf-8") == "l01 owr\n"
    assert (fixtures_dir / "L01_001_ship_raos.xlsx").read_text(encoding="utf-8") == "l01 xlsx\n"
