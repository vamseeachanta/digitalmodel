from __future__ import annotations

from pathlib import Path

import pytest

import digitalmodel.hydrodynamics.diffraction.polars_exporter as pe
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


def test_raos_to_polars_raises_when_polars_unavailable(
    mock_diffraction_results: DiffractionResults, monkeypatch: pytest.MonkeyPatch
) -> None:
    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", False)
    with pytest.raises(ImportError, match="polars is not installed"):
        exporter.raos_to_polars()


def test_added_mass_to_polars_raises_when_polars_unavailable(
    mock_diffraction_results: DiffractionResults, monkeypatch: pytest.MonkeyPatch
) -> None:
    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", False)
    with pytest.raises(ImportError, match="polars is not installed"):
        exporter.added_mass_to_polars()


def test_damping_to_polars_raises_when_polars_unavailable(
    mock_diffraction_results: DiffractionResults, monkeypatch: pytest.MonkeyPatch
) -> None:
    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", False)
    with pytest.raises(ImportError, match="polars is not installed"):
        exporter.damping_to_polars()


def test_raos_to_pandas_raises_when_pandas_unavailable(
    mock_diffraction_results: DiffractionResults, monkeypatch: pytest.MonkeyPatch
) -> None:
    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "PANDAS_AVAILABLE", False)
    with pytest.raises(ImportError, match="pandas is not installed"):
        exporter.raos_to_pandas()


def test_matrix_records_use_coupling_units_when_provided(
    mock_diffraction_results: DiffractionResults,
) -> None:
    mock_diffraction_results.added_mass.matrices[0].units["coupling"] = "kg.m"
    exporter = pe.PolarsExporter(mock_diffraction_results)
    records = exporter._build_matrix_records("added_mass")
    coupling = next(
        r
        for r in records
        if r["dof_i"] == "SURGE" and r["dof_j"] == "ROLL"
    )
    assert coupling["unit"] == "kg.m"


def test_export_added_mass_csv_falls_back_to_pandas_when_polars_disabled(
    mock_diffraction_results: DiffractionResults,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    if not pe.PANDAS_AVAILABLE:
        pytest.skip("pandas not installed")
    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", False)
    path = exporter.export_added_mass_csv(tmp_path)
    assert path.exists()
    assert path.name.endswith("_added_mass.csv")


def test_export_damping_csv_falls_back_to_pandas_when_polars_disabled(
    mock_diffraction_results: DiffractionResults,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    if not pe.PANDAS_AVAILABLE:
        pytest.skip("pandas not installed")
    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", False)
    path = exporter.export_damping_csv(tmp_path)
    assert path.exists()
    assert path.name.endswith("_damping.csv")


def test_polars_paths_work_with_stubbed_polars_module(
    mock_diffraction_results: DiffractionResults, monkeypatch: pytest.MonkeyPatch
) -> None:
    class DummyFrame:
        def __init__(self, records):
            self.records = records

        def write_csv(self, _path: Path) -> None:
            return None

    class DummyPolars:
        @staticmethod
        def DataFrame(records):
            return DummyFrame(records)

    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", True)
    monkeypatch.setattr(pe, "pl", DummyPolars, raising=False)

    rao_df = exporter.raos_to_polars()
    added_df = exporter.added_mass_to_polars()
    damp_df = exporter.damping_to_polars()

    assert len(rao_df.records) > 0
    assert len(added_df.records) > 0
    assert len(damp_df.records) > 0


def test_export_csv_uses_polars_writer_when_available(
    mock_diffraction_results: DiffractionResults,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    writes: list[Path] = []

    class DummyFrame:
        def __init__(self, records):
            self.records = records

        def write_csv(self, path: Path) -> None:
            writes.append(path)
            path.write_text("dummy")

    class DummyPolars:
        @staticmethod
        def DataFrame(records):
            return DummyFrame(records)

    exporter = pe.PolarsExporter(mock_diffraction_results)
    monkeypatch.setattr(pe, "POLARS_AVAILABLE", True)
    monkeypatch.setattr(pe, "pl", DummyPolars, raising=False)

    out = exporter.export_all_csv(tmp_path)
    assert set(out) == {"raos", "added_mass", "damping"}
    assert len(writes) == 3
    assert all(path.exists() for path in writes)
