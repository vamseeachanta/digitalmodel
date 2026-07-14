# ABOUTME: End-to-end demo test: alt_fuel_ship_sizing trade-study run ->
# ABOUTME: CSVs -> report_pack render, plus checked-in example consistency.
"""alt_fuel_ship_sizing -> report_pack end-to-end demo (issue #1591).

Same pattern as the fatg_spectral_fatigue example
(``examples/workflows/report-pack/``): a synthetic sizing run produces the
CSVs, the report-pack workflow renders the standard report with citations
and a provenance manifest. The demo lives in
``examples/workflows/alt-fuel-report-pack/``.
"""

import csv
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.alt_fuel_ship_sizing.workflow import router as sizing_router
from digitalmodel.report_pack.workflow import router as report_router

REPO_ROOT = Path(__file__).resolve().parents[2]
DEMO_DIR = REPO_ROOT / "examples" / "workflows" / "alt-fuel-report-pack"

CSV_NAMES = (
    "sizing_input_fuel_chain_summary.csv",
    "sizing_input_voyage_legs.csv",
    "sizing_input_trade_study.csv",
)


def _run_sizing(config_dir: Path) -> dict:
    cfg = yaml.safe_load((DEMO_DIR / "sizing_input.yml").read_text(encoding="utf-8"))
    assert cfg["basename"] == "alt_fuel_ship_sizing"
    cfg["_config_dir_path"] = str(config_dir)
    cfg["_config_file_path"] = str(config_dir / "sizing_input.yml")
    return sizing_router(cfg)


def _run_report(config_dir: Path, output_dir: Path) -> dict:
    settings = yaml.safe_load((DEMO_DIR / "input.yml").read_text(encoding="utf-8"))
    assert settings["basename"] == "report_pack"
    pack = settings["report_pack"]
    pack["output_dir"] = str(output_dir)
    return report_router(
        {
            "basename": "report_pack",
            "report_pack": pack,
            "_config_dir_path": str(config_dir),
            "_config_file_path": str(config_dir / "input.yml"),
        }
    )


def test_demo_end_to_end_sizing_run_to_report_pack(tmp_path):
    """The full demo chain in a scratch directory: trade-study run -> CSVs
    -> report pack (md + self-contained html + citations + manifests)."""
    result = _run_sizing(tmp_path)["alt_fuel_ship_sizing"]
    for name in CSV_NAMES:
        assert (tmp_path / "data" / name).exists(), name
    # Port dwell drives the tank size past the dwell-free 1856.34 m3.
    assert result["summary"]["voyage_profile"] is True
    assert result["summary"]["gross_tank_volume_m3"] > 1856.34

    cfg = _run_report(tmp_path, tmp_path / "results")
    out = tmp_path / "results"

    md = (out / "input_report.md").read_text(encoding="utf-8")
    # All three sizing tables render into the pack.
    assert "required_fuel_mass_kg" in md
    assert "| index | leg_type |" in md
    assert "v12kn_nowind" in md
    assert "## Appendix B — Calculation Records" in md

    html = (out / "input_report.html").read_text(encoding="utf-8")
    assert html.startswith("<!DOCTYPE html>")
    assert "<script" not in html
    assert "<th>leg_type</th>" in html

    sidecar = json.loads((out / "input_citations.json").read_text(encoding="utf-8"))
    cited = {item["code_id"] for item in sidecar["citations"]}
    assert cited == {"NBS-MONOGRAPH-168", "DOE-H2-HARC"}

    manifest = json.loads(
        (out / "report-layer-manifest.json").read_text(encoding="utf-8")
    )
    assert manifest["project"] == "B0000"
    assert manifest["input_source_ids"] == ["EXAMPLE-ALT-FUEL-TRADE-STUDY"]
    assert manifest["issue"].endswith("/issues/1591")

    # PDF is optional: either a real PDF was produced or a clear message.
    status = cfg["report_pack"]["pdf_status"]
    if cfg["report_pack"]["pdf_report"] is None:
        assert "PDF not rendered" in status
    else:
        assert (out / "input_report.pdf").stat().st_size > 0


def test_checked_in_demo_csvs_match_a_fresh_run(tmp_path):
    """Guard against drift: the CSVs committed under the example's data/
    directory reproduce a fresh run of sizing_input.yml (same columns, same
    rows, values equal to float round-trip)."""
    _run_sizing(tmp_path)
    for name in CSV_NAMES:
        committed = (DEMO_DIR / "data" / name).read_text(encoding="utf-8")
        fresh = (tmp_path / "data" / name).read_text(encoding="utf-8")
        committed_rows = list(csv.DictReader(committed.splitlines()))
        fresh_rows = list(csv.DictReader(fresh.splitlines()))
        assert len(committed_rows) == len(fresh_rows), name
        for committed_row, fresh_row in zip(committed_rows, fresh_rows):
            assert committed_row.keys() == fresh_row.keys(), name
            for key, committed_value in committed_row.items():
                fresh_value = fresh_row[key]
                try:
                    assert float(committed_value) == pytest.approx(
                        float(fresh_value), rel=1e-12
                    ), (name, key)
                except ValueError:
                    assert committed_value == fresh_value, (name, key)


def test_checked_in_demo_pack_renders_from_example_dir(tmp_path):
    """The committed example renders as-is (config dir = the example dir,
    consuming the committed CSVs), matching the fatg example pattern."""
    _run_report(DEMO_DIR, tmp_path / "results")
    md = (tmp_path / "results" / "input_report.md").read_text(encoding="utf-8")
    assert "| Document number | B0000-RPT-002-00 |" in md
    assert "port" in md and "gross_tank_volume_m3" in md
