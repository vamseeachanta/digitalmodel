"""End-to-end demo: foam_system_sizing run wired into report_pack (#1587).

Same pattern as the fatg_spectral_fatigue demo
(``examples/workflows/report-pack/``): synthetic fixture -> sizing CSVs ->
md/html(/pdf) report with citations.json + report-layer provenance
manifest. The fixture and pack configs live in
``examples/workflows/foam-system-report-pack/``.
"""

import csv
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.foam_system.workflow import router as foam_router
from digitalmodel.report_pack.workflow import (
    MANIFEST_REQUIRED_FIELDS,
    router as report_router,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_DIR = REPO_ROOT / "examples" / "workflows" / "foam-system-report-pack"

SIZING_CSVS = (
    "foam_system_sizing_foam_demand.csv",
    "foam_system_sizing_foam_hydraulic_runs.csv",
    "foam_system_sizing_foam_hydraulic_terminals.csv",
)


def _run_sizing(output_dir: Path) -> dict:
    """Run the example sizing fixture with outputs redirected to ``output_dir``."""
    cfg_path = EXAMPLE_DIR / "foam_system_sizing.yml"
    cfg = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))
    assert cfg["basename"] == "foam_system_sizing"
    cfg["foam_system_sizing"]["output_dir"] = str(output_dir)
    cfg["_config_dir_path"] = str(EXAMPLE_DIR)
    cfg["_config_file_path"] = str(cfg_path)
    return foam_router(cfg)["foam_system_sizing"]


def _run_report(table_dir: Path, output_dir: Path) -> dict:
    """Run the example pack with tables read from ``table_dir``."""
    cfg_path = EXAMPLE_DIR / "input.yml"
    settings = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))
    assert settings["basename"] == "report_pack"
    pack = settings["report_pack"]
    for table in pack["results"]["tables"]:
        table["csv"] = str(table_dir / Path(table["csv"]).name)
    for appendix in pack["appendices"]:
        if appendix.get("files"):
            appendix["files"] = [str(table_dir / Path(f).name) for f in appendix["files"]]
    pack["output_dir"] = str(output_dir)
    return report_router(
        {
            "basename": "report_pack",
            "report_pack": pack,
            "_config_dir_path": str(EXAMPLE_DIR),
            "_config_file_path": str(cfg_path),
        }
    )


# -- fixture -> sizing CSVs -----------------------------------------------------


def test_sizing_fixture_regenerates_checked_in_csvs(tmp_path):
    """The checked-in demo CSVs are exactly what the fixture produces."""
    _run_sizing(tmp_path)
    for name in SIZING_CSVS:
        fresh = (tmp_path / name).read_bytes()
        checked_in = (EXAMPLE_DIR / "data" / name).read_bytes()
        assert fresh == checked_in, f"{name}: regenerate data/ from the fixture"


def test_sizing_fixture_hand_calc_and_library_citations(tmp_path):
    out = _run_sizing(tmp_path)
    demand = out["demand"]
    # greatest of the FSS deck-foam rate bases: 500 m2 x 6.0 = 3000 L/min
    assert demand["governing_area"] == "largest tank sectional area basis"
    assert demand["design_solution_flow_lpm"] == pytest.approx(3000.0)
    conc = out["concentrate_result"]
    assert conc["total_concentrate_l"] == pytest.approx(1800.0)
    assert conc["injection_rate_lpm"] == pytest.approx(90.0)
    assert out["proportioner_result"]["ok"]
    hyd = out["hydraulics_result"]
    assert hyd["governing_terminal"] == "MON_FWD"
    assert hyd["pump_head_ok"] and hyd["pump_flow_ok"]
    # criteria came from the shipped library, cited per row
    with (tmp_path / SIZING_CSVS[0]).open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 3
    for row in rows:
        assert "SOLAS FSS Code" in row["citation"]
        assert "Ch. 14" in row["citation"]


# -- sizing CSVs -> report pack ---------------------------------------------------


def test_example_pack_end_to_end(tmp_path):
    """Checked-in CSVs -> md/html report + citations.json + provenance manifest."""
    cfg = _run_report(EXAMPLE_DIR / "data", tmp_path / "results")
    out = tmp_path / "results"

    md = (out / "input_report.md").read_text(encoding="utf-8")
    assert "| Document number | B0000-RPT-002-00 |" in md
    # foam_system_sizing demand columns render inside the Results section
    results_at = md.index("## 5. Results")
    conclusions_at = md.index("## 6. Conclusions")
    demand_header_at = md.index(
        "| name | area_m2 | criterion | application_rate_lpm_per_m2 |"
    )
    assert results_at < demand_header_at < conclusions_at
    assert "largest tank sectional area basis" in md
    # per-row citation strings from the criteria library survive into the report
    assert "SOLAS FSS Code (Res. MSC.98(73) as amended (2015 consolidated edition))" in md
    # hydraulics tables render too
    assert "| terminal | flow_lpm | path |" in md
    assert "PUMP -> J1 -> MON_FWD" in md
    assert "## Appendix B — Calculation Records" in md

    html = (out / "input_report.html").read_text(encoding="utf-8")
    assert html.startswith("<!DOCTYPE html>")
    assert "<script" not in html
    assert "<th>application_rate_lpm_per_m2</th>" in html
    assert "IMO-FSS-CODE" in html

    citations = json.loads((out / "input_citations.json").read_text(encoding="utf-8"))
    code_ids = {c["code_id"] for c in citations["citations"]}
    assert code_ids == {"IMO-FSS-CODE", "NFPA-11"}

    manifest = json.loads(
        (out / "report-layer-manifest.json").read_text(encoding="utf-8")
    )
    for field in MANIFEST_REQUIRED_FIELDS:
        assert manifest[field], field
    assert manifest["issue"].endswith("/issues/1587")
    assert manifest["input_source_ids"] == ["EXAMPLE-FOAM-SYSTEM-SIZING-RESULTS"]
    assert manifest["source_artifacts"]["sizing_config"].endswith(
        "foam_system_sizing.yml"
    )
    assert "report-layer-manifest.json" in manifest["files"]

    # PDF is optional: either rendered or a clear fail-soft message recorded.
    status = cfg["report_pack"]["pdf_status"]
    if cfg["report_pack"]["pdf_report"] is None:
        assert "PDF not rendered" in status
    else:
        assert (out / "input_report.pdf").stat().st_size > 0


def test_full_pipeline_from_fresh_sizing_run(tmp_path):
    """fixture -> sizing CSVs -> report pack, with nothing read from data/."""
    sizing_dir = tmp_path / "sizing"
    _run_sizing(sizing_dir)
    cfg = _run_report(sizing_dir, tmp_path / "results")
    md = (tmp_path / "results" / "input_report.md").read_text(encoding="utf-8")
    assert "| largest tank sectional area basis | 500.0 |" in md
    assert cfg["report_pack"]["document_number"] == "B0000-RPT-002-00"
    manifest = json.loads(
        (tmp_path / "results" / "report-layer-manifest.json").read_text(
            encoding="utf-8"
        )
    )
    assert manifest["execution_tool"] == "digitalmodel.report_pack.workflow"
