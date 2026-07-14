# ABOUTME: End-to-end validation of the hull-girder report-pack demo — screening
# ABOUTME: run -> committed CSVs/margin plot -> standard report pack render.
"""End-to-end tests for the hull-girder screening report-pack demo.

Validates the full #1593 chain: the ``hull_girder_screening`` run defined by
``examples/workflows/hull-girder-report-pack/screening_input.yml`` reproduces
the committed data files byte-for-byte (the demo data can never drift from
the workflow), and the ``report_pack`` config in the same folder renders the
utilisation table + margin plot into the standard report pack.
"""

import importlib.util
import json
from pathlib import Path

import yaml

from digitalmodel.report_pack.workflow import router as report_pack_router

REPO_ROOT = Path(__file__).resolve().parents[2]
DEMO_DIR = REPO_ROOT / "examples" / "workflows" / "hull-girder-report-pack"


def _load_generator():
    spec = importlib.util.spec_from_file_location(
        "hull_girder_demo_data", DEMO_DIR / "make_demo_data.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_committed_demo_data_matches_regeneration(tmp_path):
    """Regenerating the demo data reproduces the committed files exactly."""
    generator = _load_generator()
    result = generator.generate(tmp_path)
    assert result["screening_status"] == "pass"
    committed = sorted(p.name for p in (DEMO_DIR / "data").iterdir())
    regenerated = sorted(p.name for p in tmp_path.iterdir())
    assert committed == regenerated

    def _normalized(path):
        # newline-insensitive: git normalizes CRLF -> LF on commit while the
        # csv module writes CRLF, so raw bytes differ by convention only
        return path.read_bytes().replace(b"\r\n", b"\n")

    for name in committed:
        assert _normalized(tmp_path / name) == _normalized(
            DEMO_DIR / "data" / name
        ), f"demo data drift: {name} (rerun make_demo_data.py)"


def test_demo_report_pack_end_to_end(tmp_path):
    settings = yaml.safe_load((DEMO_DIR / "input.yml").read_text(encoding="utf-8"))
    assert settings["basename"] == "report_pack"
    pack = settings["report_pack"]
    pack["output_dir"] = str(tmp_path / "results")
    pack["pdf"] = "auto"  # exercise the renderer chain; must fail soft if absent
    cfg = report_pack_router(
        {
            "basename": "report_pack",
            "report_pack": pack,
            "_config_dir_path": str(DEMO_DIR),
            "_config_file_path": str(DEMO_DIR / "input.yml"),
        }
    )
    out = tmp_path / "results"
    md = (out / "input_report.md").read_text(encoding="utf-8")
    # SF/BM utilisation table columns from the screening run
    assert (
        "| frame | x_m | total_hogging_t_m | total_sagging_t_m "
        "| bending_utilization |" in md
    )
    assert "| Midship |" in md
    # screening summary metrics + governance posture
    assert "S11 wave coefficient C" in md
    assert "class-approved loading instrument governs" in md
    assert "IACS-UR-S11" in md
    assert "## Appendix B — Calculation Records" in md

    html = (out / "input_report.html").read_text(encoding="utf-8")
    assert "<script" not in html
    # margin plot embedded as a self-contained data URI
    assert "data:image/svg+xml;base64," in html
    assert "margin plot" in html

    manifest = json.loads(
        (out / "report-layer-manifest.json").read_text(encoding="utf-8")
    )
    assert manifest["issue"].endswith("/issues/1593")
    assert manifest["input_source_ids"] == ["EXAMPLE-HULL-GIRDER-SCREENING-RESULTS"]
    assert manifest["privacy_classification"] == "public-example"

    citations = json.loads(
        (out / "input_citations.json").read_text(encoding="utf-8")
    )
    assert citations["citations"][0]["code_id"] == "IACS-UR-S11"

    # PDF is optional: either a real PDF was produced or a clear message recorded.
    if cfg["report_pack"]["pdf_report"] is None:
        assert "PDF not rendered" in cfg["report_pack"]["pdf_status"]
    else:
        assert (out / "input_report.pdf").stat().st_size > 0
