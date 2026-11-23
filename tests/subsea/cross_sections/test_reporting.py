from __future__ import annotations

import hashlib
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.subsea.cross_sections.reporting import (
    build_report_model,
    fixture_paths,
    render_html_report,
    render_markdown_report,
)

FIXTURE_IDS = {
    "66kv-inter-array-cable",
    "220kv-hvac-export-cable",
    "concrete-coated-pipeline",
    "power-optical-hybrid-umbilical",
    "steel-tube-electro-hydraulic-umbilical",
}
REQUIRED_SECTIONS = [
    "Offshore Cable, Umbilical, and Pipeline Cross-Section Report",
    "Regeneration",
    "Fixture Inventory",
    "Cross-Family Comparison",
    "Cross-Section Visuals",
    "Provenance",
    "Engineering Caveats",
    "Deferred Scope and Follow-Ups",
]


def test_report_model_loads_all_fixtures_in_deterministic_order() -> None:
    paths = fixture_paths()
    assert [path.name for path in paths] == [
        "220kv_hvac_export_cable.yml",
        "66kv_inter_array_cable.yml",
        "concrete_coated_pipeline.yml",
        "power_optical_hybrid_umbilical.yml",
        "steel_tube_electro_hydraulic_umbilical.yml",
    ]

    model = build_report_model(paths)

    assert {fixture.definition.id for fixture in model.fixtures} == FIXTURE_IDS
    assert [fixture.definition.id for fixture in model.fixtures] == [
        "66kv-inter-array-cable",
        "220kv-hvac-export-cable",
        "power-optical-hybrid-umbilical",
        "concrete-coated-pipeline",
        "steel-tube-electro-hydraulic-umbilical",
    ]
    assert any(item.source_id.startswith("wiki-") for item in model.provenance)
    assert model.caveats == [
        "cable-duty",
        "umbilical-schematic",
        "pipeline-route-coating",
        "flexible-pipe-deferred",
    ]


def test_markdown_and_html_share_required_contract_without_machine_paths() -> None:
    model = build_report_model(fixture_paths())
    markdown = render_markdown_report(model)
    html = render_html_report(model)

    for section in REQUIRED_SECTIONS:
        assert section in markdown
        assert section in html
    for fixture_id in FIXTURE_IDS:
        assert fixture_id in markdown
        assert fixture_id in html
        assert f'id="visual-{fixture_id}"' in html
    assert "flexible pipe/riser mechanics are deferred to #2516" in markdown
    assert "not-to-scale schematic" in markdown
    assert "not-to-scale schematic" in html
    assert "66 kV" in markdown
    assert "220 kV" in markdown
    assert "MPa" in markdown or "bar" in markdown
    for forbidden in ("/mnt/", "/tmp/", "workspace-hub", "digitalmodel-2515"):
        assert forbidden not in markdown
        assert forbidden not in html
    assert hashlib.sha256(render_markdown_report(model).encode()).hexdigest() == hashlib.sha256(
        markdown.encode()
    ).hexdigest()


def test_cli_generates_requested_formats_atomically(tmp_path: Path) -> None:
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "digitalmodel.subsea.cross_sections.cli",
            "--output-dir",
            str(tmp_path),
            "--format",
            "all",
        ],
        check=True,
        cwd=Path(__file__).parents[3],
        env={"PYTHONPATH": "src"},
        text=True,
        capture_output=True,
    )

    md = tmp_path / "offshore_cross_section_report.md"
    html = tmp_path / "offshore_cross_section_report.html"
    assert md.exists()
    assert html.exists()
    assert "offshore_cross_section_report.md" in result.stdout
    assert "offshore_cross_section_report.html" in result.stdout
    assert "Offshore Cable, Umbilical, and Pipeline Cross-Section Report" in md.read_text()
    assert "<html" in html.read_text()
    assert not list(tmp_path.glob("*.tmp"))


@pytest.mark.parametrize("fmt,expected", [("md", "offshore_cross_section_report.md"), ("html", "offshore_cross_section_report.html")])
def test_cli_format_filter(tmp_path: Path, fmt: str, expected: str) -> None:
    subprocess.run(
        [
            sys.executable,
            "-m",
            "digitalmodel.subsea.cross_sections.cli",
            "--output-dir",
            str(tmp_path),
            "--format",
            fmt,
        ],
        check=True,
        cwd=Path(__file__).parents[3],
        env={"PYTHONPATH": "src"},
        text=True,
        capture_output=True,
    )

    assert (tmp_path / expected).exists()
    assert len(list(tmp_path.iterdir())) == 1
