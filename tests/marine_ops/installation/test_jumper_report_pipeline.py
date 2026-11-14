"""Real, solver-free installation report readback and opt-in contracts."""
import inspect
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

from digitalmodel.marine_ops.installation.jumper_installation import PipelineConfig, run_pipeline

SPECS = Path(__file__).resolve().parents[3] / "docs/domains/orcaflex/subsea/jumper/installation"
NAMES = ["gom_tieback_mf_plet", "gom_tieback_plet_plem"]


@pytest.mark.parametrize("name", NAMES)
def test_real_pipeline_report(name, tmp_path):
    output = run_pipeline(str(SPECS / name / "spec.yml"), str(tmp_path),
                          generate_report=True, run_orcaflex=False, run_go_no_go=False)
    assert output.report_path
    assert output.output_files.count(output.report_path) == 1
    content = Path(output.report_path).read_text(encoding="utf-8")
    soup = BeautifulSoup(content, "html.parser")
    results = output.jumper_analysis
    rows = soup.select("#orcaflex-sections tbody tr")
    assert len(rows) == 27
    for row, expected in zip(rows, results["orcaflex_sections"]):
        cells = [cell.get_text() for cell in row.select("td")]
        assert cells[:2] == [expected["name"], expected["line_type"]]
        assert float(cells[2]) == pytest.approx(expected["length_m"], abs=0.0005)
        assert float(cells[2]) > 0
    assert soup.select_one("#total-weight").get_text() == f'{results["weight_check"]["grand_total_kg"]:.3f} kg'
    assert soup.select_one("#pipe-wt").get_text() == f'{results["pipe_properties"].wall_thickness_inch:.3f}'
    assert "not evaluated" in soup.select_one("#decision-criteria").get_text()
    assert "Error rendering section" not in content
    assert not soup.select("script[src], link[href], img[src], iframe[src]")
    assert content.count("* plotly.js v") == 1


def test_defaults_preserve_outputs(tmp_path):
    assert PipelineConfig.__dataclass_fields__["generate_report"].default is False
    assert inspect.signature(run_pipeline).parameters["generate_report"].default is False
    outputs = [run_pipeline(str(SPECS / NAMES[0] / "spec.yml"), str(tmp_path / str(i)),
                            run_go_no_go=False, **flags)
               for i, flags in enumerate(({}, {"generate_report": False}))]
    assert all(output.report_path == "" for output in outputs)
    assert [Path(p).name for p in outputs[0].output_files] == [Path(p).name for p in outputs[1].output_files]
    assert not list(tmp_path.rglob("*.html"))


def test_real_splash_zone_failure(tmp_path):
    output = run_pipeline(str(SPECS / NAMES[0] / "spec.yml"), str(tmp_path), generate_report=True)
    soup = BeautifulSoup(Path(output.report_path).read_text(encoding="utf-8"), "html.parser")
    criterion = next(c for c in output.go_no_go.criteria if c.name == "Splash zone Hs")
    assert criterion.value == 4.0 and criterion.state.name == "FAIL"
    row = next(row for row in soup.select("#decision-criteria tbody tr") if criterion.name in row.get_text())
    assert [c.get_text() for c in row.select("td")] == [criterion.name, "FAIL", "4.000", f"{criterion.limit:.3f}", criterion.unit, f"{criterion.margin:.3f}", criterion.description, criterion.reference]
    assert output.go_no_go.overall_state.value in soup.select_one("#executive-summary").get_text()


def test_report_write_failure_surfaces(tmp_path, monkeypatch):
    from digitalmodel.marine_ops.installation import jumper_report

    def fail(*args, **kwargs):
        raise OSError("write failed")

    monkeypatch.setattr(jumper_report, "generate_orcaflex_report", fail)
    with pytest.raises(RuntimeError, match="Report generation failed"):
        run_pipeline(str(SPECS / NAMES[0] / "spec.yml"), str(tmp_path), generate_report=True)
    assert not list(tmp_path.glob("*.html"))
