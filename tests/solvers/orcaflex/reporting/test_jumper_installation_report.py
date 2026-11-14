"""Installation body content, chart data, escaping and template inclusion."""
from fnmatch import fnmatch
from importlib.resources import files
from pathlib import Path
import tomllib

from bs4 import BeautifulSoup
import pytest

from digitalmodel.marine_ops.installation.jumper_installation import PipelineOutput
from digitalmodel.marine_ops.installation.jumper_lift import run_jumper_analysis
from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report
from tests.solvers.orcaflex.reporting.test_synthetic_jumper_example import ExampleHTML


@pytest.fixture
def report():
    from digitalmodel.marine_ops.installation.jumper_report import build_installation_report
    output = PipelineOutput(config_name="Installation <test>", timestamp="2026-11-13T12:00:00",
                            jumper_analysis=run_jumper_analysis())
    return build_installation_report(output)


def test_installation_sections_and_charts(report, tmp_path):
    content = generate_orcaflex_report(report, tmp_path / "report.html", "inline").read_text(encoding="utf-8")
    soup = BeautifulSoup(content, "html.parser")
    assert [h.get_text() for h in soup.select("section h2")] == [
        "1. Cover Page", "2. Executive Summary", "3. Pipe Properties", "4. Geometry",
        "5. Weight Tally", "6. Rigging", "7. Crane Configuration", "8. OrcaFlex Sections",
        "9. COG per KIT", "10. Go/No-Go Decision Criteria", "11. Calculation References"]
    visible = soup.get_text()
    for text in ("Python installation calculations; no OrcaFlex solver analysis performed",
                 "Operator not supplied", "Classification not supplied", "2026-11-13",
                 "Rigging stiffness and spreader dimensions not returned", "not evaluated",
                 "Cell mappings not supplied", "Insulation linear mass not returned"):
        assert text in visible
    for anchor in ("crane-configuration", "cog"):
        assert "Baseline calculation values" in soup.select_one(f"#{anchor}").get_text()
        assert "not established as responsive to arbitrary spec overrides" in soup.select_one(f"#{anchor}").get_text()
    assert soup.select_one("#pipe-wt").get_text() == "1.790"
    assert "WT [in]" in visible and "Steel linear mass [kg/m]" in visible
    p = report.jumper_installation
    assert soup.select_one("#total-length").get_text() == f'{p["pipe_geometry"]["total_length_m"]:.3f} m'
    parsed = ExampleHTML()
    parsed.feed(content)
    weights, _ = parsed.chart("jumper-weight")
    lengths, layout = parsed.chart("jumper-lengths")
    assert weights[0]["values"] == pytest.approx(list(p["weight_items"].values()))
    assert weights[0]["labels"] == list(p["weight_items"])
    assert lengths[0]["y"] == pytest.approx([s["length_m"] for s in p["orcaflex_sections"]])
    assert lengths[0]["x"] == [s["name"] for s in p["orcaflex_sections"]]
    assert layout["yaxis"]["title"]["text"] == "Length [m]"
    assert not parsed.assets


def test_payload_strings_escape_and_missing_clamps(report, tmp_path):
    injection = "<script>alert(479)</script>"
    p = report.jumper_installation
    p["orcaflex_sections"][0]["name"] = injection
    p["cranes"]["SZ"]["name"] = injection
    p["criteria"] = [dict(name=injection, state="FAIL", value=4, limit=1, unit=injection,
                          margin=-3, description=injection, reference=injection)]
    p["pipe_geometry"].pop("clamp_locations_m")
    content = generate_orcaflex_report(report, tmp_path / "escaped.html", False).read_text(encoding="utf-8")
    assert injection not in content
    assert "&lt;script&gt;alert(479)&lt;/script&gt;" in content
    assert "Installation &lt;test&gt;" in content
    assert "Clamp positions not returned" in content


def test_template_resource_and_package_pattern():
    relative = "solvers/orcaflex/reporting/templates/jumper_installation.html.j2"
    assert files("digitalmodel").joinpath(relative).is_file()
    config = tomllib.loads((Path(__file__).resolve().parents[4] / "pyproject.toml").read_text())
    patterns = config["tool"]["setuptools"]["package-data"]["digitalmodel"]
    assert "solvers/orcaflex/reporting/templates/*.j2" in patterns
    assert any(fnmatch(relative, pattern) for pattern in patterns)
