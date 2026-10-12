"""CP provisional evidence survives the shared Markdown-to-HTML report layer."""

from digitalmodel.cathodic_protection import stray_current as sc
from digitalmodel.cathodic_protection._provisional import render_provisional_table
from digitalmodel.cathodic_protection.iccp_design import AnodeMaterial, anode_bed_design
from digitalmodel.reporting import markdown_to_html


def test_provisional_checklist_html_exposes_class_and_gap() -> None:
    html = markdown_to_html(render_provisional_table(sc.PROVISIONAL_VALUES))
    assert "Evidence class" in html
    assert "confirmed-by-official-preview" in html
    assert "reproduced-by-secondary" in html
    assert "inferred" in html
    assert "Confirm against" in html


def test_iccp_report_html_exposes_evidence_for_used_defaults() -> None:
    result = anode_bed_design(1.0, 50.0, anode_material=AnodeMaterial.GRAPHITE,
                             anode_mass_kg=20.0, experimental=True)
    html = markdown_to_html("\n\n".join(result.provisional_sources))
    assert "reproduced-by-secondary" in html
    assert "PROVISIONAL" in html
    assert "p56" in html
    assert "consumption_rate" in html
    assert "max_current_density" in html
    assert "utilisation_factor" in html
