import pytest
from pathlib import Path
from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report, OrcaFlexAnalysisReport

def test_plotly_cdn_security(tmp_path):
    report_data = OrcaFlexAnalysisReport(
        project_name="Security Test",
        structure_id="SEC-001",
        structure_type="riser"
    )
    
    output_file = tmp_path / "cdn_test.html"
    generate_orcaflex_report(
        report_data,
        output_path=output_file,
        include_plotlyjs="cdn"
    )
    
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        # Verify pinned version
        assert 'src="https://cdn.plot.ly/plotly-2.26.0.min.js"' in content
        # Verify SRI hash
        assert 'integrity="sha384-xuh4dD2xC9BZ4qOrUrLt8psbgevXF2v+K+FrXxV4MlJHnWKgnaKoh74vd/6Ik8uF"' in content
        # Verify crossorigin
        assert 'crossorigin="anonymous"' in content

def test_plotly_inline_operation(tmp_path):
    report_data = OrcaFlexAnalysisReport(
        project_name="Inline Test",
        structure_id="INL-001",
        structure_type="riser"
    )
    
    output_file = tmp_path / "inline_test.html"
    generate_orcaflex_report(
        report_data,
        output_path=output_file,
        include_plotlyjs="inline"
    )
    
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        # Should NOT have CDN link
        assert 'https://cdn.plot.ly/plotly' not in content
        # Should have inline script
        assert '<script type="text/javascript">' in content
        # Check for a known plotly string in the embedded bundle
        assert 'Plotly.newPlot' in content or 'plotly.js' in content.lower()
