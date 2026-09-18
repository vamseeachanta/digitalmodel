"""PDF transfer checks, including normally collapsed report evidence."""
import importlib.util
from pathlib import Path


def test_complete_pdf_transfer(tmp_path):
    import pymupdf
    module_path = (Path(__file__).parents[2] / 'src/digitalmodel/asset_integrity/'
                   'assessment/example_vessel_pdf.py')
    spec = importlib.util.spec_from_file_location('vessel_pdf', module_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    source = tmp_path / 'source.html'
    source.write_text('<main><header class="cover"><h1>Vessel assessment</h1></header>'
        '<section id="summary"><h2>1 Summary</h2><p>No field measurements were supplied.</p>'
        '<details><summary>Evidence</summary><table><tr><th>Pressure (MPa)</th></tr>'
        '<tr><td>0.650</td></tr></table></details></section></main>', encoding='utf-8')
    output = tmp_path / 'report.pdf'
    module.export_pdf(source, output)
    with pymupdf.open(output) as pdf:
        text = ''.join(page.get_text() for page in pdf)
        assert '0.650' in text
        assert 'No field measurements were supplied.' in text
        assert pdf.get_toc()


def test_missing_figure_stops_export(tmp_path):
    import pytest
    module_path = (Path(__file__).parents[2] / 'src/digitalmodel/asset_integrity/'
                   'assessment/example_vessel_pdf.py')
    spec = importlib.util.spec_from_file_location('vessel_pdf', module_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    source = tmp_path / 'source.html'
    source.write_text('<main><img src="missing.png"></main>', encoding='utf-8')
    with pytest.raises(FileNotFoundError):
        module.export_pdf(source, tmp_path / 'report.pdf')
