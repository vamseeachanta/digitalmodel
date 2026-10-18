# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report tests consume synthetic summaries, never licensed software."""
import importlib.util
import json
import shutil
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from bs4 import BeautifulSoup

CRITERIA = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
ROOT = Path(__file__).resolve().parents[2]


def module(path, name):
    spec = importlib.util.spec_from_file_location(name, ROOT / path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


class ReportTests(unittest.TestCase):
    def test_report_uses_case_ring_elevations_and_explains_tension_shortfall(self):
        import yaml
        from digitalmodel.drilling_riser import generic_campaign
        builder = module('scripts/reporting/build_generic_riser_report.py', 'generic_report')
        driver = module('scripts/orcaflex/run_generic_drilling_riser_campaign.py', 'generic_driver')
        fixture = json.loads((Path(__file__).parent / 'fixtures/generic_synthetic_result.json').read_text(encoding='utf-8'))
        with tempfile.TemporaryDirectory() as temporary:
            folder = Path(temporary)
            campaign = folder / 'inputs'
            shutil.copytree(ROOT / generic_campaign.DEST, campaign)
            table = generic_campaign.case_table(ROOT)
            for index, case in enumerate(table['cases']):
                case['ring_initial_position_m']['value'][2] = 37 if index % 2 else 42
            (campaign / 'case-table.yml').write_text(yaml.safe_dump(table), encoding='utf-8')
            basis_path = campaign / 'screens/basis.json'
            basis = json.loads(basis_path.read_text(encoding='utf-8'))
            basis['case_table_sha256'] = generic_campaign.digest(campaign / 'case-table.yml')
            basis_path.write_text(json.dumps(basis), encoding='utf-8')
            output = folder / 'results'
            def fake(model, simulation):
                simulation.write_text('synthetic only', encoding='utf-8')
                return dict(fixture)
            self.assertEqual(driver.run_campaign(campaign, output, criteria_file=CRITERIA, case_runner=fake), 0)
            with patch.object(generic_campaign, 'case_table', return_value=table):
                spec = builder.build_spec(campaign, output, criteria_file=CRITERIA, allow_synthetic=True)
            method = spec.sections[3].blocks[0].markdown
            self.assertIn('37 m, 42 m', method)
            self.assertIn('each case', method)
            self.assertNotIn('initial 20 m', method)
            self.assertIn(table['tension_acceptance_basis'], spec.sections[2].blocks[0].markdown)

    def test_synthetic_report_and_incomplete_rejection(self):
        builder = module('scripts/reporting/build_generic_riser_report.py', 'generic_report')
        driver = module('scripts/orcaflex/run_generic_drilling_riser_campaign.py', 'generic_driver')
        from digitalmodel.drilling_riser.generic_campaign import DEST, METHOD
        campaign = ROOT / DEST
        fixture = json.loads((Path(__file__).parent / 'fixtures/generic_synthetic_result.json').read_text(encoding='utf-8'))
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary)
            def fake(model, simulation):
                simulation.write_text('synthetic only', encoding='utf-8')
                return dict(fixture)
            self.assertEqual(driver.run_campaign(campaign, output, criteria_file=CRITERIA, case_runner=fake), 0)
            with self.assertRaisesRegex(ValueError, 'criteria file'):
                builder.build_spec(campaign, output)
            with self.assertRaisesRegex(ValueError, 'Synthetic'):
                builder.build_spec(campaign, output, criteria_file=CRITERIA)
            spec = builder.build_spec(campaign, output, criteria_file=CRITERIA, allow_synthetic=True)
            self.assertEqual(len(spec.sections), 10)
            self.assertIn(METHOD, spec.sections[3].blocks[0].markdown)
            self.assertIn(METHOD, spec.sections[6].blocks[0].markdown)
            self.assertIn('not the modelled tension', spec.sections[2].blocks[0].markdown)
            self.assertNotIn('lower mean angle', str(spec.appendices))
            self.assertNotIn('upper mean angle', str(spec.appendices))
            html = builder.render(campaign, output, criteria_file=CRITERIA, allow_synthetic=True)
            soup = BeautifulSoup(html, 'html.parser')
            main_sections = soup.select('main.main > section')
            self.assertEqual([item['id'] for item in main_sections],
                ['summary', 'introduction', 'design', 'method', 'results', 'findings',
                 'limitations', 'recommendations', 'references', 'symbols', 'appendices'])
            self.assertEqual(soup.select_one('#data-sources').find_parent('section')['id'], 'appendices')
            for table in soup.select('.tbl'):
                self.assertIsNotNone(table.select_one('.tbl-wrap').find_next_sibling(class_='cap'))
            text = soup.get_text(' ', strip=True)
            self.assertNotIn('src/', text)
            supplied = json.loads(CRITERIA.read_text(encoding='utf-8'))
            self.assertIn(f"mean angle ≤ {supplied['mean_angle_deg']['value']:g}°", text)
            self.assertIn(f"maximum angle ≤ {supplied['max_angle_deg']['value']:g}°", text)
            self.assertNotIn('wave-frequency', text)
            self.assertNotIn('regular wave', text.lower())
            self.assertFalse(builder.FORBIDDEN.search(text))
            for element in soup.find_all(string=lambda value: value and 'OrcaFlex' in value):
                self.assertIn(element.find_parent('section')['id'], ('method', 'references'))
            self.assertIn('Criterion', text)
            self.assertIn('Component', text)
            self.assertIn('Demand', text)
            self.assertIn('Limit', text)
            first = next(output.glob('*.summary.json'))
            original = first.read_text(encoding='utf-8')
            payload = json.loads(original)
            payload['input_sha256'] = 'stale'
            first.write_text(json.dumps(payload), encoding='utf-8')
            with self.assertRaisesRegex(ValueError, 'identity'):
                builder.build_spec(campaign, output, criteria_file=CRITERIA)
            first.write_text(original, encoding='utf-8')
            copied = output / 'copied-inputs'
            shutil.copytree(campaign, copied)
            csv = copied / 'screens/column-screen.csv'
            csv.write_text(csv.read_text(encoding='utf-8').replace('0.0', '9.0', 1), encoding='utf-8')
            with self.assertRaisesRegex(ValueError, 'Screen identity'):
                builder.build_spec(copied, output, criteria_file=CRITERIA, allow_synthetic=True)


if __name__ == '__main__':
    unittest.main()


def test_report_holds_production_and_grades_both_binding_checks():
    builder = module('scripts/reporting/build_generic_riser_report.py', 'ruling_report')
    driver = module('scripts/orcaflex/run_generic_drilling_riser_campaign.py', 'ruling_driver')
    from digitalmodel.drilling_riser.generic_campaign import DEST
    fixture = json.loads((Path(__file__).parent/'fixtures/generic_synthetic_result.json').read_text())
    with tempfile.TemporaryDirectory() as temporary:
        output = Path(temporary)
        def fake(model, simulation):
            simulation.write_text('synthetic')
            return dict(fixture, support_loss_min_kn=8000 if model.parent.name=='o5-c3' else 9000,
                        member_min_kn=0.1 if model.parent.name=='o4-c2' else 1)
        assert driver.run_campaign(ROOT/DEST, output, criteria_file=CRITERIA, case_runner=fake) == 0
        contract = builder.driver_module()
        original_rows = contract.criterion_rows
        contract.criterion_rows = lambda *args: list(reversed(original_rows(*args)))
        with patch.object(builder, 'driver_module', return_value=contract):
            spec = builder.build_spec(ROOT/DEST, output, criteria_file=CRITERIA, allow_synthetic=True)
        prose = spec.sections[0].blocks[0].markdown
        assert 'Production remains held' in prose
        assert 'No operating window is established' in prose
        assert spec.sections[0].blocks[1].title == 'Production hold'
        rows = spec.sections[4].blocks[1].rows
        assert next(row for row in rows if row[1]=='check B one link lost')[0] == 'o5-c3'
        assert next(row for row in rows if row[1]=='check A member nonnegative')[0] == 'o4-c2'



def test_report_build_accepts_default_windows_text_checkout(tmp_path):
    from digitalmodel.drilling_riser import generic_campaign
    builder = module('scripts/reporting/build_generic_riser_report.py', 'crlf_report')
    driver = module('scripts/orcaflex/run_generic_drilling_riser_campaign.py', 'crlf_driver')
    copied = tmp_path / 'inputs'
    shutil.copytree(ROOT / generic_campaign.DEST, copied)
    output = tmp_path / 'results'
    fixture = json.loads((Path(__file__).parent/'fixtures/generic_synthetic_result.json').read_text())
    def fake(model, simulation):
        simulation.write_text('synthetic only', encoding='utf-8')
        return dict(fixture)
    assert driver.run_campaign(copied, output, criteria_file=CRITERIA, case_runner=fake) == 0
    before = {p.relative_to(copied): generic_campaign.digest(p) for p in copied.rglob('*') if p.is_file()}
    for path in copied.rglob('*'):
        if path.is_file():
            path.write_bytes(path.read_bytes().replace(b'\r\n', b'\n').replace(b'\n', b'\r\n'))
    assert {p.relative_to(copied): generic_campaign.digest(p) for p in copied.rglob('*') if p.is_file()} == before
    assert builder.build_spec(copied, output, criteria_file=CRITERIA, allow_synthetic=True)
    assert '<html' in builder.render(copied, output, criteria_file=CRITERIA, allow_synthetic=True)
