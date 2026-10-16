# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report tests consume synthetic summaries, never licensed software."""
import importlib.util
import json
import shutil
from pathlib import Path
import tempfile
import unittest

from bs4 import BeautifulSoup

CRITERIA = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
ROOT = Path(__file__).resolve().parents[2]


def module(path, name):
    spec = importlib.util.spec_from_file_location(name, ROOT / path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


class ReportTests(unittest.TestCase):
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
