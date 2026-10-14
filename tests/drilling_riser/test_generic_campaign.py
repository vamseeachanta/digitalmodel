# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Licence-free tests; all temporary output belongs to the caller's TMPDIR."""
import hashlib
import os
from pathlib import Path
import tempfile
import unittest

import yaml

from digitalmodel.drilling_riser import generic_campaign as campaign


ROOT = Path(__file__).resolve().parents[2]


class GenericCampaignTests(unittest.TestCase):
    def test_dependency_fingerprint_changes(self):
        with tempfile.TemporaryDirectory(dir=os.environ['TMPDIR']) as temporary:
            folder = Path(temporary)
            original = campaign.calculation_fingerprints()
            for name in original:
                source = Path(campaign.__file__).parent / name
                (folder / name).write_bytes(source.read_bytes())
            self.assertEqual(original, campaign.calculation_fingerprints(folder))
            dependency = folder / 'section.py'
            dependency.write_text(dependency.read_text(encoding='utf-8') + '\n# synthetic mutation\n',
                                  encoding='utf-8')
            self.assertNotEqual(original, campaign.calculation_fingerprints(folder))

    def test_case_values_have_sources(self):
        table = campaign.case_table(ROOT)
        self.assertEqual(len(table['cases']), 24)
        self.assertEqual(len({c['id'] for c in table['cases']}), 24)
        for case in table['cases']:
            for key, value in case.items():
                if key != 'id':
                    self.assertTrue(value['source'])
                    self.assertIn('value', value)
        self.assertEqual(table['method'], campaign.METHOD)

    def test_generated_models_and_repeatability(self):
        table = campaign.case_table(ROOT)
        with tempfile.TemporaryDirectory(dir=os.environ['TMPDIR']) as temporary:
            root = Path(temporary)
            for build in ('first', 'second'):
                campaign.build_models(ROOT, root / build, table)
            def hashes(folder):
                return {p.relative_to(folder).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                        for p in folder.rglob('*.yml')}
            self.assertEqual(hashes(root / 'first'), hashes(root / 'second'))
            for case in table['cases']:
                folder = root / 'first' / case['id']
                data = yaml.safe_load((folder / 'includes/20_generic_objects.yml').read_text(encoding='utf-8'))
                vessel = data['Vessels'][0]
                self.assertEqual(vessel['InitialPosition'], [-case['offset_m']['value'], 0, 0])
                self.assertEqual(vessel['PrimaryMotion'], 'None')
                self.assertEqual(vessel['SuperimposedMotion'], 'None')
                self.assertFalse(any(k.startswith('PrescribedMotion') for k in vessel))
                self.assertIn(campaign.METHOD, (folder / 'master.yml').read_text(encoding='utf-8'))
                campaign.validate_model(folder)

    def test_screen_is_explicitly_approximate(self):
        table = campaign.case_table(ROOT)
        rows = campaign.screen_rows(ROOT, table)
        self.assertEqual(len(rows), 24)
        self.assertEqual(rows[0]['lower_angle_deg'], 0)
        self.assertGreater(rows[-1]['lower_angle_deg'], 0)
        self.assertEqual(rows[0]['scope'], 'approximate riser-column screen')


if __name__ == '__main__':
    unittest.main()
