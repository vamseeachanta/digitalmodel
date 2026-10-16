# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Licence-free tests; temporary output uses the standard-library default."""
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import Mock

import yaml

from digitalmodel.drilling_riser import generic_campaign as campaign


ROOT = Path(__file__).resolve().parents[2]


class GenericCampaignTests(unittest.TestCase):
    def test_tension_acceptance_warning_is_carried_with_prepared_inputs(self):
        table = campaign.case_table(ROOT)
        note = table['tension_acceptance_basis']
        self.assertNotIn('hand estimate', note)
        self.assertIn('numerical reconciliation remains pending', note)
        self.assertIn('case by case', note)
        self.assertIn('pre-flight statics', note)
        self.assertIn('reported on #2469', note)
        self.assertIn('calculated positions', table['statics_method']['value'])
        self.assertTrue(table['statics_method']['source'])
        public = ROOT / campaign.DEST
        for path in ('case-table.yml', 'screens/basis.json'):
            data = yaml.safe_load((public / path).read_text(encoding='utf-8'))
            self.assertEqual(data['tension_acceptance_basis'], note)
        preparation = json.loads((public / 'preparation.json').read_text(encoding='utf-8'))
        self.assertEqual(preparation['tension_acceptance_basis'], note)

    def test_derived_criterion_rejects_underflow(self):
        fixture = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
        data = json.loads(fixture.read_text(encoding='utf-8'))
        # Synthetic floating-point boundary, not an engineering criterion.
        for key in ('yield_strength_mpa', 'stress_design_factor'):
            data[key]['value'] = float.fromhex('0x0.0000000000001p-1022')
        source = Mock()
        source.read_text.return_value = json.dumps(data)
        with self.assertRaisesRegex(ValueError, 'Derived stress limit'):
            campaign.load_criteria(source, campaign.case_table(ROOT))

    def test_criteria_digest_binds_the_parsed_snapshot(self):
        fixture = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
        snapshot = fixture.read_text(encoding='utf-8')
        source = Mock()
        source.read_text.return_value = snapshot
        criteria, identity = campaign.load_criteria(source, campaign.case_table(ROOT))
        self.assertEqual(identity, hashlib.sha256(snapshot.encode('utf-8')).hexdigest())
        self.assertIn('synthetic', criteria['mean_angle_deg']['source'])
        source.read_text.assert_called_once_with(encoding='utf-8')

    def test_runtime_criteria_are_not_public_values(self):
        table = campaign.case_table(ROOT)
        for key in ('mean_angle_deg', 'max_angle_deg', 'stress_limit_mpa'):
            self.assertIsNone(table['criteria'][key]['value'])
        for key in ('yield_strength_mpa', 'stress_design_factor'):
            self.assertIsNone(table['assumptions'][key]['value'])
        public_table = yaml.safe_load((ROOT / campaign.DEST / 'case-table.yml').read_text(encoding='utf-8'))
        public_basis = json.loads((ROOT / campaign.DEST / 'screens/basis.json').read_text(encoding='utf-8'))
        for public in (public_table, public_basis):
            for key in ('mean_angle_deg', 'max_angle_deg', 'stress_limit_mpa'):
                self.assertIsNone(public['criteria'][key]['value'])
            for key in ('yield_strength_mpa', 'stress_design_factor'):
                self.assertIsNone(public['assumptions'][key]['value'])
        with self.assertRaisesRegex(ValueError, 'criteria file'):
            campaign.load_criteria(None, table)
        fixture = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
        limits, _ = campaign.load_criteria(fixture, table)
        self.assertGreater(limits['stress_limit_mpa']['value'], 0)
        self.assertIn('synthetic', limits['mean_angle_deg']['source'])

    def test_offset_with_motion_is_refused(self):
        table = campaign.case_table(ROOT)
        table['cases'][0]['PrescribedMotionSpeedValue'] = campaign.tagged(1)
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaisesRegex(ValueError, 'PrescribedMotionSpeedValue'):
                campaign.build_models(ROOT, Path(temporary), table)

    def test_offset_direction_projection(self):
        self.assertEqual(campaign.horizontal_shift(10, 90), [0, 10])
        self.assertEqual(campaign.horizontal_shift(10, 180), [-10, 0])
        self.assertEqual(str(campaign.horizontal_shift(0, 180)), '[0.0, 0.0]')

    def test_preparation_release_role(self):
        data = json.loads((ROOT / campaign.DEST / 'preparation.json').read_text(encoding='utf-8'))
        self.assertIn('maintainer acceptance', data['release_boundary'])

    def test_runtime_criteria_validation(self):
        fixture = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
        data = json.loads(fixture.read_text(encoding='utf-8'))
        table = campaign.case_table(ROOT)
        with tempfile.TemporaryDirectory() as temporary:
            criteria = Path(temporary) / 'criteria.json'
            for key, field, value in (
                    ('mean_angle_deg', 'value', True),
                    ('mean_angle_deg', 'value', float('nan')),
                    ('mean_angle_deg', 'unit', 'radians'),
                    ('mean_angle_deg', 'source', ''),
                    ('mean_angle_deg', 'value', data['max_angle_deg']['value'] * 10),
                    ('stress_design_factor', 'value', 10)):
                candidate = json.loads(json.dumps(data))
                candidate[key][field] = value
                criteria.write_text(json.dumps(candidate), encoding='utf-8')
                with self.subTest(key=key, field=field, value=value), self.assertRaises(ValueError):
                    campaign.load_criteria(criteria, table)

    def test_dependency_fingerprint_changes(self):
        with tempfile.TemporaryDirectory() as temporary:
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
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for build in ('first', 'second'):
                campaign.build_models(ROOT, root / build, table)
            def hashes(folder):
                return {p.relative_to(folder).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                        for p in folder.rglob('*.yml')}
            self.assertEqual(hashes(root / 'first'), hashes(root / 'second'))
            self.assertEqual(hashes(root / 'first'), hashes(ROOT / campaign.DEST / 'models'))
            for case in table['cases']:
                folder = root / 'first' / case['id']
                data = yaml.safe_load((folder / 'includes/20_generic_objects.yml').read_text(encoding='utf-8'))
                vessel = data['Vessels'][0]
                self.assertAlmostEqual(vessel['InitialPosition'][0], -case['offset_m']['value'])
                self.assertEqual(vessel['InitialPosition'][1:], [0, 0])
                self.assertEqual(vessel['PrimaryMotion'], 'None')
                self.assertEqual(vessel['SuperimposedMotion'], 'None')
                self.assertFalse(any(k.startswith('PrescribedMotion') for k in vessel))
                self.assertIn(campaign.METHOD, (folder / 'master.yml').read_text(encoding='utf-8'))
                environment = yaml.safe_load((folder / 'includes/03_environment.yml').read_text(encoding='utf-8'))
                for wave in environment['Environment']['WaveTrains']:
                    self.assertEqual(wave['WaveHeight'], 0)
                    self.assertEqual(wave['WaveType'], 'Airy')
                ring = next(item for item in data['6DBuoys'] if item['Name'] == 'Tensioner Ring')
                self.assertEqual(ring['InitialPosition'][:2], vessel['InitialPosition'][:2])
                self.assertEqual(ring['InitialPosition'], case['ring_initial_position_m']['value'])
                drill = next(item for item in data['Lines'] if item['Name'] == 'Drill String')
                connections = next(value for key, value in drill.items() if key.startswith('Connection, '))
                self.assertEqual(connections[0][1:3], vessel['InitialPosition'][:2])
                self.assertIn('assumed', (folder / 'master.yml').read_text(encoding='utf-8'))
                campaign.validate_model(folder)

    def test_screen_is_explicitly_approximate(self):
        table = campaign.case_table(ROOT)
        rows = campaign.screen_rows(ROOT, table)
        self.assertEqual(len(rows), 24)
        self.assertNotIn('lower_angle_deg', rows[0])
        self.assertNotIn('upper_angle_deg', rows[-1])
        self.assertIn('factor of two', table['screen_limitations'])
        self.assertIn('above-water', table['screen_limitations'])
        self.assertIn('lower joint angle is not established', table['screen_limitations'])
        self.assertEqual(rows[0]['scope'], 'approximate riser-column screen')


if __name__ == '__main__':
    unittest.main()
