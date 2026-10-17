# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline field contract from the pinned public runner specification."""
import hashlib
import json
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / 'docs/domains/orcaflex/library/campaigns/generic-drilling-riser'


class ManifestTests(unittest.TestCase):
    def test_required_contract_and_placeholders(self):
        data = json.loads((BASE / 'manifest.campaign.json').read_text(encoding='utf-8'))
        self.assertEqual(set(data), {'id', 'title', 'repo_dir', 'ref', 'driver', 'cwd', 'env',
            'expected_hours', 'max_hours', 'progress', 'licence_exit_codes', 'retry', 'priority', 'after'})
        self.assertEqual(data['id'], 'generic-drilling-riser-r1')
        self.assertEqual(data['repo_dir'], '<SOLVER_REPO_DIR>')
        self.assertEqual(data['cwd'], '<SOLVER_REPO_DIR>')
        self.assertRegex(data['ref'], r'^[a-f0-9]{40}$')
        self.assertEqual(data['progress']['total'], 24)
        self.assertEqual(data['licence_exit_codes'], [12])
        self.assertEqual(data['retry'], {'max_attempts': 3, 'backoff_minutes': 30, 'licence_tries_per_day': 3})
        self.assertEqual(data['max_hours'], 3 * data['expected_hours'])
        for name in data['env']:
            self.assertIsNone(re.search('TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL|KEY', name, re.I))
        spec = ROOT / 'docs/domains/orcaflex/library/templates/drilling_riser/spec.yml'
        self.assertEqual(data['env']['SOURCE_SHA256'], hashlib.sha256(
            spec.read_text(encoding='utf-8').encode('utf-8')).hexdigest())


if __name__ == '__main__':
    unittest.main()
