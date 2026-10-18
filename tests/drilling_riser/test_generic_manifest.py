# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline field contract from the pinned public runner specification."""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / 'docs/domains/orcaflex/library/campaigns/generic-drilling-riser'
SPEC_PATH = 'docs/domains/orcaflex/library/templates/drilling_riser/spec.yml'


def pinned_source_sha256(root: Path, ref: str) -> str:
    """Issue 2732: resolve a commit and hash its raw spec blob, never checkout text.

    Git cat-file documentation, <type> <object> form, specifies raw object
    contents; <ref>^{commit} requires a commit (gitrevisions, ^{<type>}).
    https://git-scm.com/docs/git-cat-file
    https://git-scm.com/docs/gitrevisions
    """
    if not (root / '.git').exists():
        raise unittest.SkipTest('Git metadata unavailable; pinned blob cannot be checked')
    env = {key: value for key, value in os.environ.items()
           if key not in {'GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR'}}

    def git(*args):
        try:
            return subprocess.run(['git', '-C', str(root), *args], env=env,
                                  capture_output=True, check=False)
        except FileNotFoundError as exc:
            raise unittest.SkipTest('Git metadata unavailable; git executable missing') from exc

    commit = git('cat-file', '-e', f'{ref}^{{commit}}')
    assert commit.returncode == 0, f'Pinned ref does not resolve to a commit: {ref}'
    blob = git('cat-file', 'blob', f'{ref}:{SPEC_PATH}')
    assert blob.returncode == 0, f'Pinned specification blob unavailable at commit {ref}'
    return hashlib.sha256(blob.stdout).hexdigest()


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
        self.assertEqual(data['env']['SOURCE_SHA256'],
                         pinned_source_sha256(ROOT, data['ref']),
                         'SOURCE_SHA256 differs from the pinned specification blob')


if __name__ == '__main__':
    unittest.main()
