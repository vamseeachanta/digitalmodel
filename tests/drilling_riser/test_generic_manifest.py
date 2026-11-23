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
import warnings

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
        raise unittest.SkipTest('pinned blob NOT verified: Git metadata unavailable')
    env = {key: value for key, value in os.environ.items()
           if key not in {'GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR'}}

    def git(*args):
        try:
            return subprocess.run(['git', '-C', str(root), *args], env=env,
                                  capture_output=True, check=False)
        except FileNotFoundError as exc:
            raise unittest.SkipTest('pinned blob NOT verified: git executable missing') from exc

    commit = git('cat-file', '-e', f'{ref}^{{commit}}')
    if commit.returncode != 0:
        shallow = git('rev-parse', '--is-shallow-repository')
        if (shallow.returncode == 0 and shallow.stdout.strip() == b'true'
                and git('cat-file', '-e', ref).returncode != 0):
            raise AssertionError(
                f'Pinned commit unavailable in shallow clone: {ref}; '
                'fetch the pinned commit or full history before running this check')
    assert commit.returncode == 0, f'Pinned ref does not resolve to a commit: {ref}'
    blob = git('cat-file', 'blob', f'{ref}:{SPEC_PATH}')
    assert blob.returncode == 0, f'Pinned specification blob unavailable at commit {ref}'
    return hashlib.sha256(blob.stdout).hexdigest()


class ManifestTests(unittest.TestCase):
    def test_checkout_spec_matches_pin_advisory(self):
        """Keep checkout drift visible separately from the hard pin contract."""
        data = json.loads((BASE / 'manifest.campaign.json').read_text(encoding='utf-8'))
        pinned_digest = pinned_source_sha256(ROOT, data['ref'])
        checkout_digest = hashlib.sha256((ROOT / SPEC_PATH).read_bytes()).hexdigest()
        if checkout_digest != pinned_digest:
            # Advisory only, but visible despite the suite's ignored UserWarnings.
            with warnings.catch_warnings():
                warnings.simplefilter('always', UserWarning)
                warnings.warn(
                    'Advisory: checkout specification differs from pinned blob; '
                    'review checkout drift before updating the manifest pin',
                    UserWarning, stacklevel=2)

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
