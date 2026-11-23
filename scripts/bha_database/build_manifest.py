"""Regenerate the fixed public database consistency manifest (not authentication)."""
import hashlib
from pathlib import Path

import yaml


def build_manifest(root):
    root = Path(root)
    raw = (root / 'examples.yml').read_bytes()
    document = yaml.safe_load(raw)
    manifest = {'schema_version': 1, 'files': {'examples.yml': {
        'sha256': hashlib.sha256(raw).hexdigest(),
        **{key: len(document[key]) for key in ('sources', 'components', 'assemblies')},
    }}}
    (root / 'manifest.yaml').write_text(yaml.safe_dump(manifest), encoding='utf-8')
    return manifest


if __name__ == '__main__':
    build_manifest(Path(__file__).resolve().parents[2] / 'data' / 'bha_database')
