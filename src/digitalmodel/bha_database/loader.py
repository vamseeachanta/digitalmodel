"""Bounded, fixed-file YAML loader; manifests establish consistency, not trust."""
from __future__ import annotations

import hashlib
from pathlib import Path

import yaml
from pydantic import BaseModel
from yaml.tokens import AliasToken, AnchorToken

from .schema import Document, Quantity

DEFAULT_ROOT = Path(__file__).resolve().parents[3] / 'data' / 'bha_database'
MAX_BYTES = 1024 * 1024


class BhaDatabaseError(ValueError):
    """Invalid public catalogue or assembly, with a rule identifier when relevant."""


class UniqueLoader(yaml.SafeLoader):
    """SafeLoader that rejects silent mapping-key overrides."""


def unique_mapping(loader, node):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if key in result:
            raise BhaDatabaseError('duplicate YAML key')
        result[key] = loader.construct_object(value_node)
    return result


UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def read_yaml(path):
    if path.is_symlink():
        raise BhaDatabaseError(f'{path.name}: symlink refused')
    with path.open('rb') as stream:
        raw = stream.read(MAX_BYTES + 1)
    if len(raw) > MAX_BYTES:
        raise BhaDatabaseError(f'{path.name}: read exceeds 1 MiB')
    if any(isinstance(t, (AliasToken, AnchorToken)) for t in yaml.scan(raw)):
        raise BhaDatabaseError(f'{path.name}: YAML aliases/anchors refused')
    # UniqueLoader retains SafeLoader's constructors and rejects duplicate keys.
    # Use PyYAML 6.0.1 load's lifecycle explicitly; never use an unsafe loader.
    # https://github.com/yaml/pyyaml/blob/6.0.1/lib/yaml/__init__.py#L74-L83
    loader = UniqueLoader(raw)
    try:
        return raw, loader.get_single_data()
    finally:
        loader.dispose()


def index_unique(rows, field):
    result = {getattr(row, field): row for row in rows}
    if len(result) != len(rows):
        raise BhaDatabaseError(f'duplicate {field}')
    return result


def validate_sources(model, sources):
    if isinstance(model, Quantity) and model.source_key not in sources:
        raise BhaDatabaseError('unresolved quantity source_key')
    if hasattr(model, 'source_key') and model.source_key not in sources:
        raise BhaDatabaseError('unresolved record source_key')
    if hasattr(model, 'data_class') and model.data_class == 'CLIENT-PRIVATE':
        raise BhaDatabaseError('CLIENT-PRIVATE rows refused by public loader')
    if isinstance(model, BaseModel):
        for name in type(model).model_fields:
            validate_sources(getattr(model, name), sources)
    elif isinstance(model, list):
        for item in model:
            validate_sources(item, sources)


def load_database(root: Path | str = DEFAULT_ROOT):
    from .calcs import Database
    from .checks import check_assembly
    try:
        root = Path(root)
        if {p.name for p in root.iterdir()} != {'examples.yml', 'manifest.yaml'}:
            raise BhaDatabaseError('unexpected or missing database file')
        _, manifest = read_yaml(root / 'manifest.yaml')
        raw, data = read_yaml(root / 'examples.yml')
        expected = {'schema_version': 1, 'files': {'examples.yml': {
            'sha256': hashlib.sha256(raw).hexdigest(),
            'components': len(data['components']), 'assemblies': len(data['assemblies']),
            'sources': len(data['sources']),
        }}}
        if manifest != expected:
            raise BhaDatabaseError('examples.yml: manifest digest or row count mismatch')
        doc = Document.model_validate(data)
        sources = index_unique(doc.sources, 'key')
        validate_sources(doc, sources)
        components = index_unique(doc.components, 'component_id')
        assemblies = index_unique(doc.assemblies, 'bha_id')
        for assembly in assemblies.values():
            check_assembly(assembly, components)
        return Database(sources, components, assemblies)
    except BhaDatabaseError:
        raise
    except Exception as exc:
        raise BhaDatabaseError(f'database validation failed ({type(exc).__name__})') from None
