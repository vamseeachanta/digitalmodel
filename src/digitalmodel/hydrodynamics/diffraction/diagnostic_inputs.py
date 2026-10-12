"""Offline preparation protocol. No persistence, native YAML or solver execution.

Rights resolver is caller-controlled evidence, not authenticated permission.
Returned bundles remain unadmitted pending exact payload review and root intake.
"""
import hashlib
import json
import re
from decimal import Decimal, InvalidOperation

import yaml

from .diagnostic_basis import canonical_intent, prescribed_basis, strict_json
from .diagnostic_mesh import transform_mesh

OWNER = 'vamseeachanta/digitalmodel-data'
MAX_SOURCE_NODES = 10000  # Shared expansion-work bound, not engineering accuracy.
FIELD_MAP = {
    'vessel.geometry.symmetry': 'symmetry',
    'vessel.geometry.reference_point': 'reference_point',
    'vessel.inertia.mass': 'mass',
    'vessel.inertia.centre_of_gravity': 'centre_of_gravity',
    'vessel.inertia.radii_of_gyration': 'radii',
    'environment.water_density': 'density', 'environment.gravity': 'gravity',
    'environment.water_depth': 'water_depth', 'frequencies.values': 'angular_frequency',
}


class _SourceLoader(yaml.SafeLoader):
    pass


def _mapping(loader, node):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=True)
        if not isinstance(key, str) or not key or key in result:
            raise ValueError('Duplicate, empty or non-string source key')
        result[key] = loader.construct_object(value_node, deep=True)
    return result


def _float_token(loader, node):
    try:
        finite = Decimal(node.value).is_finite()
    except InvalidOperation as error:
        raise ValueError('Unsupported source numeric token') from error
    if not finite:
        raise ValueError('Non-finite source numeric token')
    return node.value


_SourceLoader.add_constructor('tag:yaml.org,2002:map', _mapping)
_SourceLoader.add_constructor('tag:yaml.org,2002:float', _float_token)


def _source_spec(raw):
    if not isinstance(raw, bytes) or len(raw) > 1000000:
        raise ValueError('Specification must be bounded bytes')
    try:
        parsed = yaml.load(raw.decode('utf-8'), Loader=_SourceLoader)
    except (UnicodeError, yaml.YAMLError) as error:
        raise ValueError('Invalid source specification') from error
    if not isinstance(parsed, dict):
        raise ValueError('Specification must be an object')
    return parsed


def _leaves(value, prefix='', depth=0, budget=None):
    if budget is None:
        budget = [MAX_SOURCE_NODES]
    budget[0] -= 1
    if budget[0] < 0:
        raise ValueError('Source traversal node budget exceeded')
    if depth > 20:
        raise ValueError('Recursive or excessive source nesting')
    if isinstance(value, dict) and value:
        for key, child in value.items():
            if not isinstance(key,str) or not key or '.' in key:
                raise ValueError('Empty or ambiguous source key')
            yield from _leaves(child, f'{prefix}.{key}' if prefix else key, depth + 1, budget)
    else:
        if isinstance(value, list):
            _check_list(value, depth, budget)
        elif type(value) not in (str, int, bool, type(None), dict):
            raise ValueError('Unsupported source scalar type')
        yield prefix, value


def _check_list(value, depth, budget):
    for child in value:
        for _ in _leaves(child, depth=depth + 1, budget=budget):
            pass


def _insert(result, field, row):
    if field in result:
        raise ValueError('Duplicate ledger path collision')
    result[field] = row


def divergence_ledger(spec_raw, header):
    """Ledger every supplied leaf and header; unknown units are never inferred."""
    spec, basis = _source_spec(spec_raw), prescribed_basis()
    if set(spec) & {'gdf', 'intent'}:
        raise ValueError('Source field conflicts with reserved ledger namespace')
    if not set(FIELD_MAP.values()) <= set(basis):
        raise ValueError('Mapped basis field missing')
    result = {}
    for field, value in _leaves(spec):
        intended = basis[FIELD_MAP[field]] if field in FIELD_MAP else 'not_carried_into_prescribed_basis'
        relationship = 'prescribed_not_recovered' if field in FIELD_MAP else 'source_field_not_carried'
        _insert(result, field, {'source': value, 'source_units': 'unknown', 'intent': intended,
                               'relationship': relationship})
    for field, value in header.items():
        _insert(result, f'gdf.{field}', {'source': value, 'source_units': 'unknown',
                                       'intent': 'preserved_input_record',
                                       'relationship': 'input_not_native_semantics'})
    if 'gdf.grav' in result:
        result['gdf.grav']['effect'] = 'unknown_pending_native_readback'
    for field, value in basis.items():
        _insert(result, f'intent.{field}', {'source': 'prescribed_assumption',
                                          'source_units': 'not_applicable', 'intent': value,
                                          'relationship': 'prescribed_not_recovered'})
    return result


def _rights(raw, reference, resolver):
    if not isinstance(reference, dict) or not callable(resolver):
        raise ValueError('Source-specific rights evidence required')
    if set(reference) != {'id', 'sha256'} or not isinstance(reference['id'], str):
        raise ValueError('Invalid rights reference')
    evidence = resolver(reference['id'])
    if not isinstance(evidence, bytes) or hashlib.sha256(evidence).hexdigest() != reference['sha256']:
        raise ValueError('Rights evidence digest mismatch')
    record = strict_json(evidence)
    required = {'mesh', 'spec'}
    fields = {'sources','decision','destination','audience','operations','basis'}
    if (not isinstance(record, dict) or set(record) != fields
            or not isinstance(record.get('sources'), dict)):
        raise ValueError('Invalid rights evidence')
    if set(record['sources']) != required or any(
            record['sources'][name] != hashlib.sha256(raw[name]).hexdigest() for name in required):
        raise ValueError('Rights do not bind original source bytes')
    if (record.get('decision') != 'permitted' or record.get('destination') != OWNER
            or record.get('audience') != 'authorized-private'
            or record.get('operations') != ['transform', 'private-retention']
            or not isinstance(record.get('basis'), str) or not record['basis'].strip()):
        raise ValueError('Required transformation and destination rights not established')


def _source_tree(tree):
    if (not isinstance(tree, dict) or set(tree) != {'revision', 'dirty'}
            or not isinstance(tree['revision'], str)
            or not re.fullmatch('[0-9a-f]{40}', tree['revision'])
            or type(tree['dirty']) is not bool):
        raise ValueError('Source revision and observed dirty status required')
    return dict(tree)


def _expected_sources(raw, expected):
    if not isinstance(expected, dict) or set(expected) != {'mesh', 'spec'}:
        raise ValueError('Independent original mesh/spec digests required')
    for name, digest in expected.items():
        if (not isinstance(digest, str) or not re.fullmatch('[0-9a-f]{64}', digest)
                or hashlib.sha256(raw[name]).hexdigest() != digest):
            raise ValueError('Original source does not match independently pinned digest')


def build_bundle(raw, rights_reference, rights_resolver, source_tree, *,
                 expected_source_sha256, comparison_to_source=False):
    """Return an unadmitted in-memory diagnostic bundle after rights checks.

    Does not copy/write files. The root must review exact output before issuance.
    Rights, source-tree observation and independently pinned expected hashes are
    external trust inputs. Expected hashes must not be derived from received bytes.
    """
    if comparison_to_source is not False:
        raise ValueError('Source-model equivalence is not established')
    if not isinstance(raw, dict) or set(raw) != {'mesh', 'spec'}:
        raise ValueError('Exactly the mesh and spec sources required')
    if any(not isinstance(value, bytes) or len(value) > 1000000 for value in raw.values()):
        raise ValueError('Bounded original source bytes required')
    tree = _source_tree(source_tree)
    _expected_sources(raw, expected_source_sha256)
    _rights(raw, rights_reference, rights_resolver)
    mesh = transform_mesh(raw['mesh'])
    ledger = divergence_ledger(raw['spec'], mesh['header'])
    basis = prescribed_basis()
    intent_bytes = canonical_intent(basis)
    ledger_bytes = json.dumps({'divergence': ledger, 'mesh_transformation': mesh['panels']},
        sort_keys=True, separators=(',', ':'), ensure_ascii=False, allow_nan=False).encode('utf-8')
    return {'manifest': {'owner_repo': OWNER,
            'intake_status': 'not_established_pending_exact_payload_review',
            'engineering_qualified': False, 'source_tree': tree,
            'source_sha256': {name: hashlib.sha256(value).hexdigest() for name, value in raw.items()},
            'expected_source_sha256': dict(expected_source_sha256),
            'derived_sha256': mesh['derived_sha256'],
            'intent_sha256': hashlib.sha256(intent_bytes).hexdigest(),
            'ledger_sha256': hashlib.sha256(ledger_bytes).hexdigest(),
            'ledger_digest_scope': 'canonical-json:divergence-and-mesh-transformation',
            'rights_reference': dict(rights_reference),
            'rights_status': 'caller_evidence_checked_not_independently_authenticated',
            'comparison_to_source': 'not_established', 'native_mapping_status': 'unresolved'},
            'intent': basis, 'intent_bytes': intent_bytes,
            'ledger': ledger, 'mesh': mesh, 'native_yaml': None}


def native_yaml(*args, **kwargs):
    """No native generator exists in this offline slice, even for claimed mappings."""
    raise ValueError('Native YAML production unavailable: mappings and runtime review unresolved')
