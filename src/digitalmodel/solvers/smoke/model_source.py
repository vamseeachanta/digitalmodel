"""Recheck the retained mooring source against its reviewed raw governing basis.

The raw digests identify the unchanged template and C07 reference supplied in
OrcaFlex code review at revision 072b0270. They do not establish native parity.
"""
from copy import deepcopy
import json
from pathlib import Path

GOVERNING_SOURCE = 'docs/domains/orcaflex/library/templates/mooring_buoy/spec.yml'
GOVERNING_SHA256 = '3d2ee6ce1f35a27553e96d4809b781d07a4ab68b0260191c8eafa398e99779bd'
REFERENCE_SOURCE = 'docs/domains/orcaflex/examples/yml/C07/C07 Metocean buoy in deep water.yml'
REFERENCE_SHA256 = 'ca88f01734f1a58712ff5707e3c1f3c8010a1a01ec5ede76bacd2560d158ff4e'
ALLOWED_KEYS = ['/environment/raw_properties/VerticalWindVariationFactor',
                '/environment/raw_properties/WindType']


def _typed(document):
    return json.dumps(document, sort_keys=True, allow_nan=False, separators=(',', ':'))


def _applied_keys(template, selected):
    if _typed(template) == _typed(selected):
        return []
    expected = deepcopy(template)
    expected['environment'].setdefault('raw_properties', {}).update(
        WindType='Constant', VerticalWindVariationFactor=None)
    if _typed(expected) != _typed(selected):
        raise ValueError('source provenance has unreviewed semantic changes')
    return ALLOWED_KEYS


def verify_source_provenance(data, files):
    from .model_manifest import _pairs, digest, read_yaml
    required = {'source/template.yml', 'source/derivation.json'}
    if not required.issubset(files):
        raise ValueError('source provenance retained inputs required')
    repo = Path(__file__).resolve().parents[4]
    if (data['contract'].get('source') != GOVERNING_SOURCE
            or digest(repo / GOVERNING_SOURCE) != GOVERNING_SHA256
            or digest(files['source/template.yml']) != GOVERNING_SHA256):
        raise ValueError('governing source bytes differ from reviewed basis')
    if digest(repo / REFERENCE_SOURCE) != REFERENCE_SHA256:
        raise ValueError('governing reference bytes differ from reviewed basis')
    raw = files['source/derivation.json'].read_text(encoding='utf-8-sig')
    derivation = json.loads(raw, object_pairs_hook=_pairs,
                           parse_constant=lambda value: (_ for _ in ()).throw(ValueError(value)))
    selected = files[data['source']['path']]
    applied = _applied_keys(read_yaml(files['source/template.yml']), read_yaml(selected))
    expected = dict(template_sha256=GOVERNING_SHA256, reference_sha256=REFERENCE_SHA256,
                    source_sha256=digest(selected), applied_keys=applied,
                    contract_source_role='governing_template')
    if _typed(derivation) != _typed(expected):
        raise ValueError('source derivation metadata differs from verified retained inputs')
    return expected['contract_source_role']
