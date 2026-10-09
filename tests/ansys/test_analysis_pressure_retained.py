"""Opt-in offline checks against retained private inputs; never launches MAPDL."""
import json
import os
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes


@pytest.fixture
def retained():
    folder = os.environ.get('ANSYS_PRESSURE_INTAKE_FIXTURE_ROOT')
    if not folder:
        pytest.skip('Private retained intake fixture root was not supplied')
    folder = Path(folder)
    descriptor = json.loads((folder / 'descriptor.json').read_bytes())
    resolver = json.loads((folder / 'resolver-private.json').read_bytes())
    docs = {}
    for role, ref in descriptor['sources'].items():
        raw = Path(resolver[ref['id']]).read_bytes()
        assert digest_bytes(raw) == ref['sha256']
        docs[role] = json.loads(raw)
    return folder, descriptor, resolver, docs


def test_production_provenance_constants_match_retained_documents(retained):
    from digitalmodel.ansys.analysis_pressure_inputs import (
        EXECUTION_REVISION, EXECUTION_INVENTORY_SHA, SOURCE_PROVENANCE_SHA,
    )
    _, descriptor, _, docs = retained
    assert EXECUTION_REVISION == docs['config']['source_revision']
    assert SOURCE_PROVENANCE_SHA == descriptor['sources']['source_provenance']['sha256']
    inventory = {row['path']: row['sha256'] for row in docs['config']['source_files']}
    assert len(inventory) == 79
    assert EXECUTION_INVENTORY_SHA == digest_bytes(canonical_bytes(inventory))


def test_actual_r4_derivation_retains_64_typed_nulls(retained):
    from digitalmodel.ansys.analysis_evidence import load_package
    from digitalmodel.ansys.analysis_pressure_observed import derive_pressure_observed_case
    folder, _, resolver, _ = retained
    path = folder / 'baseline-r4.json'
    assert digest_bytes(path.read_bytes()) == 'b829f67d25e3064f0a624d0060f0ff874ae0eb99f7df93ccf661349b6cc7273a'
    baseline = load_package(path)
    assert baseline['revision'] == 'r4', 'This check binds the preserved r4 predecessor'
    reference = json.loads((folder / 'reference.json').read_bytes())
    case = derive_pressure_observed_case(baseline, reference, resolver)
    assert case['execution_status'] == 'completed'
    assert len(case['responses']) == 64
    assert all(row['value'] is None and row['calculation_status'] == 'not_evaluated'
               for row in case['responses'])
    assert case['engineering_qualified'] is False
