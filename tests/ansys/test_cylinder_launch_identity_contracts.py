# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Equivalent launch representations are observations, not normalization rules.

Contract inventory and sources (repository contracts, not engineering limits):
* cylinder_process_inventory_v2._sources permits indexed absolute scripts or
  relative scripts resolved against the observed interpreter cwd. Unlike
  cylinder_cfd_owner_evidence._controller_argument, relative subdirectories are
  supported here; the fixed historical controller join permits basenames only.
* cylinder_process_inventory_v2._relation permits an alias or resolved child
  image only with the declared resolved path, image digest and creation pair.
* cylinder_wrapper_consoles.console_pids permits one console per disjoint
  launcher/interpreter branch, attached to either endpoint.

Synthetic IDs, times, digests and row counts come from the imported fixtures.
They are identifiers and contract examples, not measured engineering inputs or
acceptance limits. Expected relations are derived by hand in each test below.
No filesystem identity, current resource clearance or solver result is claimed.
"""

import base64
from copy import deepcopy
import hashlib
import json

import pytest

from digitalmodel.ansys.cylinder_cfd_owner_evidence import resolve_owner_evidence
from tests.ansys import test_cylinder_cfd_owner_evidence as owner_tests
from tests.ansys import test_cylinder_process_inventory_v2 as inventory_tests
from tests.ansys import test_cylinder_wrapper_consoles as console_tests

evidence = inventory_tests.evidence
facts = owner_tests.facts
supplemented = console_tests.supplemented

SCRIPT_FORMS = [
    ('controller.py', 'C:/work/controller.py', 'observed_interpreter_cwd'),
    ('C:/work/controller.py', 'C:/work/controller.py', 'absolute_argument'),
    ('c:\\WORK\\CONTROLLER.PY', 'C:/work/controller.py', 'absolute_argument'),
    ('scripts/controller.py', 'C:/work/scripts/controller.py',
     'observed_interpreter_cwd'),
]


def _pin_snapshot(snapshot, binding):
    """Construct independently pinned examples; never repair a drift probe."""
    binding['processes'] = deepcopy(snapshot['rows'])
    binding['forwarders'] = deepcopy(snapshot['forwarders'])


@pytest.fixture(params=SCRIPT_FORMS, ids=['basename', 'absolute', 'case', 'subdir'])
def launch(evidence, request):
    snapshot, binding = evidence
    token, path, basis = request.param
    controller = snapshot['rows'][0]
    wrapper = snapshot['rows'][-1]
    controller['argv'][1] = wrapper['argv'][1] = token
    controller['script_sources'][0].update(path=path, resolution_basis=basis)
    _pin_snapshot(snapshot, binding)
    return snapshot, binding


def test_script_equivalence_keeps_exact_observations(launch):
    """Cwd + basename/subdir or an absolute token yields the listed source path.

    The fixture lists controller, guard, MPI, helper, rank and wrapper. Each has
    an exact pin, so that literal PID set must be preserved without rewriting
    argv, cwd, source spelling or parents in either input or raw output.
    """
    snapshot, binding = launch
    before = deepcopy(launch)
    result = inventory_tests.classify(snapshot, binding)
    assert result['status'] == 'CLEAR'
    assert {row['pid'] for row in result['dispositions']} == {1, 2, 3, 4, 5, 6}
    assert all(row['classification'] == 'PRESERVED_CFD'
               for row in result['dispositions'])
    assert result['raw_inventory'] == snapshot['rows']
    assert launch == before
    result['raw_inventory'][0]['argv'].append('output-only')
    assert launch == before


@pytest.mark.parametrize('fault', ['creation', 'image_digest', 'source_digest',
                                 'argument_spelling'])
def test_equivalent_forms_do_not_hide_fresh_identity_drift(launch, fault):
    """One changed observation breaks exact family equality for every form.

    The altered forwarder remains internally consistent with its observed child;
    only the independently pinned family differs. Even a case-only argv change
    is fresh evidence, not permission to rewrite pins or preserve other members.
    """
    snapshot, binding = launch
    pinned = deepcopy(binding)
    controller = snapshot['rows'][0]
    forwarder = snapshot['forwarders'][0]
    if fault == 'creation':
        controller['creation_time'] = forwarder['child_creation_time'] = '1.5'
    elif fault == 'image_digest':
        controller['executable_sha256'] = 'd' * 64
        forwarder['resolved_target_sha256'] = controller['executable_sha256']
    elif fault == 'source_digest':
        controller['script_sources'][0]['sha256'] = 'd' * 64
    else:
        token = controller['argv'][1].swapcase()
        controller['argv'][1] = snapshot['rows'][-1]['argv'][1] = token
    result = inventory_tests.classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert not any(row['classification'] == 'PRESERVED_CFD'
                   for row in result['dispositions'])
    assert result['raw_inventory'] == snapshot['rows']
    assert binding == pinned


@pytest.mark.parametrize('fault', ['cross_root', 'drive_relative', 'traversal',
                                 'ambiguous_index'])
def test_script_equivalence_refuses_internally_inconsistent_pins(launch, fault):
    """Pinned text alone cannot join a different root or ambiguous argument.

    These cases keep snapshot and binding identical. Refusal must therefore
    come from source resolution, rather than merely a snapshot/pin mismatch.
    """
    snapshot, binding = launch
    controller = snapshot['rows'][0]
    source = controller['script_sources'][0]
    if fault == 'cross_root':
        source['path'] = 'D:/foreign/controller.py'
    elif fault == 'ambiguous_index':
        controller['script_sources'].append(deepcopy(source))
    else:
        token = 'C:controller.py' if fault == 'drive_relative' else '../controller.py'
        controller['argv'][1] = snapshot['rows'][-1]['argv'][1] = token
    _pin_snapshot(snapshot, binding)
    with pytest.raises(ValueError):
        inventory_tests.classify(snapshot, binding)


@pytest.mark.parametrize('image', ['C:/alias/python.exe', 'C:/bin/python.exe',
                                 'c:\\ALIAS\\PYTHON.EXE'])
def test_forwarded_image_equivalence_keeps_source_and_image_pins(launch, image):
    """Alias and resolved image share the declared target digest and endpoint.

    Both spellings join the same synthetic interpreter. Combining each with
    every script form must preserve the exact PID set and observed image text.
    """
    snapshot, binding = launch
    snapshot['rows'][0]['executable_path'] = image
    _pin_snapshot(snapshot, binding)
    before = deepcopy(launch)
    result = inventory_tests.classify(snapshot, binding)
    assert result['status'] == 'CLEAR'
    assert {row['pid'] for row in result['dispositions']} == {1, 2, 3, 4, 5, 6}
    assert all(row['classification'] == 'PRESERVED_CFD'
               for row in result['dispositions'])
    assert result['raw_inventory'] == snapshot['rows']
    assert result['raw_inventory'][0]['executable_path'] == image
    assert launch == before


@pytest.mark.parametrize('image', ['C:/alias/python.exe', 'C:/bin/python.exe'])
@pytest.mark.parametrize('fault', ['cross_root', 'target_digest', 'launcher_digest',
                                 'creation', 'unpaired', 'alias_identity'])
def test_forwarded_image_equivalence_requires_complete_relationship(
        evidence, image, fault):
    """Matching pins cannot legitimize a broken image/creation/parent join.

    The child image must be one of the declared paths, the resolved digest must
    equal the child's digest, and the pair must retain its observed creation and
    direct parent identities. The alias junction must be its immediate parent.
    """
    snapshot, binding = evidence
    controller = snapshot['rows'][0]
    controller['executable_path'] = image
    forwarder = snapshot['forwarders'][0]
    if fault == 'cross_root':
        controller['executable_path'] = 'D:/foreign/python.exe'
    elif fault == 'target_digest':
        forwarder['resolved_target_sha256'] = 'd' * 64
    elif fault == 'launcher_digest':
        forwarder['launcher_sha256'] = 'd' * 64
    elif fault == 'creation':
        forwarder['child_creation_time'] = '1.5'
    elif fault == 'unpaired':
        controller['parent_pid'] = 0
    else:
        forwarder['alias_identity']['junction_path'] = 'C:/other'
    _pin_snapshot(snapshot, binding)
    with pytest.raises(ValueError):
        inventory_tests.classify(snapshot, binding)


def test_equivalent_image_spelling_is_not_a_live_pin_substitution(evidence):
    """An observed resolved image does not equal a pin recording the alias.

    Both representations are individually supported; their literal observation
    records still differ, so the original family cannot be preserved.
    """
    snapshot, binding = evidence
    snapshot['rows'][0]['executable_path'] = 'C:/bin/python.exe'
    result = inventory_tests.classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert not any(row['classification'] == 'PRESERVED_CFD'
                   for row in result['dispositions'])
    assert result['raw_inventory'] == snapshot['rows']


CONSOLE_PARENTS = [(1, 3), (1, 4), (2, 3), (2, 4)]


def _pin_console_source(bundle):
    operation, binding, snapshot, _, _, files = bundle
    raw = json.dumps(snapshot).encode('utf-8')
    reference = operation['cfd_wrapper_console_evidence']
    files[reference['path']] = raw
    digest = hashlib.sha256(raw).hexdigest()
    reference['sha256'] = binding['wrapper_console_supplement']['sha256'] = digest


@pytest.fixture(params=CONSOLE_PARENTS)
def console_launch(supplemented, request):
    _, binding, snapshot, _, _, _ = supplemented
    for pid, parent in zip((200, 201), request.param):
        for rows in (binding['processes'], snapshot['rows']):
            next(row for row in rows if row['pid'] == pid)['parent_pid'] = parent
    _pin_console_source(supplemented)
    return supplemented


@pytest.mark.parametrize('reverse', [False, True], ids=['observed', 'reordered'])
def test_console_endpoint_product_retains_raw_source(console_launch, reverse):
    """{launcher 1, interpreter 2} x {launcher 3, interpreter 4} has four pairs.

    Every listed pair gives exactly one console on each disjoint branch. Row
    order is not identity: reversing rows and forwarders leaves the same PID
    set. The fixture's 126 original members plus two consoles gives 128 pins;
    the historical execution subset remains its original 122 rows.
    """
    operation, binding, snapshot, read, _, files = console_launch
    if reverse:
        for values in (binding['processes'], binding['forwarders'],
                       snapshot['rows'], snapshot['forwarders']):
            values.reverse()
        _pin_console_source(console_launch)
    before = deepcopy((operation, binding, snapshot, files))
    resolved = resolve_owner_evidence(operation, binding, read)
    result = console_tests.classify(snapshot, binding)
    assert result['status'] == 'CLEAR'
    assert resolved['wrapper_console_pids'] == result['wrapper_console_pids'] == [200, 201]
    assert len(result['dispositions']) == 128
    assert resolved['matched_joins']['execution_rows'] == 122
    assert result['raw_inventory'] == snapshot['rows']
    supplement = resolved['wrapper_console_evidence']
    raw = files[operation['cfd_wrapper_console_evidence']['path']]
    assert base64.b64decode(supplement['raw_base64'], validate=True) == raw
    assert supplement['raw_sha256'] == hashlib.sha256(raw).hexdigest()
    assert json.loads(raw)['forwarders'] == snapshot['forwarders']
    assert (operation, binding, snapshot, files) == before


@pytest.mark.parametrize('fault', ['creation', 'digest', 'unpaired', 'extra_console'])
def test_console_variants_do_not_clear_changed_or_unknown_processes(console_launch, fault):
    """Each endpoint choice retains exact pins and rejects unlisted children.

    One changed or added live row makes the entire family unpreserved; the name
    conhost.exe does not exempt that row, even beneath a permitted endpoint.
    """
    _, binding, snapshot, _, _, _ = console_launch
    pinned = deepcopy(binding)
    console = next(row for row in snapshot['rows'] if row['pid'] == 200)
    if fault == 'creation':
        console['creation_time'] = '200.5'
    elif fault == 'digest':
        console['executable_sha256'] = 'd' * 64
    elif fault == 'unpaired':
        console['parent_pid'] = 99
    else:
        extra = dict(deepcopy(console), pid=202, creation_time='202')
        snapshot['rows'].append(extra)
        snapshot['coverage'].update(selected_count=129, excluded_count=71)
    result = console_tests.classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert not any(row['classification'] == 'PRESERVED_CFD'
                   for row in result['dispositions'])
    assert result['raw_inventory'] == snapshot['rows']
    assert binding == pinned


@pytest.mark.parametrize('parents', [(1, 2), (2, 1), (3, 4), (4, 3), (99, 4)])
def test_matching_console_pins_cannot_replace_exact_branch_coverage(supplemented, parents):
    """Two consoles on one branch leave the other empty; 99 is unpaired.

    Rehashed raw evidence and matching pins do not establish the missing branch
    relationship. Both historical resolution and fresh classification refuse.
    """
    operation, binding, snapshot, read, _, _ = supplemented
    for pid, parent in zip((200, 201), parents):
        for rows in (binding['processes'], snapshot['rows']):
            next(row for row in rows if row['pid'] == pid)['parent_pid'] = parent
    _pin_console_source(supplemented)
    with pytest.raises(ValueError):
        resolve_owner_evidence(operation, binding, read)
    with pytest.raises(ValueError):
        console_tests.classify(snapshot, binding)
