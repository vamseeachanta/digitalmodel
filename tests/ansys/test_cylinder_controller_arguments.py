"""Controller file identity variants; synthetic evidence only."""
from copy import deepcopy

import pytest

from digitalmodel.ansys.cylinder_cfd_owner_evidence import resolve_owner_evidence
from tests.ansys import test_cylinder_cfd_owner_evidence as owner_tests

facts = owner_tests.facts
assemble = owner_tests.assemble


def set_arguments(facts, script, config, basis):
    controller = facts[1]['processes'][0]
    controller['argv'][1:] = [script, config]
    controller['script_sources'][0]['resolution_basis'] = basis
    facts[1]['processes'][2]['argv'][1:] = [script, config]
    ancestors = facts[2]['current_cfd_owner_evidence']['ancestors']
    ancestors[2]['argv'] = list(controller['argv'])
    ancestors[3]['argv'] = list(facts[1]['processes'][2]['argv'])
    ancestors[2]['sources'][0]['argv_literal'] = script


@pytest.mark.parametrize('script,config,basis', [
    ('rotation_queue.py', 'controller-config-r5.json', 'observed_interpreter_cwd'),
    ('C:/control/rotation_queue.py', 'C:/control/controller-config-r5.json', 'absolute_argument'),
    ('C:/control/rotation_queue.py', 'controller-config-r5.json', 'absolute_argument'),
    ('rotation_queue.py', 'C:/control/controller-config-r5.json', 'observed_interpreter_cwd'),
    ('c:\\CONTROL\\ROTATION_QUEUE.PY', 'c:\\CONTROL\\controller-config-r5.json', 'absolute_argument'),
])
def test_equivalent_controller_paths_preserve_literal_evidence(facts, script, config, basis):
    set_arguments(facts, script, config, basis)
    operation, binding, read, _ = assemble(facts)
    before = deepcopy(binding)
    result = resolve_owner_evidence(operation, binding, read)
    assert result['matched_joins']['execution_rows'] == 122
    assert binding == before


@pytest.mark.parametrize('argument', [
    'D:/control/{name}', 'C:/other/{name}', 'C:{name}', '/control/{name}',
    '//host/share/{name}', '//?/C:/control/{name}', 'C:/control/../control/{name}',
    'C:/control/./{name}', 'C:/control//{name}', 'C:/control/{name}.',
    'C:/control/{name} ', 'C:/control/{name}:stream', '', '../{name}',
    'C:/CONTRO~1/{name}', 'C:/control/wrong.json',
])
@pytest.mark.parametrize('index,name', [(1, 'rotation_queue.py'), (2, 'controller-config-r5.json')])
def test_nonidentical_or_ambiguous_argument_refuses(facts, argument, index, name):
    args = ['C:/control/rotation_queue.py', 'C:/control/controller-config-r5.json']
    args[index - 1] = argument.format(name=name)
    set_arguments(facts, *args, 'absolute_argument')
    operation, binding, read, _ = assemble(facts)
    with pytest.raises(ValueError):
        resolve_owner_evidence(operation, binding, read)


@pytest.mark.parametrize('absolute', [True, False])
def test_resolution_basis_must_match_observed_argument_form(facts, absolute):
    script = 'C:/control/rotation_queue.py' if absolute else 'rotation_queue.py'
    basis = 'observed_interpreter_cwd' if absolute else 'absolute_argument'
    set_arguments(facts, script, 'controller-config-r5.json', basis)
    operation, binding, read, _ = assemble(facts)
    with pytest.raises(ValueError, match='basis'):
        resolve_owner_evidence(operation, binding, read)


@pytest.mark.parametrize('fault', ['root', 'digest', 'historical_path', 'source_index', 'duplicate_source'])
def test_absolute_arguments_do_not_relax_other_source_joins(facts, fault):
    set_arguments(facts, 'C:/control/rotation_queue.py', 'C:/control/controller-config-r5.json',
                  'absolute_argument')
    if fault == 'root':
        facts[3]['root'] = 'C:/other'
    elif fault == 'digest':
        facts[3]['evidence_sha256']['rotation_queue.py'] = 'f' * 64
    elif fault == 'historical_path':
        facts[2]['current_cfd_owner_evidence']['ancestors'][2]['sources'][0]['cwd_resolved_path'] = 'C:/other/rotation_queue.py'
    elif fault == 'source_index':
        facts[1]['processes'][0]['script_sources'][0]['argv_index'] = 2
    else:
        facts[1]['processes'][0]['script_sources'] *= 2
    operation, binding, read, _ = assemble(facts)
    with pytest.raises(ValueError):
        resolve_owner_evidence(operation, binding, read)
