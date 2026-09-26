"""Prepare a traced AHC-off diagnostic master without importing a solver API."""
from __future__ import annotations

import copy
import hashlib
import json
import math
from pathlib import Path

import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump


def _digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _walk(value, path=()):
    if isinstance(value, dict):
        for key, child in value.items():
            yield from _walk(child, path + (key,))
    elif isinstance(value, list):
        for index, child in enumerate(value):
            yield from _walk(child, path + (index,))
    else:
        yield path, value


def _dependencies(model, controller=None):
    for path, value in _walk(model):
        keys = [str(key).lower() for key in path]
        if any(key in ('basefile', 'includefile') for key in keys):
            raise ValueError('Unresolved model dependency')
        if controller is not None and value == controller:
            raise ValueError('Controller has another reference')
        key = keys[-1] if keys else ''
        if value not in (None, '', '(none)') and (
            key.endswith('filename') or key == 'externalfunctions'
            or (key in ('source', 'datasource') and value == 'External file')
        ):
            raise ValueError(f'Unresolved external reference at {path}')


def _selected(items, name, label):
    if not isinstance(items, list):
        raise ValueError(f'Missing {label} list')
    found = [(i, item) for i, item in enumerate(items)
             if isinstance(item, dict) and item.get('Name') == name]
    if len(found) != 1:
        raise ValueError(f'Expected exactly one matching {label}')
    return found[0]


def _changed_model(before, winch_name, controller_name):
    after = copy.deepcopy(before)
    index, winch = _selected(after.get('Winches'), winch_name, 'winch')
    required = {'WinchControlType': 'Whole simulation',
                'WholeSimulationControlMode': 'Specified payout rate',
                'StaticMode': 'Specified length',
                'WholeSimulationPayoutRate': controller_name}
    if any(winch.get(key) != value for key, value in required.items()):
        raise ValueError('Unsupported winch control configuration')
    length = winch.get('StaticValue')
    if isinstance(length, bool) or not isinstance(length, (float, int)):
        raise ValueError('Static length must be numeric')
    if not math.isfinite(length) or length <= 0:
        raise ValueError('Static length must be finite and positive')
    variable = after.get('VariableData', {})
    controllers = variable.get('Externalfunctions')
    controller_index, _ = _selected(controllers, controller_name, 'controller')
    winch['WholeSimulationPayoutRate'] = 0
    controllers.pop(controller_index)
    if not controllers:
        variable.pop('Externalfunctions')
    _dependencies(after, controller_name)
    diff = [
        {'path': ['Winches', index, 'WholeSimulationPayoutRate'],
         'operation': 'replace', 'before': controller_name, 'after': 0},
        {'path': ['VariableData', 'Externalfunctions'], 'operation': 'remove',
         'before': before['VariableData']['Externalfunctions']},
    ]
    if controllers:
        raise ValueError('Other external controllers remain')
    return after, diff


def _verify_diff(before, after, diff):
    restored = copy.deepcopy(after)
    winch_index = diff[0]['path'][1]
    restored['Winches'][winch_index]['WholeSimulationPayoutRate'] = diff[0]['before']
    restored['VariableData']['Externalfunctions'] = diff[1]['before']
    if restored != before:
        raise ValueError('Unexpected semantic change outside authorized preparation')


def build_ahc_off_master(source, expectedsha, output, winch_name, controller_name):
    """Write master.yml and preparation.json in a NEW directory; never run analysis.

    The control change is an AHC-off diagnostic, not source-case reproduction.
    Unknown file-backed dependencies fail closed. Control tags are preserved.
    """
    source, output = Path(source).resolve(strict=True), Path(output).absolute()
    if output.exists():
        raise FileExistsError(output)
    source_bytes = source.read_bytes()
    if not isinstance(expectedsha, str) or hashlib.sha256(source_bytes).hexdigest() != expectedsha:
        raise ValueError('Source digest mismatch')
    before = yaml.load(source_bytes.decode('utf-8-sig'), Loader=OrcaFlexLoader)
    if not isinstance(before, dict):
        raise ValueError('Expected a complete model mapping')
    after, diff = _changed_model(before, winch_name, controller_name)
    _verify_diff(before, after, diff)
    if _digest(source) != expectedsha:
        raise ValueError('Source changed during preparation')
    output.mkdir(parents=True, exist_ok=False)
    master = output / 'master.yml'
    orcaflex_dump(after, master)
    readback = yaml.load(master.read_text(encoding='utf-8'), Loader=OrcaFlexLoader)
    if readback != after:
        raise ValueError('Output semantic readback mismatch')
    _verify_diff(before, readback, diff)
    manifest = {'schema_version': 1, 'state': 'prepared_not_run',
                'analysis_executed': False, 'acceptance_established': False,
                'model_purpose': 'AHC-off diagnostic; not source reproduction',
                'source_name': source.name, 'source_sha256': expectedsha,
                'master_name': master.name, 'master_sha256': _digest(master),
                'semantic_diff': diff, 'all_other_model_data_unchanged': True}
    target = output / 'preparation.json'
    target.write_text(json.dumps(manifest, indent=2) + '\n', encoding='utf-8')
    if json.loads(target.read_text(encoding='utf-8')) != manifest:
        raise ValueError('Manifest readback mismatch')
    return manifest


def main(argv=None):
    """Prepare files only; this command has no solver or execution option."""
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('output', type=Path)
    parser.add_argument('--source-sha256', required=True)
    parser.add_argument('--winch-name', required=True)
    parser.add_argument('--controller-name', required=True)
    args = parser.parse_args(argv)
    result = build_ahc_off_master(
        args.source, args.source_sha256, args.output,
        args.winch_name, args.controller_name,
    )
    print(json.dumps(result, indent=2))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
