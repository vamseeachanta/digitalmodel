# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check hosted river-test coverage and additive gate configuration changes.

Run this module with before and after YAML paths to verify that formatting
preserves every parsed value except the added river-current test path.
"""

import sys
from pathlib import Path

import yaml


def test_hosted_gate_includes_river_currents():
    root = Path(__file__).resolve().parents[3]
    config = yaml.safe_load((root / '.claude/quality-gates.yaml').read_text())
    gate = config['gates']['tests-hydrodynamics-other']
    assert 'tests/hydrodynamics/river_currents/' in gate['command'].split()
    assert gate['enabled'] is True
    assert gate['failure_action'] == 'block'


if __name__ == '__main__':
    before = yaml.safe_load(Path(sys.argv[1]).read_text())
    after = yaml.safe_load(Path(sys.argv[2]).read_text())
    gate = before['gates']['tests-hydrodynamics-other']
    old = 'python -m pytest tests/hydrodynamics/hull_library/'
    new = 'python -m pytest tests/hydrodynamics/river_currents/ tests/hydrodynamics/hull_library/'
    assert gate['command'].count(old) == 1
    gate['command'] = gate['command'].replace(old, new)
    assert before == after
    print('Parsed gate configuration: only the river-current test path was added.')
