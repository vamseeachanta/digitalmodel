# ABOUTME: Safety and physical-coordinate tests for the unqualified FFS study diagnostic.
# ABOUTME: Characterization outputs cannot be consumed as allowable wall envelopes.
import math
import json

import pytest

from digitalmodel.asset_integrity.uniform_loss_diagnostic import diagnose_case, run_study, main


def case(**overrides):
    values = dict(od_in=20.0, nominal_wall_in=0.5, axial_length_in=8.0,
                  width_fraction=0.1, remaining_fraction=0.6)
    values.update(overrides)
    return diagnose_case(**values)


def test_outputs_withhold_allowable_wall_even_when_raw_algorithm_passes():
    result = case(remaining_fraction=1.0)
    assert result['raw_level1']['verdict'] == 'ACCEPT'
    assert result['qualification'] == 'UNQUALIFIED_BASELINE'
    assert result['allowable_remaining_wall_in'] is None
    assert 'WIDTH_NOT_ASSESSED' in result['limitations']


def test_coordinates_represent_cell_edges_and_diagnose_shallow_loss_length():
    deep = case()
    assert deep['raw_level2']['flaw_length_in'] == pytest.approx(8.0)
    assert deep['inputs']['width_arc_in'] == pytest.approx(math.pi * 19.5 * 0.1)
    assert deep['inputs']['sound_wall_normalized_length'] == pytest.approx(8 / math.sqrt(19 * 0.5))
    shallow = case(remaining_fraction=0.95)
    assert shallow['raw_level2']['flaw_length_in'] == pytest.approx(8.0 / 16)
    assert 'ALGORITHM_LENGTH_DIFFERS' in shallow['limitations']


def test_width_omission_is_characterized_not_presented_as_acceptance():
    narrow, wide = case(width_fraction=0.02), case(width_fraction=0.5)
    assert narrow['inputs']['width_arc_in'] != wide['inputs']['width_arc_in']
    assert narrow['raw_level2']['rsf'] == wide['raw_level2']['rsf']
    assert narrow['raw_level2']['lambda'] == wide['raw_level2']['lambda']
    assert narrow['assessment_status'] == 'UNQUALIFIED'


def test_out_of_range_cannot_become_a_pass_or_fail():
    result = case(axial_length_in=100.0)
    assert result['assessment_status'] == 'INAPPLICABLE'
    assert result['raw_level2']['applicability']['ok'] is False
    assert result['allowable_remaining_wall_in'] is None


@pytest.mark.parametrize('overrides', [dict(od_in=float('nan')), dict(od_in=0),
    dict(nominal_wall_in=11), dict(width_fraction=1.1), dict(width_fraction=0),
    dict(remaining_fraction=-0.1), dict(axial_length_in=float('inf'))])
def test_invalid_physical_inputs_are_rejected(overrides):
    with pytest.raises(ValueError):
        case(**overrides)


def test_study_is_bounded_reproducible_and_carries_source_hashes():
    result = run_study()
    assert len(result['cases']) == 4 * 7 * 4 * 7
    assert result == run_study()
    assert result['meta']['target_standard'] == 'API 579-1/ASME FFS-1 2021 Part 5'
    assert result['meta']['qualified_cases'] == 0
    assert result['meta']['source_sha256']
    assert all(len(value) == 64 for value in result['meta']['source_sha256'].values())
    assert all(row['allowable_remaining_wall_in'] is None and row['allowable_wall_reason']
               and row['assessment_status'] in {'UNQUALIFIED', 'INAPPLICABLE'}
               for row in result['cases'])
    json.dumps(result, allow_nan=False)


def test_cli_writes_strict_json_and_execution_provenance(tmp_path, monkeypatch):
    output = tmp_path / 'diagnostic.json'
    monkeypatch.setattr('sys.argv', ['diagnostic', '--output', str(output)])
    main()
    data = json.loads(output.read_text(encoding='utf-8'))
    assert len(data['cases']) == 784
    assert len(data['meta']['runtime']['executing_revision']) == 40
    assert isinstance(data['meta']['runtime']['working_tree_dirty'], bool)
    assert data['meta']['runtime']['run_utc']
    assert data['meta']['source_hash_scope']
