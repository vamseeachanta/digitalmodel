"""Regression pins for current pressure-intake review dispositions."""
from tests.ansys.test_analysis_pressure_observed import pressure, build  # noqa: F401
from tests.ansys.test_analysis_pressure_observed import rehash
import pytest


def test_recorded_source_scope_survives_new_live_selected_set(pressure, monkeypatch):
    from digitalmodel.ansys.analysis_pressure_observed import validate_pressure_observed_transition
    from digitalmodel.ansys import analysis_pressure_source as source
    package = build(pressure)
    monkeypatch.setattr(source, 'SOURCE_PATHS', ('new-future-module.py',))
    validate_pressure_observed_transition(package, pressure['base'], pressure['resolver'])


def test_existing_historical_counter_scope_is_preserved(pressure):
    from digitalmodel.ansys.analysis_pressure_observed import _coverage
    baseline = pressure['base']
    baseline['coverage']['historical_baseline_counts'] = {'earlier_intake_count': 7}
    coverage = _coverage(baseline, baseline['cases'])
    assert coverage['historical_baseline_counts']['earlier_intake_count'] == 7


def test_explicit_core_source_subset_names_actual_diagnostic_runner():
    from digitalmodel.ansys.analysis_pressure_inputs import CORE_SOURCES
    assert 'src/digitalmodel/ansys/cylinder_runner.py' in CORE_SOURCES
    assert 'src/digitalmodel/ansys/runner.py' not in CORE_SOURCES


def test_coverage_rejects_boolean_substitution_for_integer_zero(pressure):
    from digitalmodel.ansys.analysis_matrix_publish import _relation
    package = build(pressure)
    package['coverage']['qualified_responses'] = False
    package = rehash(package, pressure['resolver'])
    with pytest.raises(ValueError, match='coverage differs'):
        _relation(package, pressure['base'], pressure['resolver'])


@pytest.mark.parametrize("field", ["analysis_id", "criteria_revision", "finding_ledger", "intended_uses"])
def test_diagnostic_transition_preserves_other_study_metadata(pressure, field):
    from digitalmodel.ansys.analysis_matrix_publish import _relation
    package = build(pressure)
    package[field] = ["forged-use"] if field == "intended_uses" else ([] if isinstance(package.get(field), list) else "forged-metadata")
    if field == 'finding_ledger':
        package[field] = [{'finding': 'forged', 'disposition': 'forged', 'affected_responses': []}]
    package = rehash(package, pressure['resolver'])
    with pytest.raises(ValueError, match='study metadata differs'):
        _relation(package, pressure['base'], pressure['resolver'])
