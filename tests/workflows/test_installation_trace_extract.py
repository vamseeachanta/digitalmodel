"""Extraction contracts independent of licensed solver execution."""
from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.workflows.installation_trace_extract import checked_history, chord_diagnostics


class FakeObject:
    def __init__(self, values, units="kN"):
        self.values, self.units = values, units

    def varDetails(self, kind, objectExtra=None):
        return [SimpleNamespace(VarName="Effective tension", VarUnits=self.units)]

    def TimeHistory(self, variable, period, extra):
        return self.values


def test_checked_history_preserves_signed_results():
    api = SimpleNamespace(ResultType=SimpleNamespace(TimeHistory=0))
    result = checked_history(FakeObject([-2, 0, 4]), api, "Effective tension", "kN", None, None, 3)
    np.testing.assert_array_equal(result, [-2, 0, 4])


@pytest.mark.parametrize("values,units,n", [([1, float('nan')], "kN", 2), ([1], "kN", 2), ([1, 2], "N", 2)])
def test_checked_history_rejects_bad_data(values, units, n):
    api = SimpleNamespace(ResultType=SimpleNamespace(TimeHistory=0))
    with pytest.raises(ValueError):
        checked_history(FakeObject(values, units), api, "Effective tension", "kN", None, None, n)


def test_chord_diagnostic_is_not_slack_length():
    result = chord_diagnostics([0, 1, 2], np.zeros((3, 3)),
                               [[3, 0, 0], [4, 0, 0], [5, 0, 0]], 4)
    np.testing.assert_allclose(result['span_m'], [3, 4, 5])
    np.testing.assert_allclose(result['unstretched_length_minus_span_m'], [1, 0, -1])
    np.testing.assert_allclose(result['span_rate_m_per_s'], [1, 1, 1])
    assert 'not a measured slack length' in result['interpretation']
