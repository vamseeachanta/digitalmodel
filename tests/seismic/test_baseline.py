import numpy as np
import pytest

from digitalmodel.seismic.baseline import polynomial_detrend, pre_event_mean


def test_pre_event_mean_removes_mean_of_initial_samples():
    corrected, offset_m_s2 = pre_event_mean(
        [1.0, 3.0, 4.0, 6.0],
        samples=2,
    )

    assert offset_m_s2 == pytest.approx(2.0)
    np.testing.assert_allclose(corrected, [-1.0, 1.0, 2.0, 4.0])


def test_pre_event_mean_rejects_empty_window():
    with pytest.raises(ValueError, match="samples"):
        pre_event_mean([1.0, 2.0], samples=0)


def test_polynomial_detrend_removes_linear_trend():
    time_s = np.array([0.0, 1.0, 2.0, 3.0])
    acceleration_m_s2 = 2.0 + 0.5 * time_s

    corrected, coefficients = polynomial_detrend(
        time_s,
        acceleration_m_s2,
        order=1,
    )

    assert coefficients.tolist() == pytest.approx([0.5, 2.0])
    np.testing.assert_allclose(corrected, np.zeros_like(time_s), atol=1.0e-12)
