"""RAO phase / heading convention tests (digitalmodel #1358).

No matched AQWA/OrcaFlex vessel fixture pair exists in-repo, so these tests
verify the normalisation is *applied consistently* (the sign rule and its
effect on the complex transfer), not that it matches a real matched dataset.
"""

import numpy as np

from digitalmodel.motion_forecast.conventions import (
    RAOSource,
    heading_going_to_deg,
    normalize_phase_deg,
)
from digitalmodel.motion_forecast.rao_adapter import GridRAO


def test_phase_sign_rules():
    assert np.allclose(normalize_phase_deg([90.0], RAOSource.ORCAFLEX), [90.0])
    assert np.allclose(normalize_phase_deg([90.0], RAOSource.CANONICAL), [90.0])
    assert np.allclose(normalize_phase_deg([90.0], RAOSource.AQWA), [-90.0])


def test_heading_going_to():
    assert heading_going_to_deg(30.0, coming_from=True) == 210.0
    assert heading_going_to_deg(30.0, coming_from=False) == 30.0
    assert heading_going_to_deg(200.0, coming_from=True) == 20.0


def _one_freq_rao(phase_deg):
    """Minimal single-frequency, single-heading DisplacementRAO (heave only)."""
    from digitalmodel.marine_ops.marine_analysis.models.rao_data import (
        DisplacementRAO, DOFData,
    )

    freqs = np.array([0.8])
    heads = np.array([0.0])
    amp = np.array([[1.0]])
    zero = DOFData(np.zeros((1, 1)), np.zeros((1, 1)))
    heave = DOFData(amp, np.array([[phase_deg]]))
    return DisplacementRAO(
        frequencies=freqs, headings=heads,
        surge=zero, sway=zero, heave=heave,
        roll=DOFData(np.zeros((1, 1)), np.zeros((1, 1))),
        pitch=DOFData(np.zeros((1, 1)), np.zeros((1, 1))),
        yaw=DOFData(np.zeros((1, 1)), np.zeros((1, 1))),
    )


def test_aqwa_normalises_to_orcaflex():
    """AQWA raw phase psi (lag) and OrcaFlex raw phase -psi (lead) -> same H."""
    psi = 37.0
    aqwa = GridRAO(_one_freq_rao(psi), RAOSource.AQWA)
    orca = GridRAO(_one_freq_rao(-psi), RAOSource.ORCAFLEX)
    Ha = aqwa.H("heave", 0.8, 0.0)
    Ho = orca.H("heave", 0.8, 0.0)
    assert np.isclose(Ha, Ho)
    # both carry amplitude 1 at the canonical lead angle -psi
    assert np.isclose(abs(Ha), 1.0)
    assert np.isclose(np.angle(Ha, deg=True), -psi)
