"""Hand calculations for issue 2743; basis: docs/damping-basis.md E1-E5."""
import numpy as np
import pytest

from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_damping import AqwaDamping
from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_raos import AqwaRAOs
from digitalmodel.hydrodynamics.aqwa.viscous_damping_determination import ViscousDampingDetermination


def viscous(amplitude=1.0, unit="rad/m"):
    raos = {"heave": {"amplitude": [[1.0]], "unit": "m/m"},
            "roll": {"amplitude": [[0.1]], "unit": unit},
            "pitch": {"amplitude": [[0.1]], "unit": unit}}
    members = [{"cd": 1, "area": 10, "lever_arm": r} for r in (10, -10)]
    return AqwaDamping().derive_viscous_damping_statistical(
        [0.5], raos, {"heave": members[:1], "roll": members, "pitch": members},
        wave_amplitude={"value": amplitude, "unit": "m"})


def test_harmonic_heave_and_symmetric_roll():
    result = viscous()
    assert result["B33"][0] == pytest.approx(2175.1176, rel=0.001)
    assert result["B44"][0] == pytest.approx(435023.511, rel=0.001)
    assert result["B55"][0] == pytest.approx(435023.511, rel=0.001)
    assert result["citation_sidecar"]["citations"][0]["section"] == "E1-E3"
    assert result["linearisation"]["method"] == "harmonic"


def test_amplitude_and_degrees():
    assert viscous(2)["B33"][0] == pytest.approx(2 * viscous()["B33"][0])
    assert viscous(unit="deg/m")["B44"][0] == pytest.approx(viscous()["B44"][0] * np.pi / 180)


def test_missing_amplitude_and_invalid_units():
    with pytest.raises(ValueError, match="amplitude"):
        AqwaDamping().derive_viscous_damping_statistical([0.5], {}, {})
    with pytest.raises(ValueError, match="unit"):
        viscous(unit="unknown")


def critical(restoring=2e8, inertia=1e9, dependent=False):
    omega = np.array([0.2, 0.5, 0.8, 1.0])
    return AqwaDamping().compute_critical_damping_ratios(
        omega, {f"B{k}": np.array([1e7, 1e7, 9e7, 1e7]) for k in (33, 44, 55)},
        {f"I{k}": inertia for k in (33, 44, 55)},
        {f"C{k}": restoring if k == 44 else 2e8 for k in (33, 44, 55)},
        added_mass_infinite={f"A{k}": 2e8 for k in (33, 44, 55)},
        added_mass_frequency={f"A{k}": np.full(4, 2e8) for k in (33, 44, 55)},
        use_frequency_dependent=dependent)


@pytest.mark.parametrize("dependent", [False, True])
def test_natural_frequency_zeta_and_period(dependent):
    result = critical(dependent=dependent)["roll"]
    assert result["natural_frequency_rad_s"] == pytest.approx(0.40824829)
    assert result["natural_period_s"] == pytest.approx(15.39059796)
    assert result["zeta_at_natural_frequency"] == pytest.approx(0.0102062073)


def test_summary_uses_resonance_not_grid_peak():
    result = critical(restoring=3e8)
    assert result["roll"]["natural_frequency_rad_s"] == pytest.approx(0.5)
    assert ViscousDampingDetermination()._characteristic_ratio(result, "roll") == pytest.approx(1 / 120)


@pytest.mark.parametrize("restoring", [-1e7, 0, float("nan")])
def test_invalid_restoring_names_roll(restoring):
    with pytest.raises(ValueError, match="roll"):
        critical(restoring=restoring)


def test_invalid_effective_mass():
    with pytest.raises(ValueError, match="heave"):
        critical(inertia=-2e8)


def test_combine_components_and_percentages():
    result = AqwaDamping().combine_damping([0.5], {"B33": [2.]}, {"B33": [3.]}, {"B33": [5.]})
    assert result["total"]["B33"].tolist() == [10.]
    assert result["percentages"]["B33"]["viscous"].tolist() == [30.]


@pytest.mark.parametrize("other", [[], [0.4], [0.5, 0.8]])
def test_frequency_mismatch(other):
    with pytest.raises(ValueError, match="Frequency"):
        ViscousDampingDetermination()._validate_frequency_alignment(np.array([0.5]), np.array(other))


def test_keyed_hydrostatic_config_rejects_missing_key():
    module = ViscousDampingDetermination()
    with pytest.raises(ValueError, match="matching"):
        module._resolve_hydro_property({"BODY_A": {"C33": 888.}}, {"id": "other"}, required=True)
    assert module._resolve_hydro_property({"C33": 888.}, {"id": "other"}, required=True) == {"C33": 888.}


def test_additional_damping_percent_contract():
    module = AqwaRAOs()
    assert module.get_additional_damping(2, 1000, 5) == 1500.
    assert module.get_additional_damping(2, 1000, 1.5) == 0.
    with pytest.raises(ValueError, match="percent"):
        module.get_additional_damping(2, 1000, 0.05)
    with pytest.raises(ValueError):
        module.get_additional_damping(2, -1000, 5)
