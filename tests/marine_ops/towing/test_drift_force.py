"""Environmental load checks against existing physics and an independent table."""
import pytest


def test_uniform_current_and_wind_add_as_vectors():
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile

    barge = VesselProperties('synthetic barge', 80, 24, 4, 7000, freeboard=4)
    result = compute_drift_force(barge, 2.0, WindProfile(10, 0))
    # Existing head-current coefficient 0.55; head-wind coefficient 0.85.
    assert result.current_x_n == pytest.approx(0.5 * 1025 * 4 * 24 * 4 * 0.55)
    assert result.wind_x_n == pytest.approx(0.5 * 1.225 * 100 * 24 * 4 * 0.85)
    assert result.force_y_n == 0
    assert result.resultant_n == pytest.approx(result.current_x_n + result.wind_x_n)


def test_callable_profile_integrates_signed_square_over_draft():
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.hydrodynamics.river_currents.profiles import power_law_profile
    from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile

    barge = VesselProperties('synthetic', 80, 24, 4, 7000, freeboard=4)
    # At water depth 10 m, integrate u^2=4*(z/10)^(1/3) over z=6..10.
    def profile(z):
        return power_law_profile(z, 10, 2, exponent_m=6)
    result = compute_drift_force(barge, profile, WindProfile(0), water_depth_m=10)
    mean_square = 4 / 10**(1/3) * (10**(4/3) - 6**(4/3)) * 3/4 / 4
    assert result.resultant_n == pytest.approx(0.5 * 1025 * 96 * 0.55 * mean_square,
                                              rel=1e-5)
    reverse = compute_drift_force(barge, lambda z: -profile(z), WindProfile(0),
                                  water_depth_m=10)
    assert reverse.force_x_n == pytest.approx(-result.force_x_n)


def test_ocimf_published_vlcc_table_case_within_five_percent():
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile

    # OCIMF Static Towing Assembly Guidelines (2020), MARIN 31746-1-PO,
    # Table 5, report p29: 90deg row (Cx,Cy,Cmz)=(0.015,0.855,-0.077).
    # https://www.ocimf.org/document-libary/58-static-towing-assembly-guidelines-2020/file
    # Geometry/speed are explicit synthetic inputs, not measured VLCC data.
    vessel = VesselProperties('table verification only', 300, 50, 20, 250000,
                              frontal_wind_area=1000, lateral_wind_area=5000)
    wind = WindProfile(20, 90, air_density_kg_m3=1.25,
                       coefficients=(0.015, 0.855, -0.077))
    result = compute_drift_force(vessel, 0, wind)
    assert result.wind_x_n == pytest.approx(3750, rel=0.05)
    assert result.wind_y_n == pytest.approx(1068750, rel=0.05)
    assert result.yaw_moment_nm == pytest.approx(-28875000, rel=0.05)


@pytest.mark.parametrize('direction', [0, 90, 180, 270, -90, 360])
def test_cardinal_loads_have_no_spurious_off_axis_component(direction):
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile

    vessel = VesselProperties('synthetic', 80, 24, 4, 7000)
    result = compute_drift_force(vessel, 1, WindProfile(0), current_direction_deg=direction)
    if direction % 180 == 0:
        assert result.force_y_n == 0
        assert result.yaw_moment_nm == 0
    else:
        assert result.force_x_n == 0


@pytest.mark.parametrize('current,kwargs', [
    (float('nan'), {}), (lambda z: float('inf'), {'water_depth_m': 10}),
    (lambda z: 1, {}), (lambda z: 1, {'water_depth_m': 4}),
    (lambda z: 1, {'water_depth_m': 10, 'n_slices': 0}),
    (1, {'current_direction_deg': float('nan')}),
])
def test_invalid_current_inputs_are_rejected(current, kwargs):
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile

    vessel = VesselProperties('synthetic', 80, 24, 4, 7000)
    with pytest.raises(ValueError):
        compute_drift_force(vessel, current, WindProfile(0), **kwargs)


def test_missing_environmental_citation_fails_closed(citation_metadata):
    from digitalmodel.citations.schema import CitationResolutionError
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile

    page = citation_metadata / 'wikis/engineering/wiki/standards/ocimf-meg4.md'
    page.unlink()
    with pytest.raises(CitationResolutionError, match='OCIMF-MEG4'):
        compute_drift_force(VesselProperties('synthetic', 80, 24, 4, 7000), 1, WindProfile(0))
