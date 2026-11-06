"""Offline report environment regressions with independent hand calculations.

Basis: OrcaWave WebHelp, Results: Hydrostatics, Hydrostatic stiffness matrix
(online edition accessed 2026-10-19): for a free-floating displacement body,
C44 = rho*g*V*GM_T and C55 = rho*g*V*GM_L.
https://www.orcina.com/webhelp/OrcaWave/Content/html/Results,Hydrostatics.htm

All dimensional inputs below are synthetic required inputs, not environmental
defaults or acceptance criteria. Choose V=10 m3, C44=600000 N m/rad and
C55=900000 N m/rad. Direct division gives the following hand calculations:
rho=1000 kg/m3, g=10 m/s2: denominator=100000 N, GM_T=6 m, GM_L=9 m;
rho=1200 kg/m3, g=10 m/s2: denominator=120000 N, GM_T=5 m, GM_L=7.5 m;
rho=1000 kg/m3, g=20 m/s2: denominator=200000 N, GM_T=3 m, GM_L=4.5 m.
These exactly representable values need no numerical acceptance tolerance.
"""

import json
from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner
from digitalmodel.hydrodynamics.diffraction.output_schemas import HydrostaticResults
from digitalmodel.hydrodynamics.diffraction.postprocess import _build_html_report
from digitalmodel.hydrodynamics.diffraction.postprocess import postprocess_diffraction_run
from digitalmodel.hydrodynamics.diffraction.report_computations import (
    compute_natural_periods,
    compute_radii_of_gyration,
    compute_stability,
)
from digitalmodel.hydrodynamics.diffraction.report_data_models import HydrostaticData


@pytest.fixture
def native_hydrostatic_results():
    """Synthetic native SI data: tonnes, kN and metres; no solver is loaded.

    OrcaWave WebHelp, Data: Model, Units (online edition accessed 2026-10-25):
    https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Model.htm#Units
    Native C44=600 kN m/rad and C55=900 kN m/rad give the hand cases above
    after converting to N. Couplings exercise both off-diagonal blocks.
    """
    restoring = np.zeros((6, 6))
    restoring[2, 2] = 100.0
    restoring[2, 3] = restoring[3, 2] = 2.0
    restoring[2, 4] = restoring[4, 2] = 3.0
    restoring[3, 4] = restoring[4, 3] = 5.0
    restoring[3, 3], restoring[4, 4] = 600.0, 900.0
    native = SimpleNamespace(
        frequencies=np.array([0.2, 0.1]),
        headings=np.array([0.0]),
        displacementRAOs=np.zeros((1, 2, 6), dtype=complex),
        addedMass=np.zeros((2, 6, 6)),
        damping=np.zeros((2, 6, 6)),
        hydrostaticResults=[{
            "volume": 10.0,
            "mass": 10.0,
            "centreOfMass": [1.0, 2.0, 3.0],
            "centreOfBuoyancy": [1.0, 2.0, 3.0],
            "Awp": 10.0,
            "restoringMatrix": restoring,
        }],
    )
    runner = object.__new__(OrcaWaveRunner)
    runner._result = SimpleNamespace(spec_name="synthetic")
    runner._water_depth = 200.0
    return runner, native


def test_producer_normalizes_mass_and_preserves_geometry(native_hydrostatic_results):
    """Ten native tonnes must serialize as 10000 kg; geometry stays metric."""
    runner, native = native_hydrostatic_results
    results = runner._build_results_from_object(native, [])
    hs = results.hydrostatics
    assert results.unit_system == "SI"
    assert hs is not None
    assert hs.mass == 10000.0
    assert hs.displacement_volume == 10.0
    assert hs.waterplane_area == 10.0
    assert hs.centre_of_gravity == [1.0, 2.0, 3.0]
    assert hs.centre_of_buoyancy == [1.0, 2.0, 3.0]
    assert native.hydrostaticResults[0]["mass"] == 10.0


def test_producer_normalizes_every_restoring_block(native_hydrostatic_results):
    """Each force/moment block converts kN to N, retaining length and angle."""
    runner, native = native_hydrostatic_results
    expected = np.zeros((6, 6))
    expected[2, 2] = 100000.0
    expected[2, 3] = expected[3, 2] = 2000.0
    expected[2, 4] = expected[4, 2] = 3000.0
    expected[3, 4] = expected[4, 3] = 5000.0
    expected[3, 3], expected[4, 4] = 600000.0, 900000.0
    original = native.hydrostaticResults[0]["restoringMatrix"].copy()
    for _ in range(2):
        results = runner._build_results_from_object(native, [])
        assert results.hydrostatics is not None
        np.testing.assert_array_equal(results.hydrostatics.stiffness_matrix, expected)
        np.testing.assert_array_equal(
            native.hydrostaticResults[0]["restoringMatrix"], original
        )


@pytest.mark.parametrize(
    "density,gravity,transverse,longitudinal",
    [(1000.0, 10.0, "6.0000", "9.0000"),
     (1200.0, 10.0, "5.0000", "7.5000"),
     (1000.0, 20.0, "3.0000", "4.5000")],
)
def test_native_producer_json_public_report_gm(
    tmp_path, native_hydrostatic_results, density, gravity, transverse, longitudinal
):
    """Native stiffness must reach public reports as the independent hand cases."""
    runner, native = native_hydrostatic_results
    # Free-floating balance: native tonnes = rho[kg/m3] * V[m3] / 1000.
    native.hydrostaticResults[0]["mass"] = density * 10.0 / 1000.0
    results = runner._build_results_from_object(native, [])
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "diffraction_results.json").write_text(json.dumps(results.to_dict()))
    result = postprocess_diffraction_run(
        bundle, tmp_path / "report", make_plots=False,
        environment=EnvironmentSpec(
            water_depth=200.0, water_density=density, gravity=gravity,
        ),
    )
    assert result.report is not None
    html = result.report.read_text()
    assert ("<td>GM_T (transverse metacentric height)</td>"
            f"<td>{transverse} m</td>") in html
    assert ("<td>GM_L (longitudinal metacentric height)</td>"
            f"<td>{longitudinal} m</td>") in html
    assert json.loads(result.index.read_text())["report"] == result.report.name


@pytest.fixture
def hydrostatic_bundle(mock_diffraction_results):
    """Use the synthetic inputs and stiffnesses documented above."""
    matrix = np.zeros((6, 6))  # Six rigid-body degrees of freedom; zero coupling.
    matrix[3, 3] = 600000.0
    matrix[4, 4] = 900000.0
    mock_diffraction_results.hydrostatics = HydrostaticResults(
        vessel_name=mock_diffraction_results.vessel_name,
        displacement_volume=10.0,
        mass=10000.0,
        centre_of_gravity=[0.0, 0.0, 0.0],
        centre_of_buoyancy=[0.0, 0.0, 0.0],
        waterplane_area=10.0,
        stiffness_matrix=matrix,
    )
    # Exercise the serialized bundle boundary used by postprocessing.
    return type(mock_diffraction_results).from_dict(
        json.loads(json.dumps(mock_diffraction_results.to_dict()))
    )


@pytest.mark.parametrize(
    "density,gravity,transverse,longitudinal",
    [(1000.0, 10.0, "6.0000", "9.0000"),
     (1200.0, 10.0, "5.0000", "7.5000"),
     (1000.0, 20.0, "3.0000", "4.5000")],
)
def test_report_gm_uses_input_environment(
    tmp_path, hydrostatic_bundle, density, gravity, transverse, longitudinal
):
    """Each expected GM follows the independent divisions in this module."""
    environment = EnvironmentSpec(
        water_depth=hydrostatic_bundle.water_depth,
        water_density=density,
        gravity=gravity,
    )
    # Free-floating mass balance for each synthetic environment: M=rho*V.
    # Same WebHelp source, Hydrostatic stiffness matrix, q definition.
    hydrostatic_bundle.hydrostatics.mass = density * 10.0
    path = _build_html_report(
        hydrostatic_bundle, tmp_path, {}, environment=environment
    )
    assert path is not None
    html = path.read_text()
    assert f"<td>{transverse} m</td>" in html
    assert f"<td>{longitudinal} m</td>" in html
    assert "BM_T (transverse)" not in html
    assert "Inertia Matrix</h3>" not in html
    assert "Natural Period Estimates</h2>" not in html


def test_missing_environment_does_not_assume_gm(tmp_path, hydrostatic_bundle):
    """Missing environment must not silently select seawater or gravity."""
    path = _build_html_report(hydrostatic_bundle, tmp_path, {})
    assert path is not None
    html = path.read_text()
    assert "NOT_EVALUATED" in html
    assert "input environment" in html
    assert "GM_T (transverse metacentric height)" not in html


@pytest.mark.parametrize("volume,gravity", [(0.0, 10.0), (10.0, float("inf"))])
def test_invalid_gm_denominator_is_not_evaluated(
    tmp_path, hydrostatic_bundle, volume, gravity
):
    """Zero volume and unbounded gravity cannot define the cited GM quotient."""
    hydrostatic_bundle.hydrostatics.displacement_volume = volume
    path = _build_html_report(
        hydrostatic_bundle, tmp_path, {},
        environment=EnvironmentSpec(
            water_depth=hydrostatic_bundle.water_depth,
            water_density=1000.0, gravity=gravity,
        ),
    )
    assert path is not None
    html = path.read_text()
    assert "NOT_EVALUATED" in html
    assert "GM_T (transverse metacentric height)" not in html


def test_appendix_does_not_claim_fixed_environment(tmp_path, hydrostatic_bundle):
    """The formula must describe the caller's environment symbolically."""
    path = _build_html_report(hydrostatic_bundle, tmp_path, {})
    assert path is not None
    html = path.read_text()
    assert "rho=1025" not in html
    assert "g=9.81" not in html
    assert "density and gravity from the input environment" in html


def test_public_postprocess_passes_environment(tmp_path, hydrostatic_bundle):
    """The public bundle entry point must preserve the 6 m / 9 m hand case."""
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "diffraction_results.json").write_text(
        json.dumps(hydrostatic_bundle.to_dict())
    )
    result = postprocess_diffraction_run(
        bundle, tmp_path / "report", make_plots=False,
        environment=EnvironmentSpec(
            water_depth=hydrostatic_bundle.water_depth,
            water_density=1000.0, gravity=10.0,
        ),
    )
    assert result.report is not None
    html = result.report.read_text()
    assert "<td>6.0000 m</td>" in html
    assert "<td>9.0000 m</td>" in html
    index = json.loads(result.index.read_text())
    assert index["report"] == result.report.name


def test_partial_hydrostatics_leave_unsupported_quantities_unavailable():
    """Missing moments and inertia cannot supply BM, gyration or periods.

    The synthetic stiffness, volume and environment use the first hand case.
    Zero coordinates and waterplane area are synthetic geometry inputs only.
    """
    matrix = np.zeros((6, 6))
    matrix[3, 3], matrix[4, 4] = 600000.0, 900000.0
    hs = HydrostaticData(
        volume=10.0, mass=10000.0,
        centre_of_buoyancy=[0.0, 0.0, 0.0],
        centre_of_mass=[0.0, 0.0, 0.0],
        waterplane_area=10.0, restoring_matrix=matrix.tolist(),
    )
    stability = compute_stability(hs, rho=1000.0, g=10.0)
    assert stability["gm_transverse"] == 6.0
    assert stability["gm_longitudinal"] == 9.0
    assert stability["bm_transverse"] is None
    assert stability["bm_longitudinal"] is None
    assert compute_radii_of_gyration(hs) == {}
    assert all(value is None for value in compute_natural_periods(hs, {}, []).values())


def test_non_si_bundle_does_not_mix_stiffness_and_environment_units(
    tmp_path, hydrostatic_bundle
):
    """SI density and gravity must not divide native-unit stiffness."""
    hydrostatic_bundle.unit_system = "orcaflex"
    path = _build_html_report(
        hydrostatic_bundle, tmp_path, {},
        environment=EnvironmentSpec(water_depth=hydrostatic_bundle.water_depth),
    )
    assert path is not None
    html = path.read_text()
    assert "NOT_EVALUATED" in html
    assert "GM_T (transverse metacentric height)" not in html
