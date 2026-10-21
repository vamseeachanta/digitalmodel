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

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
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


def test_native_hydrostatics_are_normalized_before_serialized_report(tmp_path):
    """Native 600/900 kN m/rad yield 6/9 m, with 10 tonnes = 10000 kg.

    Native SI uses tonnes, metres and kN (OrcaWave WebHelp, Data: Model,
    Units). All restoring blocks share the kN-to-N factor; geometry does not.
    """
    from types import SimpleNamespace

    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner

    matrix = np.arange(36, dtype=float).reshape(6, 6)
    matrix[3, 3], matrix[4, 4] = 600.0, 900.0
    original = matrix.copy()
    native = SimpleNamespace(
        frequencies=np.array([0.1]), headings=np.array([0.0]),
        addedMass=np.zeros((1, 6, 6)), damping=np.zeros((1, 6, 6)),
        displacementRAOs=np.zeros((1, 1, 6), dtype=complex),
        hydrostaticResults=[dict(
            restoringMatrix=matrix, volume=10.0, mass=10.0,
            centreOfMass=[1.0, 2.0, 3.0], centreOfBuoyancy=[4.0, 5.0, 6.0],
            Awp=7.0,
        )],
    )
    # Avoid initialization, executable discovery and licensed API calls.
    runner = object.__new__(OrcaWaveRunner)
    runner._result = SimpleNamespace(spec_name="synthetic")
    runner._water_depth = 100.0
    produced = runner._build_results_from_object(native, [])
    bundle = type(produced).from_dict(json.loads(json.dumps(produced.to_dict())))
    assert bundle.unit_system == "SI"
    hs = bundle.hydrostatics
    assert hs.mass == 10000.0
    np.testing.assert_array_equal(hs.stiffness_matrix, original * 1000.0)
    np.testing.assert_array_equal(matrix, original)
    assert native.hydrostaticResults[0]["mass"] == 10.0
    assert hs.displacement_volume == 10.0
    assert hs.waterplane_area == 7.0
    np.testing.assert_array_equal(hs.centre_of_gravity, [1.0, 2.0, 3.0])
    np.testing.assert_array_equal(hs.centre_of_buoyancy, [4.0, 5.0, 6.0])
    path = _build_html_report(
        bundle, tmp_path, {},
        environment=EnvironmentSpec(
            water_depth=100.0, water_density=1000.0, gravity=10.0,
        ),
    )
    assert path is not None
    html = path.read_text()
    assert "<td>6.0000 m</td>" in html
    assert "<td>9.0000 m</td>" in html


@pytest.mark.parametrize("expose_units", [False, True])
def test_native_hydrostatic_mass_and_all_stiffness_blocks_use_si(expose_units):
    """Ten tonnes is 10000 kg; each native kN-based cell gains factor 1000."""
    from types import SimpleNamespace

    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner

    matrix = np.arange(36, dtype=float).reshape(6, 6)
    native = SimpleNamespace(hydrostaticResults=[dict(
        restoringMatrix=matrix, volume=10.0, mass=10.0,
        centreOfMass=[1.0, 2.0, 3.0], centreOfBuoyancy=[4.0, 5.0, 6.0],
        Awp=7.0,
    )])
    if expose_units:
        native.UnitsSystem = "SI"
    hs = OrcaWaveRunner._extract_hydrostatics(native, "synthetic")
    assert hs.mass == 10000.0
    np.testing.assert_array_equal(hs.stiffness_matrix, matrix * 1000.0)


@pytest.mark.parametrize("units", ["US", "User", None, ""])
def test_native_hydrostatics_reject_exposed_non_si_units(units):
    """Unconfirmed native units cannot be scaled as tonnes and kN."""
    from types import SimpleNamespace

    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner

    native = SimpleNamespace(
        UnitsSystem=units,
        hydrostaticResults=[dict(
            restoringMatrix=np.eye(6), volume=10.0, mass=10.0,
            centreOfMass=[0.0, 0.0, 0.0], centreOfBuoyancy=[0.0, 0.0, 0.0],
            Awp=7.0,
        )],
    )
    assert OrcaWaveRunner._extract_hydrostatics(native, "synthetic") is None


def test_native_force_conversion_accepts_scalar():
    """A synthetic 1.25 kN force corresponds to 1250 N."""
    from digitalmodel.hydrodynamics.diffraction.diffraction_units import kn_to_n

    assert kn_to_n(1.25) == 1250.0


def test_native_force_conversion_preserves_array_input():
    """Signed coupling entries retain their shape and native input values."""
    from digitalmodel.hydrodynamics.diffraction.diffraction_units import kn_to_n

    native = np.array([[0.0, 1.25], [-2.0, 3.0]])
    original = native.copy()
    converted = kn_to_n(native)

    np.testing.assert_array_equal(converted, [[0.0, 1250.0], [-2000.0, 3000.0]])
    np.testing.assert_array_equal(native, original)
