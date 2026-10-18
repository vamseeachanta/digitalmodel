"""One executable node per C-check; offline cases never qualify native output."""

from hashlib import sha256
import numpy as np
import pytest
from digitalmodel.orcawave.panel_mesh import (
    compute_panel_quality,
    compute_displaced_volume,
    read_gdf,
    write_gdf,
)
from digitalmodel.orcawave.rao_processing import RAOTable, read_rao_csv
from digitalmodel.orcawave.hydro_coefficients import (
    from_native_added_mass,
    HydroCoefficients,
    HydroMatrix6x6,
    create_hydrostatic_restoring,
)
from digitalmodel.orcawave.drift_forces import compute_wind_current_drift
from digitalmodel.orcawave.vessel_database import generate_parametric_hull
from digitalmodel.orcawave.qa_gates import (
    native_gate_record,
    require_native_gate,
    implementation_map,
    check_balance,
    compute_buoyancy_centroid,
    check_mesh_resolution,
    check_waterline,
    check_normals,
    check_hydrodynamics,
    build_trace,
)
from tests.orcawave.test_audit_p1 import box


def test_C0_CSV():
    table = read_rao_csv(
        "DOF,Heave\nUnit,m/m\nPeriod\\Heading,0,90\n5,.2/10,.3/15\n10,.8/-30,1/-25\n"
    )
    assert table.periods == [5, 10] and table.headings_deg == [0, 90]
    np.testing.assert_allclose(table.amplitudes, [[0.2, 0.8], [0.3, 1]], atol=1e-6)


def test_C0_GDF():
    parsed = read_gdf(write_gdf(box()))
    assert len(parsed.panels) == 6 and parsed.ulen == 1
    assert compute_displaced_volume(parsed) == 8


def test_C0_COEFF():
    matrices = from_native_added_mass("0.6283185307179586 3 3 8200000\n")
    assert len(matrices) == 1
    assert matrices[0].to_numpy()[2, 2] == 8200000
    assert matrices[0].frequency_rad_s == pytest.approx(2 * np.pi / 10)


def test_C0_MAP():
    mapping = implementation_map()
    required = {
        "panel_mesh.compute_mesh_summary",
        "panel_mesh.read_gdf",
        "wave_spectrum.generate_spectrum",
        "motion_statistics.long_term_extreme",
        "motion_statistics.motion_sickness_incidence",
        "hydro_coefficients.create_hydrostatic_restoring",
        "drift_forces.compute_wind_current_drift",
        "rao_processing.compare_raos",
        "vessel_database.get_representative_raos",
        "qa_gates.build_trace",
    }
    assert required <= mapping.keys()
    assert mapping["panel_mesh.compute_mesh_summary"] == "panel_mesh.py"
    # Every mapped path is a direct-file producer, never a native binary.
    assert all(path.endswith(".py") for path in mapping.values())


def test_C1_OW():
    assert native_gate_record("C1-OW")["status"] == "NOT_RUN"
    with pytest.raises(RuntimeError, match="NOT_RUN"):
        require_native_gate("C1-OW")


def test_C1_AQWA():
    assert native_gate_record("C1-AQWA")["status"] == "NOT_RUN"
    with pytest.raises(RuntimeError, match="NOT_RUN"):
        require_native_gate("C1-AQWA")


def test_C4_AREA():
    assert compute_panel_quality(box().panels[0]).area == pytest.approx(4, abs=0.01)


def test_C4_ASPECT():
    assert compute_panel_quality(box().panels[0]).aspect_ratio == pytest.approx(
        1, abs=0.01
    )


def test_C4_SKEW():
    assert compute_panel_quality(box().panels[0]).skewness == pytest.approx(0, abs=0.01)


def test_C4_VOLUME():
    assert compute_displaced_volume(box()) == pytest.approx(8, rel=0.005)


def test_C4_BALANCE():
    assert check_balance(box(), 8200) == pytest.approx(0)
    with pytest.raises(ValueError, match="balance"):
        check_balance(box(), 9000)


def test_C4_FRAME():
    result = compute_buoyancy_centroid(box())
    assert result["frame"] == "x_forward_y_port_z_up"
    np.testing.assert_allclose(result["centre_of_buoyancy_m"], [0, 1, -1], atol=1e-12)
    c = np.asarray(create_hydrostatic_restoring(8, 4, 2, 100))
    assert c[3, 3] / (1025 * 9.81 * 8) == pytest.approx(2, rel=1e-12)
    assert c[4, 4] / (1025 * 9.81 * 8) == pytest.approx(100, rel=1e-12)
    # Shifted coordinates independently exercise the first moment integrals.
    mesh = box()
    for p in mesh.panels:
        p.vertices = [[x + 3, y - 2, z] for x, y, z in p.vertices]
    np.testing.assert_allclose(
        compute_buoyancy_centroid(mesh)["centre_of_buoyancy_m"], [3, -1, -1], atol=1e-12
    )


def test_C4_MESH():
    assert check_mesh_resolution(box(), 12, 6) == {"max_edge_m": 2.0, "panel_count": 6}
    for wavelength, limit in [(11, 6), (12, 5)]:
        with pytest.raises(ValueError):
            check_mesh_resolution(box(), wavelength, limit)


def test_C4_WATERLINE():
    result = check_waterline(box(lid=False), 2)
    assert result["closure_gap_m"] == 0 and result["waterplane_area_m2"] == 4
    with pytest.raises(ValueError, match="draft"):
        check_waterline(box(lid=False), 2.1)
    mesh = box(lid=False)
    mesh.panels.pop()
    with pytest.raises(ValueError, match="closure"):
        check_waterline(mesh, 2)


def test_C4_NORMALS():
    assert check_normals(box()) == {"signed_volume_m3": 8.0, "inconsistent_edges": 0}
    with pytest.raises(ValueError, match="signed volume"):
        check_normals(box(inward=True))
    mesh = box()
    mesh.panels[1].vertices.reverse()
    with pytest.raises(ValueError, match="normals"):
        check_normals(mesh)


def test_C4_IRREGULAR():
    # This check needs native pressure/spike evidence; offline status is NOT_RUN.
    assert native_gate_record("C4-IRREGULAR")["status"] == "NOT_RUN"
    with pytest.raises(RuntimeError, match="NOT_RUN"):
        require_native_gate("C4-IRREGULAR")


def test_C4_HYDRO():
    def matrix(value):
        return HydroMatrix6x6(frequency_rad_s=1, values=(np.eye(6) * value).tolist())

    coeffs = HydroCoefficients(added_mass=[matrix(1)], radiation_damping=[matrix(1)])
    assert check_hydrodynamics(coeffs, _heave_table())["status"] == "PASS"
    coeffs.radiation_damping = [matrix(-1)]
    with pytest.raises(ValueError, match="damping"):
        check_hydrodynamics(coeffs, _heave_table())
    coeffs.radiation_damping = [matrix(1)]
    with pytest.raises(ValueError, match="heave"):
        check_hydrodynamics(coeffs, _heave_table(0.9))


def test_C4_WIND():
    r = compute_wind_current_drift(20, 1, 90)
    assert r.wind_force_sway_kn == pytest.approx(588, rel=1e-9)
    assert r.current_force_sway_kn == pytest.approx(922.5, rel=1e-9)
    assert r.total_yaw_knm == pytest.approx(37762.5, rel=1e-9)


def test_C4_HULL():
    hull = generate_parametric_hull("Barge", 100, 30, 5)
    assert hull.waterline_offsets[10][1] == pytest.approx(15, rel=0.01)


def test_C5_TRACE():
    quantities = {
        "volume": {
            "producer": "panel_mesh.compute_displaced_volume",
            "value": compute_displaced_volume(box()),
            "unit": "m3",
        }
    }
    result = build_trace(
        b"fixture", quantities, {"C4-VOLUME": "PASS"}, "offline only", "maintainer",
        required_quantities={"volume"}
    )
    assert result["input_sha256"] == sha256(b"fixture").hexdigest()
    assert result["coverage_percent"] == 100
    assert result["quantities"]["volume"]["value"] == 8
    assert result["maintainer_signature"] == "maintainer"
    with pytest.raises(ValueError, match="Missing trace"):
        build_trace(b"fixture", quantities, {}, "offline only", "maintainer",
                    required_quantities={"volume"})


def _heave_table(longest=1.0):
    return RAOTable(
        dof="Heave", unit="m/m", periods=[30, 5, 15], headings_deg=[0, 90],
        amplitudes=[[longest, 0.2, 0.8], [1.0, 0.3, 0.9]],
        phases_deg=[[0, 0, 0], [0, 0, 0]],
    )


@pytest.mark.parametrize("family", ["added_mass", "radiation_damping"])
@pytest.mark.parametrize("asymmetry,passes", [(0.01, True), (0.03, False)])
def test_hydrodynamics_applies_gate_reciprocity_tolerance(asymmetry, passes, family):
    values = np.eye(6)
    values[0, 1], values[1, 0] = 1, 1 + asymmetry
    symmetric = HydroMatrix6x6(frequency_rad_s=1, values=np.eye(6).tolist())
    coeffs = HydroCoefficients(added_mass=[symmetric], radiation_damping=[symmetric])
    setattr(coeffs, family, [HydroMatrix6x6(frequency_rad_s=1, values=values.tolist())])
    if passes:
        assert check_hydrodynamics(coeffs, _heave_table())["status"] == "PASS"
    else:
        with pytest.raises(ValueError, match="asymmetry"):
            check_hydrodynamics(coeffs, _heave_table())


def test_hydrodynamics_derives_long_wave_heave_from_all_headings():
    matrix = HydroMatrix6x6(frequency_rad_s=1, values=np.eye(6).tolist())
    coeffs = HydroCoefficients(added_mass=[matrix], radiation_damping=[matrix])
    result = check_hydrodynamics(coeffs, _heave_table())
    assert result["heave_period_s"] == 30
    assert result["heave_m_per_m"] == 1
    table = _heave_table()
    table.amplitudes[1][0] = 0.9
    with pytest.raises(ValueError, match="heave"):
        check_hydrodynamics(coeffs, table)


@pytest.mark.parametrize("invalid", ["scalar", "dof", "unit", "shape", "nonfinite"])
def test_hydrodynamics_rejects_unqualified_heave_evidence(invalid):
    matrix = HydroMatrix6x6(frequency_rad_s=1, values=np.eye(6).tolist())
    coeffs = HydroCoefficients(added_mass=[matrix], radiation_damping=[matrix])
    table = _heave_table()
    if invalid == "scalar":
        table = 1.0
    elif invalid == "dof":
        table.dof = "Roll"
    elif invalid == "unit":
        table.unit = "m"
    elif invalid == "shape":
        table.amplitudes[0].pop()
    else:
        table.amplitudes[0][0] = float("nan")
    with pytest.raises(ValueError, match="heave"):
        check_hydrodynamics(coeffs, table)


def test_trace_rejects_incomplete_required_quantity_coverage():
    quantities = {
        "volume": {"producer": "panel_mesh.compute_displaced_volume", "value": 8, "unit": "m3"},
        "extra": {"producer": "panel_mesh.compute_displaced_volume", "value": 8, "unit": "m3"},
    }
    with pytest.raises(ValueError, match="50.0 percent.*mass"):
        build_trace(b"fixture", quantities, {"C4-VOLUME": "PASS"}, "offline only", "maintainer",
                    required_quantities={"volume", "mass"})


def test_trace_requires_independent_coverage_requirements():
    quantity = {"volume": {"producer": "panel_mesh.compute_displaced_volume", "value": 8, "unit": "m3"}}
    with pytest.raises(ValueError, match="required quantities"):
        build_trace(b"fixture", quantity, {"C4-VOLUME": "PASS"}, "offline only", "maintainer",
                    required_quantities=set())
