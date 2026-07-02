"""Validation tests for the wave-excited floating body (#1302).

ALWAYS (no OpenFOAM): the response helpers recover synthetic signals, the
hydrostatics/long-wave premise holds for the default config, and the case
builder produces a structurally valid two-mesh overset case.

CONDITIONALLY (solver-capable host AND DIGITALMODEL_RUN_LONG_CFD=1): the
fast variant is solved end-to-end via two OpenFOAMRunner invocations
(component mesh prep, then mergeMeshes -> topoSet -> setFields ->
overInterDyMFoam; ~2.5 h serial) and the heave RAO / draft / periodicity
gates are asserted.
"""

from __future__ import annotations

import json
import math
import os

import pytest

from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)
from digitalmodel.solvers.openfoam.validation import (
    DRAFT_TOLERANCE,
    PERIOD_TOLERANCE,
    RAO_TOLERANCE,
    WAVE_EXCITED_BODY_CASE,
    WaveExcitedBodyConfig,
    analyze_excited_heave,
    build_wave_excited_body_case,
    extract_heave_from_log,
    harmonic_amplitude,
    incident_wave_split,
    response_period,
)

from .conftest import solver_capable


# --------------------------------------------------------------------------- #
#  Response helpers — always run                                              #
# --------------------------------------------------------------------------- #


def test_harmonic_amplitude_recovers_synthetic() -> None:
    import numpy as np

    t = np.linspace(0.0, 12.0, 1200)
    y = 0.42 + 0.031 * np.cos(2 * math.pi * t / 3.0 - 0.7)
    mean, amp, _ = harmonic_amplitude(t, y, 3.0)
    assert mean == pytest.approx(0.42, abs=1e-6)
    assert amp == pytest.approx(0.031, rel=1e-6)


def test_harmonic_amplitude_rejects_bad_input() -> None:
    with pytest.raises(ValueError):
        harmonic_amplitude([0, 1, 2, 3], [0, 1, 2, 3], 0.0)
    with pytest.raises(ValueError):
        harmonic_amplitude([0, 1], [0, 1], 3.0)


def test_response_period_zero_upcrossings() -> None:
    import numpy as np

    t = np.linspace(0.0, 15.0, 3000)
    y = 5.0 + np.sin(2 * math.pi * t / 2.5)
    assert response_period(t, y) == pytest.approx(2.5, rel=1e-3)
    # constant signal -> no upcrossings -> NaN
    assert math.isnan(response_period(t, np.ones_like(t)))


def test_analyze_excited_heave_synthetic_wave_follower() -> None:
    import numpy as np

    cfg = WaveExcitedBodyConfig()
    a_i = cfg.wave_height / 2.0
    t = np.linspace(0.0, cfg.end_time, 3000)
    z = cfg.equilibrium_com_height + a_i * np.cos(
        2 * math.pi * t / cfg.wave_period
    )
    res = analyze_excited_heave(t.tolist(), z.tolist(), a_i, cfg)
    assert res["rao"] == pytest.approx(1.0, abs=1e-6)
    assert abs(res["draft_error"]) < 1e-6
    assert res["period_measured"] == pytest.approx(cfg.wave_period, rel=1e-3)
    assert all(res["gates"].values())


def test_analyze_excited_heave_rejects_bad_incident() -> None:
    with pytest.raises(ValueError):
        analyze_excited_heave([0, 1], [0, 1], 0.0)


def _write_synthetic_gauges(case_dir, cfg, corrupt_x=None) -> None:
    """Synthetic interfaceHeight output: unit incident wave at every gauge,
    optionally one gauge offset + doubled (the on-face ray artifact)."""
    import numpy as np

    a = cfg.wave_height / 2.0
    om = 2 * math.pi / cfg.wave_period
    k = cfg.wavenumber
    t = np.arange(cfg.steady_window[0], cfg.steady_window[1], 0.02)
    cols = [t]
    for xg in cfg.gauges_x:
        lvl = cfg.depth + a * np.cos(om * t - k * xg)
        if corrupt_x is not None and xg == corrupt_x:
            lvl = cfg.depth + 0.08 + 2 * a * np.cos(om * t - k * xg)
        cols.extend([lvl, lvl])  # FO writes two identical-format columns
    out = case_dir / "postProcessing" / "waveGauges" / "0"
    out.mkdir(parents=True)
    np.savetxt(out / "height.dat", np.column_stack(cols))


def test_incident_wave_split_recovers_synthetic_amplitude(tmp_path) -> None:
    cfg = WaveExcitedBodyConfig()
    _write_synthetic_gauges(tmp_path, cfg)
    res = incident_wave_split(tmp_path, cfg)
    assert res["incident_amplitude"] == pytest.approx(cfg.wave_height / 2, rel=0.02)
    assert res["reflection_kr"] < 0.05
    assert res["excluded_gauges_x"] == []
    assert res["k_error"] == pytest.approx(0.0, abs=0.02)


def test_incident_wave_split_excludes_on_face_gauge_artifact(tmp_path) -> None:
    # A gauge ray bit-exactly on a cell face double-counts cell columns and
    # reports an impossible mean level; the split must exclude it, not
    # average it in (observed on the x = 8.8 m gauge at two resolutions).
    cfg = WaveExcitedBodyConfig()
    _write_synthetic_gauges(tmp_path, cfg, corrupt_x=8.8)
    res = incident_wave_split(tmp_path, cfg)
    assert res["excluded_gauges_x"] == [8.8]
    assert res["incident_amplitude"] == pytest.approx(cfg.wave_height / 2, rel=0.02)
    bad = [g for g in res["gauges"] if g["x"] == 8.8][0]
    assert not bad["healthy"]


def test_incident_wave_split_needs_three_healthy_gauges(tmp_path) -> None:
    cfg = WaveExcitedBodyConfig(gauges_x=(8.0, 8.8, 9.6))
    _write_synthetic_gauges(tmp_path, cfg, corrupt_x=8.8)
    with pytest.raises(RuntimeError):
        incident_wave_split(tmp_path, cfg)


# --------------------------------------------------------------------------- #
#  Long-wave premise + hydrostatics — always run                              #
# --------------------------------------------------------------------------- #


def test_long_wave_premise_holds() -> None:
    cfg = WaveExcitedBodyConfig()
    # lambda/B ~ 29 and T >> heave natural period: the wave-follower limit.
    assert cfg.lambda_over_beam > 20.0
    assert cfg.wave_period > 3.0 * cfg.heave_natural_period
    assert cfg.wavelength == pytest.approx(5.77, rel=0.01)


def test_body_floats_at_half_height() -> None:
    cfg = WaveExcitedBodyConfig()
    assert cfg.mass == pytest.approx(500.0 * 0.2 * 0.04 * 0.2)
    assert cfg.draft == pytest.approx(cfg.body_height / 2.0, rel=1e-9)
    # CoM starts exactly at the waterline (no decay transient)
    assert cfg.centre_of_mass[2] == pytest.approx(cfg.depth, rel=1e-9)


def test_geometry_stays_interior_and_face_aligned() -> None:
    cfg = WaveExcitedBodyConfig()
    bx0, bx1, bz0, bz1 = cfg.body_extent
    cx0, cx1, cz0, cz1 = cfg.comp_extent
    # component strictly inside the tank (fringe needs interior donors)
    assert 0.0 < cx0 < cx1 < cfg.tank_length
    assert 0.0 < cz0 < cz1 < cfg.tank_height
    # body strictly inside the component block
    assert cx0 < bx0 < bx1 < cx1
    assert cz0 < bz0 < bz1 < cz1
    # waterline on a background cell face (VOF lesson from #1165)
    dz = cfg.tank_height / cfg.nz
    assert cfg.depth / dz == pytest.approx(round(cfg.depth / dz), abs=1e-9)
    # body edges on component cell faces
    cdx = (cx1 - cx0) / cfg.comp_nx
    cdz = (cz1 - cz0) / cfg.comp_nz
    for v, d, o in ((bx0, cdx, cx0), (bx1, cdx, cx0), (bz0, cdz, cz0), (bz1, cdz, cz0)):
        assert (v - o) / d == pytest.approx(round((v - o) / d), abs=1e-9)


def test_gauges_upstream_of_component_and_wavemaker_clear() -> None:
    cfg = WaveExcitedBodyConfig()
    cx0 = cfg.comp_extent[0]
    split_gauges = [x for x in cfg.gauges_x if 7.9 <= x <= 12.9]
    assert len(split_gauges) >= 3
    assert all(x < cx0 for x in split_gauges)          # clear of the body
    assert all(x >= cfg.wavelength for x in split_gauges)  # clear of paddle


def test_validation_case_registered() -> None:
    assert WAVE_EXCITED_BODY_CASE.metadata["issue"] == "#1302"
    assert WAVE_EXCITED_BODY_CASE.reference["heave_rao"] == 1.0
    assert WAVE_EXCITED_BODY_CASE.tolerance == RAO_TOLERANCE == 0.15


# --------------------------------------------------------------------------- #
#  Case builder — always run (no OpenFOAM needed)                             #
# --------------------------------------------------------------------------- #


def test_build_produces_two_mesh_overset_case(tmp_path) -> None:
    root = build_wave_excited_body_case(WaveExcitedBodyConfig(), parent_dir=tmp_path)
    bg, body = root / "background", root / "body"
    for case in (bg, body):
        for sub in ("system", "constant", "0"):
            assert (case / sub).is_dir()
        assert (case / "system" / "blockMeshDict").is_file()
        assert (case / "system" / "controlDict").is_file()
        assert (case / "system" / "topoSetDict").is_file()

    control = (bg / "system" / "controlDict").read_text()
    assert "overInterDyMFoam" in control
    assert "interfaceHeight" in control
    # sixDoFRigidBodyState FATALs under dynamicOversetFvMesh — must NOT be used
    assert "type            sixDoFRigidBodyState" not in control

    dyn = (bg / "constant" / "dynamicMeshDict").read_text()
    assert "dynamicOversetFvMesh" in dyn
    assert "sixDoFRigidBodyMotionConstraint line" in dyn  # heave-only
    assert "innerDistance   100" in dyn                   # rigid component

    schemes = (bg / "system" / "fvSchemes").read_text()
    assert "oversetInterpolation" in schemes
    assert "inverseDistance" in schemes

    # dummy oversetPatch must be the FIRST background patch
    mesh = (bg / "system" / "blockMeshDict").read_text()
    assert mesh.index("oversetPatch") < mesh.index("inlet")

    for f in ("U", "alpha.water", "p_rgh", "pointDisplacement", "zoneID"):
        assert (bg / "0" / f).is_file(), f"missing 0/{f}"
    assert "waveModel       StokesII" in (bg / "constant" / "waveProperties").read_text()


def test_build_stamps_provenance_with_citations(tmp_path) -> None:
    root = build_wave_excited_body_case(WaveExcitedBodyConfig(), parent_dir=tmp_path)
    prov = json.loads((root / "provenance.json").read_text())
    assert prov["issue"] == "#1302"
    assert any("overInterDyMFoam" in c for c in prov["citations"])
    assert any("Newman" in c for c in prov["citations"])
    assert prov["wave"]["lambda_over_beam"] > 20.0
    assert prov["body"]["draft_m"] == pytest.approx(0.1, rel=1e-6)


# --------------------------------------------------------------------------- #
#  Solve assertion — solver-capable hosts, long-CFD opt-in                    #
# --------------------------------------------------------------------------- #


def test_wave_excited_body_solve_matches_long_wave_limit(tmp_path) -> None:
    """End-to-end: component mesh prep, then mergeMeshes -> topoSet ->
    setFields -> overInterDyMFoam (~2.5 h serial), then gate heave RAO,
    draft and periodicity. Long solve — opt-in via DIGITALMODEL_RUN_LONG_CFD=1
    on top of the solver gate (same tier as the Kleefsman impact)."""
    if not solver_capable(tmp_path):
        pytest.skip("OpenFOAM not available — run gated to solver-capable hosts")
    if os.environ.get("DIGITALMODEL_RUN_LONG_CFD") != "1":
        pytest.skip("long CFD solve — set DIGITALMODEL_RUN_LONG_CFD=1 to run")

    cfg = WaveExcitedBodyConfig()
    root = build_wave_excited_body_case(cfg, parent_dir=tmp_path)

    prep = OpenFOAMRunner(OpenFOAMRunConfig(
        run_topo_set=True,
        subset_mesh_set="c0",
        subset_mesh_patch="floatingObject",
        run_solver=False,
    ))
    prep_result = prep.run(root / "body")
    assert prep_result.status == OpenFOAMRunStatus.COMPLETED, prep_result.error_message

    runner = OpenFOAMRunner(OpenFOAMRunConfig(
        merge_meshes_source="../body",
        run_topo_set=True,
        run_set_fields=True,
        to_vtk=False,
        timeout_seconds=6 * 3600,
    ))
    result = runner.run(root / "background")
    assert result.status == OpenFOAMRunStatus.COMPLETED, result.error_message
    names = [s.name for s in result.stages]
    assert names == ["blockMesh", "mergeMeshes", "topoSet", "setFields",
                     "overInterDyMFoam"]

    # incident amplitude from the broken-gauge-safe upstream split
    quality = incident_wave_split(root / "background", cfg)
    a_i = quality["incident_amplitude"]
    assert a_i > 0.0

    t, z = extract_heave_from_log(root / "background")
    res = analyze_excited_heave(t, z, a_i, cfg)
    assert abs(res["rao"] - 1.0) <= RAO_TOLERANCE, (
        f"heave RAO {res['rao']:.3f} outside 1.0 +/- {RAO_TOLERANCE:.0%} "
        f"(amp {res['heave_amplitude']:.4f} m vs incident {a_i:.4f} m)"
    )
    assert abs(res["draft_error"]) <= DRAFT_TOLERANCE, (
        f"mean draft error {res['draft_error']:.1%} exceeds {DRAFT_TOLERANCE:.0%}"
    )
    assert abs(res["period_error"]) <= PERIOD_TOLERANCE, (
        f"response period {res['period_measured']:.3f} s not at the wave "
        f"period {cfg.wave_period} s within {PERIOD_TOLERANCE:.0%}"
    )
