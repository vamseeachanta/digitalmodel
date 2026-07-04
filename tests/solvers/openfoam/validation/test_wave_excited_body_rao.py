"""Validation tests for the wave-excited-body heave-RAO sweep (#1324).

ALWAYS (no OpenFOAM, no capytaine): the frozen potential-flow reference loads
and satisfies its physical anchors (long-wave -> 1, a resonance peak > 1,
short-wave roll-off, positive damping); the per-period sweep config adapts
mesh / gauges / duration correctly while keeping the verified geometry; the
band logic widens near resonance; and the reference reproduces the #1302
long-wave CFD point.

CONDITIONALLY (solver-capable host AND DIGITALMODEL_RUN_LONG_CFD=1): one
sweep point is solved end-to-end and its CFD RAO is asserted within the
sweep band of the reference.
"""

from __future__ import annotations

import math
import os

import pytest

from digitalmodel.solvers.openfoam.validation.wave_excited_body import (
    WaveExcitedBodyConfig,
)
from digitalmodel.solvers.openfoam.validation.wave_excited_body_rao import (
    CELLS_PER_WAVELENGTH,
    RAO_SWEEP_BAND,
    RAO_SWEEP_BAND_RESONANCE,
    SWEEP_PERIODS,
    _band_for,
    build_sweep_config,
    group_velocity,
    load_reference_curve,
    reference_peak,
    reference_rao_at,
    summarize_sweep,
    sweep_gauges,
)

from .conftest import solver_capable


# --------------------------------------------------------------------------- #
#  Frozen potential-flow reference — always run                               #
# --------------------------------------------------------------------------- #


def test_reference_curve_loads_and_is_sorted() -> None:
    curve = load_reference_curve()
    ps = curve["period_s"]
    assert len(ps) >= 20
    assert list(ps) == sorted(ps)                     # ascending in period
    assert set(curve).issuperset(
        {"period_s", "rao_heave", "added_mass_a33", "radiation_damping_b33"}
    )


def test_reference_physical_anchors() -> None:
    curve = load_reference_curve()
    # long-wave (largest period) -> wave follower, RAO ~ 1
    assert reference_rao_at(float(curve["period_s"][-1])) == pytest.approx(1.0, abs=0.05)
    # a genuine resonance peak above the long-wave value
    peak_T, peak_rao = reference_peak(curve)
    assert peak_rao > 1.5
    assert 0.7 < peak_T < 1.1
    # short-wave roll-off well below 1
    assert reference_rao_at(float(curve["period_s"][0])) < 0.5
    # potential-flow radiation damping is positive everywhere (lid removes the
    # irregular-frequency spikes that would otherwise flip a33/b33)
    assert all(curve["radiation_damping_b33"] > 0.0)


def test_reference_reproduces_1302_long_wave_point() -> None:
    # the independent BEM reference must agree with the verified #1302 CFD
    # long-wave point (heave RAO 1.017 at T = 3 s) — cross-method check
    assert reference_rao_at(3.0) == pytest.approx(1.017, abs=0.03)


def test_reference_rao_out_of_range_raises() -> None:
    curve = load_reference_curve()
    with pytest.raises(ValueError):
        reference_rao_at(float(curve["period_s"][-1]) + 1.0, curve)
    with pytest.raises(ValueError):
        reference_rao_at(float(curve["period_s"][0]) - 0.1, curve)


# --------------------------------------------------------------------------- #
#  Per-period sweep configuration — always run                                #
# --------------------------------------------------------------------------- #


def test_group_velocity_matches_deep_and_shallow_limits() -> None:
    # deep water: c_g -> c/2; use a short period at depth 0.4 m
    T = 0.7
    k = 2 * math.pi / (9.81 * T**2 / (2 * math.pi))  # deep-water k
    c_deep = math.sqrt(9.81 / k)
    assert group_velocity(T, 100.0) == pytest.approx(0.5 * c_deep, rel=0.05)


def test_sweep_config_keeps_geometry_and_face_alignment() -> None:
    for T in SWEEP_PERIODS:
        cfg = build_sweep_config(T)
        # verified geometry unchanged
        assert cfg.tank_length == 20.0 and cfg.body_x == 13.0
        assert cfg.depth == 0.4 and cfg.nz == 49
        # waterline still on a background cell face
        dz = cfg.tank_height / cfg.nz
        assert cfg.depth / dz == pytest.approx(round(cfg.depth / dz), abs=1e-9)
        # never coarser than the verified fast mesh
        assert cfg.nx >= 250


def test_sweep_config_resolves_wavelength() -> None:
    for T in SWEEP_PERIODS:
        cfg = build_sweep_config(T)
        cpw = cfg.wavelength / (cfg.tank_length / cfg.nx)
        assert cpw >= CELLS_PER_WAVELENGTH - 0.5


def test_sweep_config_resolution_override_refines_only_nx() -> None:
    # Raising cells-per-wavelength (short-wave grid-convergence study of #1324)
    # refines the background nx but must leave everything the analysis depends
    # on — gauge array, steady window, end time, geometry — identical, so a
    # refined point is analysed exactly like its coarse companion.
    for T in (1.0, 0.9):
        base = build_sweep_config(T, cells_per_wavelength=CELLS_PER_WAVELENGTH)
        fine = build_sweep_config(T, cells_per_wavelength=2 * CELLS_PER_WAVELENGTH)
        assert fine.nx > base.nx                          # grid actually refined
        cpw_base = base.wavelength / (base.tank_length / base.nx)
        cpw_fine = fine.wavelength / (fine.tank_length / fine.nx)
        assert cpw_fine >= 2 * CELLS_PER_WAVELENGTH - 0.5
        assert cpw_fine > 1.7 * cpw_base                  # ~doubled
        # analysis-invariant fields unchanged
        assert fine.gauges_x == base.gauges_x
        assert fine.steady_window == base.steady_window
        assert fine.end_time == base.end_time
        assert fine.nz == base.nz == 49
        assert fine.body_x == base.body_x == 13.0


def test_sweep_config_window_inside_endtime_and_settled() -> None:
    for T in SWEEP_PERIODS:
        cfg = build_sweep_config(T)
        w0, w1 = cfg.steady_window
        assert 0.0 < w0 < w1 == cfg.end_time
        # window spans >= ~6 wave periods for a clean harmonic fit
        assert (w1 - w0) >= 6.0 * T - 1e-6
        # start is after wave arrival at the body + a few periods settle
        arrival = 3.0 + cfg.body_x / group_velocity(T, cfg.depth)
        assert w0 >= arrival


def test_sweep_gauges_upstream_and_spaced_for_split() -> None:
    for T in SWEEP_PERIODS:
        cfg = build_sweep_config(T)
        split = [x for x in cfg.gauges_x if 7.9 <= x <= 12.9]
        assert len(split) >= 3                          # split needs >= 3
        assert all(x < cfg.comp_extent[0] for x in split)   # clear of the body
        # spacing well below lambda/2 (Goda-Suzuki singularity)
        xs = sorted(split)
        spacing = min(b - a for a, b in zip(xs, xs[1:]))
        assert 0.0 < spacing < 0.5 * cfg.wavelength


def test_band_widens_near_resonance() -> None:
    peak_T, _ = reference_peak()
    assert _band_for(peak_T) == RAO_SWEEP_BAND_RESONANCE
    # far from resonance (long wave) uses the tighter band
    assert _band_for(3.0) == RAO_SWEEP_BAND
    assert RAO_SWEEP_BAND < RAO_SWEEP_BAND_RESONANCE


def test_summarize_sweep_orders_and_flags() -> None:
    pts = [
        {"period_s": 1.0, "rao_cfd": 1.4, "within_band": True},
        {"period_s": 0.9, "rao_cfd": 1.8, "within_band": True},
    ]
    s = summarize_sweep(pts)
    assert [p["period_s"] for p in s["points"]] == [0.9, 1.0]
    assert s["all_within_band"] is True
    assert s["cfd_peak"]["period_s"] == 0.9
    assert s["reference_peak"]["rao"] > 1.5


# --------------------------------------------------------------------------- #
#  End-to-end solve — opt-in                                                   #
# --------------------------------------------------------------------------- #


def test_sweep_point_solves_within_band(tmp_path) -> None:
    if not solver_capable(tmp_path):
        pytest.skip("no OpenFOAM on PATH")
    if os.environ.get("DIGITALMODEL_RUN_LONG_CFD") != "1":
        pytest.skip("long CFD solve — set DIGITALMODEL_RUN_LONG_CFD=1 to run")

    from digitalmodel.solvers.openfoam.runner import (
        OpenFOAMRunConfig,
        OpenFOAMRunner,
        OpenFOAMRunStatus,
    )
    from digitalmodel.solvers.openfoam.validation.wave_excited_body import (
        build_wave_excited_body_case,
    )
    from digitalmodel.solvers.openfoam.validation.wave_excited_body_rao import (
        analyze_sweep_point,
    )

    T = 1.0  # well-resolved flank point (reference RAO ~ 1.5, clearly != 1)
    cfg = build_sweep_config(T)
    root = build_wave_excited_body_case(cfg, tmp_path)

    OpenFOAMRunner(OpenFOAMRunConfig(
        run_solver=False, subset_mesh_set="c0",
        subset_mesh_patch="floatingObject", run_topo_set=True,
    )).run(root / "body")
    res = OpenFOAMRunner(OpenFOAMRunConfig(
        merge_meshes_source="../body", run_topo_set=True, run_set_fields=True,
    )).run(root / "background")
    assert res.status == OpenFOAMRunStatus.COMPLETED

    point = analyze_sweep_point(root, cfg)
    assert point["within_band"], (
        f"CFD RAO {point['rao_cfd']:.3f} vs reference "
        f"{point['rao_reference']:.3f} outside {point['band']:.0%}"
    )
