"""Referent-free resistance scoring (#2023) - what is left when the benchmark goes.

These tests are written against a hull that has NO published Ct. Every assertion
here is chosen so that it CANNOT be satisfied by quietly reintroducing a
referent, and so that no verdict in the manifest can be read as a validation.

The load-bearing tests in this file are:

* :func:`test_referent_free_run_never_loads_a_referent` - the evaluation must
  complete with ``load_referent`` sabotaged. If it passes only because a fixture
  happens to be on disk, the mode does not exist.
* :func:`test_manifest_carries_no_aggregate_pass_and_no_validation_claim` - the
  KCS manifest ends in ``all_passed``. This one must not, because there is
  nothing to have passed.
* :func:`test_two_sample_window_is_inadequate_and_says_so_loudly` - the
  committed KCS artifact reports ``averaging_window: 2``. A two-point mean is
  not a converged average and a two-point "standard deviation" is not a
  statistic; both must be stated conditions, not silent ones.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.solvers.openfoam.validation.referent_free_resistance import (
    AVERAGING_WINDOW_MIN_SAMPLES,
    FORM_FACTOR_BAND,
    MIN_OSCILLATION_PERIODS,
    MIN_SAMPLES_FOR_A_STATISTIC,
    ROACHE_FS_THREE_GRID,
    ROACHE_FS_TWO_GRID,
    SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT,
    SAMPLED_PEAK_DEFICIT_BUDGET,
    GridLevel,
    NormalisationArea,
    ReferentFreeConfig,
    averaging_window_adequacy,
    evaluate_referent_free_run,
    grid_uncertainty,
    iterative_uncertainty,
    plausibility_report,
)

# --------------------------------------------------------------------------- #
#  A hull with no benchmark. The numbers are the KCS model-scale CONDITION
#  (a real Reynolds number and a real speed are needed for ITTC-57 to mean
#  anything) but NO published coefficient is used anywhere in this file.
# --------------------------------------------------------------------------- #

MESH_DERIVED_AREA = NormalisationArea(
    value_m2=9.5609,
    provenance="mesh_derived",
    source="wetted area integrated over the emitted hull surface at the DWL",
)


def _config(**kw) -> ReferentFreeConfig:
    base = dict(
        name="hull_under_test",
        lpp=7.2786,
        velocity=2.1962,
        reynolds=1.4e7,
        density=998.8,
        wetted_surface=MESH_DERIVED_AREA,
        averaging_window=4000,
    )
    base.update(kw)
    return ReferentFreeConfig(**base)


def _write_force_dat(path, *, ct, cp, cv, config, times):
    """A forces log that reduces to the requested coefficients.

    Written half-domain and sign-negative, exactly as the solver reports it, so
    the parser's doubling and sign handling are exercised rather than bypassed.
    """
    q = 0.5 * config.density * config.wetted_surface.value_m2 * config.velocity**2
    lines = [
        "# Force",
        "# Time \ttotal_x total_y total_z\tpressure_x pressure_y pressure_z"
        "\tviscous_x viscous_y viscous_z",
    ]
    for t in times:
        lines.append(
            f"{t} {-ct * q / 2:.9e} 0.0 0.0 "
            f"{-cp * q / 2:.9e} 0.0 0.0 "
            f"{-cv * q / 2:.9e} 0.0 0.0"
        )
    path.write_text("\n".join(lines) + "\n")
    return path


# --------------------------------------------------------------------------- #
#  The averaging window - a stated parameter with a DERIVED minimum
# --------------------------------------------------------------------------- #

def test_window_minimum_is_derived_from_a_stated_budget_not_chosen() -> None:
    """The floor must be reconstructible from its own stated derivation.

    A threshold picked to make a particular run pass or fail is not a
    threshold. This one is the sampled-extremum deficit of a periodic signal:
    sampling a sinusoid n times per period recovers at worst cos(pi/n) of its
    true amplitude, so a declared deficit budget fixes n, and a declared
    minimum number of resolved periods fixes the sample count.
    """
    deficit = 1.0 - math.cos(math.pi / SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT)
    assert deficit < SAMPLED_PEAK_DEFICIT_BUDGET, (
        "the stated samples-per-period does not meet its own deficit budget"
    )
    # and one sample per period fewer would NOT meet it - the number is tight
    # against its derivation rather than rounded up to look safe.
    coarser = 1.0 - math.cos(math.pi / (SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT - 1))
    assert coarser > SAMPLED_PEAK_DEFICIT_BUDGET
    assert AVERAGING_WINDOW_MIN_SAMPLES == (
        SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT * MIN_OSCILLATION_PERIODS
    )


def test_two_sample_window_is_inadequate_and_says_so_loudly() -> None:
    """n = 2 is the committed KCS production window. It is not an average."""
    verdict = averaging_window_adequacy(samples=2, window_iterations=4000)
    assert verdict["adequate"] is False
    assert verdict["verdict"] == "INADEQUATE"
    assert verdict["samples"] == 2
    assert verdict["minimum_samples"] == AVERAGING_WINDOW_MIN_SAMPLES
    assert "not" in verdict["note"].lower()


def test_a_two_sample_scatter_is_not_offered_as_a_statistic() -> None:
    below = averaging_window_adequacy(
        samples=MIN_SAMPLES_FOR_A_STATISTIC - 1, window_iterations=4000
    )
    assert below["statistic_available"] is False
    at = averaging_window_adequacy(
        samples=MIN_SAMPLES_FOR_A_STATISTIC, window_iterations=4000
    )
    assert at["statistic_available"] is True


def test_window_between_the_statistic_floor_and_the_minimum_is_marginal() -> None:
    verdict = averaging_window_adequacy(samples=5, window_iterations=4000)
    assert verdict["verdict"] == "MARGINAL"
    assert verdict["adequate"] is False
    assert verdict["statistic_available"] is True


def test_a_window_at_the_stated_minimum_is_adequate() -> None:
    verdict = averaging_window_adequacy(
        samples=AVERAGING_WINDOW_MIN_SAMPLES, window_iterations=4000
    )
    assert verdict["verdict"] == "ADEQUATE"
    assert verdict["adequate"] is True


def test_window_verdict_records_the_span_it_was_taken_over() -> None:
    """A sample count without its iteration span cannot be interpreted."""
    verdict = averaging_window_adequacy(samples=40, window_iterations=4000)
    assert verdict["window_iterations"] == 4000


# --------------------------------------------------------------------------- #
#  Iterative uncertainty - ITTC 7.5-03-01-01 oscillatory criterion
# --------------------------------------------------------------------------- #

def test_iterative_uncertainty_is_half_the_range_of_the_oscillation() -> None:
    """U_I = 0.5 * (S_U - S_L), verbatim."""
    series = [1.0, 1.1, 1.0, 0.9, 1.0, 1.1, 1.0, 0.9]
    result = iterative_uncertainty(series)
    assert result["available"] is True
    assert result["u_i"] == pytest.approx(0.5 * (1.1 - 0.9))
    assert result["s_upper"] == pytest.approx(1.1)
    assert result["s_lower"] == pytest.approx(0.9)


def test_iterative_uncertainty_counts_the_periods_it_resolved() -> None:
    """Two full oscillations must be reported as two, not asserted."""
    series = [1.0, 1.1, 1.0, 0.9, 1.0, 1.1, 1.0, 0.9, 1.0]
    result = iterative_uncertainty(series)
    assert result["turning_points"] == 4
    assert result["resolved_periods"] == pytest.approx(2.0)


def test_iterative_uncertainty_is_unavailable_below_the_statistic_floor() -> None:
    result = iterative_uncertainty([1.0, 1.05])
    assert result["available"] is False
    assert result["u_i"] is None
    assert "two" in result["note"].lower() or "3" in result["note"]


def test_iterative_uncertainty_reports_a_drifting_series_as_unconverged() -> None:
    """A monotone series has not oscillated about anything, so the half-range
    is a measure of the drift, not of the convergence."""
    result = iterative_uncertainty([1.0, 1.2, 1.4, 1.6, 1.8, 2.0])
    assert result["turning_points"] == 0
    assert result["oscillatory"] is False
    assert result["u_i"] is not None  # still reported, but labelled
    assert "drift" in result["note"].lower() or "monoton" in result["note"].lower()


# --------------------------------------------------------------------------- #
#  Grid uncertainty - what survives, and what a two-level study cannot say
# --------------------------------------------------------------------------- #

def _levels_from_h_squared(f_exact: float, amp: float, cells_fine: int, r: float,
                           n: int = 3):
    """Three levels of a second-order-exact sequence f = f_exact + amp * h^2."""
    out = []
    for i in range(n):
        h = r**i
        cells = int(cells_fine / (r ** (3 * i)))
        out.append(GridLevel(name=f"L{i}", cells=cells,
                             value=f_exact + amp * h**2))
    return out


def test_three_grid_study_recovers_the_order_it_was_built_with() -> None:
    levels = _levels_from_h_squared(3.0e-3, 1.0e-4, cells_fine=8_000_000, r=2.0)
    result = grid_uncertainty(levels)
    assert result["classification"] == "monotonic"
    assert result["order_is_assumed"] is False
    assert result["observed_order"] == pytest.approx(2.0, rel=1e-3)
    assert result["extrapolated_value"] == pytest.approx(3.0e-3, rel=1e-3)


def test_three_grid_gci_uses_the_roache_factor_of_safety() -> None:
    levels = _levels_from_h_squared(3.0e-3, 1.0e-4, cells_fine=8_000_000, r=2.0)
    result = grid_uncertainty(levels)
    assert result["factor_of_safety"] == ROACHE_FS_THREE_GRID
    assert result["u_g"] == pytest.approx(
        ROACHE_FS_THREE_GRID * abs(result["richardson_error"])
    )


def test_oscillatory_triple_is_classified_and_bounded_by_its_half_range() -> None:
    """R < 0 means the sequence is not converging monotonically; ITTC bounds it
    by the half-range of the levels rather than extrapolating."""
    levels = [
        GridLevel(name="fine", cells=8_000_000, value=3.10e-3),
        GridLevel(name="medium", cells=1_000_000, value=3.00e-3),
        GridLevel(name="coarse", cells=125_000, value=3.12e-3),
    ]
    result = grid_uncertainty(levels)
    assert result["classification"] == "oscillatory"
    assert result["estimable"] is True
    assert result["u_g"] == pytest.approx(0.5 * (3.12e-3 - 3.00e-3))
    assert result["observed_order"] is None


def test_divergent_triple_yields_no_uncertainty_estimate_at_all() -> None:
    levels = [
        GridLevel(name="fine", cells=8_000_000, value=3.00e-3),
        GridLevel(name="medium", cells=1_000_000, value=3.20e-3),
        GridLevel(name="coarse", cells=125_000, value=3.25e-3),
    ]
    result = grid_uncertainty(levels)
    assert result["classification"] == "divergent"
    assert result["estimable"] is False
    assert result["u_g"] is None


def test_two_level_study_cannot_measure_the_order_and_must_say_so() -> None:
    """The committed KCS study has exactly two levels. Roache sanctions a
    two-grid GCI at Fs = 3 with an ASSUMED order; that assumption has to be a
    field in the output, not a footnote."""
    levels = [
        GridLevel(name="production", cells=1_539_965, value=3.358985e-3),
        GridLevel(name="companion", cells=546_978, value=3.546377e-3),
    ]
    result = grid_uncertainty(levels)
    assert result["levels"] == 2
    assert result["order_is_assumed"] is True
    assert result["classification"] == "indeterminate"
    assert result["factor_of_safety"] == ROACHE_FS_TWO_GRID
    assert result["estimable"] is True
    assert result["u_g"] > 0


def test_single_level_is_not_a_grid_study() -> None:
    result = grid_uncertainty([GridLevel(name="only", cells=1_500_000,
                                         value=3.3e-3)])
    assert result["estimable"] is False
    assert result["u_g"] is None
    assert result["levels"] == 1


def test_refinement_ratio_below_the_recommended_floor_is_flagged() -> None:
    levels = [
        GridLevel(name="fine", cells=1_100_000, value=3.30e-3),
        GridLevel(name="coarse", cells=1_000_000, value=3.34e-3),
    ]
    result = grid_uncertainty(levels)
    assert result["refinement_ratio_adequate"] is False


# --------------------------------------------------------------------------- #
#  Plausibility - reported, never gated, never called validation
# --------------------------------------------------------------------------- #

def test_implied_form_factor_below_unity_is_implausible() -> None:
    """The committed KCS run. Cv/Cf = 0.913, i.e. an implied form factor of
    -8.7%: the hull is computed to generate LESS viscous resistance than the
    equivalent flat plate. No benchmark is needed to call that implausible,
    and it is the same finding V2b makes with one."""
    report = plausibility_report(
        ct=3.358985e-3, cp=7.732399e-4, cv=2.5857454e-3,
        cf=2.832045e-3, froude=0.26, hull_class="conventional_displacement",
        form_factor_band=FORM_FACTOR_BAND,
    )
    checks = {c["name"]: c for c in report["checks"]}
    ff = checks["implied_form_factor"]
    assert ff["value"] == pytest.approx(2.5857454e-3 / 2.832045e-3)
    assert ff["value"] < 1.0
    assert ff["verdict"] == "implausible"


def test_a_plausible_form_factor_is_never_called_validated() -> None:
    cf = 2.832045e-3
    cv = 1.20 * cf
    report = plausibility_report(
        ct=cv + 6.0e-4, cp=6.0e-4, cv=cv, cf=cf, froude=0.26,
        hull_class="conventional_displacement", form_factor_band=FORM_FACTOR_BAND,
    )
    checks = {c["name"]: c for c in report["checks"]}
    assert checks["implied_form_factor"]["verdict"] == "not_implausible"
    blob = repr(report).lower()
    assert "validated" not in blob
    assert "passed" not in blob


def test_a_negative_residuary_component_is_flagged_on_sign_alone() -> None:
    """Ct below the flat-plate line at the same Re leaves no residuary
    resistance. That is a sign condition, not an empirical band."""
    cf = 2.832045e-3
    report = plausibility_report(
        ct=cf * 0.9, cp=1.0e-4, cv=cf * 0.9 - 1.0e-4, cf=cf, froude=0.26,
        hull_class="conventional_displacement", form_factor_band=FORM_FACTOR_BAND,
    )
    checks = {c["name"]: c for c in report["checks"]}
    assert checks["residuary_sign"]["verdict"] == "violated"
    assert checks["residuary_sign"]["tier"] == "sign"


def test_the_empirical_band_declares_its_applicability_envelope() -> None:
    """A band drawn from conventional displacement hulls says nothing about a
    planing hull or a multihull. Outside its envelope it must decline, not
    guess - the same discipline the Holtrop-Mennen envelope demands."""
    report = plausibility_report(
        ct=3.3e-3, cp=7.0e-4, cv=2.6e-3, cf=2.83e-3, froude=0.9,
        hull_class="planing", form_factor_band=FORM_FACTOR_BAND,
    )
    checks = {c["name"]: c for c in report["checks"]}
    assert checks["implied_form_factor"]["verdict"] == "not_applicable"


def test_the_identity_check_is_tiered_as_arithmetic_not_as_evidence() -> None:
    report = plausibility_report(
        ct=3.3e-3, cp=7.0e-4, cv=2.6e-3, cf=2.83e-3, froude=0.26,
        hull_class="conventional_displacement", form_factor_band=FORM_FACTOR_BAND,
    )
    checks = {c["name"]: c for c in report["checks"]}
    assert checks["ct_identity"]["tier"] == "identity"
    assert checks["ct_identity"]["verdict"] == "holds"


def test_residuary_fraction_is_reported_without_a_band() -> None:
    """Cr/Ct is strongly Froude-dependent and this repo has no Fr-conditioned
    source for it. Reporting it with an invented band would be exactly the
    manufactured criterion this mode exists to avoid."""
    report = plausibility_report(
        ct=3.56e-3, cp=7.3e-4, cv=2.83e-3, cf=2.832045e-3, froude=0.26,
        hull_class="conventional_displacement", form_factor_band=FORM_FACTOR_BAND,
    )
    checks = {c["name"]: c for c in report["checks"]}
    frac = checks["residuary_fraction"]
    assert frac["verdict"] == "reported"
    assert frac.get("band") is None


# --------------------------------------------------------------------------- #
#  The referent-free evaluation, end to end
# --------------------------------------------------------------------------- #

def test_referent_free_run_never_loads_a_referent(tmp_path, monkeypatch) -> None:
    """THE load-bearing test.

    ``load_referent`` is sabotaged. If the evaluation still completes, the mode
    is genuinely referent-free; if it raises, the mode is the KCS gate wearing
    a different name.
    """
    from digitalmodel.solvers.openfoam.validation import ship_resistance

    def _explode(*_a, **_k):
        raise AssertionError("a referent was loaded in the referent-free path")

    monkeypatch.setattr(ship_resistance, "load_referent", _explode)

    config = _config(averaging_window=200)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=range(1, 201))
    manifest = evaluate_referent_free_run(f, config, mesh_cells=1_500_000)
    assert manifest["mode"] == "referent_free_prediction"
    assert manifest["validation"]["referent_loaded"] is False

    # The monkeypatch above only catches an ATTRIBUTE-style call. A top-level
    # `from ship_resistance import load_referent` would bind the real function
    # and slip straight past it, so assert the name is absent from the source
    # as well. Two independent checks of one property, because the cheaper one
    # has a known hole.
    import inspect

    from digitalmodel.solvers.openfoam.validation import referent_free_resistance

    src = inspect.getsource(referent_free_resistance)
    assert "load_referent" not in src
    assert "load_fixture" not in src
    assert "kcs_resistance_efd" not in src


def test_referent_free_run_accepts_the_mesh_derived_area(tmp_path) -> None:
    """The KCS guard refuses any area near 9.5609 m^2 because for KCS the
    published area is the only admissible one. For a hull with no publication
    the mesh-derived area is the ONLY area there is, so that guard must not sit
    in this path."""
    config = _config(averaging_window=200)
    assert config.wetted_surface.value_m2 == pytest.approx(9.5609)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=range(1, 201))
    manifest = evaluate_referent_free_run(f, config)
    assert manifest["normalisation"]["area_m2"] == pytest.approx(9.5609)
    assert manifest["normalisation"]["provenance"] == "mesh_derived"


def test_an_area_without_declared_provenance_is_refused() -> None:
    with pytest.raises(ValueError, match="provenance"):
        NormalisationArea(value_m2=9.5, provenance="", source="somewhere")
    with pytest.raises(ValueError, match="provenance"):
        NormalisationArea(value_m2=9.5, provenance="guessed", source="somewhere")
    with pytest.raises(ValueError, match="citation|source"):
        NormalisationArea(value_m2=9.5, provenance="mesh_derived", source="")


def test_manifest_carries_no_aggregate_pass_and_no_validation_claim(
    tmp_path,
) -> None:
    config = _config(averaging_window=200)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=range(1, 201))
    manifest = evaluate_referent_free_run(f, config)
    assert "all_passed" not in manifest
    assert "criteria" not in manifest
    assert manifest["validation"]["available"] is False
    assert manifest["validation"]["reason"]


def test_manifest_states_explicitly_what_it_cannot_establish(tmp_path) -> None:
    config = _config(averaging_window=200)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=range(1, 201))
    manifest = evaluate_referent_free_run(f, config)
    text = " ".join(manifest["cannot_establish"]).lower()
    assert manifest["cannot_establish"]
    assert "model" in text  # model-form / modelling error
    assert "accur" in text or "true" in text


def test_the_empirical_cross_check_declares_itself_blocked_not_absent(
    tmp_path,
) -> None:
    """Holtrop-Mennen is the obvious corroboration and it is #2020-blocked.
    Silence would read as 'not applicable'; the manifest must say 'blocked'."""
    config = _config(averaging_window=200)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=range(1, 201))
    manifest = evaluate_referent_free_run(f, config)
    cross = manifest["cross_check"]
    assert cross["status"] == "unavailable"
    assert cross["blocked_on"] == "#2020"
    assert "corroborat" in cross["note"].lower()


def test_the_module_does_not_import_holtrop_mennen() -> None:
    """A half-remembered empirical formula wired in quietly is worse than no
    cross-check at all. Assert the dependency is referenced, not taken."""
    import inspect

    from digitalmodel.solvers.openfoam.validation import referent_free_resistance

    src = inspect.getsource(referent_free_resistance)
    assert "import holtrop" not in src
    assert "from digitalmodel.naval_architecture.holtrop_mennen" not in src


def test_reported_result_carries_a_band_that_names_itself_numerical_only(
    tmp_path,
) -> None:
    config = _config(averaging_window=4000)
    times = list(range(1000, 25001, 100))  # 40 samples inside the window
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=times)
    coarse = _write_force_dat(tmp_path / "coarse.dat", ct=3.42e-3, cp=7.4e-4,
                              cv=2.68e-3, config=config, times=times)
    manifest = evaluate_referent_free_run(
        f, config, mesh_cells=1_500_000,
        companions=[(coarse, 546_978)],
    )
    reported = manifest["reported_result"]
    assert reported["ct"] == pytest.approx(3.3e-3, rel=1e-6)
    assert reported["plus_minus"] is not None
    statement = reported["statement"].lower()
    assert "numerical" in statement
    assert "not" in statement and "valid" in statement
    assert manifest["uncertainty"]["excludes"]


def test_an_inadequate_window_makes_the_run_unreportable(tmp_path) -> None:
    """The condition the committed KCS artifact is in: two force samples.

    This is an ADMISSIBILITY condition, not a validation criterion - it does
    not say the answer is wrong, it says the number is not yet a converged
    average and so must not be quoted with a band.
    """
    config = _config(averaging_window=4000)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.358985e-3, cp=7.732399e-4,
                         cv=2.5857454e-3, config=config, times=[22500, 25000])
    manifest = evaluate_referent_free_run(f, config, mesh_cells=1_539_965)
    assert manifest["measurement"]["samples"] == 2
    assert manifest["admissibility"]["averaging_window_adequate"] is False
    assert manifest["admissibility"]["admissible"] is False
    assert manifest["reported_result"]["plus_minus"] is None
    assert any("window" in r.lower()
               for r in manifest["admissibility"]["reasons"])


def test_a_single_grid_run_is_unreportable_for_want_of_a_grid_study(
    tmp_path,
) -> None:
    config = _config(averaging_window=4000)
    times = list(range(1000, 25001, 100))
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=times)
    manifest = evaluate_referent_free_run(f, config, mesh_cells=1_500_000)
    assert manifest["grid_convergence"]["estimable"] is False
    assert manifest["admissibility"]["numerical_uncertainty_estimable"] is False
    assert manifest["admissibility"]["admissible"] is False


def test_uncertainty_combines_iterative_and_grid_in_quadrature(tmp_path) -> None:
    config = _config(averaging_window=4000)
    times = list(range(1000, 25001, 100))
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=times)
    coarse = _write_force_dat(tmp_path / "coarse.dat", ct=3.42e-3, cp=7.4e-4,
                              cv=2.68e-3, config=config, times=times)
    manifest = evaluate_referent_free_run(
        f, config, mesh_cells=1_500_000, companions=[(coarse, 546_978)],
    )
    u = manifest["uncertainty"]
    assert u["u_g"] > 0
    # the synthetic series is constant, so U_I is zero and U_SN is all grid
    assert u["u_sn"] == pytest.approx(
        math.sqrt((u["u_i"] or 0.0) ** 2 + (u["u_g"] or 0.0) ** 2)
    )


def test_provenance_records_the_condition_and_names_no_referent(
    tmp_path,
) -> None:
    config = _config(averaging_window=200)
    f = _write_force_dat(tmp_path / "force.dat", ct=3.3e-3, cp=7.0e-4,
                         cv=2.6e-3, config=config, times=range(1, 201))
    manifest = evaluate_referent_free_run(f, config)
    prov = manifest["provenance"]
    assert prov["reynolds"] == pytest.approx(1.4e7)
    assert prov["froude"] == pytest.approx(
        2.1962 / math.sqrt(9.80665 * 7.2786), rel=1e-9
    )
    assert prov["averaging_window_iterations"] == 200
    assert "reference" not in prov


# --------------------------------------------------------------------------- #
#  Non-regression: the KCS path is untouched
# --------------------------------------------------------------------------- #

def test_kcs_path_still_gates_against_its_referent(tmp_path) -> None:
    """The referent-free mode is additive. The benchmark path must still load a
    referent, still emit V1/V2a/V2b and still end in an aggregate verdict."""
    from digitalmodel.solvers.openfoam.validation.ship_resistance import (
        ShipResistanceConfig,
        evaluate_ship_resistance_run,
        load_referent,
    )

    kcs = ShipResistanceConfig(averaging_window=200)
    referent = load_referent()
    q = 0.5 * kcs.density * kcs.wetted_surface * kcs.velocity**2
    lines = ["# Time total_x total_y total_z pressure_x pressure_y pressure_z "
             "viscous_x viscous_y viscous_z"]
    for i in range(1, 201):
        lines.append(
            f"{i} {-referent.ct * q / 2:.9e} 0 0 "
            f"{-referent.cr * q / 2:.9e} 0 0 {-referent.cf * q / 2:.9e} 0 0"
        )
    path = tmp_path / "kcs_force.dat"
    path.write_text("\n".join(lines) + "\n")

    manifest = evaluate_ship_resistance_run(path, kcs)
    assert manifest["all_passed"] is True
    assert set(manifest["summary"]) == {"V1", "V2a", "V2b"}


def test_kcs_normalisation_guard_still_refuses_the_mesh_derived_area() -> None:
    """The guard the referent-free path drops must still be live on the KCS
    path. Dropping it in both places is the regression this test exists for."""
    from digitalmodel.solvers.openfoam.validation.ship_resistance import (
        KCS_GENERATED_WETTED_SURFACE,
        ShipResistanceConfig,
        coefficients_from_force,
    )
    from digitalmodel.solvers.openfoam.validation.ship_resistance import HullForce

    bad = ShipResistanceConfig(wetted_surface=KCS_GENERATED_WETTED_SURFACE)
    force = HullForce(total=76.0, pressure=17.0, viscous=59.0, samples=10,
                      first_iteration=1, last_iteration=10, scatter=0.1)
    with pytest.raises(ValueError, match="GENERATED"):
        coefficients_from_force(force, bad)


def test_force_row_reader_agrees_with_the_kcs_parser_on_the_window(
    tmp_path,
) -> None:
    """Both paths must select the same rows. Two implementations of the
    iteration-span window is how the #1173 row-count defect comes back."""
    from digitalmodel.solvers.openfoam.validation.ship_resistance import (
        parse_hull_force,
        read_force_rows,
    )

    lines = ["# Time total_x total_y total_z pressure_x pressure_y pressure_z "
             "viscous_x viscous_y viscous_z"]
    for t in range(0, 25001, 2500):
        v = 0.0 if t == 0 else -80.0
        lines.append(f"{t} {v} 0 0 {v/4} 0 0 {3*v/4} 0 0")
    path = tmp_path / "force.dat"
    path.write_text("\n".join(lines) + "\n")

    rows = read_force_rows(path, window=4000)
    force = parse_hull_force(path, window=4000, half_domain=False)
    assert len(rows) == force.samples
    assert rows[0].iteration == force.first_iteration
    assert rows[-1].iteration == force.last_iteration
