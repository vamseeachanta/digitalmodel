"""
ABOUTME: Form-factor extraction from a double-body run (#2023).

(1 + k) = C_v,double-body / C_f,ITTC-57 is the number that makes a viscous
resistance interpretable, and the value BELOW ONE is the reason this surface
exists at all. It says the hull generates less viscous resistance than the
flat plate of the same wetted area at the same Reynolds number - that
curvature and viscous form drag subtract from friction - and no hull does
that. The KCS run on this codebase returned an implied form factor near 0.91
while its force integral was reading the dry topsides at water density, and
that defect is known to follow the code. So the sub-unity case must come back
as a DEFECT and never as a small form factor.

The three worked examples pinned below are the ones the analysis programme
reads its Phase 0 gate from:

    (1 + k) = 1.25   normal          -> not_implausible
    (1 + k) = 1.02   suspicious      -> implausible, but not a defect
    (1 + k) = 0.91   the KCS value   -> violated, and a defect

VOCABULARY. This module inherits ``referent_free_resistance``'s posture, and
the tests enforce it: nothing here may say ``passed`` or ``validated``. A band
cannot confirm a number; it can only fail to contradict one.
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.validation.double_body_form_factor import (
    FORCE_STABILITY_BUDGET,
    FORM_FACTOR_FLOOR,
    FORM_FACTOR_REPORTING_RESOLUTION,
    DoubleBodyRunConfig,
    evaluate_double_body_run,
    force_stability,
    form_factor_from_coefficients,
    form_factor_verdict,
)
from digitalmodel.solvers.openfoam.validation.referent_free_resistance import (
    FORM_FACTOR_BAND,
    NormalisationArea,
)
from digitalmodel.solvers.openfoam.validation.ship_resistance import (
    ittc57_friction_coefficient,
)

# --------------------------------------------------------------------------- #
#  The programme condition. A size class, not a vessel.
# --------------------------------------------------------------------------- #

LPP = 158.196
VELOCITY = 6.6878          # 13 kn
NU = 1.19e-6
DENSITY = 1025.0
WETTED_SURFACE = 6216.8    # FULL wetted surface; the half-domain force is
                           # doubled before it is divided by this
REYNOLDS = VELOCITY * LPP / NU

#: Pressure share of the double-body force. A double-body result carries
#: viscous form drag in its pressure integral, and it is a MINORITY of the
#: total on a conventional hull -- which is why reducing the friction integral
#: alone returns (1 + k) near unity and looks converged.
PRESSURE_SHARE = 0.2

WORKED_EXAMPLES = {"normal": 1.25, "suspicious": 1.02, "kcs_defect": 0.91}


AREA = NormalisationArea(
    value_m2=WETTED_SURFACE,
    provenance="mesh_derived",
    source="wetted area integrated over the emitted hull surface at the DWL",
)


def _config(**kw) -> DoubleBodyRunConfig:
    base = dict(
        name="double_body",
        lpp=LPP,
        velocity=VELOCITY,
        reynolds=REYNOLDS,
        density=DENSITY,
        wetted_surface=AREA,
        averaging_window=500,
    )
    base.update(kw)
    return DoubleBodyRunConfig(**base)


def _force_file(
    tmp_path: Path,
    one_plus_k: float,
    *,
    samples: int = 40,
    drift: float = 0.0,
    name: str = "force.dat",
) -> Path:
    """A forces log that reduces to the requested form factor.

    Written in the solver's own convention -- half-domain, towing in -x, so
    the numbers are negative -- because the reduction has to do the doubling
    and the sign correction itself. A fixture that pre-corrected them would
    test nothing.
    """
    cf = ittc57_friction_coefficient(REYNOLDS)
    total_full = one_plus_k * cf * 0.5 * DENSITY * WETTED_SURFACE * VELOCITY**2
    half = total_full / 2.0
    lines = ["# Time (total) (pressure) (viscous)"]
    for i in range(samples):
        ramp = 1.0 + drift * (i / max(samples - 1, 1) - 0.5)
        t = -half * ramp
        p = t * PRESSURE_SHARE
        v = t * (1.0 - PRESSURE_SHARE)
        lines.append(
            f"{i + 1} ({t:.10e} 0 0) ({p:.10e} 0 0) ({v:.10e} 0 0)"
        )
    path = tmp_path / name
    path.write_text("\n".join(lines) + "\n")
    return path


# --------------------------------------------------------------------------- #
#  The ratio itself
# --------------------------------------------------------------------------- #

def test_the_ittc57_line_at_the_programme_condition() -> None:
    """Cf = 0.075 / (log10(Re) - 2)^2, pinned so a change to the line cannot
    move every form factor this lane produces without a test noticing."""
    assert REYNOLDS == pytest.approx(8.8906152e8, rel=1e-6)
    assert ittc57_friction_coefficient(REYNOLDS) == pytest.approx(
        1.5531921e-3, rel=1e-6
    )


def test_the_form_factor_is_the_ratio_to_the_correlation_line() -> None:
    cf = ittc57_friction_coefficient(REYNOLDS)
    for expected in WORKED_EXAMPLES.values():
        one_plus_k, k = form_factor_from_coefficients(expected * cf, cf)
        assert one_plus_k == pytest.approx(expected)
        assert k == pytest.approx(expected - 1.0)


def test_a_non_positive_correlation_coefficient_is_refused() -> None:
    for bad in (0.0, -1e-3):
        with pytest.raises(ValueError):
            form_factor_from_coefficients(2e-3, bad)


# --------------------------------------------------------------------------- #
#  The three worked examples
# --------------------------------------------------------------------------- #

def _verdict(one_plus_k: float, **kw) -> dict:
    cf = ittc57_friction_coefficient(REYNOLDS)
    return form_factor_verdict(cv_double_body=one_plus_k * cf, cf_ittc57=cf, **kw)


def test_a_normal_form_factor_is_not_implausible_and_nothing_stronger() -> None:
    """1.25 sits in the middle of the declared band. The strongest thing that
    may be said about it is that it has failed to contradict a weak
    expectation."""
    v = _verdict(WORKED_EXAMPLES["normal"])
    assert v["one_plus_k"] == pytest.approx(1.25)
    assert v["k"] == pytest.approx(0.25)
    assert v["verdict"] == "not_implausible"
    assert v["defect"] is False


def test_a_suspicious_form_factor_is_implausible_but_is_not_a_defect() -> None:
    """1.02 is possible and unlikely, and the difference matters: a defect
    stops the programme, a suspicious value sends someone to look. It reports
    a hull that is almost a flat plate, which a displacement form is not - and
    a partially-corrected force integral lands in exactly this region."""
    v = _verdict(WORKED_EXAMPLES["suspicious"])
    assert v["one_plus_k"] == pytest.approx(1.02)
    assert v["verdict"] == "implausible"
    assert v["defect"] is False
    band = next(c for c in v["checks"] if c["tier"] == "band")
    assert "SUSPICIOUS" in band["note"]
    sign = next(c for c in v["checks"] if c["tier"] == "sign")
    assert sign["verdict"] == "holds"


def test_the_kcs_defect_value_is_reported_as_a_defect_not_a_number() -> None:
    """0.91 is the value this gate exists to catch. It must not come back as
    'a low form factor'."""
    v = _verdict(WORKED_EXAMPLES["kcs_defect"])
    assert v["one_plus_k"] == pytest.approx(0.91)
    assert v["k"] == pytest.approx(-0.09)
    assert v["verdict"] == "violated"
    assert v["defect"] is True
    sign = next(c for c in v["checks"] if c["tier"] == "sign")
    assert sign["verdict"] == "violated"
    assert "PHYSICALLY IMPOSSIBLE" in sign["note"]
    assert "flat plate" in sign["note"]


def test_the_floor_is_unity_and_is_not_a_tolerance() -> None:
    assert FORM_FACTOR_FLOOR == 1.0
    assert _verdict(0.9999)["defect"] is True
    assert _verdict(1.0)["defect"] is False


def test_the_band_is_the_one_already_declared_in_this_repository() -> None:
    """Two bands would be two different statements about the same population,
    and the one a reader happened to find would decide the verdict."""
    lo, hi = FORM_FACTOR_BAND
    assert _verdict(lo)["verdict"] == "not_implausible"
    assert _verdict(hi)["verdict"] == "not_implausible"
    assert _verdict(hi + 0.01)["verdict"] == "implausible"
    assert _verdict(lo - 0.01)["verdict"] == "implausible"


def test_the_sign_check_is_not_conditional_on_the_hull_class() -> None:
    """A band drawn from a population does not apply outside it, so the band
    declines. (1 + k) < 1 is not a statement about a population -- it follows
    from what viscous resistance is -- so it must survive the class going
    unrecognised. A defect that hides behind an unusual hull type is worse
    than no gate."""
    v = _verdict(0.91, hull_class="planing")
    assert v["defect"] is True
    assert v["verdict"] == "violated"
    band = next(c for c in v["checks"] if c["tier"] == "band")
    assert band["verdict"] == "not_applicable"

    ok = _verdict(1.25, hull_class="planing")
    assert ok["verdict"] == "not_applicable"
    assert ok["defect"] is False


def test_no_verdict_anywhere_claims_a_pass_or_a_validation(tmp_path) -> None:
    """The vocabulary is load-bearing. ``referent_free_resistance`` says why:
    a criterion invented after looking at the answer is not a criterion, and a
    band that says 'passed' has been promoted to one."""
    manifest = evaluate_double_body_run(
        _force_file(tmp_path, WORKED_EXAMPLES["normal"]), _config()
    )
    blob = json.dumps(manifest).lower()
    for forbidden in ("passed", "validated", "all_passed"):
        assert forbidden not in blob, forbidden
    assert manifest["form_factor"]["verdict"] in (
        "not_implausible",
        "implausible",
        "not_applicable",
        "violated",
    )


# --------------------------------------------------------------------------- #
#  Reducing a solved run
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("label,expected", sorted(WORKED_EXAMPLES.items()))
def test_a_solved_run_reduces_to_its_form_factor(
    tmp_path, label: str, expected: float
) -> None:
    manifest = evaluate_double_body_run(
        _force_file(tmp_path, expected, name=f"{label}.dat"), _config()
    )
    assert manifest["form_factor"]["one_plus_k"] == pytest.approx(expected, rel=1e-6)
    assert manifest["mode"] == "double_body_form_factor"


def test_the_reduction_uses_the_total_and_not_the_friction_integral(
    tmp_path,
) -> None:
    """The defect that would look like a converged answer.

    The pressure part of a double-body force IS the viscous form drag, and it
    is what k measures. The form-factor method's own premise is that the
    hull's friction equals the flat plate's and the EXCESS is form drag, so a
    reduction that divides only the friction integral by the ITTC-57 line
    returns 1.000 - a form factor of exactly ZERO - and does so for every
    hull. The fixture's 20% pressure share is that premise at k = 0.25:
    k / (1 + k) = 0.2.
    """
    manifest = evaluate_double_body_run(
        _force_file(tmp_path, 1.25), _config()
    )
    m = manifest["measurement"]
    assert m["cv_double_body"] == pytest.approx(m["cv_pressure"] + m["cv_friction"])
    assert m["pressure_fraction"] == pytest.approx(PRESSURE_SHARE, rel=1e-6)

    cf = ittc57_friction_coefficient(REYNOLDS)
    friction_only = m["cv_friction"] / cf
    assert friction_only < 1.25, "the friction integral cannot carry form drag"
    assert friction_only == pytest.approx(1.0, abs=1e-6), (
        "reducing the wrong component returns k = 0 for a hull whose form "
        "factor is 0.25, and 1.000 is inside no band and above no floor -- it "
        "would be reported as a clean, converged, wrong answer"
    )
    assert manifest["form_factor"]["one_plus_k"] == pytest.approx(1.25)


def test_the_half_domain_force_is_doubled(tmp_path) -> None:
    """The solve is cut at the centreplane and reports half of every force.
    Left undoubled it is a clean factor of two in the form factor."""
    force = _force_file(tmp_path, 1.25)
    halved = evaluate_double_body_run(force, _config())
    whole = evaluate_double_body_run(force, _config(half_domain=False))
    assert halved["form_factor"]["one_plus_k"] == pytest.approx(
        2.0 * whole["form_factor"]["one_plus_k"]
    )


def test_the_solver_sign_convention_does_not_reach_the_coefficient(
    tmp_path,
) -> None:
    """The template tows in -x, so the raw force is negative. A form factor
    that inherited the sign would be negative and would trip the floor for a
    reason that has nothing to do with the hull."""
    manifest = evaluate_double_body_run(_force_file(tmp_path, 1.25), _config())
    assert manifest["measurement"]["cv_double_body"] > 0
    assert manifest["measurement"]["force_total_N"] > 0


# --------------------------------------------------------------------------- #
#  Convergence of a STEADY run
# --------------------------------------------------------------------------- #

def test_the_stability_budget_is_derived_from_the_reported_resolution() -> None:
    """Not chosen and not tuned against a result: (1 + k) is quoted to two
    decimals, and near the middle of the band that digit needs a mean stable
    to this fraction."""
    mid = sum(FORM_FACTOR_BAND) / 2.0
    assert FORCE_STABILITY_BUDGET == pytest.approx(
        FORM_FACTOR_REPORTING_RESOLUTION / mid
    )
    assert FORCE_STABILITY_BUDGET == pytest.approx(0.008)


def test_a_settled_force_is_stable_and_a_drifting_one_is_not(tmp_path) -> None:
    settled = evaluate_double_body_run(
        _force_file(tmp_path, 1.25, name="settled.dat"), _config()
    )
    assert settled["force_convergence"]["verdict"] == "stable"
    assert settled["admissibility"]["admissible"] is True

    drifting = evaluate_double_body_run(
        _force_file(tmp_path, 1.25, drift=0.05, name="drifting.dat"), _config()
    )
    assert drifting["force_convergence"]["verdict"] == "drifting"
    assert drifting["admissibility"]["admissible"] is False
    assert any(
        "settled" in reason for reason in drifting["admissibility"]["reasons"]
    )


def test_a_steady_run_is_not_scored_by_the_oscillatory_criterion() -> None:
    """A converged steady SIMPLE run approaches its answer monotonically. The
    ITTC oscillatory half-range applied to it reports a tiny number for a run
    that has merely slowed down, so drift is scored instead -- and a monotone
    series is not silently treated as converged."""
    monotone = [1.0 + 0.02 * i for i in range(30)]
    verdict = force_stability(monotone)
    assert verdict["verdict"] == "drifting"
    assert verdict["relative_drift"] > verdict["budget"]


def test_a_window_with_no_statistic_in_it_says_so() -> None:
    result = force_stability([1.0, 1.0])
    assert result["verdict"] == "insufficient_samples"
    assert result["relative_drift"] is None


def test_a_defect_makes_the_run_inadmissible(tmp_path) -> None:
    """A form factor below unity must not be carried forward as a number, and
    admissibility is where that is enforced rather than in the verdict text."""
    manifest = evaluate_double_body_run(
        _force_file(tmp_path, WORKED_EXAMPLES["kcs_defect"]), _config()
    )
    assert manifest["admissibility"]["admissible"] is False
    assert any(
        "flat-plate floor" in reason
        for reason in manifest["admissibility"]["reasons"]
    )


def test_what_this_module_cannot_establish_is_stated_not_implied(
    tmp_path,
) -> None:
    manifest = evaluate_double_body_run(
        _force_file(tmp_path, 1.25), _config()
    )
    text = " ".join(manifest["cannot_establish"]).lower()
    assert "no measurement of this hull" in text
    assert "correlation line" in text
    assert "boundary-layer resolution" in text


# --------------------------------------------------------------------------- #
#  Configuration refusals
# --------------------------------------------------------------------------- #

def test_an_area_without_a_provenance_cannot_enter() -> None:
    with pytest.raises(ValueError):
        NormalisationArea(value_m2=WETTED_SURFACE, provenance="guessed", source="x")


def test_a_zero_averaging_window_is_refused() -> None:
    with pytest.raises(ValueError) as excinfo:
        _config(averaging_window=0)
    assert "transient" in str(excinfo.value)


@pytest.mark.parametrize(
    "field", ["lpp", "velocity", "reynolds", "density"]
)
def test_a_non_positive_condition_is_refused(field: str) -> None:
    with pytest.raises(ValueError):
        _config(**{field: 0.0})


def test_the_normalisation_area_is_carried_into_the_manifest(tmp_path) -> None:
    """The form factor is a ratio of coefficients, and it cancels the area
    only if the ITTC-57 line is applied on the same surface. Carrying the
    provenance is what makes that checkable."""
    manifest = evaluate_double_body_run(_force_file(tmp_path, 1.25), _config())
    assert manifest["normalisation"]["area_m2"] == pytest.approx(WETTED_SURFACE)
    assert manifest["normalisation"]["provenance"] == "mesh_derived"
    assert math.isfinite(manifest["provenance"]["cf_ittc57"])
