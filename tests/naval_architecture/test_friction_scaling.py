"""W1 (#2239) tests: ITTC-57 friction, ITTC-78 simplified transfer, Prohaska fit.

Expected values are the plan's TDD table (docs/plans/2026-09-27-issue-2239-
analytical-resistance-methods.md). All inputs are dimensionless or synthetic.
"""
from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.naval_architecture.friction_scaling import (
    Fluid,
    froude_number,
    ittc57_cf,
    ittc57_cf_cited,
    prohaska_form_factor,
    reynolds_number_si,
    transfer_model_to_ship,
    transfer_ship_to_model,
)
from digitalmodel.naval_architecture.resistance import ittc_1957_cf

# Vendored citation fixtures (shared with tests/citations/ and test_resistance_citations.py)
CIT_ROOT = Path(__file__).resolve().parent.parent / "citations" / "fixtures"

CF_1E9 = 0.075 / 49.0  # (log10(1e9) - 2)^2 = 49
CF_1E7 = 0.075 / 25.0  # = 0.003


# ---------------------------------------------------------------- ITTC-57


def test_ittc57_reference_values():
    assert ittc57_cf(1e6) == pytest.approx(0.0046875, abs=1e-12)
    assert ittc57_cf(1e9) == pytest.approx(CF_1E9, abs=1e-12)
    assert ittc57_cf(1e9) == pytest.approx(0.00153061, abs=1e-8)


def test_ittc57_consistent_with_resistance_module():
    for re in (2.5e6, 1e7, 3.3e8, 1e9, 4e9):
        assert ittc57_cf(re) == ittc_1957_cf(re)


def test_ittc57_cited_reuses_resistance_wrapper(monkeypatch):
    from digitalmodel.naval_architecture import resistance as res

    sentinel = object()

    def _fake_cited(rn, *, repo_root=None):
        return {"value": ittc_1957_cf(rn), "units": "dimensionless", "citations": [sentinel]}

    monkeypatch.setattr(res, "ittc_1957_cf_cited", _fake_cited)
    out = ittc57_cf_cited(1e9)
    assert out["value"] == pytest.approx(CF_1E9, abs=1e-12)
    assert out["citations"] == [sentinel]


@pytest.mark.parametrize("re", [0.0, -1.0, float("nan"), float("inf"), 100.0, 50.0])
def test_invalid_reynolds_refused(re):
    with pytest.raises(ValueError):
        ittc57_cf(re)


@pytest.mark.parametrize("rho,nu", [(1000.0, 0.0), (1000.0, -1e-6), (0.0, 1e-6), (float("nan"), 1e-6)])
def test_invalid_fluid_refused(rho, nu):
    with pytest.raises(ValueError):
        Fluid(name="test", rho=rho, nu=nu)


def test_reynolds_and_froude_si_with_fluid_parameter():
    fresh = Fluid(name="fresh water 15C (caller-declared)", rho=999.1, nu=1.1386e-6)
    assert reynolds_number_si(2.0, 5.0, fresh) == pytest.approx(2.0 * 5.0 / 1.1386e-6, rel=1e-14)
    assert froude_number(2.0, 5.0, g=9.81) == pytest.approx(2.0 / math.sqrt(9.81 * 5.0), rel=1e-14)
    with pytest.raises(ValueError):
        reynolds_number_si(-1.0, 5.0, fresh)
    with pytest.raises(ValueError):
        reynolds_number_si(1.0, 0.0, fresh)


# ---------------------------------------------------------------- transfer


def _forward(k, **kw):
    return transfer_model_to_ship(
        ct_model=4.0e-3, re_model=1e7, re_ship=1e9, form_factor_k=k,
        fn_model=0.25, fn_ship=0.25, repo_root=CIT_ROOT, **kw,
    )


def test_transfer_forward_case_k():
    r = _forward(0.2)
    assert r.cr == pytest.approx(4.0e-3 - 1.2 * 0.003, abs=1e-15)
    assert r.ct_ship == pytest.approx(1.2 * CF_1E9 + 0.0004, abs=1e-15)
    assert r.ct_ship == pytest.approx(2.236735e-3, abs=5e-10)


def test_transfer_forward_case_k0():
    r = _forward(0.0)
    assert r.ct_ship == pytest.approx(CF_1E9 + 0.001, abs=1e-15)
    assert r.ct_ship == pytest.approx(2.530612e-3, abs=5e-10)


def test_transfer_allowances_declared():
    base = _forward(0.2)
    with_ca = _forward(0.2, ca_ship=4e-4)
    assert with_ca.ct_ship - base.ct_ship == pytest.approx(4e-4, abs=1e-15)
    assert with_ca.allowances["ca_ship"].declared is True
    assert with_ca.allowances["ca_ship"].value == 4e-4
    for name in ("ca_model", "ca_ship", "delta_cf"):
        term = base.allowances[name]
        assert term.value == 0.0 and term.declared is False
    assert any("ca_ship" in s for s in base.omitted_corrections)


def test_transfer_model_allowance_reduces_residual():
    r = _forward(0.2, ca_model=1e-4)
    assert r.cr == pytest.approx(4.0e-3 - 1.2 * 0.003 - 1e-4, abs=1e-15)


def test_transfer_metadata_lists_assumptions():
    r = _forward(0.2)
    text = " ".join(r.assumptions).lower()
    assert "froude" in text and "form factor" in text and "geometric" in text
    omitted = " ".join(r.omitted_corrections).lower()
    assert "air" in omitted and "appendage" in omitted and "roughness" in omitted
    assert "7.5-02-03-01.4" in r.procedure


def test_transfer_roundtrip_identity():
    kw = dict(re_model=1e7, re_ship=1e9, form_factor_k=0.17, fn_model=0.22, fn_ship=0.22,
              ca_model=5e-5, ca_ship=3.5e-4, delta_cf=1.1e-4, repo_root=CIT_ROOT)
    fwd = transfer_model_to_ship(ct_model=4.3e-3, **kw)
    back = transfer_ship_to_model(ct_ship=fwd.ct_ship, **kw)
    assert back.ct_model == pytest.approx(4.3e-3, abs=1e-12)
    assert back.cr == pytest.approx(fwd.cr, abs=1e-12)


def test_transfer_refuses_froude_mismatch():
    with pytest.raises(ValueError, match="[Ff]roude"):
        transfer_model_to_ship(ct_model=4e-3, re_model=1e7, re_ship=1e9, form_factor_k=0.2,
                               fn_model=0.25, fn_ship=0.26)


@pytest.mark.parametrize("k", [-0.1, float("nan")])
def test_transfer_refuses_invalid_form_factor(k):
    with pytest.raises(ValueError):
        _forward(k)


def test_transfer_refuses_invalid_ct():
    with pytest.raises(ValueError):
        transfer_model_to_ship(ct_model=-1e-3, re_model=1e7, re_ship=1e9, form_factor_k=0.2,
                               fn_model=0.25, fn_ship=0.25)


# ---------------------------------------------------------------- Prohaska


def _synthetic_prohaska(k, c, n, sigma, rng):
    """Synthetic data under the fit's error model: y = C_T/C_F = (1+k) + c Fn^4/C_F + e,
    e ~ N(0, sigma^2) homoscedastic and additive on y."""
    fn = np.linspace(0.10, 0.20, n)
    g, lm, nu = 9.81, 5.0, 1.1386e-6  # synthetic 5 m model in caller-declared fresh water
    re = fn * np.sqrt(g * lm) * lm / nu
    cf = np.array([ittc57_cf(r) for r in re])
    y = (1.0 + k) + c * fn**4 / cf
    if sigma:
        y = y + sigma * rng.standard_normal(n)
    return fn, y * cf, cf


def test_prohaska_recovers_k_exact_data():
    fn, ct, cf = _synthetic_prohaska(0.15, 0.5, 8, 0.0, None)
    fit = prohaska_form_factor(fn, ct, cf)
    assert fit.k == pytest.approx(0.15, abs=1e-10)
    assert fit.c == pytest.approx(0.5, abs=1e-8)
    assert fit.residual_rms < 1e-12


def test_prohaska_recovers_k_with_noise():
    # One realisation of the error model; a single draw misses a 95 % CI about 1 time in 20
    # by construction, so the statistical claim is tested by the coverage test below.
    fn, ct, cf = _synthetic_prohaska(0.15, 0.5, 8, 0.01, np.random.default_rng(0))
    fit = prohaska_form_factor(fn, ct, cf)
    lo, hi = fit.k_ci95
    assert lo < 0.15 < hi
    assert lo < fit.k < hi
    assert fit.n_points == 8
    assert np.isfinite(fit.condition_number) and fit.condition_number <= 1e6
    assert fit.residual_rms > 0.0
    assert "homoscedastic" in fit.error_model


def test_prohaska_ci_matches_independent_regression():
    """Intercept standard error cross-checked against scipy.stats.linregress."""
    from scipy import stats

    fn, ct, cf = _synthetic_prohaska(0.15, 0.5, 9, 0.01, np.random.default_rng(7))
    fit = prohaska_form_factor(fn, ct, cf)
    lr = stats.linregress(fn**4 / cf, ct / cf)
    half = stats.t.ppf(0.975, 9 - 2) * lr.intercept_stderr
    assert fit.k == pytest.approx(lr.intercept - 1.0, abs=1e-12)
    assert fit.k_ci95[1] - fit.k == pytest.approx(half, rel=1e-9)
    assert fit.k - fit.k_ci95[0] == pytest.approx(half, rel=1e-9)


def test_prohaska_ci_coverage_is_nominal():
    """Under the stated error model the 95 % CI covers the true k at its nominal rate.
    400 draws from one fixed-seed generator: binomial s.d. ~1.1 %, band 0.91-0.99."""
    rng = np.random.default_rng(2239)
    trials = 400
    hits = 0
    for _ in range(trials):
        fn, ct, cf = _synthetic_prohaska(0.15, 0.5, 8, 0.01, rng)
        lo, hi = prohaska_form_factor(fn, ct, cf).k_ci95
        hits += lo < 0.15 < hi
    assert 0.91 <= hits / trials <= 0.99


def test_prohaska_refuses_few_points():
    fn, ct, cf = _synthetic_prohaska(0.15, 0.5, 4, 0.0, None)
    with pytest.raises(ValueError, match="6"):
        prohaska_form_factor(fn, ct, cf)


def test_prohaska_refuses_outside_fn_window():
    fn, ct, cf = _synthetic_prohaska(0.15, 0.5, 8, 0.0, None)
    fn = fn.copy()
    fn[-1] = 0.25
    with pytest.raises(ValueError, match="0.10"):
        prohaska_form_factor(fn, ct, cf)


def test_prohaska_refuses_ill_conditioned():
    fn = np.full(8, 0.15)
    cf = np.full(8, 3e-3)
    ct = np.full(8, 4e-3)
    with pytest.raises(ValueError, match="condition"):
        prohaska_form_factor(fn, ct, cf)


# ---------------------------------------------------------------- review r1: citations


def test_transfer_emits_citation_sidecar_by_default():
    r = transfer_model_to_ship(ct_model=4.0e-3, re_model=1e7, re_ship=1e9, form_factor_k=0.2,
                               fn_model=0.25, fn_ship=0.25, repo_root=CIT_ROOT)
    assert len(r.citations) == 1
    assert r.citations[0].code_id == "EN400"
    assert r.citations[0].source_sibling == "generic"


def test_transfer_procedure_citation_is_explicitly_unresolved():
    r = _forward(0.2)
    rec = [u for u in r.unresolved_citations if u.source_id == "ITTC-7.5-02-03-01.4"]
    assert len(rec) == 1
    assert rec[0].status == "unresolved-in-registry"
    assert rec[0].publisher == "ITTC"
    assert rec[0].clause


def test_transfer_citation_fail_closed_on_missing_page(tmp_path):
    from digitalmodel.citations import CitationResolutionError

    with pytest.raises(CitationResolutionError) as exc:
        transfer_model_to_ship(ct_model=4.0e-3, re_model=1e7, re_ship=1e9, form_factor_k=0.2,
                               fn_model=0.25, fn_ship=0.25, repo_root=tmp_path)
    assert exc.value.code_id == "EN400" and exc.value.reason == "page_missing"
    with pytest.raises(CitationResolutionError):
        transfer_ship_to_model(ct_ship=2.2e-3, re_model=1e7, re_ship=1e9, form_factor_k=0.2,
                               fn_model=0.25, fn_ship=0.25, repo_root=tmp_path)


def test_transfer_citation_standalone_degrades_with_warning(monkeypatch):
    from digitalmodel.citations.schema import CitationResolutionError as _CRE
    from digitalmodel.naval_architecture import resistance as res

    def _unconfigured(*_a, **_k):
        raise _CRE(code_id="EN400", wiki_path="wikis/...", reason="resolver_unconfigured:test")

    monkeypatch.setattr(res, "get_en400_reference", _unconfigured, raising=False)
    monkeypatch.setattr(res, "_EN400_STANDALONE_WARNED", False)
    with pytest.warns(RuntimeWarning, match="standalone"):
        r = transfer_model_to_ship(ct_model=4.0e-3, re_model=1e7, re_ship=1e9,
                                   form_factor_k=0.2, fn_model=0.25, fn_ship=0.25)
    assert r.citations == []
    assert r.ct_ship == pytest.approx(2.236735e-3, abs=5e-10)
    assert any(u.status == "unresolved-in-registry" for u in r.unresolved_citations)


def test_transfer_explicit_opt_out_is_recorded():
    r = transfer_model_to_ship(ct_model=4.0e-3, re_model=1e7, re_ship=1e9, form_factor_k=0.2,
                               fn_model=0.25, fn_ship=0.25, cite=False)
    assert r.citations == []
    assert r.cited is False
    assert any("citation" in s.lower() for s in r.omitted_corrections)
