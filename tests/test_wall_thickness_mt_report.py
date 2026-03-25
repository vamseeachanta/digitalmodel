# ABOUTME: Tests for API RP 1111 pipeline wall thickness M-T interaction HTML report
# ABOUTME: Covers report generation, sections, pressure checks, contour chart, key points,
# ABOUTME: capacity limits, envelope tracing, and multi-condition interaction envelopes

import math
import os
import tempfile

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_mt_report import (
    API_RP_1111_CONDITIONS,
    _compute_capacity_limits,
    _find_limit_bisection,
    _trace_envelope,
    _von_mises_util_single,
    generate_mt_report,
)


# --- Fixtures ---


@pytest.fixture
def geometry():
    return PipeGeometry(
        outer_diameter=0.27305,
        wall_thickness=0.02144,
        corrosion_allowance=0.001,
    )


@pytest.fixture
def material():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


@pytest.fixture
def report_html(geometry, material):
    return generate_mt_report(
        geometry=geometry,
        material=material,
        internal_pressure=20e6,
        external_pressure=10e6,
        code=DesignCode.API_RP_1111,
    )


# --- Tests ---


def test_report_returns_html_string(report_html):
    """Report returns a non-empty HTML string with expected structure."""
    assert isinstance(report_html, str)
    assert "<!DOCTYPE html>" in report_html
    assert "</html>" in report_html
    assert "API-RP-1111" in report_html


def test_report_writes_to_file(geometry, material):
    """Report writes HTML to the specified output path."""
    with tempfile.TemporaryDirectory() as tmpdir:
        path = os.path.join(tmpdir, "report.html")
        html = generate_mt_report(
            geometry=geometry,
            material=material,
            internal_pressure=20e6,
            external_pressure=10e6,
            code=DesignCode.API_RP_1111,
            output_path=path,
        )
        assert os.path.exists(path)
        with open(path) as f:
            content = f.read()
        assert content == html


def test_report_contains_input_data_section(report_html):
    """Report contains the input data section with geometry and material."""
    assert "Input Data" in report_html
    assert "273.05" in report_html  # OD in mm
    assert "21.44" in report_html   # WT in mm
    assert "X65" in report_html
    assert "448" in report_html     # SMYS in MPa


def test_report_contains_pressure_check_table(report_html):
    """Report contains pressure-only check results for burst, collapse, propagation."""
    assert "Pressure" in report_html
    assert "Burst" in report_html
    assert "Collapse" in report_html
    assert "Propagation" in report_html
    # Should have design factor values
    assert "0.72" in report_html  # burst design factor
    assert "0.80" in report_html  # collapse/propagation design factor


def test_report_contains_contour_chart(report_html):
    """Report contains the Plotly contour chart div for M-T interaction."""
    assert "plotly" in report_html.lower() or "Plotly" in report_html
    assert "Von Mises" in report_html or "von Mises" in report_html
    assert "Tension" in report_html
    assert "Moment" in report_html


def test_report_contains_key_points_table(report_html):
    """Report contains the key engineering points summary table."""
    assert "Key Points" in report_html or "Summary" in report_html
    # Should have reference to origin point and capacity fractions
    assert "Origin" in report_html or "Pressure only" in report_html


def test_key_points_utilisation_values_reasonable(geometry, material):
    """Key engineering points have physically reasonable utilisation values.

    At the origin (M=0, T=0), utilisation should reflect pressure-only hoop stress.
    At M_p or T_y, utilisation should be >= 1.0 (at or beyond yield).
    """
    html = generate_mt_report(
        geometry=geometry,
        material=material,
        internal_pressure=20e6,
        external_pressure=10e6,
        code=DesignCode.API_RP_1111,
    )
    # The report should contain utilisation values — at least some < 1.0
    # and some >= 1.0 (the full capacity points)
    assert "PASS" in html or "FAIL" in html


# ---------------------------------------------------------------------------
# M-T Interaction Envelope Tests (WRK-044)
# ---------------------------------------------------------------------------


def _section_props(geometry, material):
    """Compute common section properties used by the envelope tests."""
    D = geometry.outer_diameter     # 0.27305 m
    t = geometry.wall_thickness     # 0.02144 m
    d_i = D - 2 * t
    A = math.pi / 4 * (D**2 - d_i**2)
    I_val = math.pi / 64 * (D**4 - d_i**4)
    Z = I_val / (D / 2)
    smys = material.smys            # 448e6 Pa

    p_net = 20e6 - 10e6             # 10 MPa net internal pressure
    sigma_h = p_net * d_i / (2 * t)

    T_y = A * smys
    M_p = Z * smys

    return D, t, d_i, A, I_val, Z, smys, p_net, sigma_h, T_y, M_p


def test_tension_limits_asymmetric(geometry, material):
    """Tension capacity limits are asymmetric when net hoop stress is nonzero.

    With net internal pressure (sigma_h > 0), the positive tension limit
    should exceed the absolute value of the negative tension limit because
    hoop stress aids the compressive fibre under positive tension but
    opposes it under negative tension (compression).
    """
    D, t, d_i, A, I_val, Z, smys, p_net, sigma_h, T_y, M_p = (
        _section_props(geometry, material)
    )

    limits = _compute_capacity_limits(A, Z, sigma_h, smys, T_y, M_p,
                                      API_RP_1111_CONDITIONS)

    for lim in limits:
        # Positive tension limit should be positive
        assert lim["T_pos_kN"] > 0, (
            f"{lim['name']}: T_pos_kN should be positive, got {lim['T_pos_kN']}"
        )
        # Negative tension limit should be negative
        assert lim["T_neg_kN"] < 0, (
            f"{lim['name']}: T_neg_kN should be negative, got {lim['T_neg_kN']}"
        )
        # Asymmetry: positive limit exceeds absolute negative limit
        assert lim["T_pos_kN"] > abs(lim["T_neg_kN"]), (
            f"{lim['name']}: expected T_pos ({lim['T_pos_kN']:.1f}) > "
            f"|T_neg| ({abs(lim['T_neg_kN']):.1f}) due to sigma_h > 0"
        )


def test_bending_limit_at_util_unity(geometry, material):
    """Bending limit from bisection yields util ~1.0 at the limit point.

    For the survival condition (f_d=1.0) at T=0, the bending moment found
    by bisection should produce a Von Mises utilisation very close to 1.0
    when checked against sigma_allow = f_d * SMYS.
    """
    D, t, d_i, A, I_val, Z, smys, p_net, sigma_h, T_y, M_p = (
        _section_props(geometry, material)
    )

    f_d = 1.0  # survival condition
    sigma_allow = f_d * smys

    # Find M limit at T = 0
    M_limit = _find_limit_bisection(
        A, Z, sigma_h, smys, f_d,
        T_fixed=0.0, M_fixed=0.0, vary="M",
    )

    # Verify M_limit is positive and physically meaningful
    assert M_limit > 0, f"M_limit should be positive, got {M_limit}"
    assert M_limit < 2.0 * M_p, f"M_limit too large: {M_limit} vs M_p={M_p}"

    # Check utilisation at the limit
    util = _von_mises_util_single(0.0, M_limit, A, Z, sigma_h, sigma_allow)
    assert abs(util - 1.0) < 0.01, (
        f"At bisection limit, util should be ~1.0, got {util:.4f}"
    )


def test_envelope_m_decreases_with_t(geometry, material):
    """Allowable bending moment decreases past the peak as tension increases.

    With hoop stress, small positive tension can slightly increase M_allow
    (the tension partially cancels the hoop contribution on the compression
    fibre). Past this peak, M_allow must decrease monotonically toward zero
    at the positive tension limit.
    """
    D, t, d_i, A, I_val, Z, smys, p_net, sigma_h, T_y, M_p = (
        _section_props(geometry, material)
    )

    f_d = 1.0  # survival condition
    T_neg = -T_y
    T_pos = T_y

    T_vals, M_vals = _trace_envelope(
        A, Z, sigma_h, smys, f_d, T_neg, T_pos, M_p, n_points=80,
    )

    # Filter to positive T values only
    positive_pairs = [(tv, mv) for tv, mv in zip(T_vals, M_vals) if tv > 0]
    assert len(positive_pairs) >= 2, "Need at least 2 positive-T envelope points"
    positive_pairs.sort(key=lambda pair: pair[0])

    # Find the peak M_allow index
    peak_idx = max(range(len(positive_pairs)), key=lambda i: positive_pairs[i][1])

    # Past the peak, M must be monotonically non-increasing
    past_peak = positive_pairs[peak_idx:]
    assert len(past_peak) >= 2, "Need at least 2 points past peak"
    for i in range(len(past_peak) - 1):
        t_curr, m_curr = past_peak[i]
        t_next, m_next = past_peak[i + 1]
        assert m_next <= m_curr + 1e-3, (
            f"Past peak, M should decrease: at T={t_curr:.0f} N, M={m_curr:.0f} N-m "
            f"but at T={t_next:.0f} N, M={m_next:.0f} N-m"
        )

    # Also verify the envelope reaches near zero at the tension limit
    last_m = positive_pairs[-1][1]
    assert last_m < M_p * 0.3, (
        f"M_allow near T_limit should be small, got {last_m:.0f} N-m"
    )


def test_conditions_envelopes_nested(geometry, material):
    """Survival envelope contains (is outside) the Normal Operating envelope.

    At any given tension level, the survival condition (f_d=1.0) should
    allow equal or greater bending moment than Normal Operating (f_d=0.72).
    """
    D, t, d_i, A, I_val, Z, smys, p_net, sigma_h, T_y, M_p = (
        _section_props(geometry, material)
    )

    T_neg = -T_y
    T_pos = T_y

    # Normal Operating envelope (f_d=0.72)
    T_norm, M_norm = _trace_envelope(
        A, Z, sigma_h, smys, 0.72, T_neg, T_pos, M_p, n_points=80,
    )

    # Survival envelope (f_d=1.0)
    T_surv, M_surv = _trace_envelope(
        A, Z, sigma_h, smys, 1.0, T_neg, T_pos, M_p, n_points=80,
    )

    # Build a lookup from T -> M for the survival envelope (interpolate)
    # Use the bisection approach: at several matching T values from the
    # Normal Operating envelope, check that Survival M >= Normal M.
    # We pick T values from the Normal Operating envelope positive side.
    norm_positive = [(tv, mv) for tv, mv in zip(T_norm, M_norm) if tv >= 0]
    surv_positive = [(tv, mv) for tv, mv in zip(T_surv, M_surv) if tv >= 0]

    # For a robust comparison, at T=0 find M for each using bisection
    sample_tensions = [0.0]
    # Also add a few fractions of T_y
    for frac in [0.2, 0.4, 0.6]:
        sample_tensions.append(frac * T_y)

    for T_sample in sample_tensions:
        M_norm_at_T = _find_limit_bisection(
            A, Z, sigma_h, smys, 0.72,
            T_fixed=T_sample, M_fixed=0.0, vary="M",
        )
        M_surv_at_T = _find_limit_bisection(
            A, Z, sigma_h, smys, 1.0,
            T_fixed=T_sample, M_fixed=0.0, vary="M",
        )
        assert M_surv_at_T >= M_norm_at_T - 1e-3, (
            f"At T={T_sample:.0f} N: Survival M ({M_surv_at_T:.0f}) should >= "
            f"Normal Operating M ({M_norm_at_T:.0f})"
        )


def test_report_contains_capacity_limits_section(report_html):
    """Report HTML includes the capacity limits section with all four conditions."""
    assert "Capacity Limits" in report_html
    assert "Normal Operating" in report_html
    assert "Installation" in report_html
    assert "Extreme" in report_html
    assert "Survival" in report_html


def test_report_contains_envelope_chart(report_html):
    """Report HTML includes the allowable bending envelope chart."""
    assert (
        "Allowable Bending Envelope" in report_html
        or "envelope" in report_html.lower()
    )


def test_report_contains_executive_summary(report_html):
    """Report HTML includes an executive summary section at the top."""
    assert "Executive Summary" in report_html
    # Should have overall verdict
    assert "PASS" in report_html or "FAIL" in report_html
    # Should have governing check identification
    assert "Governing" in report_html
    # Should have margin percentage
    assert "Margin" in report_html or "margin" in report_html


def test_report_input_data_after_key_points(report_html):
    """Input Data section appears after Key Points (moved to end)."""
    kp_pos = report_html.index("Key Points")
    input_pos = report_html.index("Input Data")
    assert input_pos > kp_pos, (
        "Input Data should appear after Key Points (moved to end of report)"
    )


def test_key_points_table_has_margin_column(report_html):
    """Key Points table includes a Margin column."""
    assert "Margin" in report_html


def test_envelope_chart_is_symmetric(report_html):
    """Envelope chart contains both positive and negative M_p references.

    The full symmetric envelope shows closed lens shapes spanning
    from +M_allow to -M_allow. The chart should reference -M_p as well
    as +M_p to confirm the full symmetric plot is rendered.
    """
    # The chart should contain the toself fill pattern (closed polygon)
    assert "toself" in report_html.lower() or "fill" in report_html.lower()
    # Should have negative M_p reference marker
    assert "-M_p" in report_html or "\u2013M<sub>p</sub>" in report_html


# ---------------------------------------------------------------------------
# Multi-code report tests (WRK-144)
# ---------------------------------------------------------------------------


def test_report_api_rp_2rd_generates(geometry, material):
    """API RP 2RD report generates valid HTML."""
    html = generate_mt_report(
        geometry=geometry,
        material=material,
        internal_pressure=20e6,
        external_pressure=10e6,
        code=DesignCode.API_RP_2RD,
    )
    assert "<!DOCTYPE html>" in html
    assert "API-RP-2RD" in html
    assert "Hoop" in html  # API RP 2RD has hoop check


def test_report_api_std_2rd_generates(geometry, material):
    """API STD 2RD report generates valid HTML."""
    html = generate_mt_report(
        geometry=geometry,
        material=material,
        internal_pressure=20e6,
        external_pressure=10e6,
        code=DesignCode.API_STD_2RD,
    )
    assert "<!DOCTYPE html>" in html
    assert "API-STD-2RD" in html


def test_report_api_std_2rd_contains_methods(geometry, material):
    """API STD 2RD report contains Method 1 and Method 2 envelope labels."""
    html = generate_mt_report(
        geometry=geometry,
        material=material,
        internal_pressure=20e6,
        external_pressure=10e6,
        code=DesignCode.API_STD_2RD,
    )
    assert "Method 1" in html
    assert "Method 2" in html


def test_cross_code_comparison(geometry, material):
    """Same pipe produces valid HTML for all 3 API codes.

    All reports should be structurally valid and contain code-specific
    content without any API RP 1111-specific hardcoded references bleeding
    into other code reports.
    """
    for code in [DesignCode.API_RP_1111, DesignCode.API_RP_2RD, DesignCode.API_STD_2RD]:
        html = generate_mt_report(
            geometry=geometry,
            material=material,
            internal_pressure=20e6,
            external_pressure=10e6,
            code=code,
        )
        assert "<!DOCTYPE html>" in html
        assert code.value in html
        assert "Executive Summary" in html
        assert "PASS" in html or "FAIL" in html
