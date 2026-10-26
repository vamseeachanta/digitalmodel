# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Strategy reconciliation and independent arithmetic checks for M-T reports."""

import math
from dataclasses import replace

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode, DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.interaction import (
    ReportInteraction, report_conditions,
)


@pytest.fixture
def pipe():
    return PipeGeometry(0.3, 0.02, corrosion_allowance=0.002)


@pytest.fixture
def steel():
    return PipeMaterial("test steel", 400e6, 500e6)


@pytest.mark.parametrize("code", [DesignCode.API_STD_2RD, DesignCode.DNV_ST_F101])
@pytest.mark.parametrize("pressures", [(0.0, 0.0), (12e6, 3e6), (3e6, 12e6)])
def test_every_envelope_point_satisfies_registered_check(code, pressures, pipe, steel):
    """Reconciliation contract: both moment signs, including tension endpoints.

    Acceptance tolerance is 1e-9 on utilisation; boundary tolerance is 1e-7.
    These are cross-checks of the adapter, not independent physics anchors.
    """
    strategy = CODE_REGISTRY[code]()
    loads = DesignLoads(*pressures)
    grids = []
    for condition in report_conditions(strategy):
        interaction = ReportInteraction(strategy, pipe, steel, loads, condition)
        tensions, moments = interaction.trace(n_points=24)
        assert len(tensions) == 25
        grids.append(tensions)
        for tension, moment in zip(tensions, moments):
            for sign in (-1, 1):
                checked = strategy.run_checks(
                    pipe, steel, replace(loads, effective_tension=tension,
                                         bending_moment=sign * moment),
                    condition["factors"],
                )[interaction.check_name][0]
                assert checked <= 1.0 + 1e-9
                assert checked == pytest.approx(1.0, abs=1e-7)
    if code == DesignCode.DNV_ST_F101:
        assert grids[0] != grids[-1]


def test_method1_zero_pressure_intercepts_by_hand(pipe, steel, monkeypatch):
    """Arithmetic-only fixture with an explicitly assumed factor of 0.6.

    For a thin ring of mean radius r, dA=r*t*dtheta. Integrating dA gives
    A=2*pi*r*t; integrating y^2*dA gives I=pi*r^3*t and Z=I/r.
    The existing linear interaction at zero pressure has intercepts
    f*SMYS*A and f*SMYS*Z. This does not validate a standard's design factor.
    """
    strategy = CODE_REGISTRY[DesignCode.API_STD_2RD]()
    monkeypatch.setattr(strategy, "COMBINED_DESIGN_FACTOR", 0.6)
    condition = report_conditions(strategy)[0]
    interaction = ReportInteraction(strategy, pipe, steel, DesignLoads(), condition)
    limits = interaction.limits()
    # Hand calculation: r=(0.3-0.02)/2=0.14 m;
    # A=2*pi*0.14*0.02=0.01759291886 m^2, Z=pi*0.14^2*0.02;
    # T=0.6*400e6*A=4222.300526 kN, M=0.6*400e6*Z=295.561037 kN.m.
    assert limits["T_pos_kN"] == pytest.approx(4222.300526, abs=1e-6)
    assert limits["T_neg_kN"] == pytest.approx(-4222.300526, abs=1e-6)
    assert limits["M_allow_kNm"] == pytest.approx(295.561037, abs=1e-6)


@pytest.mark.parametrize("code", [DesignCode.API_RP_1111, DesignCode.API_RP_2RD])
def test_unregistered_combined_routes_are_screening(code, pipe, steel):
    strategy = CODE_REGISTRY[code]()
    conditions = report_conditions(strategy)
    assert len(conditions) == 1
    condition = conditions[0]
    assert "SCREENING" in condition["basis"]
    interaction = ReportInteraction(strategy, pipe, steel, DesignLoads(), condition)
    assert interaction.check_name is None
    # First principles at zero pressure and moment: axial stress = force / area.
    area = math.pi * (0.3**2 - 0.26**2) / 4
    assert interaction.utilisation(0.1 * area * steel.smys, 0) == pytest.approx(
        0.1 / condition["f_d"]
    )


@pytest.mark.parametrize("code", [DesignCode.API_RP_1111, DesignCode.API_RP_2RD])
@pytest.mark.parametrize("pressure_sign", [-1, 1])
@pytest.mark.parametrize("hoop_ratio", [0.9, 1.0, 1.1])
def test_screening_bounds_surround_stress_minimum(
    code, pressure_sign, hoop_ratio, pipe, steel,
):
    """Arithmetic-only screening fixture; 0.6 is assumed, not a code factor."""
    strategy = CODE_REGISTRY[code]()
    condition = dict(report_conditions(strategy)[0], f_d=0.6)
    allowable = 0.6 * steel.smys
    hoop = pressure_sign * hoop_ratio * allowable
    pressure = hoop * 2 * pipe.wall_thickness / pipe.inner_diameter
    loads = DesignLoads(max(pressure, 0), max(-pressure, 0))
    interaction = ReportInteraction(strategy, pipe, steel, loads, condition)
    if hoop_ratio == 1:
        # Make the origin exactly unity, independent of pressure round-trip ulps.
        interaction.hoop = hoop
    area = math.pi * (0.3**2 - 0.26**2) / 4
    # Complete the square: equivalent stress squared is
    # (axial - hoop/2)**2 + 3*hoop**2/4, with minimum at axial=hoop/2.
    centre = area * hoop / 2
    half_width = area * math.sqrt(allowable**2 - 0.75 * hoop**2)
    assert interaction.utilisation(centre, 0) == pytest.approx(
        math.sqrt(3) * hoop_ratio / 2,
    )
    limits = interaction.limits()
    assert limits["feasible"]
    assert limits["T_neg_kN"] == pytest.approx((centre - half_width) / 1e3)
    assert limits["T_pos_kN"] == pytest.approx((centre + half_width) / 1e3)
    if hoop_ratio > 1:
        assert limits["M_allow_kNm"] is None
    elif hoop_ratio == 1:
        assert limits["M_allow_kNm"] == pytest.approx(0, abs=1e-7)
    else:
        assert limits["M_allow_kNm"] > 0
    tensions, moments = interaction.trace(n_points=24)
    assert len(tensions) == len(moments) == 25
    assert tensions[0] == pytest.approx(centre - half_width)
    assert tensions[-1] == pytest.approx(centre + half_width)
    assert moments[len(moments) // 2] > 0
    for tension, moment in zip(tensions, moments):
        for sign in (-1, 1):
            assert interaction.utilisation(tension, sign * moment) <= 1 + 1e-9
            assert interaction.utilisation(tension, sign * moment) == pytest.approx(
                1, abs=1e-7,
            )


@pytest.mark.parametrize("code", [DesignCode.API_RP_1111, DesignCode.API_RP_2RD])
@pytest.mark.parametrize("pressure_sign", [-1, 1])
def test_screening_empty_domain_above_minimum(code, pressure_sign, pipe, steel):
    strategy = CODE_REGISTRY[code]()
    condition = dict(report_conditions(strategy)[0], f_d=0.6)
    # Assumed screening stress: sqrt(3)/2 * 1.2 exceeds the unity limit.
    hoop = pressure_sign * 1.2 * 0.6 * steel.smys
    pressure = hoop * 2 * pipe.wall_thickness / pipe.inner_diameter
    loads = DesignLoads(max(pressure, 0), max(-pressure, 0))
    interaction = ReportInteraction(strategy, pipe, steel, loads, condition)
    limits = interaction.limits()
    assert not limits["feasible"]
    assert limits["T_pos_kN"] is None
    assert limits["T_neg_kN"] is None
    assert limits["M_allow_kNm"] is None
    assert interaction.trace() == ([], [])


@pytest.mark.parametrize("code", [DesignCode.API_RP_1111, DesignCode.API_RP_2RD])
@pytest.mark.parametrize("vary", ["T", "M"])
def test_limit_rejects_an_infeasible_start(code, vary, pipe, steel):
    strategy = CODE_REGISTRY[code]()
    condition = report_conditions(strategy)[0]
    interaction = ReportInteraction(strategy, pipe, steel, DesignLoads(), condition)
    with pytest.raises(ValueError, match="No feasible starting point"):
        interaction.limit(vary, fixed=1e12)


@pytest.mark.parametrize("hoop", [float("nan"), float("inf"), -float("inf")])
def test_screening_nonfinite_stress_never_emits_curve(hoop, pipe, steel):
    strategy = CODE_REGISTRY[DesignCode.API_RP_1111]()
    condition = report_conditions(strategy)[0]
    interaction = ReportInteraction(strategy, pipe, steel, DesignLoads(), condition)
    interaction.hoop = hoop
    assert not interaction.limits()["feasible"]
    assert interaction.trace() == ([], [])


@pytest.mark.parametrize("code", [DesignCode.API_RP_1111, DesignCode.API_RP_2RD])
@pytest.mark.parametrize("pressure_sign", [-1, 1])
def test_report_retains_shifted_screening_domain(code, pressure_sign, pipe, steel, monkeypatch):
    import inspect
    import digitalmodel.structural.analysis.wall_thickness_mt_report as report
    import plotly.graph_objects as go

    monkeypatch.setattr(go.Figure, "to_html", lambda *args, **kwargs: "<div>plot</div>")
    captured = {}
    original = report._build_html
    def capture_html(*args, **kwargs):
        captured.update(inspect.signature(original).bind(*args, **kwargs).arguments)
        return original(*args, **kwargs)
    monkeypatch.setattr(report, "_build_html", capture_html)
    condition = report_conditions(CODE_REGISTRY[code]())[0]
    hoop = pressure_sign * 1.1 * condition["f_d"] * steel.smys
    pressure = hoop * 2 * pipe.wall_thickness / pipe.inner_diameter
    html = report.generate_mt_report(
        pipe, steel, max(pressure, 0), max(-pressure, 0), code,
    )
    allowable = condition["f_d"] * steel.smys
    area = math.pi * (0.3**2 - 0.26**2) / 4
    centre = area * hoop / 2
    half_width = area * math.sqrt(allowable**2 - 0.75 * hoop**2)
    limits = captured["capacity_limits"][0]
    assert limits["T_pos_kN"] == pytest.approx((centre + half_width) / 1e3)
    assert limits["T_neg_kN"] == pytest.approx((centre - half_width) / 1e3)
    assert pressure_sign * limits["T_pos_kN"] > 0
    assert pressure_sign * limits["T_neg_kN"] > 0
    assert limits["M_allow_kNm"] is None
    assert captured["pressure_sensitivity"][0]["limits"][0]["M_allow_kNm"] is None
    # Both the limits table and the baseline pressure-sensitivity row retain
    # the interval, while distinguishing an absent zero-tension intercept.
    assert html.count("Unavailable at zero tension") == (2 if pressure_sign > 0 else 4)
    assert "No feasible curve</td>" not in html
    assert "no admissible origin" not in html
    assert html.count("T<sub>upper</sub> (kN)") == 2
    assert html.count("T<sub>lower</sub> (kN)") == 2


@pytest.mark.parametrize("code", [DesignCode.API_STD_2RD, DesignCode.DNV_ST_F101])
def test_excess_pressure_has_no_feasible_curve(code, pipe, steel):
    strategy = CODE_REGISTRY[code]()
    condition = report_conditions(strategy)[0]
    interaction = ReportInteraction(strategy, pipe, steel, DesignLoads(1e12), condition)
    assert interaction.trace() == ([], [])
    assert not interaction.limits()["feasible"]


def test_conditions_follow_strategy_factors(pipe, steel):
    strategy = CODE_REGISTRY[DesignCode.API_STD_2RD]()
    assert report_conditions(strategy)[0]["f_d"] == strategy.COMBINED_DESIGN_FACTOR
    strategy.COMBINED_DESIGN_FACTOR = 0.61
    condition = report_conditions(strategy)[0]
    assert condition["f_d"] == 0.61
    rp = CODE_REGISTRY[DesignCode.API_RP_2RD]()
    assert report_conditions(rp)[0]["f_d"] == rp.C_A
    rp1111 = CODE_REGISTRY[DesignCode.API_RP_1111]()
    assert report_conditions(rp1111)[0]["f_d"] == rp1111.get_edition_factors()["f_d"]
    for condition in report_conditions(CODE_REGISTRY[DesignCode.DNV_ST_F101]()):
        factors = condition["factors"]
        assert condition["f_d"] == 1 / (factors.gamma_sc * factors.gamma_m)


@pytest.mark.parametrize("code,key,wall_key", [
    (DesignCode.ASME_B31_4, "burst", "t_eff"),
    (DesignCode.PD_8010_2, "hoop_stress", "t_min"),
    (DesignCode.ISO_13623, "pressure_containment", "t"),
    (DesignCode.DNV_ST_F101, "pressure_containment", "t1"),
    (DesignCode.DNV_ST_F201, "pressure_containment", "t1"),
])
def test_pressure_wall_allowance_is_deducted_once(code, key, wall_key, pipe, steel):
    """Arithmetic-only fixture; 0.1 fabrication tolerance is explicitly assumed."""
    strategy = CODE_REGISTRY[code]()
    pipe = replace(pipe, fabrication_tolerance=0.1)
    result = strategy.run_checks(pipe, steel, DesignLoads(5e6), DesignFactors())[key]
    # Hand subtraction: 0.020-0.002=0.018 m; reduction: 0.018*(1-0.1)=0.0162 m.
    expected = 0.018
    if code in (DesignCode.DNV_ST_F101, DesignCode.DNV_ST_F201):
        expected = 0.0162
    assert result[1][wall_key] == pytest.approx(expected)


@pytest.mark.parametrize("code", [DesignCode.API_STD_2RD, DesignCode.DNV_ST_F101,
                                  DesignCode.API_RP_1111, DesignCode.API_RP_2RD])
def test_report_uses_one_criterion_everywhere(code, pipe, steel, monkeypatch):
    """Capture report inputs and Plotly traces without relying on rounded HTML."""
    import inspect
    import digitalmodel.structural.analysis.wall_thickness_mt_report as report
    import plotly.graph_objects as go

    plots = []
    def capture_plot(figure, **kwargs):
        plots.append(figure.to_plotly_json())
        return "<div>captured plot</div>"
    monkeypatch.setattr(go.Figure, "to_html", capture_plot)
    captured = {}
    original = report._build_html
    def capture_html(*args, **kwargs):
        captured.update(inspect.signature(original).bind(*args, **kwargs).arguments)
        return original(*args, **kwargs)
    monkeypatch.setattr(report, "_build_html", capture_html)
    html = report.generate_mt_report(pipe, steel, 12e6, 3e6, code)
    strategy = CODE_REGISTRY[code]()
    for condition in captured["conditions"]:
        interaction = ReportInteraction(strategy, pipe, steel, DesignLoads(12e6, 3e6), condition)
        tensions, moments = captured["envelopes"][condition["name"]]
        for tension, moment in zip(tensions, moments):
            for sign in (-1, 1):
                assert interaction.utilisation(tension, sign * moment) <= 1 + 1e-9
        assert any(
            list(trace.get("x", []))[:len(tensions)] == [v / 1e3 for v in tensions]
            and list(trace.get("y", []))[:len(moments)] == [v / 1e3 for v in moments]
            for plot in plots for trace in plot["data"] if trace["type"] == "scatter"
        )
    first = ReportInteraction(strategy, pipe, steel, DesignLoads(12e6, 3e6),
                              captured["conditions"][0])
    for point in captured["key_points"]:
        assert point["util"] == pytest.approx(first.utilisation(
            point["T_kN"] * 1e3, point["M_kNm"] * 1e3))
    contour = next(trace for plot in plots for trace in plot["data"]
                   if trace["type"] == "contour")
    for row in (0, len(contour["y"]) // 2, len(contour["y"]) - 1):
        for col in (0, len(contour["x"]) // 2, len(contour["x"]) - 1):
            assert contour["z"][row][col] == pytest.approx(first.utilisation(
                contour["x"][col] * 1e3, contour["y"][row] * 1e3))
    for case in captured["pressure_sensitivity"]:
        for condition in captured["conditions"]:
            interaction = ReportInteraction(strategy, pipe, steel,
                DesignLoads(case["pi_MPa"] * 1e6, 3e6), condition)
            for tension, moment in zip(*case["envelopes"][condition["name"]]):
                for sign in (-1, 1):
                    assert interaction.utilisation(tension, sign * moment) <= 1 + 1e-9
    assert "Pressure basis: ASSUMED gauge" in html
    if code in (DesignCode.API_RP_1111, DesignCode.API_RP_2RD):
        assert "SCREENING" in html
        assert "Capacity Limits" not in html
    if code == DesignCode.DNV_ST_F101:
        assert "simplified form; not checked against the standard's text" in html


def test_table_pairs_each_conditions_own_grid():
    from digitalmodel.structural.analysis.wall_thickness_mt_report import _build_envelope_table
    table = _build_envelope_table({
        "first": ([-1000, 1000], [10e3, 20e3]),
        "second": ([-3000, 0, 3000], [30e3, 40e3, 50e3]),
    }, [{"name": "first"}, {"name": "second"}])
    assert "<tr><td>second</td><td>-3.0</td><td>30.0</td></tr>" in table
    assert "<tr><td>second</td><td>3.0</td><td>50.0</td></tr>" in table
    assert "<tr><td>first</td><td>-1.0</td><td>10.0</td></tr>" in table


def test_unreviewed_registration_does_not_enable_rp1111_claim(pipe, steel):
    strategy = CODE_REGISTRY[DesignCode.API_RP_1111]()
    strategy.check_names = [*strategy.check_names, "combined"]
    condition = report_conditions(strategy)[0]
    assert "SCREENING" in condition["basis"]
    assert ReportInteraction(strategy, pipe, steel, DesignLoads(), condition).check_name is None


def test_no_feasible_curve_is_reported_without_zero_capacity_claim(pipe, steel, monkeypatch):
    import digitalmodel.structural.analysis.wall_thickness_mt_report as report
    import plotly.graph_objects as go
    monkeypatch.setattr(go.Figure, "to_html", lambda *args, **kwargs: "<div>plot</div>")
    html = report.generate_mt_report(pipe, steel, 1e12, 0, DesignCode.API_STD_2RD)
    assert "No feasible curve at this pressure" in html
    assert "No feasible curve</td>" in html
