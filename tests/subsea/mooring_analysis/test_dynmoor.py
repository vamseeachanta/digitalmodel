"""Validation of the legacy DYNMOOR static + passing-ship port.

Sources of truth
----------------
1. **Legacy compiled binary**: golden values below were produced by running
   the original ``DynmoorMult.exe`` (DYNMOOR v6.10, March 2007, Compaq
   Visual Fortran Win32 build) on four fully synthetic input decks
   (round-number barge/ship, line and passing-ship parameters invented for
   this validation; no project data). The frozen numbers are exactly what
   the binary printed to its .OUT/.MOR/.PAS files:

   * case A - 4 x single-segment chain to drag anchors (``CATINB`` table,
     pretension summary);
   * case B - 4 x chain-wire-wire with a buoy (``CATNRY`` table);
   * case C - parallel passing ship (``PARSHIP``, Wang 1975), .PAS force
     history;
   * case D - perpendicular passing ship (``PERSHIP``, Kriebel 2006), .PAS
     force history.

2. **Transliteration oracles**: line-by-line Python transliterations of
   the Fortran arithmetic (kept below with the original variable names),
   compared to the library at rel 1e-12.

Tolerances
----------
The legacy binary computes in ``REAL*4`` and prints rounded values; the
port computes in float64:

* catenary tables / pretension summary: ``0.011 + 6e-5 * |value|``
  (print rounding + single precision), except the ``CATNRY`` distance and
  suspended-length columns which use ``0.011 + 6e-4 * |value|`` - the
  Newton iteration's stopping test ``|DXI/XI| < 5e-4`` makes the accepted
  root precision-dependent (a float32 re-run of the loop reproduces the
  binary's numbers, confirming the difference is precision, not logic);
* passing-ship histories: ``0.06 + 1e-4 * |value|`` (F10.1 print rounding
  + single-precision Simpson sums);
* library vs transliteration oracle: rel 1e-12 (same arithmetic in the
  same order).
"""

import math

import pytest

from digitalmodel.subsea.mooring_analysis.dynmoor import (
    DynmoorLine,
    DynmoorLineType,
    DynmoorSegment,
    Fender,
    WindArea,
    anchor_holding_power,
    anchor_pull,
    build_line_table,
    build_wind_model,
    fender_forces,
    interp_table,
    parship_forces,
    passing_ship_sweep,
    pership_forces,
    restoring_force,
    solve_pretension,
    static_wind_force,
    wind_height_coefficient,
)

REL_ORACLE = 1e-12


def _tol_exe(value: float, rel: float = 6e-5) -> float:
    """Tolerance vs a value printed by the legacy binary."""
    return 0.011 + rel * abs(value)


def _tol_pas(value: float) -> float:
    return 0.06 + 1e-4 * abs(value)


# ---------------------------------------------------------------------------
# Synthetic validation cases (same round-number decks fed to the binary)
# ---------------------------------------------------------------------------

CASE_A_TYPE = DynmoorLineType(
    segment1=DynmoorSegment("chain", 1000.0, 60.0, 10.0, 3000.0, 500.0, 2.5),
    water_depth_ft=100.0,
    bottom_friction_percent=2.0,
    anchor_weight_kips=10.0,
    anchor_mud_efficiency=1.0,
    anchor_sand_efficiency=1.5,
)
CASE_A_LINES = [
    DynmoorLine(1, 200.0, -50.0, 300.0, 15.0),
    DynmoorLine(1, 200.0, 50.0, 60.0, 15.0),
    DynmoorLine(1, -200.0, 50.0, 120.0, 15.0),
    DynmoorLine(1, -200.0, -50.0, 240.0, 15.0),
]

CASE_B_TYPE = DynmoorLineType(
    segment1=DynmoorSegment("chain", 300.0, 75.0, 25.0, 2500.0, 900.0, 3.0),
    segment2=DynmoorSegment("wire", 1500.0, 8.0, 2.0, 13000.0, 350.0, 2.0),
    segment3=DynmoorSegment("wire", 200.0, 8.0, 2.0, 13000.0, 350.0, 2.0),
    water_depth_ft=350.0,
    bottom_friction_percent=1.0,
    anchor_weight_kips=20.0,
    anchor_mud_efficiency=1.0,
    anchor_sand_efficiency=1.0,
    buoy_buoyancy_kips=10.0,
)
CASE_B_LINE = DynmoorLine(1, 250.0, -60.0, 315.0, 20.0)

# ---------------------------------------------------------------------------
# Frozen golden values printed by DynmoorMult.exe (synthetic decks)
# ---------------------------------------------------------------------------

# (row, dist-to-anchor ft, H kips, T kips, suspended ft, vertical anchor kips)
CASE_A_CATENARY = [
    (1, 900.0, 0.0, 5.22, 100.0, 0.0),
    (5, 951.05, 3.33, 8.55, 150.88, 0.0),
    (10, 975.45, 16.87, 22.09, 273.18, 0.0),
    (15, 984.81, 40.82, 46.04, 407.9, 0.0),
    (20, 990.21, 75.18, 80.4, 545.93, 0.0),
    (25, 994.23, 119.95, 125.17, 685.26, 0.0),
    (26, 994.95, 130.15, 135.37, 713.21, 0.0),
    (30, 997.73, 175.14, 180.36, 825.24, 0.0),
    (35, 1001.11, 240.73, 245.95, 965.58, 0.0),
    (40, 1004.43, 316.74, 322.02, 1000.0, 5.84),
    (45, 1007.74, 403.17, 408.65, 1000.0, 14.51),
    (50, 1011.21, 500.0, 505.81, 1000.0, 24.24),
]
# (PT kips, TI pct, hor. dist ft, X-anchor ft, Y-anchor ft, fairlead deg)
CASE_A_PRETENSION = (75.0, 15.0, 989.6, 694.8, -907.0, 21.5)

CASE_B_CATENARY = [
    (1, 1650.0, 0.0, 2.23, 350.0, 0.0),
    (5, 1855.04, 2.33, 4.56, 815.82, 0.0),
    (10, 1930.85, 11.81, 14.03, 1392.66, 0.0),
    (15, 1957.94, 28.57, 30.76, 1728.43, 0.0),
    (20, 1965.85, 52.62, 55.07, 1802.35, 0.0),
    (25, 1970.68, 83.97, 86.86, 1894.61, 0.0),
    (26, 1971.59, 91.11, 94.11, 1915.01, 0.0),
    (30, 1975.44, 122.59, 126.04, 2000.0, 0.16),
    (35, 1979.73, 168.51, 172.62, 2000.0, 8.3),
    (40, 1984.04, 221.72, 226.62, 2000.0, 17.75),
    (45, 1988.46, 282.22, 288.04, 2000.0, 28.49),
    (50, 1993.41, 350.0, 356.86, 2000.0, 40.53),
]
CASE_B_PRETENSION = (70.0, 20.0, 1968.4, 1641.8, -1451.9, 15.82)

# Passing-ship sweeps: barge LBP 600 ft, beam 100 ft, displacement 30000
# kips, water depth 50 ft; passing ship 650 ft; DIR = -1, 6 kn, current
# 0.5 kn at 0 deg; parallel: SSEP 300 ft, section areas 3000/3200 ft^2;
# perpendicular: SSEP 800 ft, draft 35 ft, displacement 140000 kips.
# (x/L, surge kips, sway kips, yaw kip-ft) as printed to the .PAS file.
CASE_C_PAS = [
    (2.0, -0.9, -1.6, -92.7),
    (1.662, -1.3, -3.3, -218.0),
    (1.324, -1.1, -7.4, -512.0),
    (0.987, 2.5, -13.8, -578.6),
    (0.649, 11.6, -8.2, 1632.5),
    (0.311, 13.6, 20.2, 3282.6),
    (-0.027, -1.5, 37.4, -390.4),
    (-0.364, -14.5, 15.1, -3367.7),
    (-0.702, -10.2, -10.8, -1134.0),
    (-1.04, -1.5, -13.0, 662.4),
    (-1.378, 1.2, -6.5, 453.4),
    (-1.715, 1.2, -2.9, 189.7),
    (-1.986, 1.0, -1.6, 96.0),
]
CASE_D_PAS = [
    (2.0, 2.0, -19.6, 280.8),
    (1.662, 2.6, 2.0, 123.0),
    (1.324, 4.2, 2.2, 469.1),
    (0.987, 13.3, 12.0, 464.5),
    (0.649, 7.6, 92.0, -4345.2),
    (0.311, -26.4, 109.8, -5718.0),
    (-0.027, -42.3, -66.8, 3145.1),
    (-0.364, -15.7, -156.3, 10137.8),
    (-0.702, 10.9, -56.5, 2265.3),
    (-1.04, 7.3, -66.8, -1071.7),
    (-1.378, 8.7, -11.6, -183.8),
    (-1.715, 8.3, 66.8, -1156.9),
    (-1.986, 4.4, 31.3, -1027.1),
]


def _sweep_args(mode: str) -> dict:
    args = dict(
        mode=mode,
        nsteps=300,
        start_step=10,
        direction=-1.0,
        speed_knots=6.0,
        moored_length_ft=600.0,
        moored_beam_ft=100.0,
        moored_displacement_kips=30000.0,
        passing_length_ft=650.0,
        passing_beam_ft=100.0,
        water_depth_ft=50.0,
        current_knots=0.5,
        current_direction_deg=0.0,
    )
    if mode == "parallel":
        args.update(
            separation_ft=300.0,
            moored_midship_area_ft2=3000.0,
            passing_midship_area_ft2=3200.0,
        )
    else:
        args.update(
            separation_ft=800.0,
            passing_draft_ft=35.0,
            passing_displacement_kips=140000.0,
        )
    return args


# ---------------------------------------------------------------------------
# Golden tests vs the legacy binary
# ---------------------------------------------------------------------------


class TestCatinbVsLegacyBinary:
    def test_catenary_table(self):
        table = build_line_table(CASE_A_TYPE)
        assert table.model == "catinb"
        for row, u, h, t, susp, vp in CASE_A_CATENARY:
            j = row - 1
            assert table.u[j] == pytest.approx(u, abs=_tol_exe(u))
            assert table.h[j] / 1000.0 == pytest.approx(h, abs=_tol_exe(h))
            assert table.t[j] / 1000.0 == pytest.approx(t, abs=_tol_exe(t))
            assert table.total_length_ft - table.z[j] == pytest.approx(
                susp, abs=_tol_exe(susp)
            )
            assert table.p[j] / 1000.0 == pytest.approx(vp, abs=_tol_exe(vp))

    def test_pretension_summary(self):
        table = build_line_table(CASE_A_TYPE)
        pt, ti, ustar, xan, yan, beta = CASE_A_PRETENSION
        sol = solve_pretension(CASE_A_LINES[0], CASE_A_TYPE, table)
        assert sol.pretension_lb / 1000.0 == pytest.approx(pt, abs=0.05)
        assert sol.horizontal_distance_ft == pytest.approx(ustar, abs=0.055)
        assert sol.anchor_x_ft == pytest.approx(xan, abs=0.07)
        assert sol.anchor_y_ft == pytest.approx(yan, abs=0.07)
        assert sol.fairlead_angle_deg == pytest.approx(beta, abs=0.006)

    def test_anchor_holding_power(self):
        # HMUD = AEF*WA0, HSAND = SEF*WA0 (binary prints 10 and 15 kips)
        assert anchor_holding_power(CASE_A_TYPE) == (10.0, 15.0)


class TestCatnryVsLegacyBinary:
    def test_catenary_table(self):
        table = build_line_table(CASE_B_TYPE)
        assert table.model == "catnry"
        for row, u, h, t, susp, vp in CASE_B_CATENARY:
            j = row - 1
            # distance/suspended columns: Newton-acceptance is REAL*4
            # precision sensitive (see module docstring)
            assert table.u[j] == pytest.approx(u, abs=_tol_exe(u, rel=6e-4))
            assert table.h[j] / 1000.0 == pytest.approx(h, abs=_tol_exe(h))
            assert table.t[j] / 1000.0 == pytest.approx(t, abs=_tol_exe(t))
            assert table.total_length_ft - table.z[j] == pytest.approx(
                susp, abs=_tol_exe(susp, rel=6e-4)
            )
            assert table.p[j] / 1000.0 == pytest.approx(vp, abs=_tol_exe(vp))

    def test_pretension_summary(self):
        table = build_line_table(CASE_B_TYPE)
        pt, ti, ustar, xan, yan, beta = CASE_B_PRETENSION
        sol = solve_pretension(CASE_B_LINE, CASE_B_TYPE, table)
        assert sol.pretension_lb / 1000.0 == pytest.approx(pt, abs=0.05)
        assert sol.horizontal_distance_ft == pytest.approx(ustar, abs=1.3)
        assert sol.anchor_x_ft == pytest.approx(xan, abs=1.0)
        assert sol.anchor_y_ft == pytest.approx(yan, abs=1.0)
        assert sol.fairlead_angle_deg == pytest.approx(beta, abs=0.02)


class TestPassingShipVsLegacyBinary:
    def test_parallel_sweep(self):
        samples = {
            round(s.x_over_l, 3): s for s in passing_ship_sweep(**_sweep_args("parallel"))
        }
        assert len(samples) == 237  # binary wrote 237 .PAS rows
        for x, surge, sway, yaw in CASE_C_PAS:
            s = samples[x]
            assert s.surge_lb / 1000.0 == pytest.approx(surge, abs=_tol_pas(surge))
            assert s.sway_lb / 1000.0 == pytest.approx(sway, abs=_tol_pas(sway))
            assert s.yaw_lb_ft / 1000.0 == pytest.approx(yaw, abs=_tol_pas(yaw))

    def test_perpendicular_sweep(self):
        samples = {
            round(s.x_over_l, 3): s
            for s in passing_ship_sweep(**_sweep_args("perpendicular"))
        }
        assert len(samples) == 237
        for x, surge, sway, yaw in CASE_D_PAS:
            s = samples[x]
            assert s.surge_lb / 1000.0 == pytest.approx(surge, abs=_tol_pas(surge))
            assert s.sway_lb / 1000.0 == pytest.approx(sway, abs=_tol_pas(sway))
            assert s.yaw_lb_ft / 1000.0 == pytest.approx(yaw, abs=_tol_pas(yaw))


# ---------------------------------------------------------------------------
# Transliteration oracles (original Fortran variable names and flow)
# ---------------------------------------------------------------------------


def _catinb_oracle(BRKM, S1, W, EA, ZA, ALPHA, FRIC):
    """Subroutine CATINB, line by line (no nonlinear spring)."""
    N = 50
    FAC = N - 1.0
    BRKMP = BRKM * 1000.0
    CC = math.cos(ALPHA)
    SS = math.sin(ALPHA)
    C1 = SS / CC
    FACT = SS + CC * FRIC
    if EA == 0.0:
        EA = 1.0e20
    FUNC1 = lambda A: math.sqrt(1.0 + A * A)
    FUNC2 = lambda A: math.log(A + FUNC1(A))
    FUNC3 = lambda A, B, C, D: (
        (A + B * math.sqrt(1.0 + 4.0 * D * D / (C * C * (B * B - A * A)))) * C / 2.0
    )
    H = [0.0] * (N + 1)
    T = [0.0] * (N + 1)
    U = [0.0] * (N + 1)
    HA = [0.0] * (N + 1)
    Z = [0.0] * (N + 1)
    P = [0.0] * (N + 1)
    for J in range(2, N + 1):
        H[J] = BRKMP * ((J - 1.0) / FAC) ** 2
        C = H[J] / W
        HH = H[J]
        C2 = -C * FUNC1(C1)
        C3 = -C * FUNC2(C1)
        C4 = ZA - S1 * SS - C2
        C5 = 1.0 - SS * SS
        C6 = 2.0 * (C * C1 - C4 * SS)
        C7 = C**2 * (1.0 + C1 * C1) - C4**2
        S = (-C6 + math.sqrt(C6**2 - 4.0 * C5 * C7)) / (2.0 * C5)
        if S <= S1:
            X0 = C * FUNC2(S / C + C1) + C3
            Z[J] = S1 - S
            ZZ = Z[J]
            # STRETC inline
            if HH > FACT * W * ZZ:
                HT = 2.0 * HH / CC - FACT * W * ZZ
                UEB = HT * ZZ / (2.0 * EA) * CC
                HAN = HH / CC - FACT * W * ZZ
            else:
                Z0 = HH / (FACT * W)
                UEB = HH * Z0 / (2.0 * EA) * CC
                HAN = 0.0
            UET = HH * (S1 - ZZ) / EA + UEB
            HA[J] = HAN
            T[J] = FUNC1(S / C + C1) * HH
            XH = X0 + Z[J] * CC
            U[J] = XH + UET
            P[J] = 0.0
        else:
            Y = math.sqrt(S1**2 - ZA**2) / (2.0 * C)
            T[J] = FUNC3(ZA, S1, W, H[J])
            XH = 2.0 * C * FUNC2(Y)
            U[J] = XH + S1 * H[J] / EA
            Z[J] = 0.0
            SLOPE = (-S1 + ZA * FUNC1(Y) / Y) / (2.0 * C)
            ANGLE = SLOPE - C1
            TB = H[J] / math.cos(SLOPE)
            HA[J] = TB * math.cos(ANGLE)
            P[J] = TB * math.sin(ANGLE)
    Z[1] = (S1 - ZA) / (1.0 - SS)
    U[1] = Z[1] * CC
    T[1] = W * (ZA - Z[1] * SS)
    return H[1:], T[1:], U[1:], HA[1:], Z[1:], P[1:]


def _parship_oracle(L, Length2, sep, h, xi, U, SA1, SA2):
    """Subroutine PARSHIP + SIMPSON + Y, line by line (implicit-INTEGER L2
    and integer-division step2 preserved)."""
    rho = 1.9905
    A1 = SA1
    A2 = SA2
    pi = 3.1415279
    L2 = int(Length2)  # implicit INTEGER in blank COMMON
    NT = 50
    step = L / NT
    step2 = float(L2 // NT)  # INTEGER division
    S1dS1 = lambda x: (-8.0 * x / L**2 * A1 * x) + ((1.0 - 4.0 * x**2 / L**2) * A1)
    dS1 = lambda x: -8.0 * x / L**2 * A1
    x1 = [(i * L / NT) - L / 2.0 for i in range(NT + 1)]
    x2 = [float((i * L2) // NT) - L2 / 2.0 for i in range(NT + 1)]

    def Y(x, xx, xxx, eta):
        func1 = (-8.0 * xx / (L2**2) * A2) / (((xx - x + xxx) ** 2 + eta**2) ** 1.5)
        func2 = (
            (-8.0 * xx / (L2**2) * A2)
            * (xx - x + xxx)
            / (((xx - x + xxx) ** 2 + eta**2) ** 1.5)
        )
        return func1, func2

    def Simpson(a, b, c, eta):
        f1, f2 = Y(a, b[0], c, eta)
        sum1, sum2 = f1, f2
        f1, f2 = Y(a, b[NT], c, eta)
        sum1 += f1
        sum2 += f2
        for i in range(1, NT - 1 + 1, 2):
            f1, f2 = Y(a, b[i], c, eta)
            sum1 += 4.0 * f1
            sum2 += 4.0 * f2
        for j in range(2, NT - 2 + 1, 2):
            f1, f2 = Y(a, b[j], c, eta)
            sum1 += 2.0 * f1
            sum2 += 2.0 * f2
        return step2 / 3.0 * sum1, step2 / 3.0 * sum2

    Yaw = Sway = Surge = 0.0
    n = -10
    for _k in range(2 * abs(-10) + 1):
        eta = math.sqrt(sep**2 + 4.0 * n**2 * h**2)
        DeepSurge = DeepSway = DeepYaw = 0.0
        del_sway, del_surge = Simpson(x1[0], x2, xi, eta)
        DeepSurge += dS1(x1[0]) * del_surge
        DeepSway += dS1(x1[0]) * del_sway
        DeepYaw += S1dS1(x1[0]) * del_sway
        del_sway, del_surge = Simpson(x1[NT], x2, xi, eta)
        DeepSurge += dS1(x1[NT]) * del_surge
        DeepSway += dS1(x1[NT]) * del_sway
        DeepYaw += S1dS1(x1[NT]) * del_sway
        for i in range(1, NT - 1 + 1, 2):
            del_sway, del_surge = Simpson(x1[i], x2, xi, eta)
            DeepSurge += 4.0 * dS1(x1[i]) * del_surge
            DeepSway += 4.0 * dS1(x1[i]) * del_sway
            DeepYaw += 4.0 * S1dS1(x1[i]) * del_sway
        for j in range(2, NT - 2 + 1, 2):
            del_sway, del_surge = Simpson(x1[j], x2, xi, eta)
            DeepSurge += 2.0 * dS1(x1[j]) * del_surge
            DeepSway += 2.0 * dS1(x1[j]) * del_sway
            DeepYaw += 2.0 * S1dS1(x1[j]) * del_sway
        DeepSurge = step / 3.0 * DeepSurge * rho * (U**2) / (2.0 * pi)
        DeepSway = step / 3.0 * DeepSway * rho * (U**2) * eta / pi
        DeepYaw = step / 3.0 * DeepYaw * rho * (U**2) * eta / pi
        Surge += DeepSurge
        Sway += DeepSway / eta
        Yaw += DeepYaw / eta
        n += 1
    Sway = sep * Sway
    Yaw = sep * Yaw
    return Surge, Sway, Yaw


def _pership_oracle(MSL, PSL, PST, MDISP, PDISP, xi, SOA, VWDEP, SSEP):
    """Subroutine PERSHIP, line by line."""
    rho = 62.4
    GRAV = 32.174
    PDoverMD = PDISP / MDISP
    ToverWD = PST / VWDEP
    CLSoverSL = SSEP / MSL
    CX = 0.0043 * PDoverMD * math.exp(2.3 * ToverWD) * math.exp(
        -1.8 * (CLSoverSL - 0.5)
    )
    CY = 0.0044 * PDoverMD * math.exp(4.0 * ToverWD) * math.exp(
        -1.6 * (CLSoverSL - 0.5)
    )
    CM = 0.0007 * PDoverMD * math.exp(3.7 * ToverWD) * math.exp(
        -2.0 * (CLSoverSL - 0.5)
    )
    Fnormal = 0.5 * rho * PSL * PST * SOA**2 / GRAV
    Fnormalm = 0.5 * rho * PSL**2 * PST * SOA**2 / GRAV
    Xnormal = Fnormal * CX
    Ynormal = Fnormal * CY
    Mnormal = Fnormalm * CM
    XoverL = xi / MSL
    if -2 <= XoverL < -1:
        Surge = (
            1.419 * XoverL**4 + 9.0382 * XoverL**3 + 20.849 * XoverL**2
            + 20.649 * XoverL + 7.6018
        ) * Xnormal
        Sway = (
            -9.7937 * XoverL**5 - 69.192 * XoverL**4 - 187.36 * XoverL**3
            - 241.7 * XoverL**2 - 148.4 * XoverL - 35.033
        ) * Ynormal
        Yaw = (
            3.731 * XoverL**5 + 27.595 * XoverL**4 + 79.179 * XoverL**3
            + 109.54 * XoverL**2 + 72.6 * XoverL + 18.243
        ) * Mnormal
    elif -1 <= XoverL < 0:
        Surge = (
            6.7137 * XoverL**4 + 17.523 * XoverL**3 + 13.05 * XoverL**2
            + 1.0989 * XoverL - 0.9719
        ) * Xnormal
        Sway = (
            -20.71 * XoverL**5 - 51.384 * XoverL**4 - 37.299 * XoverL**3
            - 3.4041 * XoverL**2 + 3.2853 * XoverL - 0.3051
        ) * Ynormal
        Yaw = (
            17.298 * XoverL**5 + 52.456 * XoverL**4 + 49.428 * XoverL**3
            + 11.82 * XoverL**2 - 2.081 * XoverL + 0.2464
        ) * Mnormal
    elif 0 <= XoverL < 1:
        Surge = (
            1.1534 * XoverL**4 - 7.087 * XoverL**3 + 7.8348 * XoverL**2
            - 0.6277 * XoverL - 0.9807
        ) * Xnormal
        Sway = (
            -7.6176 * XoverL**5 + 25.002 * XoverL**4 - 24.951 * XoverL**3
            + 4.5771 * XoverL**2 + 3.3527 * XoverL - 0.2989
        ) * Ynormal
        Yaw = (
            5.7009 * XoverL**5 - 21.431 * XoverL**4 + 23.595 * XoverL**3
            - 5.5488 * XoverL**2 - 2.4947 * XoverL + 0.225
        ) * Mnormal
    elif 1 <= XoverL <= 2:
        Surge = (
            -0.0334 * XoverL**4 - 0.5831 * XoverL**3 + 3.5534 * XoverL**2
            - 6.3502 * XoverL + 3.7323
        ) * Xnormal
        Sway = (
            -0.539 * XoverL**5 + 4.2775 * XoverL**4 - 14.152 * XoverL**3
            + 23.885 * XoverL**2 - 20.221 * XoverL + 6.8103
        ) * Ynormal
        Yaw = (
            0.6101 * XoverL**5 - 5.9171 * XoverL**4 + 21.92 * XoverL**3
            - 38.921 * XoverL**2 + 33.14 * XoverL - 10.778
        ) * Mnormal
    else:
        Surge = Sway = Yaw = 0.0
    return Surge, Sway, Yaw


class TestLibraryVsOracle:
    def test_catinb_matches_oracle(self):
        seg = CASE_A_TYPE.segment1
        H, T, U, HA, Z, P = _catinb_oracle(
            BRKM=CASE_A_TYPE.brkm_kips,
            S1=seg.length_ft,
            W=seg.weight_water_lb_ft,
            EA=seg.ea_lb,
            ZA=CASE_A_TYPE.water_depth_ft,
            ALPHA=0.0,
            FRIC=0.02,
        )
        table = build_line_table(CASE_A_TYPE)
        for j in range(50):
            assert table.h[j] == pytest.approx(H[j], rel=REL_ORACLE, abs=1e-12)
            assert table.t[j] == pytest.approx(T[j], rel=REL_ORACLE)
            assert table.u[j] == pytest.approx(U[j], rel=REL_ORACLE)
            assert table.ha[j] == pytest.approx(HA[j], rel=REL_ORACLE, abs=1e-12)
            assert table.z[j] == pytest.approx(Z[j], rel=REL_ORACLE, abs=1e-12)
            assert table.p[j] == pytest.approx(P[j], rel=REL_ORACLE, abs=1e-12)

    def test_catinb_sloped_bottom_matches_oracle(self):
        lt = DynmoorLineType(
            segment1=DynmoorSegment("wire", 800.0, 12.0, 3.0, 13000.0, 400.0, 2.0),
            water_depth_ft=150.0,
            bottom_slope_deg=5.0,
            bottom_friction_percent=1.0,
        )
        seg = lt.segment1
        H, T, U, HA, Z, P = _catinb_oracle(
            BRKM=400.0,
            S1=800.0,
            W=seg.weight_water_lb_ft,
            EA=seg.ea_lb,
            ZA=150.0,
            ALPHA=5.0 * 3.1415927 / 180.0,
            FRIC=0.01,
        )
        table = build_line_table(lt)
        for j in range(50):
            assert table.u[j] == pytest.approx(U[j], rel=1e-7)
            assert table.t[j] == pytest.approx(T[j], rel=1e-7)

    def test_parship_matches_oracle(self):
        for xi in (-1200.0, -450.0, 0.0, 333.3, 1180.0):
            got = parship_forces(600.0, 650.0, 300.0, 50.0, xi, 10.5772, 3000.0, 3200.0)
            exp = _parship_oracle(600.0, 650.0, 300.0, 50.0, xi, 10.5772, 3000.0, 3200.0)
            assert got.surge_lb == pytest.approx(exp[0], rel=REL_ORACLE, abs=1e-9)
            assert got.sway_lb == pytest.approx(exp[1], rel=REL_ORACLE, abs=1e-9)
            assert got.yaw_lb_ft == pytest.approx(exp[2], rel=REL_ORACLE, abs=1e-9)

    def test_pership_matches_oracle(self):
        for xi in (-1200.0, -750.0, -25.0, 599.9, 1200.0):
            got = pership_forces(
                600.0, 30000.0, 650.0, 35.0, 140000.0, xi, 10.5772, 50.0, 800.0
            )
            exp = _pership_oracle(
                600.0, 650.0, 35.0, 30000.0, 140000.0, xi, 10.5772, 50.0, 800.0
            )
            assert got.surge_lb == pytest.approx(exp[0], rel=REL_ORACLE, abs=1e-12)
            assert got.sway_lb == pytest.approx(exp[1], rel=REL_ORACLE, abs=1e-12)
            assert got.yaw_lb_ft == pytest.approx(exp[2], rel=REL_ORACLE, abs=1e-12)


# ---------------------------------------------------------------------------
# Behaviour / property tests
# ---------------------------------------------------------------------------


class TestTablesAndStatics:
    def test_table_monotonic(self):
        table = build_line_table(CASE_A_TYPE)
        assert all(b > a for a, b in zip(table.h, table.h[1:]))
        assert all(b > a for a, b in zip(table.t, table.t[1:]))
        assert all(b > a for a, b in zip(table.u, table.u[1:]))

    def test_interp_recovers_pretension(self):
        table = build_line_table(CASE_A_TYPE)
        sol = solve_pretension(CASE_A_LINES[0], CASE_A_TYPE, table)
        hten, tens = interp_table(table, sol.horizontal_distance_ft)
        assert tens == pytest.approx(sol.pretension_lb, rel=1e-3)
        assert hten == pytest.approx(sol.horizontal_tension_lb, rel=1e-3)

    def test_interp_slack_and_off_table(self):
        table = build_line_table(CASE_A_TYPE)
        assert interp_table(table, 0.0) == (table.h[0], table.t[0])
        assert interp_table(table, 1.0e6) == (table.h[-1], table.t[-1])

    def test_anchor_pull_zero_when_grounded(self):
        table = build_line_table(CASE_A_TYPE)
        v, s = anchor_pull(table, 50_000.0)
        assert v == pytest.approx(0.0, abs=1e-9)
        assert 0.0 < s < CASE_A_TYPE.segment1.length_ft

    def test_symmetric_pattern_equilibrium(self):
        table = build_line_table(CASE_A_TYPE)
        tables = [table]
        anchors = [
            (
                solve_pretension(line, CASE_A_TYPE, table).anchor_x_ft,
                solve_pretension(line, CASE_A_TYPE, table).anchor_y_ft,
            )
            for line in CASE_A_LINES
        ]
        result = restoring_force(CASE_A_LINES, tables, anchors, 0.0, 0.0, 0.0)
        assert result.surge_lb == pytest.approx(0.0, abs=1e-6)
        assert result.sway_lb == pytest.approx(0.0, abs=1e-6)
        assert result.yaw_lb_ft == pytest.approx(0.0, abs=1e-4)
        # offsetting the vessel creates an opposing force
        offset = restoring_force(CASE_A_LINES, tables, anchors, 10.0, 0.0, 0.0)
        assert offset.surge_lb < -1000.0

    def test_broken_line_carries_no_load(self):
        table = build_line_table(CASE_A_TYPE)
        anchors = [
            (
                solve_pretension(line, CASE_A_TYPE, table).anchor_x_ft,
                solve_pretension(line, CASE_A_TYPE, table).anchor_y_ft,
            )
            for line in CASE_A_LINES
        ]
        result = restoring_force(
            CASE_A_LINES, [table], anchors, 5.0, 0.0, 0.0, broken=[1]
        )
        assert result.total_tensions_lb[0] == 0.0
        assert result.horizontal_tensions_lb[0] == 0.0

    def test_pretension_guards(self):
        table = build_line_table(CASE_A_TYPE)
        with pytest.raises(ValueError, match="too small"):
            solve_pretension(
                DynmoorLine(1, 0.0, 0.0, 0.0, 0.1), CASE_A_TYPE, table
            )
        with pytest.raises(ValueError, match="too large"):
            solve_pretension(
                DynmoorLine(1, 0.0, 0.0, 0.0, 150.0), CASE_A_TYPE, table
            )

    def test_fulcat_dispatch_and_shape(self):
        lt = DynmoorLineType(
            segment1=DynmoorSegment("wire", 300.0, 10.0, 2.0, 13000.0, 300.0, 1.5),
            water_depth_ft=60.0,
            pile_height_ft=20.0,
        )
        table = build_line_table(lt)
        assert table.model == "fulcat"
        # table tops out at 2/3 of breaking strength (FAC = 49/0.8165)
        assert table.h[-1] == pytest.approx(
            300000.0 * (49.0 / (49.0 / 0.8165)) ** 2, rel=1e-12
        )
        assert all(b > a for a, b in zip(table.u, table.u[1:]))


class TestFendersAndWind:
    FENDER = Fender(
        station_x_ft=100.0,
        initial_gap_ft=0.5,
        deflections_ft=(0.7, 2.8, 3.5, 4.2),
        forces_kips=(43.932, 175.728, 219.66, 263.592),
    )

    def test_not_engaged(self):
        fl, fmz, per = fender_forces([self.FENDER], 0.0, 0.0, side=1)
        assert fl == 0.0 and fmz == 0.0 and per == [0.0]

    def test_linear_first_segment(self):
        # deflection 0.6 ft into a 62.76 kip/ft first segment
        fl, fmz, per = fender_forces([self.FENDER], 1.1, 0.0, side=1)
        sk1 = 43.932 / 0.7
        assert per[0] == pytest.approx(sk1 * -0.6, rel=1e-12)
        assert fl == pytest.approx(1000.0 * per[0], rel=1e-12)
        assert fmz == pytest.approx(100.0 * 1000.0 * per[0], rel=1e-12)

    def test_breakpoint_continuity(self):
        eps = 1e-9
        for e in self.FENDER.deflections_ft[:3]:
            below = fender_forces([self.FENDER], 0.5 + e - eps, 0.0, side=1)[2][0]
            above = fender_forces([self.FENDER], 0.5 + e + eps, 0.0, side=1)[2][0]
            assert above == pytest.approx(below, rel=1e-6)

    def test_side_convention(self):
        fl_neg, _, _ = fender_forces([self.FENDER], -1.5, 0.0, side=-1)
        assert fl_neg != 0.0
        fl_pos, _, _ = fender_forces([self.FENDER], -1.5, 0.0, side=1)
        assert fl_pos == 0.0

    def test_wcoef_steps(self):
        assert wind_height_coefficient(10.0) == 1.0
        assert wind_height_coefficient(50.0) == 1.0
        assert wind_height_coefficient(51.0) == 1.1
        assert wind_height_coefficient(101.0) == 1.2
        assert wind_height_coefficient(900.0) == 1.8

    def test_wind_model_and_force(self):
        # deck A windage: frontal 100x20 ft, lateral 400x20 ft, Cs=1,
        # VCA 25 ft, draft 10 ft -> height 15 ft -> Ch=1
        model = build_wind_model(
            [WindArea(100.0, 20.0, 25.0, 1.0)],
            [WindArea(400.0, 20.0, 25.0, 1.0, lever_arm_ft=50.0)],
            draft_ft=10.0,
        )
        assert model.frontal_force_100kt_kips == pytest.approx(0.0338 * 2000.0)
        assert model.lateral_force_100kt_kips == pytest.approx(0.0338 * 8000.0)
        assert model.yaw_moment_100kt_kip_ft == pytest.approx(0.0338 * 8000.0 * 50.0)
        # head wind at 100 kn: pure surge, magnitude SUM1 kips (in lb)
        fx, fy, mz = static_wind_force(model, 100.0, 0.0)
        assert fx == pytest.approx(-0.0338 * 2000.0 * 1000.0)
        assert fy == pytest.approx(0.0, abs=1e-9)
        # beam wind: pure sway + yaw
        fx, fy, mz = static_wind_force(model, 50.0, 90.0)
        assert fx == pytest.approx(0.0, abs=1e-6)
        assert fy == pytest.approx(-0.0338 * 8000.0 * 250.0)
        assert mz == pytest.approx(-0.0338 * 8000.0 * 50.0 * 250.0)


class TestPassingShipBehaviour:
    def test_parship_zero_stagger_symmetry(self):
        # at zero stagger the parabolic hulls give zero surge and yaw
        f = parship_forces(600.0, 650.0, 300.0, 50.0, 0.0, 10.0, 3000.0, 3200.0)
        assert abs(f.surge_lb) < 1e-6
        assert abs(f.yaw_lb_ft) < 1e-6
        assert f.sway_lb > 0.0
        # antisymmetric surge/yaw, symmetric sway
        fwd = parship_forces(600.0, 650.0, 300.0, 50.0, 200.0, 10.0, 3000.0, 3200.0)
        aft = parship_forces(600.0, 650.0, 300.0, 50.0, -200.0, 10.0, 3000.0, 3200.0)
        assert fwd.surge_lb == pytest.approx(-aft.surge_lb, rel=1e-9)
        assert fwd.sway_lb == pytest.approx(aft.sway_lb, rel=1e-9)
        assert fwd.yaw_lb_ft == pytest.approx(-aft.yaw_lb_ft, rel=1e-9)

    def test_parship_speed_squared_scaling(self):
        f1 = parship_forces(600.0, 650.0, 300.0, 50.0, 150.0, 5.0, 3000.0, 3200.0)
        f2 = parship_forces(600.0, 650.0, 300.0, 50.0, 150.0, 10.0, 3000.0, 3200.0)
        assert f2.sway_lb == pytest.approx(4.0 * f1.sway_lb, rel=1e-12)
        assert f2.surge_lb == pytest.approx(4.0 * f1.surge_lb, rel=1e-12)

    def test_parship_integer_length_quirk(self):
        # legacy implicit-INTEGER L2 truncates the passing ship length
        a = parship_forces(600.0, 650.0, 300.0, 50.0, 150.0, 10.0, 3000.0, 3200.0)
        b = parship_forces(600.0, 650.9, 300.0, 50.0, 150.0, 10.0, 3000.0, 3200.0)
        assert a == b

    def test_pership_outside_window_is_zero(self):
        f = pership_forces(
            600.0, 30000.0, 650.0, 35.0, 140000.0, 1500.0, 10.0, 50.0, 800.0
        )
        assert f == pership_forces(
            600.0, 30000.0, 650.0, 35.0, 140000.0, -1500.0, 10.0, 50.0, 800.0
        )
        assert f.surge_lb == 0.0 and f.sway_lb == 0.0 and f.yaw_lb_ft == 0.0

    def test_separation_guards(self):
        args = _sweep_args("parallel")
        args["separation_ft"] = 90.0  # <= (100+100)/2
        with pytest.raises(ValueError, match="separation"):
            passing_ship_sweep(**args)
        args = _sweep_args("perpendicular")
        args["separation_ft"] = 250.0  # <= 600/2
        with pytest.raises(ValueError, match="separation"):
            passing_ship_sweep(**args)

    def test_sweep_window(self):
        samples = passing_ship_sweep(**_sweep_args("parallel"))
        assert all(abs(s.x_over_l) <= 2.0 for s in samples)
        assert samples[0].x_over_l == pytest.approx(2.0)


# ---------------------------------------------------------------------------
# TODO(dynmoor-time-domain): follow-up test skeleton for the un-ported
# time-domain simulation (OSCILL chain). Un-skip as the pieces land.
# ---------------------------------------------------------------------------


@pytest.mark.skip(
    reason="TODO(dynmoor-time-domain): OSCILL integrator, random-sea Fourier "
    "synthesis, Morison hull loads, wave drift, gusting and SIG statistics "
    "are not ported yet (static + passing-ship chain only)"
)
class TestTimeDomainFollowUp:
    def test_oscill_regular_wave_response(self):
        """Regular-wave (M=1) 3-DOF response vs legacy binary golden run."""

    def test_random_sea_statistics(self):
        """Average/significant/1/10/1/100-highest excursions and tensions
        (SIG) vs legacy binary golden run."""

    def test_wave_drift_options(self):
        """API / L.M. Harris / read-in / reflection-coefficient drift."""

    def test_broken_line_transient(self):
        """Line failure at TZERO with load redistribution (IFAIL/FBS)."""
