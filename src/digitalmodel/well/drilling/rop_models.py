# ABOUTME: ROP prediction models — Bourgoyne-Young (8-param) and Warren (power-law)
# ABOUTME: Reference: SPE Textbook Vol 2, Chapter 5; Warren (1987) SPE-13919

"""
ROP Prediction Models
=====================

Bourgoyne-Young (1986): 8-parameter exponential model capturing formation
strength, compaction, overbalance, WOB, RPM, bit wear, and jet impact.

Warren (1987): 2-parameter power law K*(WOB/d_b)^a * RPM^b, fitted via
log-linear least squares regression from offset-well data.

Both return RopPrediction dataclasses for consistent downstream handling.
"""

import math
from dataclasses import dataclass
from typing import Sequence


@dataclass
class RopPrediction:
    """Result from any ROP model."""
    rop_ft_hr: float
    model: str


class BourgoineYoungROP:
    """
    Bourgoyne-Young 8-parameter ROP model.

    ROP = exp(a1) * f2(depth) * f3(depth) * f4(overbalance)
          * f5(WOB/d_b) * f6(RPM) * f7(bit_wear) * f8(jet_impact)

    Parameters
    ----------
    a1 : formation strength coefficient (intercept in log space)
    a2 : normal compaction coefficient (depth effect)
    a3 : under-compaction coefficient (transition zone)
    a4 : overbalance / chip hold-down coefficient
    a5 : WOB per unit bit diameter exponent
    a6 : RPM exponent
    a7 : bit tooth wear coefficient
    a8 : jet impact function coefficient
    """

    def __init__(
        self,
        a1: float,
        a2: float,
        a3: float,
        a4: float,
        a5: float,
        a6: float,
        a7: float,
        a8: float,
    ) -> None:
        self.a1 = a1
        self.a2 = a2
        self.a3 = a3
        self.a4 = a4
        self.a5 = a5
        self.a6 = a6
        self.a7 = a7
        self.a8 = a8

    def predict(
        self,
        depth_ft: float,
        wob_klb: float,
        rpm: float,
        bit_dia_in: float,
        overbalance_kpsi: float,
        bit_wear: float,
    ) -> RopPrediction:
        """
        Predict ROP using the Bourgoyne-Young model.

        Parameters
        ----------
        depth_ft        : measured depth, ft
        wob_klb         : weight on bit, kilo-lbf
        rpm             : rotary speed, RPM
        bit_dia_in      : bit diameter, inches
        overbalance_kpsi: overbalance pressure (mud weight - pore pressure), kpsi
        bit_wear        : fractional bit wear 0 (new) to 1 (fully worn)

        Returns
        -------
        RopPrediction with rop_ft_hr and model="bourgoyne_young"
        """
        if wob_klb < 0.0:
            raise ValueError("wob_klb must be >= 0")
        if rpm < 0.0:
            raise ValueError("rpm must be >= 0")
        if not 0.0 <= bit_wear <= 1.0:
            raise ValueError("bit_wear must be in [0, 1]")

        if wob_klb == 0.0 or rpm == 0.0:
            return RopPrediction(rop_ft_hr=0.0, model="bourgoyne_young")

        # f1: formation strength (base intercept)
        f1 = math.exp(2.303 * self.a1)

        # f2: normal compaction (increases ROP with depth in soft rock)
        # convention: depth normalised to 10,000 ft baseline
        f2 = math.exp(self.a2 * (10000.0 - depth_ft) / 1000.0)

        # f3: under-compaction (transition zone effect)
        # approximately zero for normally pressured wells
        f3 = math.exp(self.a3 * (depth_ft ** 0.69) * 9.0e-6)

        # f4: chip hold-down (overbalance penalty)
        # higher overbalance → lower ROP
        f4 = math.exp(-self.a4 * overbalance_kpsi)

        # f5: WOB normalised to bit diameter
        wob_per_dia = wob_klb / bit_dia_in
        f5 = (wob_per_dia) ** self.a5

        # f6: rotary speed
        f6 = (rpm / 100.0) ** self.a6

        # f7: bit tooth wear (tooth wear degrades cutting efficiency)
        f7 = math.exp(-self.a7 * bit_wear)

        # f8: jet impact (default to 1.0 — a8 is jet enhancement term)
        f8 = math.exp(self.a8)

        rop = f1 * f2 * f3 * f4 * f5 * f6 * f7 * f8
        return RopPrediction(rop_ft_hr=max(0.0, rop), model="bourgoyne_young")

    def sensitivity_wob(
        self,
        depth_ft: float,
        wob_klb: float,
        rpm: float,
        bit_dia_in: float,
        overbalance_kpsi: float,
        bit_wear: float,
        delta: float = 0.5,
    ) -> float:
        """Return dROP/dWOB (ft/hr per klb) via finite difference."""
        r_hi = self.predict(
            depth_ft, wob_klb + delta, rpm, bit_dia_in, overbalance_kpsi, bit_wear
        )
        r_lo = self.predict(
            depth_ft, max(0.0, wob_klb - delta), rpm, bit_dia_in, overbalance_kpsi, bit_wear
        )
        return (r_hi.rop_ft_hr - r_lo.rop_ft_hr) / (2.0 * delta)

    def sensitivity_rpm(
        self,
        depth_ft: float,
        wob_klb: float,
        rpm: float,
        bit_dia_in: float,
        overbalance_kpsi: float,
        bit_wear: float,
        delta: float = 5.0,
    ) -> float:
        """Return dROP/dRPM (ft/hr per RPM) via finite difference."""
        r_hi = self.predict(
            depth_ft, wob_klb, rpm + delta, bit_dia_in, overbalance_kpsi, bit_wear
        )
        r_lo = self.predict(
            depth_ft, wob_klb, max(0.0, rpm - delta), bit_dia_in, overbalance_kpsi, bit_wear
        )
        return (r_hi.rop_ft_hr - r_lo.rop_ft_hr) / (2.0 * delta)


class WarrenROP:
    """
    Warren (1987) power-law ROP model.

        ROP = K * (WOB / d_b)^a * RPM^b

    where WOB is in klb, d_b is bit diameter in inches, RPM is rotary speed.
    K, a, b are empirical constants fitted from offset-well data.

    Parameters
    ----------
    K : pre-exponential coefficient (formation + bit type)
    a : WOB/diameter exponent (typically 0.4–0.8)
    b : RPM exponent (typically 0.3–0.6)
    """

    def __init__(self, K: float, a: float, b: float) -> None:
        self.K = K
        self.a = a
        self.b = b

    def predict(
        self,
        wob_klb: float,
        rpm: float,
        bit_dia_in: float,
    ) -> RopPrediction:
        """
        Predict ROP using the Warren power-law model.

        Parameters
        ----------
        wob_klb    : weight on bit, kilo-lbf
        rpm        : rotary speed, RPM
        bit_dia_in : bit diameter, inches (must be > 0)

        Returns
        -------
        RopPrediction with rop_ft_hr and model="warren"
        """
        if wob_klb < 0.0:
            raise ValueError("wob_klb must be >= 0")
        if bit_dia_in <= 0.0:
            raise ValueError("bit_dia_in must be > 0")

        if wob_klb == 0.0 or rpm == 0.0:
            return RopPrediction(rop_ft_hr=0.0, model="warren")

        rop = self.K * (wob_klb / bit_dia_in) ** self.a * rpm ** self.b
        return RopPrediction(rop_ft_hr=max(0.0, rop), model="warren")

    @classmethod
    def fit_from_data(
        cls,
        wob_klb: Sequence[float],
        rpm: Sequence[float],
        bit_dia_in: Sequence[float],
        rop_ft_hr: Sequence[float],
    ) -> "WarrenROP":
        """
        Fit K, a, b via log-linear least squares.

        Linearises:   ln(ROP) = ln(K) + a*ln(WOB/d_b) + b*ln(RPM)
        Then solves the 3×3 normal equations directly (no scipy dependency).

        Parameters
        ----------
        wob_klb, rpm, bit_dia_in, rop_ft_hr : equal-length sequences of floats

        Returns
        -------
        WarrenROP instance with fitted K, a, b
        """
        # Validate inputs
        n = len(wob_klb)
        if not (len(rpm) == len(bit_dia_in) == len(rop_ft_hr) == n):
            raise ValueError("wob_klb, rpm, bit_dia_in, rop_ft_hr must be same length")
        if n < 3:
            raise ValueError("fit_from_data requires at least 3 data points (3 unknowns)")

        # Build design matrix X (n × 3) and response y (n,)
        # row i: [1, ln(WOB_i/d_i), ln(RPM_i)]
        # y_i = ln(ROP_i)
        x0 = [1.0] * n
        x1 = [math.log(w / d) for w, d in zip(wob_klb, bit_dia_in)]
        x2 = [math.log(r) for r in rpm]
        y = [math.log(rop) for rop in rop_ft_hr]

        # Normal equations: (X^T X) beta = X^T y
        # X^T X is 3×3; solve analytically via Cramer's rule
        def dot(u: list[float], v: list[float]) -> float:
            return sum(ui * vi for ui, vi in zip(u, v))

        # Assemble 3×3 matrix A and rhs b
        cols = [x0, x1, x2]
        A = [[dot(cols[i], cols[j]) for j in range(3)] for i in range(3)]
        rhs = [dot(cols[i], y) for i in range(3)]

        # Gaussian elimination with partial pivoting
        aug = [A[i][:] + [rhs[i]] for i in range(3)]
        for col in range(3):
            # Find pivot
            pivot_row = max(range(col, 3), key=lambda r: abs(aug[r][col]))
            aug[col], aug[pivot_row] = aug[pivot_row], aug[col]
            pivot = aug[col][col]
            if abs(pivot) < 1e-12:
                raise ValueError("Singular system — data may be collinear")
            for row in range(col + 1, 3):
                factor = aug[row][col] / pivot
                aug[row] = [aug[row][k] - factor * aug[col][k] for k in range(4)]

        # Back substitution
        beta = [0.0] * 3
        for row in range(2, -1, -1):
            beta[row] = aug[row][3]
            for k in range(row + 1, 3):
                beta[row] -= aug[row][k] * beta[k]
            beta[row] /= aug[row][row]

        ln_K, a, b = beta
        return cls(K=math.exp(ln_K), a=a, b=b)
