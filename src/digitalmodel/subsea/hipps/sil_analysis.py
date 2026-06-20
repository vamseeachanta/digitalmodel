"""HIPPS SIL (Safety Integrity Level) reliability analysis — IEC 61508 / 61511.

API 17O specifies that a subsea HIPPS Safety Instrumented Function (SIF) operating
in *low-demand* mode is rated by its **average Probability of Failure on Demand**
(PFDavg) and the corresponding **Safety Integrity Level (SIL)**, per the functional-
safety standards IEC 61508-1 (2010) and IEC 61511-1 (2016).

Standard relations encoded
--------------------------
Low-demand SIL band table (IEC 61508-1:2010, Table 2 / IEC 61511-1 Table 4):

    SIL 4 :  1e-5  <= PFDavg < 1e-4      (RRF 10000 .. 100000)
    SIL 3 :  1e-4  <= PFDavg < 1e-3      (RRF  1000 ..  10000)
    SIL 2 :  1e-3  <= PFDavg < 1e-2      (RRF   100 ..   1000)
    SIL 1 :  1e-2  <= PFDavg < 1e-1      (RRF    10 ..    100)

Risk Reduction Factor (IEC 61508-5):

    RRF = 1 / PFDavg                                                    (1)

Average PFD of a simplified architecture (IEC 61508-6, Annex B simplified
equations).  For a single channel (1oo1) of a low-demand subsystem with dangerous-
undetected failure rate ``lambda_du`` [1/h] and proof-test interval ``ti`` [h]:

    PFDavg(1oo1) = lambda_du * ti / 2                                   (2)

For a dual-redundant 1oo2 voted subsystem (the common HIPPS valve architecture),
including the common-cause beta-factor (IEC 61508-6 B.3.2.2.2, MTTR neglected for
the deterministic core):

    PFDavg(1oo2) = ( (1 - beta) * lambda_du )**2 * ti**2 / 3
                   + beta * lambda_du * ti / 2                          (3)

All rates are dangerous-undetected (lambda_du); the diagnostic-covered portion is
assumed handled by online diagnostics and is excluded from the demand-mode PFD,
consistent with the simplified-equation assumptions in IEC 61508-6.

Units: ``lambda_du`` in failures per hour [1/h]; ``ti`` (proof-test interval) in
hours [h].  PFDavg is dimensionless.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import IntEnum


class SIL(IntEnum):
    """Safety Integrity Level (IEC 61508). ``NONE`` = below SIL 1 (PFDavg >= 0.1)."""

    NONE = 0
    SIL1 = 1
    SIL2 = 2
    SIL3 = 3
    SIL4 = 4


#: Lower PFDavg bound for each SIL band (IEC 61508-1 Table 2, low-demand mode).
#: A PFDavg qualifies for a SIL when ``lower <= PFDavg < 10*lower``.
_SIL_LOWER_BOUND: dict[SIL, float] = {
    SIL.SIL1: 1e-2,
    SIL.SIL2: 1e-3,
    SIL.SIL3: 1e-4,
    SIL.SIL4: 1e-5,
}


@dataclass(frozen=True)
class SILResult:
    """Outcome of a SIL classification for a HIPPS SIF.

    Attributes
    ----------
    pfd_avg:
        Average probability of failure on demand (dimensionless).
    sil:
        Achieved SIL band.
    rrf:
        Risk reduction factor = 1 / PFDavg.
    target_sil:
        Required SIL (if a target was supplied), else ``None``.
    meets_target:
        ``True`` when ``sil >= target_sil``; ``None`` when no target given.
    """

    pfd_avg: float
    sil: SIL
    rrf: float
    target_sil: SIL | None = None
    meets_target: bool | None = None


def risk_reduction_factor(pfd_avg: float) -> float:
    """Risk Reduction Factor, RRF = 1 / PFDavg (IEC 61508-5, eq. 1).

    Raises
    ------
    ValueError
        If ``pfd_avg`` is not in the open interval (0, 1].
    """
    if not 0.0 < pfd_avg <= 1.0:
        raise ValueError(f"pfd_avg must be in (0, 1], got {pfd_avg!r}")
    return 1.0 / pfd_avg


def pfd_avg_1oo1(lambda_du: float, ti: float) -> float:
    """PFDavg of a single channel (1oo1), IEC 61508-6 eq. (2).

    Parameters
    ----------
    lambda_du:
        Dangerous-undetected failure rate [1/h], must be > 0.
    ti:
        Proof-test interval [h], must be > 0.
    """
    if lambda_du <= 0.0:
        raise ValueError(f"lambda_du must be > 0, got {lambda_du!r}")
    if ti <= 0.0:
        raise ValueError(f"ti must be > 0, got {ti!r}")
    return lambda_du * ti / 2.0


def pfd_avg_1oo2(lambda_du: float, ti: float, beta: float = 0.0) -> float:
    """PFDavg of a 1oo2 redundant subsystem with common cause, IEC 61508-6 eq. (3).

    Parameters
    ----------
    lambda_du:
        Per-channel dangerous-undetected failure rate [1/h], must be > 0.
    ti:
        Proof-test interval [h], must be > 0.
    beta:
        Common-cause failure fraction in [0, 1] (IEC 61508-6 Annex D).
        ``beta = 0`` gives the ideal independent-channel result.
    """
    if lambda_du <= 0.0:
        raise ValueError(f"lambda_du must be > 0, got {lambda_du!r}")
    if ti <= 0.0:
        raise ValueError(f"ti must be > 0, got {ti!r}")
    if not 0.0 <= beta <= 1.0:
        raise ValueError(f"beta must be in [0, 1], got {beta!r}")
    independent = ((1.0 - beta) * lambda_du) ** 2 * ti**2 / 3.0
    common_cause = beta * lambda_du * ti / 2.0
    return independent + common_cause


def classify_sil(pfd_avg: float, target_sil: SIL | int | None = None) -> SILResult:
    """Classify a PFDavg into a SIL band (IEC 61508-1 Table 2, low-demand mode).

    Parameters
    ----------
    pfd_avg:
        Average probability of failure on demand, in (0, 1].
    target_sil:
        Optional required SIL; if given, the result reports whether the achieved
        SIL meets or exceeds it.

    Returns
    -------
    SILResult
        Achieved SIL, RRF, and (when a target is supplied) the pass/fail verdict.

    Notes
    -----
    A PFDavg at or above 0.1 does not qualify for any SIL band and is reported as
    ``SIL.NONE``.  A PFDavg below 1e-5 is capped at ``SIL.SIL4`` (the highest band
    addressed by the standard).
    """
    if not 0.0 < pfd_avg <= 1.0:
        raise ValueError(f"pfd_avg must be in (0, 1], got {pfd_avg!r}")

    if pfd_avg >= 0.1:
        sil = SIL.NONE
    elif pfd_avg < _SIL_LOWER_BOUND[SIL.SIL4]:
        # Below the SIL 4 floor — cap at the highest band the standard addresses.
        sil = SIL.SIL4
    else:
        # Highest band whose [lower, 10*lower) interval contains the PFDavg.
        sil = SIL.NONE
        for band in (SIL.SIL1, SIL.SIL2, SIL.SIL3, SIL.SIL4):
            lower = _SIL_LOWER_BOUND[band]
            if lower <= pfd_avg < 10.0 * lower:
                sil = band
                break

    rrf = 1.0 / pfd_avg

    if target_sil is None:
        return SILResult(pfd_avg=pfd_avg, sil=sil, rrf=rrf)

    target = SIL(int(target_sil))
    return SILResult(
        pfd_avg=pfd_avg,
        sil=sil,
        rrf=rrf,
        target_sil=target,
        meets_target=int(sil) >= int(target),
    )
