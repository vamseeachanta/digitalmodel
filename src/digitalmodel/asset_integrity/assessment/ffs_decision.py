"""FFS decision engine — translate Level 1/2 results into action verdicts.

Decision tree follows API 579-1/ASME FFS-1 2021 Edition Figure 2.1
(Assessment Flowchart) and Section 2.4.2.

Verdict outcomes
----------------
ACCEPT       Both Level 1 and Level 2 pass.  Component is fit-for-service.
MONITOR      Level 2 passes with margin close to RSFa — increase inspection
             frequency.  (RSFa <= RSF < RSFa + MONITOR_BAND)
RE_RATE      Level 2 fails but reduced MAWP brings the component back to
             acceptance.  (Not implemented at this scope — returned when RSF
             is between RE_RATE_FLOOR and RSFa.)
REPAIR       Level 1 or Level 2 fails and re-rating is not viable.  The
             component requires physical repair per ASME PCC-2.
REPLACE      Remaining life is exhausted or RSF is below the safe operating
             threshold RE_RATE_FLOOR.

The ``remaining_life_yr`` is computed as:
  (t_mm - t_min) / corrosion_rate    [simple linear projection]

A corrosion_rate of zero returns ``float('inf')``.

References:
  API 579-1/ASME FFS-1 2021 §2.4.2, Figure 2.1
  ASME PCC-2-2022 (repair methods)
"""

from __future__ import annotations

# Band above RSFa below which a passing result gets a MONITOR recommendation
_MONITOR_BAND = 0.05  # RSFa + 0.05

# Minimum RSF below which re-rating is no longer sufficient — replace
_RE_RATE_FLOOR = 0.50


class FFSDecision:
    """Static decision engine for FFS assessment verdicts."""

    @staticmethod
    def decide(
        level1_verdict: str,
        level2_verdict: str,
        rsf: float,
        rsf_a: float,
        t_mm_in: float,
        t_min_in: float,
        corrosion_rate_in_per_yr: float,
    ) -> dict:
        """Determine the final FFS action verdict.

        Args:
            level1_verdict: ``'ACCEPT'`` or ``'FAIL_LEVEL_1'``.
            level2_verdict: ``'ACCEPT'`` or ``'FAIL_LEVEL_2'``.
            rsf: Computed remaining strength factor from Level 2 engine.
            rsf_a: Allowable remaining strength factor threshold.
            t_mm_in: Minimum measured wall thickness (inches).
            t_min_in: Code-required minimum wall thickness (inches).
            corrosion_rate_in_per_yr: Future corrosion rate (inches/year).
                Use 0.0 for no further degradation.

        Returns:
            dict with keys:
                verdict (str): One of ACCEPT, MONITOR, RE_RATE, REPAIR,
                    REPLACE.
                remaining_life_yr (float): Estimated remaining service life.
                    ``float('inf')`` when corrosion_rate_in_per_yr == 0.
                governing_criterion (str): Human-readable description of the
                    governing assessment criterion.
                rsf (float): Echoed input RSF.
                rsf_a (float): Echoed input RSFa.
        """
        remaining_life = FFSDecision._remaining_life(
            t_mm_in, t_min_in, corrosion_rate_in_per_yr
        )

        # Both levels pass
        if level1_verdict == "ACCEPT" and level2_verdict == "ACCEPT":
            if rsf < rsf_a + _MONITOR_BAND:
                verdict = "MONITOR"
                criterion = (
                    f"Level 1 and Level 2 ACCEPT; RSF={rsf:.3f} is within "
                    f"{_MONITOR_BAND:.2f} of RSFa={rsf_a:.2f} — increased "
                    "inspection frequency recommended."
                )
            else:
                verdict = "ACCEPT"
                criterion = (
                    f"Level 1 ACCEPT (t_mm >= t_min) and Level 2 ACCEPT "
                    f"(RSF={rsf:.3f} >= RSFa={rsf_a:.2f})."
                )
            return {
                "verdict": verdict,
                "remaining_life_yr": remaining_life,
                "governing_criterion": criterion,
                "rsf": rsf,
                "rsf_a": rsf_a,
            }

        # At least one level fails — determine remediation path
        if rsf < _RE_RATE_FLOOR:
            verdict = "REPLACE"
            criterion = (
                f"RSF={rsf:.3f} is below the re-rating floor "
                f"RSF_floor={_RE_RATE_FLOOR:.2f}.  Component must be replaced."
            )
        elif rsf < rsf_a:
            # Re-rating band: RSF_floor <= RSF < RSFa
            verdict = "RE_RATE"
            criterion = (
                f"Level 1 or Level 2 FAIL; RSF={rsf:.3f} in re-rating band "
                f"[{_RE_RATE_FLOOR:.2f}, {rsf_a:.2f}).  "
                "Reduce MAWP or operating pressure."
            )
        else:
            # Level 1 fails but Level 2 still passes (RSF >= RSFa) —
            # this can occur when Level 1 is conservative; Level 2 governs.
            # Recommend repair if remaining life is short.
            if remaining_life < 2.0:
                verdict = "REPAIR"
                criterion = (
                    "Level 1 FAIL; Level 2 RSF acceptable but remaining life "
                    f"{remaining_life:.1f} yr is short.  Repair recommended."
                )
            else:
                verdict = "RE_RATE"
                criterion = (
                    "Level 1 FAIL; Level 2 RSF acceptable.  "
                    "Consider re-rating or accepting at reduced MAWP."
                )

        return {
            "verdict": verdict,
            "remaining_life_yr": remaining_life,
            "governing_criterion": criterion,
            "rsf": rsf,
            "rsf_a": rsf_a,
        }

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _remaining_life(
        t_mm: float, t_min: float, rate: float
    ) -> float:
        """Linear remaining-life projection.

        Returns:
            Remaining life in years.  ``float('inf')`` when rate == 0.
            Returns 0.0 when t_mm <= t_min (no remaining life).
        """
        if t_mm <= t_min:
            return 0.0
        if rate == 0.0:
            return float("inf")
        return (t_mm - t_min) / rate
