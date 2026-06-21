"""
Weld-Fatigue S-N Quick-Check  (DNV-RP-C203 / BS 7608)

A self-contained, unit-testable quick-check for welded-joint fatigue:

    1. Take a stress-range histogram (one or many bins), a DNV-RP-C203
       weld detail class (D, E, F, F1, F3, G, ...) and an environment
       (air / seawater_cp / free_corrosion).
    2. Optionally apply the DNV-RP-C203 thickness correction
           S_corr = S * (t / t_ref)^k          (only when t > t_ref).
    3. Compute the allowable cycles for each bin from the bi-linear S-N
       curve  log10(N) = log10(a) - m * log10(S):

           N = a * S^(-m)        (a = 10^log_a,  m = slope)

    4. Accumulate Palmgren-Miner damage  D = sum(n_i / N_i).
    5. Scale one block of histogram cycles to the design life and report
       PASS / FAIL against the allowable damage (default 1.0).

This module is a thin orchestration layer over the already-validated
``digitalmodel.fatigue`` engine and does NOT re-implement the S-N math:

    * S-N class parameters  -> ``get_sn_curve`` (DNV-RP-C203 Table 2-1 /
      Table 2-2, pyLife ``WoehlerCurve`` backend).
    * allowable cycles + Miner damage -> ``miner_damage``.
    * thickness correction            -> ``thickness_correction``.

S-N class parameters encoded in the engine (DNV-RP-C203, in air, N <= 1e7):

    Class   m (k_1)   log10(a1)   environment-CP log10(a1)
    -----   -------   ---------   ------------------------
    D       3.0       12.164      11.764
    E       3.0       12.010      11.610
    F       3.0       11.855      11.455
    F1      3.0       11.699      11.299
    F3      3.0       11.546      11.146
    G       3.0       11.398      10.998

(All slopes m=3 below the 1e7 knee; m=5 above. Values: DNV-RP-C203
Table 2-1 in air, Table 2-2 seawater with cathodic protection.)

References
----------
DNV-RP-C203 "Fatigue design of offshore steel structures":
  - Table 2-1  S-N curves in air.
  - Table 2-2  S-N curves in seawater with cathodic protection.
  - Sec. 2.4   thickness effect  S = S0 * (t/t_ref)^k, t_ref = 25 mm.
BS 7608 uses the same N = a * S^(-m) form (different class names /
constants); the weld-class letters used here follow DNV-RP-C203.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Sequence

import pandas as pd

from .sn_curves import get_sn_curve
from .damage import miner_damage, thickness_correction

# DNV-RP-C203 Sec. 2.4 default thickness-correction reference thickness.
_DEFAULT_T_REF = 25.0
# Thickness exponent k by weld class (DNV-RP-C203 Table 2-1, last column).
# D=0.20, others (E..G welded details) = 0.25.  Used only when t > t_ref.
_THICKNESS_EXPONENT = {
    "D": 0.20,
    "E": 0.25,
    "F": 0.25,
    "F1": 0.25,
    "F3": 0.25,
    "G": 0.25,
}
_DEFAULT_K = 0.25


@dataclass
class StressBin:
    """One bin of a stress-range histogram (per the design block)."""

    stress_range: float  # MPa, nominal/hot-spot stress range
    cycles: float  # applied cycle count in the histogram block


@dataclass
class WeldFatigueResult:
    """Outcome of a weld-fatigue S-N quick-check."""

    weld_class: str
    environment: str
    standard: str
    thickness_mm: float | None
    t_ref_mm: float
    thickness_exponent: float
    design_life_years: float
    block_years: float
    total_damage: float  # Miner damage over the design life
    allowable_damage: float
    life_factor: float  # allowable_damage / total_damage (>=1 -> PASS)
    pass_fail: str
    bins: pd.DataFrame  # per-bin stress, corrected stress, N, damage
    notes: list[str] = field(default_factory=list)

    @property
    def passed(self) -> bool:
        return self.pass_fail == "PASS"


def weld_fatigue_quickcheck(
    histogram: Sequence[StressBin] | pd.DataFrame,
    weld_class: str,
    *,
    environment: str = "air",
    thickness_mm: float | None = None,
    t_ref_mm: float = _DEFAULT_T_REF,
    thickness_exponent: float | None = None,
    design_life_years: float = 25.0,
    block_years: float = 1.0,
    allowable_damage: float = 1.0,
) -> WeldFatigueResult:
    """
    Run a weld-fatigue S-N quick-check (DNV-RP-C203).

    The ``histogram`` represents the stress cycles accumulated over
    ``block_years`` (default: one year).  Damage for one block is
    extrapolated linearly to ``design_life_years`` and compared with
    ``allowable_damage`` (Palmgren-Miner, default limit 1.0).

    Parameters
    ----------
    histogram : sequence of StressBin or DataFrame
        Stress-range histogram.  A DataFrame must have columns
        ``stress_range`` (MPa) and ``cycles``.
    weld_class : str
        DNV-RP-C203 detail category, e.g. 'D', 'E', 'F', 'F1', 'F3', 'G'.
    environment : str
        'air', 'seawater_cp', or 'free_corrosion'.
    thickness_mm : float, optional
        Actual plate thickness.  If given and > ``t_ref_mm`` the
        DNV-RP-C203 thickness correction is applied to every stress
        range; otherwise no correction (factor capped at 1.0).
    t_ref_mm : float
        Reference thickness (default 25 mm, DNV-RP-C203 Sec. 2.4).
    thickness_exponent : float, optional
        Override the per-class thickness exponent k.
    design_life_years : float
        Required fatigue design life (default 25 yr).
    block_years : float
        Calendar period represented by the histogram (default 1 yr).
    allowable_damage : float
        Miner damage limit for PASS (default 1.0).

    Returns
    -------
    WeldFatigueResult
    """
    if block_years <= 0:
        raise ValueError("block_years must be > 0")
    if design_life_years <= 0:
        raise ValueError("design_life_years must be > 0")

    # Normalise the histogram to a DataFrame with float dtypes.
    if isinstance(histogram, pd.DataFrame):
        hist = histogram.copy()
        if not {"stress_range", "cycles"} <= set(hist.columns):
            raise ValueError(
                "histogram DataFrame needs 'stress_range' and 'cycles' columns"
            )
    else:
        if not histogram:
            raise ValueError("histogram is empty")
        hist = pd.DataFrame(
            {
                "stress_range": [b.stress_range for b in histogram],
                "cycles": [b.cycles for b in histogram],
            }
        )
    hist["stress_range"] = hist["stress_range"].astype(float)
    hist["cycles"] = hist["cycles"].astype(float)
    if (hist["stress_range"] <= 0).any():
        raise ValueError("all stress ranges must be > 0 MPa")
    if (hist["cycles"] < 0).any():
        raise ValueError("cycle counts must be >= 0")

    notes: list[str] = []

    # --- thickness correction (DNV-RP-C203 Sec. 2.4) ------------------
    k = thickness_exponent
    if k is None:
        k = _THICKNESS_EXPONENT.get(weld_class.upper(), _DEFAULT_K)

    nominal = hist["stress_range"].to_numpy()
    if thickness_mm is not None and thickness_mm > t_ref_mm:
        corrected = thickness_correction(nominal, thickness_mm, t_ref_mm, k)
        notes.append(
            f"thickness correction applied: t={thickness_mm} mm > "
            f"t_ref={t_ref_mm} mm, k={k}, factor={(thickness_mm / t_ref_mm) ** k:.4f}"
        )
    else:
        corrected = nominal.copy()
        if thickness_mm is not None:
            notes.append(
                f"no thickness correction (t={thickness_mm} mm <= "
                f"t_ref={t_ref_mm} mm)"
            )

    # --- allowable cycles + Miner damage for one block ----------------
    sn_curve = get_sn_curve(weld_class, environment)
    block = pd.DataFrame(
        {"stress_range": corrected, "cycles": hist["cycles"].to_numpy()}
    )
    block = miner_damage(block, sn_curve)
    block_damage = float(block.attrs["total_damage"])

    # --- extrapolate to design life -----------------------------------
    life_blocks = design_life_years / block_years
    total_damage = block_damage * life_blocks

    if total_damage > 0:
        life_factor = allowable_damage / total_damage
    else:
        life_factor = float("inf")
    pass_fail = "PASS" if total_damage <= allowable_damage else "FAIL"

    bins = block.copy()
    bins.insert(0, "nominal_stress_range", nominal)
    bins = bins.rename(columns={"stress_range": "applied_stress_range"})

    return WeldFatigueResult(
        weld_class=weld_class.upper(),
        environment=environment,
        standard="DNV-RP-C203",
        thickness_mm=thickness_mm,
        t_ref_mm=t_ref_mm,
        thickness_exponent=k,
        design_life_years=design_life_years,
        block_years=block_years,
        total_damage=total_damage,
        allowable_damage=allowable_damage,
        life_factor=life_factor,
        pass_fail=pass_fail,
        bins=bins,
        notes=notes,
    )


# ----------------------------------------------------------------------
# YAML-driven runner + artifact emitters (for the demo / B4 GIF).
# ----------------------------------------------------------------------
def run_from_config(config: dict) -> WeldFatigueResult:
    """
    Run the quick-check from a parsed YAML/dict 'house contract'.

    Expected shape (see examples/manufacturing/weld_fatigue.yml)::

        weld_fatigue:
          weld_class: F
          environment: seawater_cp
          thickness_mm: 40
          design_life_years: 25
          block_years: 1.0
          histogram:
            - {stress_range: 90.0, cycles: 1.0e6}
            - {stress_range: 60.0, cycles: 3.0e6}
    """
    cfg = config.get("weld_fatigue", config)
    bins = [
        StressBin(float(row["stress_range"]), float(row["cycles"]))
        for row in cfg["histogram"]
    ]
    return weld_fatigue_quickcheck(
        bins,
        weld_class=cfg["weld_class"],
        environment=cfg.get("environment", "air"),
        thickness_mm=cfg.get("thickness_mm"),
        t_ref_mm=cfg.get("t_ref_mm", _DEFAULT_T_REF),
        thickness_exponent=cfg.get("thickness_exponent"),
        design_life_years=cfg.get("design_life_years", 25.0),
        block_years=cfg.get("block_years", 1.0),
        allowable_damage=cfg.get("allowable_damage", 1.0),
    )


def result_to_csv(result: WeldFatigueResult) -> str:
    """Return the per-bin breakdown as CSV text."""
    df = result.bins.copy()
    df = df[
        [
            "nominal_stress_range",
            "applied_stress_range",
            "cycles",
            "allowable_cycles",
            "damage",
        ]
    ]
    return df.to_csv(index=False)


def result_to_html(result: WeldFatigueResult) -> str:
    """Return a short standalone HTML report (demo artifact)."""
    badge = "PASS" if result.passed else "FAIL"
    colour = "#1a7f37" if result.passed else "#cf222e"
    rows = result.bins.to_html(
        index=False,
        float_format=lambda x: f"{x:,.3f}" if abs(x) < 1e6 else f"{x:,.0f}",
        border=0,
    )
    notes = "".join(f"<li>{n}</li>" for n in result.notes) or "<li>none</li>"
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>Weld-Fatigue Quick-Check — {result.weld_class}</title>
<style>
 body{{font-family:system-ui,Arial,sans-serif;margin:2rem;color:#1f2328}}
 .badge{{display:inline-block;padding:.3rem .9rem;border-radius:.4rem;
   color:#fff;font-weight:700;background:{colour}}}
 table{{border-collapse:collapse;margin-top:1rem}}
 th,td{{border:1px solid #d0d7de;padding:.35rem .6rem;text-align:right}}
 th{{background:#f6f8fa}}
 .meta td{{text-align:left;border:none;padding:.15rem .6rem}}
</style></head><body>
<h1>Weld-Fatigue S-N Quick-Check</h1>
<p class="badge">{badge}</p>
<table class="meta">
 <tr><td><b>Standard</b></td><td>{result.standard}</td></tr>
 <tr><td><b>Weld class</b></td><td>{result.weld_class}</td></tr>
 <tr><td><b>Environment</b></td><td>{result.environment}</td></tr>
 <tr><td><b>Thickness</b></td><td>{result.thickness_mm} mm
     (t_ref {result.t_ref_mm} mm, k={result.thickness_exponent})</td></tr>
 <tr><td><b>Design life</b></td><td>{result.design_life_years} yr
     (block {result.block_years} yr)</td></tr>
 <tr><td><b>Total Miner damage</b></td>
     <td>{result.total_damage:.4f} / {result.allowable_damage:.2f} allowable</td></tr>
 <tr><td><b>Life factor</b></td><td>{result.life_factor:.3f}</td></tr>
</table>
<h2>Per-bin breakdown</h2>
{rows}
<h2>Notes</h2><ul>{notes}</ul>
<p style="color:#57606a;font-size:.85rem">
 S-N class parameters: DNV-RP-C203 Table 2-1 (air) / Table 2-2
 (seawater with cathodic protection). N = a·S<sup>-m</sup>.</p>
</body></html>"""


def main(argv: Sequence[str] | None = None) -> int:
    """CLI: ``python -m digitalmodel.fatigue.weld_fatigue_quickcheck cfg.yml``."""
    import argparse
    import pathlib

    import yaml

    parser = argparse.ArgumentParser(
        description="Weld-fatigue S-N quick-check (DNV-RP-C203)."
    )
    parser.add_argument("config", help="YAML config (house contract).")
    parser.add_argument(
        "--out-dir", default=None, help="Directory for CSV/HTML artifacts."
    )
    args = parser.parse_args(argv)

    cfg_path = pathlib.Path(args.config)
    config = yaml.safe_load(cfg_path.read_text())
    result = run_from_config(config)

    print(
        f"Weld class {result.weld_class} ({result.environment}) | "
        f"design life {result.design_life_years} yr | "
        f"Miner damage {result.total_damage:.4f} -> {result.pass_fail}"
    )

    out_dir = pathlib.Path(args.out_dir) if args.out_dir else cfg_path.parent
    out_dir.mkdir(parents=True, exist_ok=True)
    stem = cfg_path.stem
    (out_dir / f"{stem}_result.csv").write_text(result_to_csv(result))
    (out_dir / f"{stem}_result.html").write_text(result_to_html(result))
    print(f"artifacts: {out_dir / (stem + '_result.csv')}, "
          f"{out_dir / (stem + '_result.html')}")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
