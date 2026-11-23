# ABOUTME: DualGradient — deepwater single- vs dual-gradient drilling narrow-window screen.
# ABOUTME: Reference: Smith, Gault, Witt & Weddle (SPE/IADC 79880) DGD; Schubert et al.; API RP 92.

"""
DualGradient
============

Closed-form, license-free screen comparing **conventional single-gradient**
drilling against **dual-gradient drilling (DGD)** in deepwater, where the
pore-pressure / fracture-gradient window is narrow.

Why deepwater is different
--------------------------
Above the mud line the only overburden is a tall column of seawater, so the
formation fracture gradient at the seabed is barely above the seawater
hydrostatic, while pore pressure deep in the well climbs with geopressure.
The drillable mud-weight window between them is razor-thin, forcing many
casing strings.

Conventional single-gradient
----------------------------
A single mud column of weight ``MW`` runs from the rig floor to total depth, so
the equivalent mud weight (EMW) the formation sees is constant with depth::

    BHP_conv(D) = 0.052 * MW * D            [psi]
    EMW_conv(D) = MW                        [ppg]

Dual-gradient (the "u-tube" effect)
-----------------------------------
Returns are taken at the mud line, so the column above the mud line is
seawater and only the column below the mud line carries the heavy mud::

    BHP_dgd(D) = 0.052 * SW * Dml + 0.052 * MW * (D - Dml)         [psi]
    EMW_dgd(D) = BHP_dgd(D) / (0.052 * D)
               = [ SW * Dml + MW * (D - Dml) ] / D                 [ppg]

where ``SW`` is the seawater EMW, ``Dml`` the mud-line depth below the rig
floor and ``D`` the true vertical depth (both referenced to the rig floor /
RKB, as are the pore and fracture EMW profiles).

The drillable window transform
------------------------------
Requiring ``pore + kick_margin <= EMW <= frac - frac_margin`` at every depth
maps to bounds on the deep mud weight ``MW``. For DGD the admissible
mud-weight band is the conventional band scaled by ``D / (D - Dml) > 1``::

    window_dgd(D) = window_conv(D) * D / (D - Dml)

so the effective window is always wider, and dramatically so just below the
mud line (where ``D`` is close to ``Dml``) — exactly where the deepwater
problem bites. A wider window means fewer casing strings to reach TD.

Riser margin
------------
If the riser is lost, the mud above the mud line is replaced by seawater and
the bottom-hole pressure drops to the dual-gradient value. The well stays dead
only if ``EMW_dgd(TD) >= pore(TD)``. In deepwater the conventional "riser
margin" needed to satisfy this is often impractically large (it would fracture
the shallow hole), whereas a DGD well already operates at that condition and is
inherently riser-margin safe.

Closed-form screening tier (concept level). A transient / managed-pressure
hydraulics model is a follow-up.

Units: depth in feet (TVD from rig floor), mud weight / gradients in ppg,
pressure in psi.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

__all__ = [
    "SEAWATER_EMW_PPG",
    "PressurePoint",
    "DrillingWindow",
    "CasingDesign",
    "RiserMargin",
    "DualGradientScreen",
    "router",
]

_PSI_PER_PPG_FT = 0.052
SEAWATER_EMW_PPG = 8.55  # typical drilling-fluid seawater EMW, ppg


def _interp(x: float, xs: list[float], ys: list[float]) -> float:
    """Linear interpolation of ``ys(xs)`` at ``x``, clamped at the ends."""
    if x <= xs[0]:
        return ys[0]
    if x >= xs[-1]:
        return ys[-1]
    lo = 0
    hi = len(xs) - 1
    while hi - lo > 1:
        mid = (lo + hi) // 2
        if xs[mid] <= x:
            lo = mid
        else:
            hi = mid
    span = xs[hi] - xs[lo]
    if span == 0.0:
        return ys[lo]
    frac = (x - xs[lo]) / span
    return ys[lo] + frac * (ys[hi] - ys[lo])


class PressurePoint:
    """Single-depth comparison of conventional vs dual-gradient EMW/BHP."""

    def __init__(
        self,
        *,
        depth_ft: float,
        pore_emw_ppg: float,
        fracture_emw_ppg: float,
        emw_conventional_ppg: float,
        emw_dual_gradient_ppg: float,
        bhp_conventional_psi: float,
        bhp_dual_gradient_psi: float,
    ) -> None:
        self.depth_ft = depth_ft
        self.pore_emw_ppg = pore_emw_ppg
        self.fracture_emw_ppg = fracture_emw_ppg
        self.emw_conventional_ppg = emw_conventional_ppg
        self.emw_dual_gradient_ppg = emw_dual_gradient_ppg
        self.bhp_conventional_psi = bhp_conventional_psi
        self.bhp_dual_gradient_psi = bhp_dual_gradient_psi

    def as_dict(self) -> dict[str, float]:
        return {
            "depth_ft": self.depth_ft,
            "pore_emw_ppg": self.pore_emw_ppg,
            "fracture_emw_ppg": self.fracture_emw_ppg,
            "emw_conventional_ppg": self.emw_conventional_ppg,
            "emw_dual_gradient_ppg": self.emw_dual_gradient_ppg,
            "bhp_conventional_psi": self.bhp_conventional_psi,
            "bhp_dual_gradient_psi": self.bhp_dual_gradient_psi,
        }


class DrillingWindow:
    """Pore-pressure / fracture-gradient profile (EMW, ppg) vs depth (ft, RKB)."""

    def __init__(
        self,
        *,
        depths_ft: list[float],
        pore_emw_ppg: list[float],
        fracture_emw_ppg: list[float],
    ) -> None:
        if not (len(depths_ft) == len(pore_emw_ppg) == len(fracture_emw_ppg)):
            raise ValueError("depths, pore and fracture arrays must be equal length")
        if len(depths_ft) < 2:
            raise ValueError("need at least two depth points")
        if any(b <= a for a, b in zip(depths_ft, depths_ft[1:])):
            raise ValueError("depths_ft must be strictly increasing")
        for pp, fg in zip(pore_emw_ppg, fracture_emw_ppg):
            if fg < pp:
                raise ValueError("fracture EMW must be >= pore EMW at every depth")
        self.depths_ft = [float(d) for d in depths_ft]
        self.pore_emw_ppg = [float(p) for p in pore_emw_ppg]
        self.fracture_emw_ppg = [float(f) for f in fracture_emw_ppg]

    @classmethod
    def from_gradients(
        cls,
        *,
        top_depth_ft: float,
        bottom_depth_ft: float,
        pore_top_ppg: float,
        pore_bottom_ppg: float,
        fracture_top_ppg: float,
        fracture_bottom_ppg: float,
    ) -> "DrillingWindow":
        """Build a two-point (linear) window from top/bottom EMW gradients."""
        return cls(
            depths_ft=[top_depth_ft, bottom_depth_ft],
            pore_emw_ppg=[pore_top_ppg, pore_bottom_ppg],
            fracture_emw_ppg=[fracture_top_ppg, fracture_bottom_ppg],
        )

    @property
    def td_ft(self) -> float:
        return self.depths_ft[-1]

    def pore_at(self, depth_ft: float) -> float:
        return _interp(depth_ft, self.depths_ft, self.pore_emw_ppg)

    def fracture_at(self, depth_ft: float) -> float:
        return _interp(depth_ft, self.depths_ft, self.fracture_emw_ppg)


class CasingDesign:
    """Casing-string estimate for one drilling mode (conventional or DGD)."""

    def __init__(
        self,
        *,
        mode: str,
        n_strings: int,
        seat_depths_ft: list[float],
        section_mud_weights_ppg: list[float],
        window_violated: bool,
    ) -> None:
        self.mode = mode
        self.n_strings = n_strings
        self.seat_depths_ft = seat_depths_ft
        self.section_mud_weights_ppg = section_mud_weights_ppg
        self.window_violated = window_violated  # window closed somewhere even for DGD

    def as_dict(self) -> dict[str, Any]:
        return {
            "mode": self.mode,
            "n_strings": self.n_strings,
            "seat_depths_ft": self.seat_depths_ft,
            "section_mud_weights_ppg": self.section_mud_weights_ppg,
            "window_violated": self.window_violated,
        }


class RiserMargin:
    """Riser-margin (keep-the-well-dead-on-riser-loss) check at a depth."""

    def __init__(
        self,
        *,
        depth_ft: float,
        mud_weight_ppg: float,
        pore_emw_ppg: float,
        emw_after_riser_loss_ppg: float,
        margin_ppg: float,
        well_dead_after_loss: bool,
    ) -> None:
        self.depth_ft = depth_ft
        self.mud_weight_ppg = mud_weight_ppg
        self.pore_emw_ppg = pore_emw_ppg
        self.emw_after_riser_loss_ppg = emw_after_riser_loss_ppg
        self.margin_ppg = margin_ppg
        self.well_dead_after_loss = well_dead_after_loss

    def as_dict(self) -> dict[str, Any]:
        return {
            "depth_ft": self.depth_ft,
            "mud_weight_ppg": self.mud_weight_ppg,
            "pore_emw_ppg": self.pore_emw_ppg,
            "emw_after_riser_loss_ppg": self.emw_after_riser_loss_ppg,
            "margin_ppg": self.margin_ppg,
            "well_dead_after_loss": self.well_dead_after_loss,
        }


class DualGradientScreen:
    """Single-gradient vs dual-gradient deepwater drilling-window screen."""

    def __init__(
        self,
        *,
        water_depth_ft: float,
        window: DrillingWindow,
        air_gap_ft: float = 75.0,
        seawater_emw_ppg: float = SEAWATER_EMW_PPG,
        kick_margin_ppg: float = 0.3,
        fracture_margin_ppg: float = 0.3,
    ) -> None:
        if water_depth_ft <= 0:
            raise ValueError("water_depth_ft must be > 0")
        if air_gap_ft < 0:
            raise ValueError("air_gap_ft must be >= 0")
        if seawater_emw_ppg <= 0:
            raise ValueError("seawater_emw_ppg must be > 0")
        if kick_margin_ppg < 0 or fracture_margin_ppg < 0:
            raise ValueError("margins must be >= 0")
        self.water_depth_ft = float(water_depth_ft)
        self.air_gap_ft = float(air_gap_ft)
        self.seawater_emw_ppg = float(seawater_emw_ppg)
        self.window = window
        self.kick_margin_ppg = float(kick_margin_ppg)
        self.fracture_margin_ppg = float(fracture_margin_ppg)

        self.mud_line_depth_ft = self.air_gap_ft + self.water_depth_ft
        if window.td_ft <= self.mud_line_depth_ft:
            raise ValueError("window TD must be below the mud line")

    # -- pressure / EMW profiles ------------------------------------------

    def bhp_conventional(self, depth_ft: float, mud_weight_ppg: float) -> float:
        """Bottom-hole pressure for a single mud column from the rig floor, psi."""
        return _PSI_PER_PPG_FT * mud_weight_ppg * depth_ft

    def emw_conventional(self, depth_ft: float, mud_weight_ppg: float) -> float:
        """Equivalent mud weight seen by the formation (constant = MW), ppg."""
        return mud_weight_ppg

    def bhp_dual_gradient(self, depth_ft: float, mud_weight_ppg: float) -> float:
        """Bottom-hole pressure with seawater above and mud below mud line, psi."""
        dml = self.mud_line_depth_ft
        if depth_ft <= dml:
            return _PSI_PER_PPG_FT * self.seawater_emw_ppg * depth_ft
        return _PSI_PER_PPG_FT * (
            self.seawater_emw_ppg * dml + mud_weight_ppg * (depth_ft - dml)
        )

    def emw_dual_gradient(self, depth_ft: float, mud_weight_ppg: float) -> float:
        """Equivalent mud weight of the dual-gradient column, ppg."""
        if depth_ft <= self.mud_line_depth_ft:
            return self.seawater_emw_ppg
        return self.bhp_dual_gradient(depth_ft, mud_weight_ppg) / (
            _PSI_PER_PPG_FT * depth_ft
        )

    def pressure_point(self, depth_ft: float, mud_weight_ppg: float) -> PressurePoint:
        """Side-by-side conventional vs DGD point at one depth."""
        return PressurePoint(
            depth_ft=depth_ft,
            pore_emw_ppg=self.window.pore_at(depth_ft),
            fracture_emw_ppg=self.window.fracture_at(depth_ft),
            emw_conventional_ppg=self.emw_conventional(depth_ft, mud_weight_ppg),
            emw_dual_gradient_ppg=self.emw_dual_gradient(depth_ft, mud_weight_ppg),
            bhp_conventional_psi=self.bhp_conventional(depth_ft, mud_weight_ppg),
            bhp_dual_gradient_psi=self.bhp_dual_gradient(depth_ft, mud_weight_ppg),
        )

    # -- mud-weight window per mode ---------------------------------------

    def mud_weight_bounds(
        self, mode: str, depth_ft: float
    ) -> tuple[float, float]:
        """Admissible deep mud-weight band ``(lo, hi)`` at a depth, ppg.

        ``lo`` keeps EMW above pore + kick margin; ``hi`` keeps EMW below
        fracture - fracture margin. For DGD the conventional band is scaled
        by ``D / (D - Dml)``.
        """
        lo_emw = self.window.pore_at(depth_ft) + self.kick_margin_ppg
        hi_emw = self.window.fracture_at(depth_ft) - self.fracture_margin_ppg
        if mode == "conventional":
            return lo_emw, hi_emw
        if mode == "dual_gradient":
            dml = self.mud_line_depth_ft
            denom = depth_ft - dml
            if denom <= 0:
                # at/above mud line any deep MW is admissible (seawater column)
                return float("-inf"), float("inf")
            lo = (lo_emw * depth_ft - self.seawater_emw_ppg * dml) / denom
            hi = (hi_emw * depth_ft - self.seawater_emw_ppg * dml) / denom
            return lo, hi
        raise ValueError("mode must be 'conventional' or 'dual_gradient'")

    def available_window_ppg(self, mode: str, depth_ft: float) -> float:
        """Width of the admissible mud-weight band at a depth, ppg (can be < 0)."""
        lo, hi = self.mud_weight_bounds(mode, depth_ft)
        return hi - lo

    def min_available_window_ppg(
        self, mode: str, step_ft: float = 250.0
    ) -> float:
        """Narrowest mud-weight band over the open hole (mud line -> TD), ppg."""
        worst = float("inf")
        for d in self._open_hole_grid(step_ft):
            worst = min(worst, self.available_window_ppg(mode, d))
        return worst

    # -- casing-string estimate -------------------------------------------

    def _open_hole_grid(self, step_ft: float) -> list[float]:
        """Sampling grid from just below the mud line to TD (inclusive)."""
        if step_ft <= 0:
            raise ValueError("step_ft must be > 0")
        start = self.mud_line_depth_ft + step_ft
        td = self.window.td_ft
        grid: list[float] = []
        d = start
        while d < td:
            grid.append(d)
            d += step_ft
        grid.append(td)
        # merge in the supplied profile depths that fall inside the open hole
        for dp in self.window.depths_ft:
            if start <= dp <= td:
                grid.append(dp)
        grid = sorted(set(grid))
        return grid

    def _interval_feasible(
        self, mode: str, depths: list[float]
    ) -> tuple[bool, float]:
        """Is one mud weight admissible across ``depths``? Return (ok, chosen MW)."""
        max_lo = float("-inf")
        min_hi = float("inf")
        for d in depths:
            lo, hi = self.mud_weight_bounds(mode, d)
            max_lo = max(max_lo, lo)
            min_hi = min(min_hi, hi)
        return (max_lo <= min_hi, max_lo)

    def casing_design(self, mode: str, step_ft: float = 250.0) -> CasingDesign:
        """Greedy casing-seat count keeping EMW inside the window per section.

        Walks top-down from the mud line, extending each open-hole section as
        deep as one mud weight allows, then sets a casing string. Fewer
        sections == fewer strings == cheaper well.
        """
        grid = self._open_hole_grid(step_ft)
        n = len(grid)
        n_strings = 0
        seats: list[float] = []
        muds: list[float] = []
        window_violated = False
        start = 0
        while start < n:
            j = start
            # extend while [start..j] stays feasible
            while j + 1 <= n - 1 and self._interval_feasible(
                mode, grid[start : j + 2]
            )[0]:
                j += 1
            ok, mw = self._interval_feasible(mode, grid[start : j + 1])
            if not ok:
                # even a single point has no admissible mud weight
                window_violated = True
            n_strings += 1
            seats.append(grid[j])
            muds.append(mw)
            start = j + 1
        return CasingDesign(
            mode=mode,
            n_strings=n_strings,
            seat_depths_ft=seats,
            section_mud_weights_ppg=muds,
            window_violated=window_violated,
        )

    # -- riser margin ------------------------------------------------------

    def riser_margin(
        self, mud_weight_ppg: float, depth_ft: float | None = None
    ) -> RiserMargin:
        """Can the well stay dead if the riser is lost (mud -> seawater above ML)?

        On riser loss the column above the mud line becomes seawater, so BHP
        falls to the dual-gradient value. The well stays dead if that EMW still
        exceeds the pore pressure.
        """
        depth = self.window.td_ft if depth_ft is None else depth_ft
        emw_after = self.emw_dual_gradient(depth, mud_weight_ppg)
        pore = self.window.pore_at(depth)
        margin = emw_after - pore
        return RiserMargin(
            depth_ft=depth,
            mud_weight_ppg=mud_weight_ppg,
            pore_emw_ppg=pore,
            emw_after_riser_loss_ppg=emw_after,
            margin_ppg=margin,
            well_dead_after_loss=margin >= 0.0,
        )


# ---------------------------------------------------------------------------
# Router (engine entry point)
# ---------------------------------------------------------------------------


def _build_window(window_cfg: dict[str, Any]) -> DrillingWindow:
    if "depths_ft" in window_cfg:
        return DrillingWindow(
            depths_ft=[float(d) for d in window_cfg["depths_ft"]],
            pore_emw_ppg=[float(p) for p in window_cfg["pore_emw_ppg"]],
            fracture_emw_ppg=[float(f) for f in window_cfg["fracture_emw_ppg"]],
        )
    g = window_cfg["gradients"]
    return DrillingWindow.from_gradients(
        top_depth_ft=float(g["top_depth_ft"]),
        bottom_depth_ft=float(g["bottom_depth_ft"]),
        pore_top_ppg=float(g["pore_top_ppg"]),
        pore_bottom_ppg=float(g["pore_bottom_ppg"]),
        fracture_top_ppg=float(g["fracture_top_ppg"]),
        fracture_bottom_ppg=float(g["fracture_bottom_ppg"]),
    )


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/dual_gradient"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "dual_gradient_summary.json"
    )
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path


def router(cfg: dict) -> dict:
    """Run the deepwater dual-gradient drilling-window screen from ``cfg``."""
    settings = cfg.get("dual_gradient") or {}
    geom = settings["geometry"]
    window = _build_window(settings["window"])
    margins = settings.get("margins", {})

    screen = DualGradientScreen(
        water_depth_ft=float(geom["water_depth_ft"]),
        window=window,
        air_gap_ft=float(geom.get("air_gap_ft", 75.0)),
        seawater_emw_ppg=float(geom.get("seawater_emw_ppg", SEAWATER_EMW_PPG)),
        kick_margin_ppg=float(margins.get("kick_margin_ppg", 0.3)),
        fracture_margin_ppg=float(margins.get("fracture_margin_ppg", 0.3)),
    )

    step_ft = float(settings.get("step_ft", 250.0))
    mud_weight = float(settings.get("mud_weight_ppg", window.pore_at(window.td_ft) + 0.5))

    conv = screen.casing_design("conventional", step_ft=step_ft)
    dgd = screen.casing_design("dual_gradient", step_ft=step_ft)

    # depth profile for the JSON (mud line + each window depth + TD)
    profile_depths = sorted(
        {screen.mud_line_depth_ft, *window.depths_ft, window.td_ft}
    )
    profile = [
        screen.pressure_point(d, mud_weight).as_dict() for d in profile_depths
    ]

    result = {
        "mud_line_depth_ft": screen.mud_line_depth_ft,
        "td_ft": window.td_ft,
        "operating_mud_weight_ppg": mud_weight,
        "casing_design": {
            "conventional": conv.as_dict(),
            "dual_gradient": dgd.as_dict(),
            "strings_saved_by_dgd": conv.n_strings - dgd.n_strings,
        },
        "min_window_ppg": {
            "conventional": screen.min_available_window_ppg(
                "conventional", step_ft=step_ft
            ),
            "dual_gradient": screen.min_available_window_ppg(
                "dual_gradient", step_ft=step_ft
            ),
        },
        "riser_margin": {
            "conventional": screen.riser_margin(mud_weight).as_dict(),
            # DGD already operates at the riser-lost hydrostatic, so it is
            # inherently riser-margin safe (same check, shown for contrast).
            "dual_gradient_inherently_safe": True,
        },
        "pressure_profile": profile,
    }

    payload = {"calculation": "dual_gradient", **settings, "result": result}
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
    cfg["dual_gradient"] = payload
    return cfg
