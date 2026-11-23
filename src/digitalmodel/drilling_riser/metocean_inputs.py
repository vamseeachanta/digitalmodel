"""Metocean inputs for the drilling-riser operating envelope (#1282, epic #1279).

Turns a metocean time-series record stream (from
``data_systems.data_procurement.metocean.MetoceanClient`` or a committed offline
fixture) into the inputs the envelope engine consumes:

  * ``reduce_to_scatter`` bins the (Hs, Tp) record stream into a
    :class:`digitalmodel.orcaflex.weather_window.ScatterDiagram` (reused — no new
    scatter type); bins are UPPER edges (DNV-RP-C205 §3.3).
  * ``scatter_to_seastates`` emits :class:`SeaState` at bin **midpoints** (bins
    are upper edges, so a design case at the edge would bias Hs/Tp high by half a
    bin); occupied cells only.
  * ``current_to_surface_speeds`` extracts the surface speed(s) a sweep needs.

The envelope currently sweeps surface-current only; a depth-resolved current
profile is carried on :class:`CurrentProfile` but dormant (a depth-integrated
drag is a deferred engine edit). Public-provider data only (NOAA public domain in
CI); no client-derived archive tables are read here.
"""
from __future__ import annotations

from typing import Iterable, Mapping, Optional, Sequence

from digitalmodel.drilling_riser.envelope import CurrentProfile, SeaState
from digitalmodel.orcaflex.weather_window import ScatterDiagram

#: Default Hs / Tp upper-edge bins (match ScatterDiagram's DNV-RP-C205 defaults).
_DEFAULT_HS_BINS = [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0]
_DEFAULT_TP_BINS = [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]


def _bin_index(value: float, upper_edges: Sequence[float]) -> Optional[int]:
    """Index of the first bin whose UPPER edge is >= value; None if above range."""
    for idx, edge in enumerate(upper_edges):
        if value <= edge:
            return idx
    return None


def reduce_to_scatter(
    records: Iterable[Mapping[str, object]],
    *,
    hs_bins: Sequence[float] = tuple(_DEFAULT_HS_BINS),
    tp_bins: Sequence[float] = tuple(_DEFAULT_TP_BINS),
    hs_key: str = "wave_height",
    tp_key: str = "wave_period",
) -> ScatterDiagram:
    """Bin a metocean record stream into an Hs-Tp occurrence ScatterDiagram.

    ``hs_key`` / ``tp_key`` name the record fields (NOAA raw records use
    ``dominant_wave_period`` for Tp; the abstract client normalizes to
    ``wave_period``). Records missing/None Hs or Tp, or outside the bin ranges,
    are skipped. ``occurrences[i][j]`` is the count in Hs-bin i x Tp-bin j.
    """
    hs_bins = list(hs_bins)
    tp_bins = list(tp_bins)
    occ = [[0.0] * len(tp_bins) for _ in hs_bins]
    for rec in records:
        hs = rec.get(hs_key)
        tp = rec.get(tp_key)
        if hs is None or tp is None:
            continue
        i = _bin_index(float(hs), hs_bins)
        j = _bin_index(float(tp), tp_bins)
        if i is None or j is None:
            continue
        occ[i][j] += 1.0
    return ScatterDiagram(hs_bins=hs_bins, tp_bins=tp_bins, occurrences=occ)


def scatter_to_seastates(
    scatter: ScatterDiagram, *, min_occurrence: float = 0.0
) -> list[SeaState]:
    """Emit a ``SeaState`` at each occupied cell's Hs/Tp **midpoint**.

    Bins are upper edges; the midpoint of bin i is ``(lower_edge + upper_edge)/2``
    (lower edge = the previous upper edge, or 0 for the first bin). Only cells
    with ``occurrences > min_occurrence`` are emitted.
    """
    seastates: list[SeaState] = []
    occ = scatter.occurrences or []
    for i, hs_upper in enumerate(scatter.hs_bins):
        hs_lower = scatter.hs_bins[i - 1] if i > 0 else 0.0
        hs_mid = (hs_lower + hs_upper) / 2.0
        for j, tp_upper in enumerate(scatter.tp_bins):
            tp_lower = scatter.tp_bins[j - 1] if j > 0 else 0.0
            tp_mid = (tp_lower + tp_upper) / 2.0
            count = occ[i][j] if i < len(occ) and j < len(occ[i]) else 0.0
            if count > min_occurrence:
                seastates.append(SeaState(hs_m=hs_mid, tp_s=tp_mid))
    return seastates


def current_to_surface_speeds(
    profiles: CurrentProfile | Sequence[CurrentProfile],
) -> list[float]:
    """Surface current speed(s) [m/s] the envelope sweep consumes.

    Accepts one profile or a sequence. The dormant depth structure is carried on
    the profile but not integrated by the envelope yet.
    """
    if isinstance(profiles, CurrentProfile):
        profiles = [profiles]
    return [p.surface_speed_mps for p in profiles]
