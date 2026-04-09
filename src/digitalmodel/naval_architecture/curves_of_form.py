# ABOUTME: Curves of form — displacement, KM, TPI, MT1 vs draft interpolation
# ABOUTME: Requires ship-specific hydrostatic data tables (DDG51, FFG-7, etc.)
"""
Curves of form reader and interpolator.

Provides displacement, KM, TPI, MT1" as functions of draft
for specific vessel types. Requires ship data tables.
"""


def displacement_at_draft(vessel: str, draft_ft: float) -> float:
    """Interpolate displacement from curves of form at given draft.

    For vessels with tabular curves of form data (e.g. DDG-51), performs
    linear interpolation. For registered fleet vessels without tabular data,
    estimates using the block coefficient method.
    """
    from digitalmodel.naval_architecture.ship_data import (
        get_curves_of_form, get_ship, estimate_vessel_hydrostatics,
    )

    cof = get_curves_of_form(vessel)
    if cof is not None:
        drafts = cof["drafts_ft"]
        disps = cof["displacement_lt"]
        return _interpolate(drafts, disps, draft_ft)

    ship = get_ship(vessel)
    if ship is not None:
        hydro = estimate_vessel_hydrostatics(ship)
        if hydro is not None:
            cb = hydro["cb"]
            loa = float(ship.get("loa_ft", 0))
            beam = float(ship.get("beam_ft", 0))
            if loa > 0 and beam > 0 and draft_ft > 0:
                vol = cb * loa * beam * draft_ft
                return 64.0 * vol / 2240.0

    raise NotImplementedError(
        f"Curves of form data for {vessel} not yet available. "
        "Need hydrostatic tables from ship design data."
    )


def _interpolate(xs, ys, x):
    """Linear interpolation with clamping at boundaries."""
    if x <= xs[0]:
        return float(ys[0])
    if x >= xs[-1]:
        return float(ys[-1])
    for i in range(1, len(xs)):
        if xs[i] >= x:
            t = (x - xs[i - 1]) / (xs[i] - xs[i - 1])
            return ys[i - 1] + t * (ys[i] - ys[i - 1])
    return float(ys[-1])
