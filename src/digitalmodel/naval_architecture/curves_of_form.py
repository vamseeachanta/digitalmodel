# ABOUTME: Curves of form — displacement, KM, TPI, MT1 vs draft interpolation
# ABOUTME: Requires ship-specific hydrostatic data tables (DDG51, FFG-7, etc.)
"""
Curves of form reader and interpolator.

Provides displacement, KM, TPI, MT1" as functions of draft
for specific vessel types. Requires ship data tables.
"""


def displacement_at_draft(vessel: str, draft_ft: float) -> float:
    """Interpolate displacement from curves of form at given draft.

    Args:
        vessel: vessel identifier (e.g. "DDG51", "FFG7")
        draft_ft: mean draft in feet

    Returns:
        Displacement in long tons

    Raises:
        NotImplementedError: ship data tables not yet loaded
    """
    raise NotImplementedError(
        f"Curves of form data for {vessel} not yet available. "
        "Need hydrostatic tables from ship design data."
    )
