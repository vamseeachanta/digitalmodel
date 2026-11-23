"""Drilling riser tool passage calculations — clearance, spacing, minimum ID.

Implements tool passage analysis per 144507 SpaceOut drawing data
and API SPEC 16F riser running equipment requirements.
"""

from __future__ import annotations

# ---------------------------------------------------------------------------
# Default margins (industry practice)
# ---------------------------------------------------------------------------
DEFAULT_MARGIN_M: float = 1.5  # [m] default spacing margin for tool passage


def annular_clearance_mm(
    riser_id_mm: float,
    tool_od_mm: float,
) -> float:
    """Calculate radial annular clearance between riser ID and tool OD.

    clearance = (riser_ID - tool_OD) / 2

    Per 144507 SpaceOut — tool passage clearance assessment.

    Parameters
    ----------
    riser_id_mm : float
        Riser internal diameter [mm].
    tool_od_mm : float
        Tool outer diameter [mm].

    Returns
    -------
    float
        Radial annular clearance [mm].
    """
    return (riser_id_mm - tool_od_mm) / 2.0


def minimum_riser_id_required(
    tool_od_mm: float,
    clearance_mm: float,
) -> float:
    """Calculate minimum riser ID required for tool passage.

    min_ID = tool_OD + 2 * clearance

    Parameters
    ----------
    tool_od_mm : float
        Tool outer diameter [mm].
    clearance_mm : float
        Required radial clearance [mm].

    Returns
    -------
    float
        Minimum required riser internal diameter [mm].
    """
    return tool_od_mm + 2.0 * clearance_mm


def spacing_requirement_m(
    connector_length_m: float,
    tool_length_m: float,
    margin_m: float = DEFAULT_MARGIN_M,
) -> float:
    """Calculate connector spacing requirement for tool passage.

    spacing = connector_length + tool_length + margin

    Per 144507 SpaceOut drawing — component spacing.

    Parameters
    ----------
    connector_length_m : float
        Riser connector length [m].
    tool_length_m : float
        Downhole tool assembly length [m].
    margin_m : float, optional
        Safety margin [m]. Default 1.5.

    Returns
    -------
    float
        Required spacing between connectors [m].
    """
    return connector_length_m + tool_length_m + margin_m
