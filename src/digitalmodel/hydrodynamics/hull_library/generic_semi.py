# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate explicitly assumed proportions for generic drilling semi-submersibles."""

from math import isfinite

from .column_pontoon_form import ColumnPontoonParameters


def generic_semi_parameters(
    loa: float,
    beam: float,
    draft: float,
    panel_target_size: float,
) -> ColumnPontoonParameters:
    """Return a four-column, twin-pontoon geometry rounded to 0.1 m.

    Every proportion is assumed, based on previous experience. This geometry
    omits braces, column rotation and pontoon overhangs; its lid is disabled.

    Args:
        loa: Overall longitudinal extent in metres.
        beam: Overall transverse extent in metres.
        draft: Operating draft in metres.
        panel_target_size: Generator target edge size in metres.

    Returns:
        Validated rectangular column-and-pontoon parameters.

    Raises:
        ValueError: If dimensions are nonpositive, nonfinite or incompatible.
    """
    if any(not isfinite(value) or value <= 0
           for value in (loa, beam, draft, panel_target_size)):
        raise ValueError("principal dimensions and panel size must be finite and positive")
    # All proportions below: assumed, based on previous experience.
    side = round(0.177 * loa, 1)
    radius = round(0.088 * side, 1)
    width = min(round(0.794 * side, 1), round(side - 2 * radius, 1))
    height = round(0.44 * draft, 1)
    return ColumnPontoonParameters(
        count=4, spacing=round(loa - side, 1),
        transverse_spacing=round(beam - side, 1),
        square_side=side, corner_radius=radius, draft=draft,
        pontoon_layout="twin", pontoon_width=width, pontoon_height=height,
        pontoon_corner_radius=round(0.1 * height, 1),
        panel_target_size=panel_target_size, lid=False,
    )
