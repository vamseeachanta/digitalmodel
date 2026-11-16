"""Mooring endpoint coordinates and straight endpoint-to-endpoint distance.

A legacy engineering workbook prepares endpoint geometry with
x = x_fairlead + span*cos(bearing), y = y_fairlead + span*sin(bearing).
The existing dynmoor.anchor_position kernel is reused directly with SI spans.
Offsets are translated in SI to preserve coincident-point identities.
Bearings are in degrees from positive x towards positive y in this coordinate
plane. Optional reflections negate the entire translated x or y coordinate.
The chord is the Euclidean distance between fairlead and the final anchor.

No standard reference is stated in the source for this geometry; it does not
establish standards compliance.

Inputs are SI coordinates and span [m], bearing [deg], and explicit boolean
reflection choices. Limits and Input gaps: this is straight chord geometry,
not a catenary length, force or pretension solver. Coordinate datum, axis
orientation, reflection choices and anchor elevation must be supplied. Dynamic
response and line strength acceptance remain outside this method.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
import math
import numbers

from digitalmodel.subsea.mooring_analysis.dynmoor import DynmoorLine, anchor_position


@dataclass(frozen=True)
class EndpointInput:
    """Endpoint coordinates and a horizontal span in a caller-defined plane."""

    fairlead_x_m: float
    fairlead_y_m: float
    fairlead_z_m: float
    horizontal_span_m: float
    bearing_deg: float
    anchor_z_m: float
    reflect_x: bool
    reflect_y: bool

    def __post_init__(self) -> None:
        for field in fields(self):
            value = getattr(self, field.name)
            if field.name in {"reflect_x", "reflect_y"}:
                if not isinstance(value, bool):
                    raise ValueError(f"{field.name} must be a bool")
            elif (isinstance(value, bool) or not isinstance(value, numbers.Real)
                  or not math.isfinite(value)):
                raise ValueError(f"{field.name} must be a finite number")
        if self.horizontal_span_m < 0:
            raise ValueError("horizontal_span_m must be nonnegative")


@dataclass(frozen=True)
class EndpointResult:
    """Final anchor coordinates [m] and straight three-dimensional chord [m]."""

    anchor_x_m: float
    anchor_y_m: float
    anchor_z_m: float
    chord_length_m: float


def mooring_endpoint_geometry(inputs: EndpointInput) -> EndpointResult:
    """Adapt SI geometry to the existing planar kernel, then apply reflections.

    Reflection is applied to the anchor coordinate only; the chord is measured
    from the unreflected fairlead, so with reflection it is a bookkeeping
    distance, not a line length.
    """
    line = DynmoorLine(
        line_type=1,  # Unused metadata for the planar-coordinate kernel.
        fairlead_x_ft=0.0,
        fairlead_y_ft=0.0,
        angle_deg=inputs.bearing_deg,
        pretension_percent=0.0,  # No pretension calculation is requested.
    )
    # The kernel is unit-agnostic despite its _ft field names.
    x_m, y_m = anchor_position(line, inputs.horizontal_span_m)
    x_m = inputs.fairlead_x_m + x_m
    y_m = inputs.fairlead_y_m + y_m
    if inputs.reflect_x:
        x_m = -x_m
    if inputs.reflect_y:
        y_m = -y_m
    chord = math.dist((inputs.fairlead_x_m, inputs.fairlead_y_m, inputs.fairlead_z_m),
                      (x_m, y_m, inputs.anchor_z_m))
    return EndpointResult(x_m, y_m, inputs.anchor_z_m, chord)
