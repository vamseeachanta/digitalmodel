# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic worked example; see docs/domains/artificial_lift/dynacard-quantities.md."""

import json

from digitalmodel.marine_ops.artificial_lift.dynacard import (
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
    SurfaceUnit,
    calculate_card_geometry,
    calculate_shape_similarity,
    estimate_neutral_point,
)


def run_example():
    """Return quantities for explicitly synthetic inputs, not design defaults.

    Hand calculation: rectangle A = 12*100 = 1200 lb.in, C = (6,50).
    Travel is 2*12 = 24 in; load excursion is 2*100 = 200 lb.
    Normalized perimeter is 2*(12/12 + 100/100) = 4 (dimensionless).
    A reflected centered load vector has cosine -1 with its reference.
    Sources: polygon centroid note (1988); NIST cosine definition (2018).
    """
    card = CardData(position=[0, 0, 12, 12], load=[0, 100, 100, 0])
    context = DynacardAnalysisContext(
        api14="synthetic",
        surface_card=card,
        rod_string=[RodSection(diameter=1, length=100)],
        pump=PumpProperties(diameter=1, depth=100),
        surface_unit=SurfaceUnit(stroke_length=12),
        spm=10,
    )
    geometry = calculate_card_geometry(context, raise_on_error=True)
    reflected = CardData(position=card.position, load=[100 - value for value in card.load])
    return {
        "area_lb_in": float(geometry.area),
        "centroid_position_in": geometry.centroid_position,
        "centroid_load_lb": geometry.centroid_load,
        "position_path_length_in": geometry.position_path_length,
        "load_path_length_lb": geometry.load_path_length,
        "normalized_perimeter": geometry.normalized_perimeter,
        "physical_perimeter": geometry.model_dump()["perimeter"],
        # Presentation rounding only; the calculator retains full precision.
        "reflected_load_similarity": round(calculate_shape_similarity(card, reflected), 1),
        "neutral_point_depth_ft": estimate_neutral_point(100, 0, 50, rod_length=100),
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2))
