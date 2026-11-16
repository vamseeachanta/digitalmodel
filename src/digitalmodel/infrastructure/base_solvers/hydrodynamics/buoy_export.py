"""Format supplied buoy properties without calculating or validating them.

Helpers mutate the supplied output dictionary, preserve unrelated keys and input
properties, and return None. Missing keys and invalid scalars propagate errors;
rotational fields already written before an error remain in the output.
"""


def update_rotational_buoy_data(properties: dict, zone: str, buoy_data: dict) -> None:
    """Export rotational vectors in x/y/z order with three-decimal rounding."""
    DragAreaMoment = [
        round(properties["rotational"]["am"][zone]["x"], 3),
        round(properties["rotational"]["am"][zone]["y"], 3),
        round(properties["rotational"]["am"][zone]["z"], 3),
    ]
    buoy_data.update({"DragAreaMoment": DragAreaMoment})

    DragMomentCoefficient = [
        round(properties["rotational"]["cd"]["x"], 3),
        round(properties["rotational"]["cd"]["y"], 3),
        round(properties["rotational"]["cd"]["z"], 3),
    ]
    buoy_data.update({"DragMomentCoefficient": DragMomentCoefficient})

    HydrodynamicInertia = [
        round(properties["rotational"]["I"]["x"], 3),
        round(properties["rotational"]["I"]["y"], 3),
        round(properties["rotational"]["I"]["z"], 3),
    ]
    buoy_data.update({"HydrodynamicInertia": HydrodynamicInertia})

    AddedInertiaCoefficient = [
        float(round(properties["rotational"]["ca"]["x"], 3)),
        float(round(properties["rotational"]["ca"]["y"], 3)),
        float(round(properties["rotational"]["ca"]["z"], 3)),
    ]
    buoy_data.update({"AddedInertiaCoefficient": AddedInertiaCoefficient})


def update_mass_inertia_buoy_data(properties: dict, buoy_data: dict) -> None:
    """Export principal inertia from kg.m^2 to t.m^2 in Ix/Iy/Iz order."""
    # kg.m^2 -> t.m^2: NIST SP 811 (2008), section 5.1.1, Table 6.
    # https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-5-units-outside-si
    # Preserve the existing three-decimal export representation.
    MomentsOfInertia = [
        round(properties["I"]["Ix"] / 1000, 3),
        round(properties["I"]["Iy"] / 1000, 3),
        round(properties["I"]["Iz"] / 1000, 3),
    ]
    buoy_data.update({"MomentsOfInertia": MomentsOfInertia})
