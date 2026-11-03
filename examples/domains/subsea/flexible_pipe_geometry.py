# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Print synthetic full-wall geometry and mass; see flexible_pipe_geometry.md."""

from digitalmodel.subsea.flexible_pipes.cross_section import (
    FlexiblePipeSection,
    Layer,
    LayerType,
)

# Source for all inputs: flexible_pipe_geometry.md, Synthetic inputs.
# These chosen densities illustrate arithmetic, not material specifications.
EXTERNAL_FLUID_DENSITY = 800.0  # kg/m^3, explicitly supplied synthetic fluid
BORE_FLUID_DENSITY = 600.0  # kg/m^3, explicitly supplied synthetic contents


def build_section() -> FlexiblePipeSection:
    """Build the note's contiguous full-wall layers from bore to exterior."""
    return FlexiblePipeSection(
        layers=[
            Layer(
                LayerType.PRESSURE_SHEATH,
                inner_diameter=0.100,
                thickness=0.010,
                density=1000.0,
            ),
            Layer(
                LayerType.ANTI_WEAR,
                inner_diameter=0.120,
                thickness=0.020,
                density=2000.0,
            ),
            Layer(
                LayerType.OUTER_SHEATH,
                inner_diameter=0.160,
                thickness=0.020,
                density=500.0,
            ),
        ],
    )


def main() -> None:
    """Print unit-labelled layer areas and masses per unit pipe length."""
    section = build_section()
    print("Synthetic full-wall geometry only; design qualification remains open.")
    print(f"Bore diameter: {section.bore_diameter:.3f} m")
    print(f"Outer diameter: {section.outer_diameter:.3f} m")
    for index, layer in enumerate(section.layers, start=1):
        label = f"Layer {index} ({layer.layer_type.value})"
        print(f"{label} annular area: {layer.annular_area:.9f} m^2")
        print(f"{label} dry mass: {layer.mass_per_length():.6f} kg/m")
    print(f"Total dry mass: {section.dry_mass_per_length():.6f} kg/m")
    displaced_mass = section.displaced_mass_per_length(
        fluid_density=EXTERNAL_FLUID_DENSITY,
    )
    content_mass = section.content_mass_per_length(content_density=BORE_FLUID_DENSITY)
    print(
        f"Displaced mass (external fluid density {EXTERNAL_FLUID_DENSITY:g} kg/m^3): "
        f"{displaced_mass:.6f} kg/m"
    )
    print(
        f"Content mass (bore fluid density {BORE_FLUID_DENSITY:g} kg/m^3): "
        f"{content_mass:.6f} kg/m"
    )


if __name__ == "__main__":
    main()
