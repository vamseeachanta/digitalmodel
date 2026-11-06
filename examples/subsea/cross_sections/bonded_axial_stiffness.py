# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic bonded-annulus example; no material-table or standards values.

Hand calculation: radii 1, 2, 3 m give A1=3*pi and A2=5*pi m².
Assumed moduli 10 and 20 Pa give EA=130*pi N and Eeq=130/8 Pa.
Basis: OpenStax Prealgebra 2e, section 9.5, and University Physics
Volume 1, section 12.3. See the bonded axial stiffness guide for links.
"""

import math

from digitalmodel.subsea.cross_sections.calculations import (
    BondedAxialLayer,
    bonded_axial_stiffness,
)
from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer, UnitValue


def main() -> None:
    """Run and check the saved example against independent arithmetic."""
    geometry_source = Provenance(
        source_id="synthetic-geometry", source_type="project_assumption"
    )
    modulus_source = Provenance(
        source_id="synthetic-modulus", source_type="project_assumption",
        note="Assumed for arithmetic only; not a material property.",
    )
    layers = [
        BondedAxialLayer(
            layer=RadialLayer(
                name=name, role="example", material="unspecified",
                inner_diameter=UnitValue(value=inner, unit="m"),
                outer_diameter=UnitValue(value=outer, unit="m"),
                provenance=geometry_source,
            ),
            youngs_modulus_pa=modulus,
            modulus_unit="Pa",
            modulus_provenance=modulus_source,
        )
        for name, inner, outer, modulus in (
            ("inner annulus", 2, 4, 10), ("outer annulus", 4, 6, 20)
        )
    ]
    result = bonded_axial_stiffness(layers)
    # Relative tolerance permits binary64 rounding, not modelling uncertainty.
    assert math.isclose(result.area_m2, 8 * math.pi, rel_tol=1e-12)
    assert math.isclose(result.axial_stiffness_n, 130 * math.pi, rel_tol=1e-12)
    assert math.isclose(result.equivalent_modulus_pa, 130 / 8, rel_tol=1e-12)
    print(f"Material area: {result.area_m2:.1f} m²")
    print(f"Axial rigidity EA: {result.axial_stiffness_n:.1f} N")
    print(f"Equivalent modulus: {result.equivalent_modulus_pa:.1f} Pa")


if __name__ == "__main__":
    main()
