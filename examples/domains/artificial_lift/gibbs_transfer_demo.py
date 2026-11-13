"""Synthetic quarter-wave example; inputs and hand solution in gibbs-transfer.md.

Basis: Applied Mechanics of Solids, online edition read 2026-10-19, section
4.3.3, and the independent quarter-wave hand derivation in that document.
All numerical inputs below define the mathematical example, not field data.
"""

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import (
    DynacardPhysicsSolver,
)


def main():
    # 64 distinct phase samples resolve the prescribed harmonic exactly.
    theta = 2 * np.pi * np.arange(64) / 64
    context = DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=CardData(
            position=(2 * np.cos(theta)).tolist(),
            load=(20 + 12 * np.sin(theta)).tolist(),
        ),
        rod_string=[
            RodSection(
                diameter=2 / np.sqrt(np.pi),
                length=15,
                modulus_of_elasticity=1440,
                speed_of_sound=60,
                density=144,
                damping_factor=0,
            )
        ],
        pump=PumpProperties(diameter=1, depth=15),
        spm=60,
        fluid_density=0,
    )
    result = DynacardPhysicsSolver(context).solve_wave_equation()
    # Exact hand results: W=15 lb, EA*omega/a=4*pi lb/in; see module docstring.
    # Absolute tolerance budgets floating arithmetic, not engineering accuracy.
    np.testing.assert_allclose(
        result.downhole_card.load, 5 + 8 * np.pi * np.cos(theta), atol=1e-12
    )
    np.testing.assert_allclose(
        np.ptp(result.downhole_card.position), 6 / np.pi, atol=1e-12
    )
    print(f"Pump stroke: {np.ptp(result.downhole_card.position):.1f} in")
    print(f"Mean pump tension: {np.mean(result.downhole_card.load):.1f} lb")
    print("Field diagnostic validity: NOT_EVALUATED")


if __name__ == "__main__":
    main()
