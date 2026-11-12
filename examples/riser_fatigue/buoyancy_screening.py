"""Synthetic SLWR excursion ranking; print JSON without writing result files.

Run with the package dependencies installed and PYTHONPATH=src:
    python examples/riser_fatigue/buoyancy_screening.py

All dimensions, loads, stresses, PSD ordinates and criteria are invented inputs
for the hand calculation in docs/riser-buoyancy-screening.html. They describe no
real installation and shall not be used as engineering design data.
"""

from dataclasses import replace
import json

import pandas as pd

from digitalmodel.parametric.atlas import Atlas, Axis
from digitalmodel.riser_fatigue.buoyancy_screening import (
    BuoyancyLayout,
    CandidateResponse,
    ConstraintLimits,
    ExcursionResponse,
    SpectralBasis,
    rank_buoyancy_candidates,
    stress_spectra_from_excursions,
)


def worked_example() -> dict:
    """Return the independently calculable example with both validity tiers."""
    # Synthetic input basis: docs/riser-buoyancy-screening.html, worked example.
    layouts = {
        name: BuoyancyLayout(count, 2.0, 1.0, 100.0, 10.0)
        for name, count in (("a", 4), ("b", 6), ("c", 8))
    }
    atlas = Atlas(
        basename="slwr-screen",
        atlas_id="synthetic-example",
        physics="linear",
        response="unused",
        axes=[Axis("module_count", grid=[4, 6, 8])],
        grid=pd.DataFrame(
            [
                {"case_id": name, **layout.to_dict(), "unused": 0.0}
                for name, layout in layouts.items()
            ]
        ),
    )
    responses = {}
    for name, scale in (("a", 1.0), ("b", 0.5), ("c", 0.25)):
        stress_profiles = (
            (10.0, 10.0, 10.0),
            tuple(10.0 + scale * r for r in (4.0, 2.0, 6.0)),
            tuple(10.0 + scale * r for r in (2.0, 1.0, 3.0)),
        )
        # C has the lowest proxy but exceeds the caller's tension limit.
        tension = 140.0 if name == "c" else 100.0
        excursions = tuple(
            ExcursionResponse(case, stress, tension, 20.0, 25.0, 40.0, 3.0)
            for case, stress in zip(("near", "far", "cross"), stress_profiles)
        )
        response = CandidateResponse(
            layouts[name],
            (0.0, 50.0, 100.0),
            (50.0, 100.0),
            excursions,
            "synthetic hand calculation; geometry and dynamics not qualified",
        )
        # Assumed scalar transfer G=(stress range / 2 m)**2 * motion PSD.
        # PSD [0,1,0] on [0,1,2] Hz has trapezoidal variance 1 m².
        spectra = stress_spectra_from_excursions(
            response,
            (0.0, 1.0, 2.0),
            (0.0, 1.0, 0.0),
            2.0,
            "synthetic motion PSD and scalar transfer assumption",
        )
        responses[name] = replace(response, spectra=spectra)
    limits = ConstraintLimits(120.0, 10.0, 5.0, 2.0, "synthetic example limits")
    # Invented N=1000/S²; T=10 s. No standard curve values are embedded.
    basis = SpectralBasis(2.0, 3.0, 10.0, "synthetic power-law S-N equation")
    return rank_buoyancy_candidates(atlas, responses, limits, basis).to_dict()


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
