"""Illustrative ground-supported tank; assumed site and tank parameters.

Run from the repository root with PYTHONPATH=src uv run python
examples/hydrodynamics/api650_annex_e.py. Output is SI JSON with citations.
The example is arithmetic validation, not a qualified site or tank design.
"""
from dataclasses import asdict
import json

from digitalmodel.hydrodynamics.sloshing import AnnexEResult, api650_annex_e


def worked_example() -> AnnexEResult:
    """20 m water tank, SUG II; all project inputs are illustrative assumptions."""
    return api650_annex_e(
        diameter_m=20.0, fill_height_m=10.0, liquid_density_kg_m3=1000.0,
        shell_weight_n=1_000_000.0, roof_weight_n=200_000.0,
        bottom_weight_n=190_000.0,  # about an 8 mm steel bottom plate
        shell_cg_m=5.5, roof_cg_m=11.0, roof_reaction_n=200_000.0,
        shell_thickness_m=0.012,  # net wall = 12.000 mm
        sds_g=0.6, sd1_g=0.3, tl_s=8.0,
        importance_factor=1.25, rwi=3.5, rwc=2.0, k=1.5,
        minimum_ai_g=0.007, vertical_acceleration_g=0.0,
        # Below the E.6.2.1.1-1a cap 201.1*H*D*Ge = 40,220 N/m for this tank.
        annular_resistance_n_per_m=40_000.0, seismic_use_group="II",
        available_freeboard_m=1.0, purchaser_freeboard=False,
    )


if __name__ == "__main__":
    print(json.dumps(asdict(worked_example()), indent=2, allow_nan=False))
