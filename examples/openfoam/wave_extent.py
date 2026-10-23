"""Synthetic wake coverage example; derive a case without running a solver.

All geometry, speeds and the requested two wavelengths are illustrative inputs.
Method: Ship wakes: Kelvin or Mach angle? (2013), arXiv:1304.2653, pp. 1–2.
The existing builder uses standard gravity for its wavelength and Froude number.
"""

from pathlib import Path

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, derive_hull_case
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest


def example_results() -> list[dict[str, float]]:
    """Return the speed-dependent extent for one synthetic hull."""
    manifest = HullManifest.from_dict(
        {
            "source_file": "synthetic_hull.stl",
            "source_sha256": "0" * 64,
            "units_in": "m",
            "scale_to_m": 1.0,
            "orientation": {"x": "forward", "y": "port", "z": "up"},
            "origin": "aft_perpendicular_keel",
            "lpp_m": 6.0,
            "beam_m": 0.9,
            "draft_m": 0.3,
            "wetted_surface_m2": 7.2,
            "displacement_m3": 0.72,
            "watertight": True,
            "n_triangles": 12,
            "bbox_min_m": [-0.05, -0.45, 0.0],
            "bbox_max_m": [6.15, 0.45, 0.55],
        }
    )
    rows = []
    for speed in (1.0, 2.0):
        case = derive_hull_case(
            HullCaseConfig(
                manifest=manifest,
                stl_path=Path("synthetic_hull.stl"),
                velocity=speed,
                ranks=8,
                free_surface_wake_wavelengths=2.0,
            )
        )
        wave = case.wave_extent
        rows.append(
            {
                "speed_m_s": speed,
                "wavelength_m": wave.wavelength_m,
                "wake_length_m": wave.wake_length_m,
                "aft_half_width_m": wave.half_width_aft_m,
                "achieved_wavelengths": wave.achieved_wake_wavelengths,
            }
        )
    return rows


if __name__ == "__main__":
    for row in example_results():
        print(" ".join(f"{name}={value:.1f}" for name, value in row.items()))
