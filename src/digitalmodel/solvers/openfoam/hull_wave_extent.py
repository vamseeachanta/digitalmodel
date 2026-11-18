"""Wavelength-scaled refinement coverage for calm-water deep-water cases.

Source: Rabaud & Moisy, "Ship wakes: Kelvin or Mach angle?",
Phys. Rev. Lett. 110, 214503 (2013), arXiv:1304.2653, pp. 1–2. The transverse
wavelength is 2*pi*U**2/g and the Kelvin envelope has sin(alpha)=1/3, hence
tan(alpha)=1/sqrt(8). Wake length is a required
caller criterion, not an inferred universal acceptance limit. Kelvin geometry
is a bounding choice; the paper finds narrower wakes at high Froude number.
Newman, Marine Hydrodynamics (2018), sections 6.2–6.3 and 6.10, gives the
half-wavelength depth screen and the classical Kelvin construction.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Sequence

from .hull_domain import Box, HullDomain
from .hull_free_surface import deep_water_wavelength
from .hull_manifest import HullManifest

# Exact geometric consequence of sin(alpha)=1/3 in the cited 2013 paper.
_KELVIN_SLOPE = 1 / math.sqrt(8)


class WaveExtentError(ValueError):
    """A requested wave region cannot be provided by the emitted domain."""


@dataclass(frozen=True)
class WaveExtent:
    """The requested and achieved cell-selection envelope in case coordinates."""

    wavelength_m: float
    required_wake_wavelengths: float | None
    achieved_wake_wavelengths: float
    x_aft_m: float
    x_bow_m: float
    half_width_bow_m: float
    half_width_aft_m: float
    water_depth_m: float

    @property
    def enabled(self) -> bool:
        return self.required_wake_wavelengths is not None

    @property
    def wake_length_m(self) -> float:
        return self.achieved_wake_wavelengths * self.wavelength_m

    @property
    def kelvin_half_angle_deg(self) -> float:
        return math.degrees(math.atan(_KELVIN_SLOPE))

    def to_provenance(self) -> dict[str, Any]:
        deep_water = self.water_depth_m >= self.wavelength_m / 2
        if not self.enabled:
            reason = "wake wavelengths is a required input to evaluate extent"
        elif not deep_water:
            reason = "finite-depth wave extent not evaluated: h < wavelength/2"
        else:
            reason = "requested deep-water wave envelope fits the domain"
        return {
            "status": "PASS" if self.enabled and deep_water else "NOT_EVALUATED",
            "reason": reason,
            "water_depth_m": self.water_depth_m,
            "depth_wavelength_ratio": self.water_depth_m / self.wavelength_m,
            "depth_basis": (
                "h >= wavelength/2; Newman, Marine Hydrodynamics (2018), "
                "sections 6.2–6.3"
            ),
            "required_wake_wavelengths": self.required_wake_wavelengths,
            "achieved_wake_wavelengths": self.achieved_wake_wavelengths,
            "wake_length_m": self.wake_length_m,
            "wavelength_m": self.wavelength_m,
            "kelvin_half_angle_deg": self.kelvin_half_angle_deg,
            "x_aft_m": self.x_aft_m,
            "x_bow_m": self.x_bow_m,
            "half_width_bow_m": self.half_width_bow_m,
            "half_width_aft_m": self.half_width_aft_m,
            "source": (
                "Rabaud & Moisy, Ship wakes: Kelvin or Mach angle?, "
                "Phys. Rev. Lett. 110, 214503 (2013), arXiv:1304.2653, pp. 1–2; "
                "Newman, Marine Hydrodynamics (2018), section 6.10"
            ),
            "coverage_basis": (
                "hull boxes union a bow-origin Kelvin trapezoid at every stage; "
                "wake length is measured aft of the actual stern; each stage "
                "uses its hull box z limits; inside and intersecting cells selected"
                if self.enabled
                else "largest complete Kelvin envelope inside the finest hull box"
            ),
            "cell_budget_basis": (
                "enclosing boxes for hull and wave union; uniform-z cell estimate; "
                "cut-cell growth beyond prism bounds is not included"
            ),
            "limits": (
                "geometric pre-mesh coverage with a depth screen only; "
                "finite-depth wave propagation, wave amplitude, "
                "mesh quality, reflection and force convergence are not evaluated"
            ),
        }

    def budget_boxes(self, boxes: Sequence[Box]) -> list[Box]:
        """Nested enclosing estimates of the hull and wave-region cell cost."""
        if not self.enabled:
            return list(boxes)
        return [
            ((min(lo[0], self.x_aft_m), min(lo[1], -self.half_width_aft_m), lo[2]), hi)
            for lo, hi in boxes
        ]


def derive_wave_extent(
    manifest: HullManifest,
    domain: HullDomain,
    boxes: Sequence[Box],
    velocity: float,
    wake_wavelengths: float | None,
) -> WaveExtent:
    """Refuse a clipped criterion before writing any case files.

    Bow and stern use the geometry's coordinates, without translating the hull.
    The initial half width is the hull half beam, so waves from either side
    remain within the envelope. No far-field dimensions are silently enlarged.
    """
    if not math.isfinite(velocity) or velocity <= 0:
        raise WaveExtentError("velocity must be finite and positive")
    if wake_wavelengths is not None and (
        not math.isfinite(wake_wavelengths) or wake_wavelengths <= 0
    ):
        raise WaveExtentError("wake_wavelengths must be finite and positive")
    wavelength = deep_water_wavelength(velocity)
    if not math.isfinite(wavelength) or wavelength <= 0:
        raise WaveExtentError("velocity produces a non-finite or zero wavelength")
    stern, bow = manifest.bbox_min_m[0], manifest.bbox_max_m[0]
    half_beam = max(manifest.half_beam_m, -manifest.bbox_min_m[1])
    if wake_wavelengths is None:
        lo, _ = boxes[-1]
        # Solve b+(bow-x)*tan(alpha) <= finest half width, and x >= box x_lo.
        reach = max(
            0.0,
            min(stern - lo[0], (-lo[1] - half_beam) / _KELVIN_SLOPE - (bow - stern)),
        )
        achieved = reach / wavelength
    else:
        reach = wake_wavelengths * wavelength
    aft = stern - reach
    if wake_wavelengths is not None:
        # Round the coordinate outward if subtraction loses requested reach;
        # report coverage from the coordinates actually written to the STL.
        if (stern - aft) / wavelength < wake_wavelengths:
            aft = math.nextafter(aft, -math.inf)
        achieved = (stern - aft) / wavelength
    width = half_beam + (bow - aft) * _KELVIN_SLOPE
    if wake_wavelengths is not None:
        if not math.isfinite(aft) or not math.isfinite(width):
            raise WaveExtentError("wake_wavelengths produces non-finite extent")
        if aft < domain.x_outlet:
            raise WaveExtentError(
                "wave extent cannot fit: increase downstream_lpp or reduce "
                "free_surface_wake_wavelengths"
            )
        if -width < domain.y_side:
            raise WaveExtentError(
                "Kelvin envelope cannot fit: increase lateral_lpp or reduce "
                "free_surface_wake_wavelengths"
            )
        if bow >= domain.x_inlet:
            raise WaveExtentError("wave bow lies outside domain: increase upstream_lpp")
    return WaveExtent(
        wavelength,
        wake_wavelengths,
        achieved,
        aft,
        bow,
        half_beam,
        width,
        domain.waterline - domain.z_levels[0],
    )


def wave_selection_tokens(
    wave: WaveExtent, domain: HullDomain, stages: int, divisions: Mapping[str, int]
) -> dict[str, str]:
    """Add the wave surface to c0 after the existing hull box selection.

    Key set and negative distance/curvature sentinels: OpenFOAM v2312,
    surfaceToCell.C dictionary constructor (lines 431–461) and checkSettings.
    https://api.openfoam.com/2312/surfaceToCell_8C_source.html#l00431
    includeCut also selects boundary-intersecting cells, protecting the envelope.
    """
    # Use the emitted divisions, including speed-driven in-plane refinement.
    # A third of a cell avoids both background and dyadic refinement faces.
    # Keep x within the last finest possible cell and ahead of the prism bow.
    dx = domain.length / divisions["nx"] / 2**stages
    dy = domain.width / divisions["ny"]
    dz = (domain.z_levels[3] - domain.z_levels[2]) / divisions["nzb"]
    outside = (
        domain.x_inlet - min(dx, domain.x_inlet - wave.x_bow_m) / 3,
        domain.y_side + dy / 3,
        domain.z_levels[2] + dz / 3,
    )
    point = " ".join(f"{v:.17g}" for v in outside)
    return {
        f"WAVE{i}": (
            "    {\n        name    c0;\n        type    cellSet;\n"
            "        action  add;\n        source  surfaceToCell;\n"
            f'        file    "constant/triSurface/waveExtent{i}.stl";\n'
            f"        outsidePoints (({point}));\n"
            "        includeCut true;\n        includeInside true;\n"
            "        includeOutside false;\n        nearDistance -1;\n"
            "        curvature -1;\n    }"
        )
        if wave.enabled
        else ""
        for i in range(1, stages + 1)
    }


def write_wave_surfaces(case: Path, wave: WaveExtent, boxes: Sequence[Box]) -> None:
    """Write one closed trapezoidal prism per stage, with outward normals."""
    if not wave.enabled:
        return
    xy = (
        (wave.x_aft_m, 0.0),
        (wave.x_bow_m, 0.0),
        (wave.x_bow_m, -wave.half_width_bow_m),
        (wave.x_aft_m, -wave.half_width_aft_m),
    )
    # A quadrilateral prism: two triangles per end, two per side.
    faces = [(0, 1, 2), (0, 2, 3), (4, 6, 5), (4, 7, 6)]
    for i in range(4):
        j = (i + 1) % 4
        faces.extend(((i, i + 4, j + 4), (i, j + 4, j)))
    surface_dir = case / "constant" / "triSurface"
    surface_dir.mkdir(parents=True, exist_ok=True)
    for stage, (lo, hi) in enumerate(boxes, start=1):
        points = [(x, y, z) for z in (lo[2], hi[2]) for x, y in xy]
        lines = ["solid waveExtent"]
        for face in faces:
            a, b, c = (points[i] for i in face)
            u, v = [b[i] - a[i] for i in range(3)], [c[i] - a[i] for i in range(3)]
            normal = (
                u[1] * v[2] - u[2] * v[1],
                u[2] * v[0] - u[0] * v[2],
                u[0] * v[1] - u[1] * v[0],
            )
            length = math.sqrt(sum(n * n for n in normal))
            lines.append(
                "  facet normal " + " ".join(f"{n / length:.17g}" for n in normal)
            )
            lines.append("    outer loop")
            for p in (a, b, c):
                lines.append("      vertex " + " ".join(f"{n:.17g}" for n in p))
            lines.extend(("    endloop", "  endfacet"))
        lines.append("endsolid waveExtent")
        (surface_dir / f"waveExtent{stage}.stl").write_text("\n".join(lines) + "\n")
