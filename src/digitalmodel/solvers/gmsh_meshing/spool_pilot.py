"""ABOUTME: Synthetic flanged spool parameters, closed-form comparator and judge.

CAD imports are deferred so the comparator and judge run without FreeCAD/gmsh.
Lengths are mm, volumes mm^3, density kg/m^3 and masses kg.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from math import isfinite, pi, sin
from pathlib import Path

VOLUME_REL_TOL = 0.005
DIMENSION_TOL_MM = 0.01
MIN_SCALED_JACOBIAN = 0.2
ELEMENT_COUNT_BAND = (1_000, 80_000)  # 3D linear tetrahedra only
ELEMENT_SIZE_MM = 5.0


@dataclass(frozen=True)
class SpoolParameters:
    """Generic weld-neck flange geometry; no dimensional-standard claim."""

    pipe_length: float = 300.0
    pipe_outer_diameter: float = 90.0
    bore_diameter: float = 78.0
    flange_outer_diameter: float = 160.0
    left_flange_thickness: float = 18.0
    right_flange_thickness: float = 18.0
    neck_length: float = 30.0
    neck_base_diameter: float = 120.0
    bolt_circle_diameter: float = 140.0
    bolt_hole_diameter: float = 12.0
    bolt_hole_count: int = 4
    density_kg_m3: float = 7850.0

    def __post_init__(self):
        for name, value in asdict(self).items():
            if not isfinite(value) or value <= 0:
                raise ValueError(f"{name} must be finite and positive")
        if type(self.bolt_hole_count) is not int or self.bolt_hole_count < 3:
            raise ValueError("bolt_hole_count must be an integer >= 3")
        if not (
            self.bore_diameter
            < self.pipe_outer_diameter
            < self.neck_base_diameter
            < self.flange_outer_diameter
        ):
            raise ValueError("require bore < pipe OD < neck base OD < flange OD")
        if not (
            self.neck_base_diameter + self.bolt_hole_diameter
            < self.bolt_circle_diameter
            < self.flange_outer_diameter - self.bolt_hole_diameter
        ):
            raise ValueError("bolt holes must clear the neck and flange rim")
        if self.bolt_hole_diameter >= (
            self.bolt_circle_diameter * sin(pi / self.bolt_hole_count)
        ):
            raise ValueError("bolt holes must not overlap")

    @property
    def total_length(self):
        return (
            self.pipe_length
            + 2 * self.neck_length
            + self.left_flange_thickness
            + self.right_flange_thickness
        )

    def dimensions(self):
        """All geometric inputs, expanded to check both ends and every hole."""
        dims = {
            "pipe_length": self.pipe_length,
            "pipe_outer_diameter": self.pipe_outer_diameter,
            "bore_diameter": self.bore_diameter,
            "total_length": self.total_length,
        }
        for end in ("left", "right"):
            dims.update(
                {
                    f"{end}.flange_thickness": getattr(self, f"{end}_flange_thickness"),
                    f"{end}.flange_outer_diameter": self.flange_outer_diameter,
                    f"{end}.neck_length": self.neck_length,
                    f"{end}.neck_base_diameter": self.neck_base_diameter,
                    f"{end}.hole_count": self.bolt_hole_count,
                }
            )
            for i in range(self.bolt_hole_count):
                dims[f"{end}.hole_{i}.diameter"] = self.bolt_hole_diameter
                dims[f"{end}.hole_{i}.bolt_circle"] = self.bolt_circle_diameter
        return dims


@dataclass(frozen=True)
class HandCalculation:
    pipe_mm3: float
    necks_mm3: float
    flanges_mm3: float
    bolt_holes_mm3: float
    volume_mm3: float
    mass_kg: float


def hand_calculation(p: SpoolParameters) -> HandCalculation:
    """Sum two annuli and two conical frusta, subtract bores and bolt holes."""
    r, a = p.pipe_outer_diameter / 2, p.bore_diameter / 2
    b, f = p.neck_base_diameter / 2, p.flange_outer_diameter / 2
    t = p.left_flange_thickness + p.right_flange_thickness
    pipe = pi * (r * r - a * a) * p.pipe_length
    necks = 2 * pi * p.neck_length * ((b * b + b * r + r * r) / 3 - a * a)
    flanges = pi * (f * f - a * a) * t
    holes = p.bolt_hole_count * pi * (p.bolt_hole_diameter / 2) ** 2 * t
    volume = pipe + necks + flanges - holes
    return HandCalculation(
        pipe, necks, flanges, holes, volume, volume * 1e-9 * p.density_kg_m3
    )


@dataclass(frozen=True)
class SolidMeasurements:
    volume_mm3: float
    dimensions: dict[str, float]
    valid: bool
    solid_count: int


@dataclass(frozen=True)
class MeshMeasurements:
    """One signed determinant and scaled Jacobian per 3D linear tetrahedron."""

    determinants: tuple[float, ...]
    scaled_jacobians: tuple[float, ...]

    @property
    def element_count(self):
        return len(self.determinants)


@dataclass(frozen=True)
class Verdict:
    failures: tuple[str, ...]

    @property
    def passed(self):
        return not self.failures

    def assert_passed(self):
        if self.failures:
            raise AssertionError("; ".join(self.failures))


def judge_solid(p: SpoolParameters, measured: SolidMeasurements) -> Verdict:
    """Compare observations to the original specification, never to CAD metadata."""
    failures = []
    if not measured.valid or measured.solid_count != 1:
        failures.append("solid: require one valid closed solid")
    reference = hand_calculation(p)
    cad_mass = measured.volume_mm3 * 1e-9 * p.density_kg_m3
    for name, actual, expected in (
        ("volume", measured.volume_mm3, reference.volume_mm3),
        ("mass", cad_mass, reference.mass_kg),
    ):
        if not isfinite(actual) or abs(actual - expected) / expected > VOLUME_REL_TOL:
            failures.append(f"{name}: {actual} vs {expected}; tolerance 0.5%")
    expected_dims = p.dimensions()
    if measured.dimensions.keys() != expected_dims.keys():
        failures.append("dimensions: missing or unexpected measurements")
    for name, expected in expected_dims.items():
        actual = measured.dimensions.get(name, float("nan"))
        tolerance = 0 if name.endswith("hole_count") else DIMENSION_TOL_MM
        if not isfinite(actual) or abs(actual - expected) > tolerance:
            failures.append(
                f"dimension {name}: {actual} vs {expected}; tolerance {tolerance}"
            )
    return Verdict(tuple(failures))


def judge_mesh(measured: MeshMeasurements) -> Verdict:
    failures = []
    n = measured.element_count
    if not ELEMENT_COUNT_BAND[0] <= n <= ELEMENT_COUNT_BAND[1]:
        failures.append(f"mesh element count: {n} outside {ELEMENT_COUNT_BAND}")
    if len(measured.scaled_jacobians) != n or n == 0:
        failures.append("mesh: require one quality value per volume element")
    if any(not isfinite(v) or v <= 0 for v in measured.determinants):
        failures.append("mesh: nonpositive or nonfinite signed Jacobian determinant")
    if any(
        not isfinite(v) or v < MIN_SCALED_JACOBIAN for v in measured.scaled_jacobians
    ):
        failures.append("mesh: minimum scaled Jacobian below 0.2 or nonfinite")
    return Verdict(tuple(failures))


def mesh_step(step_path: Path) -> MeshMeasurements:
    """Mesh the actual exported solid through the repository's gmsh adapter."""
    import gmsh

    from .mesh_generator import GMSHMeshGenerator

    if gmsh.isInitialized():
        raise RuntimeError("spool pilot requires a fresh gmsh context")
    generator = GMSHMeshGenerator()
    try:
        generator.generate_mesh_from_step(str(step_path), element_size=ELEMENT_SIZE_MM)
        gmsh.model.mesh.optimize()
        types, tag_groups, _ = gmsh.model.mesh.getElements(3)
        if list(types) != [4]:  # MSH type 4: first-order four-node tetrahedron
            raise ValueError(f"expected only linear tetrahedra, got {list(types)}")
        tags = tag_groups[0]
        measured = MeshMeasurements(
            tuple(
                float(v) for v in gmsh.model.mesh.getElementQualities(tags, "minDetJac")
            ),
            tuple(float(v) for v in gmsh.model.mesh.getElementQualities(tags, "minSJ")),
        )
        if measured.element_count != len(tags) or len(measured.scaled_jacobians) != len(
            tags
        ):
            raise ValueError("gmsh did not return quality for every volume element")
        return measured
    finally:
        generator.finalize()


def run_pilot(p: SpoolParameters, output_dir: Path) -> tuple[Verdict, Verdict]:
    """Build, measure, export and mesh; a failing judgment remains a failure."""
    from digitalmodel.visualization.design_tools.flanged_spool import (
        build_spool,
        measure_spool,
    )

    output_dir.mkdir(parents=True, exist_ok=True)
    shape = build_spool(p)
    solid_verdict = judge_solid(p, measure_spool(shape))
    step = output_dir / "spool.step"
    shape.exportStep(str(step))
    return solid_verdict, judge_mesh(mesh_step(step))
