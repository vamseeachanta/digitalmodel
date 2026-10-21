# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bottom-up one-foot shell sizing in SI units.

Equation locator: API 650, 5.6.3.2, as transcribed in public issue 2958
(2026-10-19; edition not established). The issue is a task brief, not a
qualified code source. Method coefficients, applicability limit (5.6.3.1),
diameter-specific nominal floor (5.6.1.1) and material allowables are caller
inputs: no unverified standard values or material tables are embedded here.

Without resolved method, material AND applicability Citations, numbers are
illustrative and code qualification is NOT_EVALUATED. Supplied Citations resolve fail-closed;
the caller owns the upstream source selection and edition-specific values.
CALCULATED means only these shell equations were evaluated, not tank approval.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
import math
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation
from digitalmodel.reporting.thickness import validate_number


@dataclass(frozen=True)
class MaterialAllowables:
    name: str
    design_mpa: float
    test_mpa: float
    source: str
    citation: Citation | None = None


@dataclass(frozen=True)
class OneFootBasis:
    """Explicit edition-dependent inputs; minimum is nominal, not net wall.

    ``coefficient`` has units MPa*mm/m²; ``head_offset_m`` is the design-point
    offset. ``minimum_nominal_mm`` is supplied for the actual tank diameter,
    not a universal floor. Missing floor/diameter evidence is never inferred.
    The two applicability citations must identify the same code and edition
    as the method citation. Resolution checks source metadata, not the numeric
    contents; the caller remains responsible for edition-specific value selection.
    """

    coefficient: float
    head_offset_m: float
    diameter_limit_m: float | None
    minimum_nominal_mm: float | None
    source: str
    citation: Citation | None = None
    diameter_limit_citation: Citation | None = None
    minimum_nominal_citation: Citation | None = None


@dataclass(frozen=True)
class TankDesign:
    """Shell sizing inputs; test_level_m describes the pressure plot fill only.

    Both one-foot thickness equations use design_level_m as their liquid level.
    """

    diameter_m: float
    course_heights_m: tuple[float, ...]
    design_level_m: float
    test_level_m: float
    specific_gravity: float
    corrosion_allowance_mm: float
    density_kg_m3: float
    plate_sizes_mm: tuple[float, ...]
    material: MaterialAllowables
    basis: OneFootBasis


@dataclass(frozen=True)
class CourseResult:
    course: int
    bottom_m: float
    top_m: float
    design_head_m: float
    test_head_m: float
    design_mm: float
    test_mm: float
    required_mm: float
    selected_mm: float
    utilisation: float
    governing_case: str


@dataclass(frozen=True)
class ShellResult:
    inputs: TankDesign
    courses: tuple[CourseResult, ...]
    shell_mass_kg: float
    status: str
    not_evaluated: tuple[str, ...]
    citations: tuple[Citation, ...]

    def citation_sidecar(self) -> dict:
        return {"citations": [asdict(item) for item in self.citations]}


def _validate(inputs: TankDesign) -> None:
    for name in ("diameter_m", "specific_gravity", "density_kg_m3"):
        validate_number(name, getattr(inputs, name))
    for name in ("design_level_m", "test_level_m", "corrosion_allowance_mm"):
        validate_number(name, getattr(inputs, name), zero=True)
    for name in ("course_heights_m", "plate_sizes_mm"):
        values = getattr(inputs, name)
        if not values:
            raise ValueError(f"{name} must not be empty")
        for value in values:
            validate_number(name, value)
    if any(a >= b for a, b in zip(inputs.plate_sizes_mm, inputs.plate_sizes_mm[1:])):
        raise ValueError("plate sizes must be strictly increasing")
    total_height = math.fsum(inputs.course_heights_m)
    if max(inputs.design_level_m, inputs.test_level_m) > total_height:
        raise ValueError("liquid level exceeds shell height")
    validate_number("design allowable", inputs.material.design_mpa)
    validate_number("test allowable", inputs.material.test_mpa)
    if not inputs.material.name.strip() or not inputs.material.source.strip():
        raise ValueError("material name and source are required")
    basis = inputs.basis
    validate_number("coefficient", basis.coefficient)
    validate_number("head_offset_m", basis.head_offset_m, zero=True)
    if not basis.source.strip():
        raise ValueError("method source is required")
    for name in ("diameter_limit_m", "minimum_nominal_mm"):
        if getattr(basis, name) is not None:
            validate_number(name, getattr(basis, name))
    if (
        basis.diameter_limit_m is not None
        and inputs.diameter_m > basis.diameter_limit_m
    ):
        raise ValueError("Diameter exceeds supplied one-foot limit; use API 650 5.6.4")


def _qualification(inputs: TankDesign, wiki_root: Path | None) -> tuple:
    citations, gaps = [], []
    for label, citation in (
        ("method edition and coefficients", inputs.basis.citation),
        ("material allowables", inputs.material.citation),
    ):
        if citation is None:
            gaps.append(f"NOT_EVALUATED: qualified {label} source is missing")
        else:
            validate_citation(citation, repo_root=wiki_root)
            if citation not in citations:
                citations.append(citation)
    basis = inputs.basis
    for field, label, citation in (
        ("diameter_limit_m", "API 650 5.6.3.1 diameter limit", basis.diameter_limit_citation),
        ("minimum_nominal_mm", "API 650 5.6.1.1 nominal floor", basis.minimum_nominal_citation),
    ):
        if citation is not None:
            validate_citation(citation, repo_root=wiki_root)
            if basis.citation is not None and any(
                getattr(citation, key) != getattr(basis.citation, key)
                for key in ("code_id", "publisher", "revision")
            ):
                raise ValueError("Method and applicability sources must identify the same edition")
            if citation not in citations:
                citations.append(citation)
        if getattr(basis, field) is None:
            gaps.append(f"NOT_EVALUATED: {label} is missing")
        elif citation is None:
            gaps.append(f"NOT_EVALUATED: qualified {label} source is missing")
    return tuple(citations), tuple(gaps)


def _course(
    inputs: TankDesign, index: int, bottom: float, height: float
) -> CourseResult:
    basis, material = inputs.basis, inputs.material
    design_head = max(inputs.design_level_m - bottom, 0)
    test_head = design_head  # API 650 5.6.3.2 uses the same design liquid level H.
    # API 650 5.6.3.2 equation form, public issue 2958 (edition unverified).
    # Both standards-derived coefficients are explicit, upstream-sourced inputs.
    design = (
        basis.coefficient
        * inputs.diameter_m
        * max(design_head - basis.head_offset_m, 0)
        * inputs.specific_gravity
        / material.design_mpa
        + inputs.corrosion_allowance_mm
    )
    test = (
        basis.coefficient
        * inputs.diameter_m
        * max(test_head - basis.head_offset_m, 0)
        / material.test_mpa
    )
    cases = {"design": design, "hydrostatic test": test}
    if basis.minimum_nominal_mm is not None:
        cases["minimum nominal"] = basis.minimum_nominal_mm
    governing = max(cases, key=cases.get)
    required = cases[governing]
    selected = next(
        (plate for plate in inputs.plate_sizes_mm if plate >= required), None
    )
    if selected is None:
        raise ValueError(f"No available plate meets course {index} requirement")
    return CourseResult(
        index,
        bottom,
        bottom + height,
        design_head,
        test_head,
        design,
        test,
        required,
        selected,
        required / selected,
        governing,
    )


def design_shell(inputs: TankDesign, *, wiki_root: Path | None = None) -> ShellResult:
    """Size courses from the bottom up; retain unrounded arithmetic.

    Hydrotest sizing uses water (unit specific gravity), the design liquid
    level H, and no design corrosion addition. The separate test fill level
    is retained for the applied-pressure plot only. Utilisation compares the
    governing nominal requirement with the selected nominal plate. Shell mass
    uses circumference at the specified diameter (thin-shell approximation),
    excludes roof/bottom/appurtenances, and uses caller-supplied density.
    """
    _validate(inputs)
    citations, gaps = _qualification(inputs, wiki_root)
    courses, bottom = [], 0.0
    for index, height in enumerate(inputs.course_heights_m, start=1):
        courses.append(_course(inputs, index, bottom, height))
        bottom = math.fsum((bottom, height))
    # SI conversion: 1000 mm per m (unit definition, not a code coefficient).
    area = math.fsum(
        (row.top_m - row.bottom_m) * row.selected_mm / 1000 for row in courses
    )
    mass = math.pi * inputs.diameter_m * area * inputs.density_kg_m3
    if not math.isfinite(mass):
        raise ValueError("shell mass overflow")
    return ShellResult(
        inputs,
        tuple(courses),
        mass,
        "NOT_EVALUATED" if gaps else "CALCULATED",
        gaps,
        citations,
    )
