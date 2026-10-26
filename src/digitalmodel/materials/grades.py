# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Validated caller-supplied material properties and unit adapters.
# ABOUTME: No bundled strength, elastic, density or toughness tables.
"""Material queries over the catalogue selected with use_catalog.

Specification names identify products; they never establish a numerical basis.
All engineering properties and aliases come from caller-owned sourced rows.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Optional
from collections.abc import Mapping
from types import MappingProxyType

from .catalog import Catalog, CatalogView, Source, current_catalog, positive, text

PSI_PER_MPA = 145.0377
MPA_PER_PSI = 1.0 / PSI_PER_MPA


@dataclass(frozen=True)
class MaterialGrade:
    """Immutable sourced material inputs, with strengths and modulus in MPa."""

    name: str
    standard: str
    smys_mpa: float
    smts_mpa: Optional[float]
    E_mpa: float
    nu: float
    rho_kg_m3: float
    source: Source
    row_id: str
    designation: Optional[str] = None
    grade_ksi: Optional[float] = None
    toughness_grade: Optional[str] = None
    smts_psl1_mpa: Optional[float] = None
    notes: str = ""
    material_class: str = "ferritic"
    aliases: tuple[str, ...] = ()
    charpy_test_temperature_degc: Optional[float] = None

    def __post_init__(self) -> None:
        import math
        from numbers import Real

        for field in ("name", "standard", "row_id"):
            text(getattr(self, field), field)
        for field in ("smys_mpa", "E_mpa", "rho_kg_m3"):
            positive(getattr(self, field), field)
        for field in ("smts_mpa", "smts_psl1_mpa"):
            value = getattr(self, field)
            if value is not None:
                positive(value, field)
                if value < self.smys_mpa:
                    raise ValueError(f"{field} must be at least smys_mpa")
        if self.smts_mpa is None and self.smts_psl1_mpa is None:
            raise ValueError("at least one tensile strength must be supplied")
        if (isinstance(self.nu, bool) or not isinstance(self.nu, Real)
                or not math.isfinite(self.nu) or not -1 < self.nu < 0.5):
            raise ValueError("nu must be finite and between -1 and 0.5")
        if not isinstance(self.source, Source):
            raise ValueError("source must be a validated Source")
        if self.material_class not in {"ferritic", "austenitic", "duplex"}:
            raise ValueError("unknown material_class")
        if self.grade_ksi is not None:
            positive(self.grade_ksi, "grade_ksi")
        if not isinstance(self.aliases, tuple):
            raise ValueError("aliases must be an immutable tuple")
        for alias in self.aliases:
            text(alias, "alias")
        value = self.charpy_test_temperature_degc
        if value is not None and (isinstance(value, bool) or not isinstance(value, Real)
                                  or not math.isfinite(value)):
            raise ValueError("charpy_test_temperature_degc must be finite")

    @property
    def source_psl1(self) -> str:
        return self.source.citation if self.smts_psl1_mpa is not None else ""

    @property
    def smys_psi(self) -> float:
        return self.smys_mpa * PSI_PER_MPA

    @property
    def smts_psi(self) -> float:
        if self.smts_mpa is None:
            raise ValueError(f"{self.name}: PSL2 strength was not supplied")
        return self.smts_mpa * PSI_PER_MPA


GRADES = CatalogView(lambda catalog: {g.name: g for g in catalog.grades})


def get(name: str) -> MaterialGrade:
    """Resolve only the caller's exact names and aliases, ignoring case."""
    key = name.strip().casefold()
    for grade in current_catalog().grades:
        if key in (grade.name.casefold(), *(a.casefold() for a in grade.aliases)):
            return grade
    raise KeyError(f"Unknown material grade: {name!r}")


def all_grades() -> list[MaterialGrade]:
    return list(current_catalog().grades)


def by_standard(standard: str) -> list[MaterialGrade]:
    return [g for g in all_grades() if standard.strip().casefold() in g.standard.casefold()]


def smys_mpa(name: str) -> float:
    return get(name).smys_mpa


def smts_mpa(name: str, *, psl: str = "PSL2") -> float:
    grade = get(name)
    if psl.upper() not in ("PSL1", "PSL2"):
        raise ValueError("PSL must be PSL1 or PSL2")
    value = grade.smts_psl1_mpa if psl.upper() == "PSL1" else grade.smts_mpa
    if value is None:
        raise ValueError(f"{grade.name}: {psl.upper()} strength was not supplied")
    return value


def smys_psi(name: str) -> float:
    return smys_mpa(name) * PSI_PER_MPA


def smts_psi(name: str, *, psl: str = "PSL2") -> float:
    return smts_mpa(name, psl=psl) * PSI_PER_MPA


def smys_kn_m2(name: str) -> float:
    return smys_mpa(name) * 1000.0


def smts_kn_m2(name: str, *, psl: str = "PSL2") -> float:
    return smts_mpa(name, psl=psl) * 1000.0


def smys_pa(name: str) -> float:
    return smys_mpa(name) * 1.0e6


def smts_pa(name: str, *, psl: str = "PSL2") -> float:
    return smts_mpa(name, psl=psl) * 1.0e6


# Legacy names retain their units and key shapes, but no copied snapshots.
# Yield always comes from the supplied strength, never a grade designation.
def _pipe_grades(catalog: Catalog) -> list[MaterialGrade]:
    return [g for g in catalog.grades if g.standard == "API 5L"]


def legacy_smys_psi_dict() -> Mapping[str, float]:
    return CatalogView(lambda c: {g.name: g.smys_psi for g in _pipe_grades(c)})


def legacy_smts_psi_dict() -> Mapping[str, float]:
    return CatalogView(lambda c: {g.name: g.smts_psi for g in _pipe_grades(c)
                                  if g.smts_mpa is not None})


def legacy_smys_table_mpa() -> Mapping[str, float]:
    return CatalogView(lambda c: {g.name: g.smys_mpa for g in _pipe_grades(c)})


def legacy_pipe_db_grades_mpa() -> Mapping[str, Mapping[str, float]]:
    return CatalogView(lambda c: {g.name: MappingProxyType({
        "yield_strength": g.smys_mpa, "tensile_strength": g.smts_mpa,
    }) for g in _pipe_grades(c) if g.smts_mpa is not None})


def legacy_pipe_db_grades_pa() -> Mapping[str, Mapping[str, float]]:
    return CatalogView(lambda c: {g.name: MappingProxyType({
        "smys": g.smys_mpa * 1e6, "smts": g.smts_mpa * 1e6,
    }) for g in _pipe_grades(c) if g.smts_mpa is not None})


def legacy_riser_pipe_grades_mpa() -> Mapping[str, Mapping[str, float]]:
    return CatalogView(lambda c: {g.name: MappingProxyType({
        "smys_mpa": g.smys_mpa, "smts_mpa": g.smts_mpa,
        "E_gpa": g.E_mpa / 1000, "density_kg_m3": g.rho_kg_m3,
    }) for g in _pipe_grades(c) if g.smts_mpa is not None})


def legacy_pipeline_material_library_mpa() -> Mapping[str, Mapping[str, float]]:
    return CatalogView(lambda c: {g.name: MappingProxyType({
        "smys": g.smys_mpa, "smts": g.smts_mpa,
    }) for g in _pipe_grades(c) if g.smts_mpa is not None})
