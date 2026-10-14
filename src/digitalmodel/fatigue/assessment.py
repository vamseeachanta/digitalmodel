# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validated load-block assessment using the native fatigue modules.

The complete set of load cases represents one block of ``block_years``.
Cases are additive, not alternative scenarios. Miner damage and endurance
are calculated by the existing damage and S-N modules; no table is copied.
"""

from __future__ import annotations

from typing import Annotated, Literal

import numpy as np
import pandas as pd
from pydantic import BaseModel, ConfigDict, Field, model_validator

from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.rainflow import rainflow_count
from digitalmodel.fatigue.sn_curves import get_sn_curve

Positive = Annotated[float, Field(gt=0, strict=True)]
Nonnegative = Annotated[float, Field(ge=0, strict=True)]
CurveName = Literal["B1", "B2", "C", "C1", "C2", "D", "E", "F", "F1",
                    "F3", "G", "W1", "W2", "W3"]


class _Input(BaseModel):
    model_config = ConfigDict(extra="forbid", allow_inf_nan=False,
                              str_strip_whitespace=True)


class HistogramCase(_Input):
    """Applied ranges in MPa and cycle counts for one load case."""

    name: str = Field(min_length=1)
    kind: Literal["histogram"]
    stress_ranges_mpa: list[Nonnegative] = Field(min_length=1)
    cycle_counts: list[Nonnegative] = Field(min_length=1)

    @model_validator(mode="after")
    def matching_lengths(self) -> HistogramCase:
        """Reject unpaired histogram entries."""
        if len(self.stress_ranges_mpa) != len(self.cycle_counts):
            raise ValueError("stress ranges and cycle counts must have equal lengths")
        return self


class HistoryCase(_Input):
    """Ordered stress samples in MPa; residuals remain half cycles."""

    name: str = Field(min_length=1)
    kind: Literal["history"]
    stress_history_mpa: list[Annotated[float, Field(strict=True)]] = Field(min_length=2)


class AssessmentInput(_Input):
    """Explicit duration, environment and curve selection contract.

    Curve choice is supplied by the engineer; geometry does not establish
    a weld class. Mean-stress, thickness and environmental corrections beyond
    the selected library environment are not applied automatically.
    """

    design_code: Literal["DNV-RP-C203"] = "DNV-RP-C203"
    sn_curve_name: CurveName = "D"
    environment: Literal["air", "seawater_cp", "free_corrosion"] = "air"
    block_years: Positive
    design_years: Positive
    design_fatigue_factor: Annotated[float, Field(ge=1, strict=True)] = 1.0
    load_cases: list[Annotated[HistogramCase | HistoryCase,
                               Field(discriminator="kind")]] = Field(min_length=1)

    @model_validator(mode="after")
    def unique_names(self) -> AssessmentInput:
        """Ensure every result can be traced to one named case."""
        names = [case.name for case in self.load_cases]
        if len(set(names)) != len(names):
            raise ValueError("load case names must be unique")
        return self


class CycleResult(BaseModel):
    """Native per-cycle endurance and damage, in MPa and cycles."""

    stress_range_mpa: float
    cycles: float
    allowable_cycles: float | None
    damage: float


class CaseResult(BaseModel):
    """Damage contribution from a named load case."""

    name: str
    block_damage: float
    cycles: list[CycleResult]


class AssessmentResult(BaseModel):
    """Life and utilisation against the supplied design-life criterion.

    Null life denotes unbounded life under zero damage, not a measured life.
    Utilisation includes the design fatigue factor; life is unfactored.
    """

    block_damage: float
    life_years: float | None
    utilisation: float
    passed: bool
    governing_load_case: str
    cases: list[CaseResult]


def _case_histogram(case: HistogramCase | HistoryCase) -> pd.DataFrame:
    """Count exact ranges without replacing them with bin centres."""
    if isinstance(case, HistoryCase):
        return rainflow_count(np.asarray(case.stress_history_mpa))
    return pd.DataFrame({"stress_range": case.stress_ranges_mpa,
                         "cycles": case.cycle_counts})


def _case_result(name: str, frame: pd.DataFrame) -> CaseResult:
    """Preserve the native per-cycle output with JSON-finite sentinels."""
    cycles = [CycleResult(
        stress_range_mpa=float(row.stress_range), cycles=float(row.cycles),
        allowable_cycles=(float(row.allowable_cycles)
                          if np.isfinite(row.allowable_cycles) else None),
        damage=float(row.damage),
    ) for row in frame.itertuples(index=False)]
    return CaseResult(name=name, block_damage=float(frame.attrs["total_damage"]),
                      cycles=cycles)


def assess_fatigue(inp: AssessmentInput) -> AssessmentResult:
    """Calculate damage, duration-scaled life and governing case.

    The input is revalidated to reject invalid model copies. Damage uses
    ``digitalmodel.fatigue.damage.miner_damage``. Life is block duration divided
    by block damage; utilisation scales damage by design/block duration and
    the supplied fatigue factor. Acceptance requires utilisation <= 1.
    """
    inp = AssessmentInput.model_validate(inp.model_dump())
    curve = get_sn_curve(inp.sn_curve_name, inp.environment)
    cases = [_case_result(case.name, miner_damage(_case_histogram(case), curve))
             for case in inp.load_cases]
    damage = sum(case.block_damage for case in cases)
    utilisation = damage * inp.design_years / inp.block_years * inp.design_fatigue_factor
    if not np.isfinite(damage) or not np.isfinite(utilisation):
        raise ValueError("computed fatigue damage or utilisation is non-finite")
    life = inp.block_years / damage if damage > 0 else None
    if life is not None and not np.isfinite(life):
        raise ValueError("computed fatigue life is non-finite")
    return AssessmentResult(
        block_damage=damage, life_years=life, utilisation=utilisation,
        passed=utilisation <= 1.0,
        governing_load_case=max(cases, key=lambda case: case.block_damage).name,
        cases=cases,
    )
