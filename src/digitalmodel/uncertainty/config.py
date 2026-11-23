from __future__ import annotations

from pathlib import Path
from typing import Annotated, Literal

import yaml
from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    FiniteFloat,
    StringConstraints,
    model_validator,
)

from digitalmodel.uncertainty.distributions import Prior

NonEmptyStr = Annotated[str, StringConstraints(strip_whitespace=True, min_length=1)]


class DoeConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    method: Literal["lhs", "random"]
    n_samples: int = Field(gt=0)
    seed: int


class EvaluatorConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    builtin: NonEmptyStr | None = None
    dotted_path: NonEmptyStr | None = None

    @model_validator(mode="after")
    def _exactly_one_dispatch_path(self) -> EvaluatorConfig:
        if (self.builtin is None) == (self.dotted_path is None):
            raise ValueError("evaluator requires exactly one of builtin or dotted_path")
        return self


class ScreeningConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metric: NonEmptyStr
    operator: Literal["<=", "<", ">=", ">"]
    threshold: FiniteFloat


class OutputsConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    primary: NonEmptyStr
    screening: ScreeningConfig | None = None


class UncertaintyStudy(BaseModel):
    model_config = ConfigDict(extra="forbid")

    priors: list[Prior] = Field(min_length=1)
    doe: DoeConfig
    evaluator: EvaluatorConfig
    outputs: OutputsConfig


def load_study(path: str | Path) -> UncertaintyStudy:
    """Load a YAML uncertainty-study config into the typed front door."""
    with Path(path).open(encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    return UncertaintyStudy.model_validate(data)
