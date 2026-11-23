from __future__ import annotations

import math
from math import exp, log
from typing import Annotated, Any, Literal

from pydantic import BaseModel, ConfigDict, Field, StringConstraints, model_validator
from scipy import stats

NonEmptyStr = Annotated[str, StringConstraints(strip_whitespace=True, min_length=1)]


class Prior(BaseModel):
    """One scipy-backed distribution prior.

    Distribution parameter mapping:
    - ``uniform`` uses ``low`` and ``high`` as scipy ``loc`` and ``scale``.
    - ``normal`` uses ``mean`` and ``std``.
    - ``lognormal`` uses scipy ``lognorm`` with ``s=std_log`` and
      ``scale=exp(mean_log)``; ``median`` is accepted as a front door for
      ``mean_log=log(median)``.
    - ``triangular`` uses scipy ``triang`` with
      ``c=(mode-low)/(high-low)``, ``loc=low``, ``scale=high-low``.
    """

    model_config = ConfigDict(extra="forbid")

    name: NonEmptyStr
    dist: Literal["uniform", "normal", "lognormal", "triangular"]
    low: float | None = None
    high: float | None = None
    mean: float | None = None
    std: float | None = Field(default=None, gt=0)
    mean_log: float | None = None
    median: float | None = Field(default=None, gt=0)
    std_log: float | None = Field(default=None, gt=0)
    mode: float | None = None

    @model_validator(mode="after")
    def _validate_params(self) -> Prior:
        if not self.name:
            raise ValueError("prior name is required")
        numeric_params = {
            "low": self.low,
            "high": self.high,
            "mean": self.mean,
            "std": self.std,
            "mean_log": self.mean_log,
            "median": self.median,
            "std_log": self.std_log,
            "mode": self.mode,
        }
        if any(
            value is not None and not math.isfinite(value)
            for value in numeric_params.values()
        ):
            raise ValueError("prior numeric parameters must be finite")
        allowed_params = {
            "uniform": {"low", "high"},
            "normal": {"mean", "std"},
            "lognormal": {"mean_log", "median", "std_log"},
            "triangular": {"low", "high", "mode"},
        }[self.dist]
        irrelevant_params = sorted(
            name
            for name, value in numeric_params.items()
            if value is not None and name not in allowed_params
        )
        if irrelevant_params:
            raise ValueError(
                f"{self.dist} prior has irrelevant parameters: {', '.join(irrelevant_params)}"
            )
        if self.dist == "uniform":
            if self.low is None or self.high is None or self.high <= self.low:
                raise ValueError("uniform prior requires low < high")
        elif self.dist == "normal":
            if self.mean is None or self.std is None:
                raise ValueError("normal prior requires mean and std")
        elif self.dist == "lognormal":
            if self.std_log is None:
                raise ValueError("lognormal prior requires std_log")
            if (self.mean_log is None) == (self.median is None):
                raise ValueError(
                    "lognormal prior requires exactly one of mean_log or median"
                )
        elif self.dist == "triangular":
            if (
                self.low is None
                or self.mode is None
                or self.high is None
                or not (self.low <= self.mode <= self.high)
                or self.high <= self.low
            ):
                raise ValueError("triangular prior requires low <= mode <= high")
        return self

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> Prior:
        return cls.model_validate(data)

    def frozen(self):
        """Return the scipy frozen distribution for this prior."""
        if self.dist == "uniform":
            return stats.uniform(loc=self.low, scale=self.high - self.low)
        if self.dist == "normal":
            return stats.norm(loc=self.mean, scale=self.std)
        if self.dist == "lognormal":
            mean_log = self.mean_log if self.mean_log is not None else log(self.median)
            return stats.lognorm(s=self.std_log, scale=exp(mean_log))
        c = (self.mode - self.low) / (self.high - self.low)
        return stats.triang(c=c, loc=self.low, scale=self.high - self.low)

    def ppf(self, u):
        """Map unit-cube quantiles through this prior."""
        return self.frozen().ppf(u)
