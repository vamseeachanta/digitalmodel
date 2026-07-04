"""Operation go/no-go criteria for motion_forecast (digitalmodel #1359).

Thresholds live in a reviewable package-data YAML (``criteria_defaults.yml``),
never hardcoded. Each criterion binds a governing quantity (:mod:`.derived`) to
a caution/limit pair and a structure-interface point-of-interest offset.

The rolling decision reuses ``go_no_go._check_criterion``, whose WARNING band is
``[warning_factor*limit, limit]`` — so ``warning_factor = caution/limit``.
"""

from __future__ import annotations

from dataclasses import dataclass
from importlib import resources
from typing import Dict, Optional, Tuple

import yaml

from .derived import available_governing

Offset = Tuple[float, float, float]


@dataclass(frozen=True)
class Criterion:
    """One operation's governing-quantity go/no-go criterion."""

    key: str
    label: str
    governing: str
    caution: float
    limit: float
    unit: str
    alpha: float
    basis: str
    poi_offset: Offset

    @property
    def warning_factor(self) -> float:
        """caution/limit — maps the two thresholds onto _check_criterion."""
        return self.caution / self.limit


def _validate(key: str, c: Criterion) -> None:
    if c.governing not in available_governing():
        raise ValueError(
            f"criterion {key!r}: unknown governing {c.governing!r}; "
            f"available: {', '.join(available_governing())}"
        )
    if not (0.0 < c.caution <= c.limit):
        raise ValueError(
            f"criterion {key!r}: require 0 < caution ({c.caution}) <= limit ({c.limit})"
        )
    if len(c.poi_offset) != 3:
        raise ValueError(f"criterion {key!r}: poi_offset must be [rx, ry, rz]")


def _parse(raw: Dict) -> Dict[str, Criterion]:
    crits = raw.get("criteria")
    if not isinstance(crits, dict) or not crits:
        raise ValueError("criteria YAML must have a non-empty 'criteria' mapping")
    out: Dict[str, Criterion] = {}
    for key, d in crits.items():
        try:
            c = Criterion(
                key=key,
                label=str(d["label"]),
                governing=str(d["governing"]),
                caution=float(d["caution"]),
                limit=float(d["limit"]),
                unit=str(d["unit"]),
                alpha=float(d.get("alpha", 1.0)),
                basis=str(d.get("basis", "")),
                poi_offset=tuple(float(x) for x in d["poi_offset"]),
            )
        except (KeyError, TypeError, ValueError) as e:
            raise ValueError(f"criterion {key!r}: malformed ({e})") from e
        _validate(key, c)
        out[key] = c
    return out


def load_criteria(path: Optional[str] = None) -> Dict[str, Criterion]:
    """Load criteria from ``path`` (YAML) or the bundled defaults."""
    if path is None:
        text = (
            resources.files("digitalmodel.motion_forecast")
            .joinpath("criteria_defaults.yml")
            .read_text(encoding="utf-8")
        )
    else:
        with open(path, "r", encoding="utf-8") as fh:
            text = fh.read()
    return _parse(yaml.safe_load(text))
