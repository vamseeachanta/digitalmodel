# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Query caller-supplied ROV tool capabilities; no catalogue is shipped.

API 17H is the package's interface reference, not a source of embedded ratings.
Its edition and clause titles remain unverified here. Torque/turns, flow,
pressure and line-pull capacities must come from the caller's qualified source;
no rating table, standard factor or proprietary catalogue is reproduced.
Cutting and cleaning tools support kind-only selection or explicitly supplied
capacity requirements. Selection does not certify interface compatibility:
pass each returned tool's interface to ``check_interface`` separately.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from math import isfinite
from typing import Iterable

from digitalmodel.subsea.rov.interface_checks import ToolInterface, _validate_interface


class ToolKind(str, Enum):
    """Tool functions used to filter caller-supplied capabilities."""

    TORQUE = "torque"
    HOT_STAB = "hot_stab"
    PULL = "pull"
    CUTTING = "cutting"
    CLEANING = "cleaning"


@dataclass(frozen=True)
class TaskDuty:
    """Required capacities in SI units; None means no requirement for that field."""

    kind: ToolKind
    torque_nm: float | None = None
    turns: float | None = None
    flow_m3_s: float | None = None
    pressure_pa: float | None = None
    line_pull_n: float | None = None


@dataclass(frozen=True)
class ToolSpec:
    """Caller-supplied tool ratings; None means unknown, never unlimited."""

    name: str
    kind: ToolKind
    interface: ToolInterface
    torque_nm: float | None = None
    turns: float | None = None
    flow_m3_s: float | None = None
    pressure_pa: float | None = None
    line_pull_n: float | None = None


_CAPACITIES = ("torque_nm", "turns", "flow_m3_s", "pressure_pa", "line_pull_n")


def _validate_capacities(spec: ToolSpec | TaskDuty) -> None:
    if not isinstance(spec.kind, ToolKind):
        raise ValueError("kind must be a ToolKind")
    for field in _CAPACITIES:
        value = getattr(spec, field)
        if value is not None and (not isfinite(value) or value < 0):
            raise ValueError(f"{field} must be finite and nonnegative or None")


def _satisfies(tool: ToolSpec, duty: TaskDuty) -> bool:
    if tool.kind != duty.kind:
        return False
    for field in _CAPACITIES:
        required = getattr(duty, field)
        capacity = getattr(tool, field)
        if required is not None and (capacity is None or capacity < required):
            return False
    return True


def find_tools(tools: Iterable[ToolSpec], duty: TaskDuty) -> tuple[ToolSpec, ...]:
    """Return tools meeting every requested capacity, preserving input order.

    Args:
        tools: Caller-supplied catalogue with qualified ratings and interfaces.
        duty: Required function and capacities; unspecified fields do not filter.

    Returns:
        Matching tools including their interface demands; empty if none qualify.

    Raises:
        ValueError: If any supplied rating, identity or interface is invalid.
    """
    _validate_capacities(duty)
    matches = []
    for tool in tools:
        _validate_capacities(tool)
        _validate_interface(tool.interface)
        if not tool.name.strip():
            raise ValueError("tool name must be nonempty")
        if _satisfies(tool, duty):
            matches.append(tool)
    return tuple(matches)
