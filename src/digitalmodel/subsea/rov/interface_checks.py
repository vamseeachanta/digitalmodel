# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Screen caller-supplied ROV interface geometry without standard rating tables.

API 17H is the interface reference identified by the package. Its edition and
clause titles have not been verified here; this screen is not API compliance
certification. All dimensions, class identifiers and reach limits are required
caller inputs. No standard-derived dimensions or vendor catalogue is embedded.

Envelopes are aligned bounding boxes in the mating frame, with a fixed tool
orientation. Locations are relative to the manipulator base in metres. Reach is
an ideal spherical bound; collisions, joint limits, seals and mating tolerances
require separate qualification. Torque class identifiers 1 through 7 are labels,
not capacity ratings. Non-torque class identifiers are caller-defined.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from math import dist, isfinite


class InterfaceKind(str, Enum):
    """Interface families; these labels impose no geometry or capacity."""

    TORQUE = "torque"
    HOT_STAB = "hot_stab"
    OVERRIDE = "override"
    PADDLE = "paddle"
    DOCKING = "docking"
    PARKING = "parking"


@dataclass(frozen=True)
class ToolInterface:
    """Required mating family, box dimensions [m] and approach space [m]."""

    kind: InterfaceKind
    class_id: str
    envelope_m: tuple[float, float, float]
    required_approach_m: float


@dataclass(frozen=True)
class InterfaceSpec:
    """Available mating box, approach space and base-relative location [m]."""

    kind: InterfaceKind
    class_id: str
    envelope_m: tuple[float, float, float]
    approach_clearance_m: float
    location_m: tuple[float, float, float]


@dataclass(frozen=True)
class InterfaceResult:
    """Compatibility outcome with all failed screening categories."""

    compatible: bool
    reasons: tuple[str, ...]


def _finite_nonnegative(values: tuple[float, ...]) -> None:
    if any(not isfinite(value) or value < 0 for value in values):
        raise ValueError("dimensions and limits must be finite and nonnegative")


def _validate_interface(spec: ToolInterface | InterfaceSpec) -> None:
    if not isinstance(spec.kind, InterfaceKind):
        raise ValueError("kind must be an InterfaceKind")
    if not isinstance(spec.class_id, str) or not spec.class_id.strip():
        raise ValueError("class_id must be nonempty")
    if spec.kind == InterfaceKind.TORQUE and spec.class_id not in tuple("1234567"):
        raise ValueError("torque class_id must be a label from 1 through 7")
    if len(spec.envelope_m) != 3:
        raise ValueError("envelope_m must have three dimensions")
    _finite_nonnegative(spec.envelope_m)
    if isinstance(spec, ToolInterface):
        _finite_nonnegative((spec.required_approach_m,))
    if any(value == 0 for value in spec.envelope_m):
        raise ValueError("envelope dimensions must be positive")


def check_interface(
    tool: ToolInterface,
    target: InterfaceSpec,
    manipulator_reach_m: float,
) -> InterfaceResult:
    """Compare fixed-orientation geometry and ideal reach in metres.

    Args:
        tool: Required interface and tool bounding box.
        target: Available interface box and location relative to manipulator base.
        manipulator_reach_m: Caller-qualified spherical reach limit.

    Returns:
        Structured result listing kind, class, envelope, approach or reach failures.

    Raises:
        ValueError: If dimensions, limits, locations or class labels are invalid.
    """
    _validate_interface(tool)
    _validate_interface(target)
    _finite_nonnegative((tool.required_approach_m, target.approach_clearance_m,
                         manipulator_reach_m))
    if len(target.location_m) != 3 or not all(map(isfinite, target.location_m)):
        raise ValueError("location_m must have three finite coordinates")
    reasons = []
    if tool.kind != target.kind:
        reasons.append("kind")
    if tool.class_id != target.class_id:
        reasons.append("class")
    if any(needed > available for needed, available in
           zip(tool.envelope_m, target.envelope_m)):
        reasons.append("envelope")
    if tool.required_approach_m > target.approach_clearance_m:
        reasons.append("approach")
    if dist(target.location_m, (0, 0, 0)) > manipulator_reach_m:
        reasons.append("reach")
    return InterfaceResult(not reasons, tuple(reasons))
