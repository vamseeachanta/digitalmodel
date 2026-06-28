"""Inverse resolver for OrcaFlex ``ProjectInputSpec`` (outcome -> complete spec).

The OrcaFlex counterpart of the diffraction ``resolver.resolve`` (digitalmodel
#622 / #1096). Given an analysis *outcome* and sparse inputs, it fills a complete,
``ModularModelGenerator``-ready ``ProjectInputSpec`` and records every value it
*assumed* (rather than received) in an :class:`AssumptionLedger` -- the
"no silent assumptions" contract.

This first increment covers the **mooring** model type (the best-supported,
license-free path). The ``Outcome`` enum and dispatch are structured so riser /
pipeline / installation outcomes can be added the same way later.

    Outcome.MOORING_STRENGTH + sparse inputs
        |  resolve()  (fills gaps from reference defaults, ledgers each)
        v
    ProjectInputSpec  +  AssumptionLedger
        |  ModularModelGenerator
        v
    OrcaFlex model YAML
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Any, Optional

from pydantic import BaseModel, Field

from digitalmodel.common.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


class Outcome(str, Enum):
    """Supported OrcaFlex analysis outcomes for inverse resolution."""

    MOORING_STRENGTH = "mooring_strength"
    MOORING_PRETENSION = "mooring_pretension"


# Per-outcome simulation profile (stages = list of stage durations in seconds).
# Strength wants a build-up + storm dynamic stage; pretension/equilibrium is a
# static-only check.
_OUTCOME_SIMULATION: dict[Outcome, dict[str, Any]] = {
    Outcome.MOORING_STRENGTH: {"stages": [8, 16]},
    Outcome.MOORING_PRETENSION: {"stages": [10]},
}

MOORING_OUTCOMES: frozenset[Outcome] = frozenset(
    {Outcome.MOORING_STRENGTH, Outcome.MOORING_PRETENSION}
)

OUTCOME_DESCRIPTIONS: dict[Outcome, str] = {
    Outcome.MOORING_STRENGTH: (
        "Spread-mooring strength check (static + storm dynamic stage) -- peak "
        "line tensions vs MBL."
    ),
    Outcome.MOORING_PRETENSION: (
        "Mooring pretension / static equilibrium check (static stage only)."
    ),
}

# Reference defaults used when a value is not supplied (each is ledgered).
DEFAULT_WATER_DEPTH = 100.0  # m
DEFAULT_NUM_LINES = 8
DEFAULT_CHAIN_DIAMETER = 0.095  # m (95 mm studless)
DEFAULT_CHAIN_GRADE = "R4"
DEFAULT_FAIRLEAD_RADIUS = 40.0  # m from origin
DEFAULT_FAIRLEAD_Z = 0.0  # m relative to free surface
DEFAULT_VESSEL_NAME = "vessel_1"
# Anchor radius as a multiple of water depth (typical spread-mooring scope).
ANCHOR_RADIUS_DEPTH_FACTOR = 8.0
# Line length as a multiple of the straight anchor-to-fairlead distance.
LINE_LENGTH_SLACK_FACTOR = 1.15


class MooringResolverInputs(BaseModel):
    """Sparse inputs accepted by the mooring inverse resolver."""

    name: Optional[str] = None
    water_depth: Optional[float] = Field(default=None, gt=0)
    num_lines: Optional[int] = Field(default=None, gt=0)
    chain_diameter: Optional[float] = Field(default=None, gt=0)
    chain_grade: Optional[str] = None
    fairlead_radius: Optional[float] = Field(default=None, gt=0)
    anchor_radius: Optional[float] = Field(default=None, gt=0)
    line_length: Optional[float] = Field(default=None, gt=0)
    vessel_name: Optional[str] = None
    pretension: Optional[float] = Field(default=None, gt=0)
    # Escape hatch: a fully/partly specified ProjectInputSpec dict to merge.
    # Anything provided here is treated as user-supplied (not ledgered).
    partial_spec: Optional[dict[str, Any]] = None


def resolve(
    outcome: Outcome | str,
    inputs: MooringResolverInputs | dict[str, Any] | None = None,
) -> tuple[ProjectInputSpec, AssumptionLedger]:
    """Resolve an outcome + sparse inputs into a complete ProjectInputSpec."""
    outcome = Outcome(outcome)
    if outcome not in MOORING_OUTCOMES:  # pragma: no cover - guard for future
        raise ValueError(f"Unsupported outcome: {outcome}")

    resolver_inputs = (
        inputs
        if isinstance(inputs, MooringResolverInputs)
        else MooringResolverInputs.model_validate(inputs or {})
    )
    ledger = AssumptionLedger()
    data: dict[str, Any] = _deep_copy(resolver_inputs.partial_spec or {})

    _resolve_metadata(data, resolver_inputs, outcome, ledger)
    _resolve_environment(data, resolver_inputs, ledger)
    _resolve_mooring(data, resolver_inputs, ledger)
    _resolve_simulation(data, outcome, ledger)

    spec = ProjectInputSpec.model_validate(data)
    return spec, ledger


# ---------------------------------------------------------------------------
# Section resolvers
# ---------------------------------------------------------------------------


def _resolve_metadata(
    data: dict[str, Any],
    inputs: MooringResolverInputs,
    outcome: Outcome,
    ledger: AssumptionLedger,
) -> None:
    meta = _nested(data, "metadata")
    if "name" not in meta:
        meta["name"] = inputs.name or "mooring_system"
        if inputs.name is None:
            ledger.record(
                "metadata.name",
                meta["name"],
                AssumptionSource.ASSUMED_DEFAULT,
                "Default model name",
                Confidence.LOW,
                reference="default",
                impact=1,
            )
    for field, value, basis in (
        ("description", f"{outcome.value} model", "Derived from outcome"),
        ("structure", "mooring", "Mooring model type"),
        ("operation", outcome.value, "Derived from outcome"),
    ):
        if field not in meta:
            meta[field] = value
            ledger.record(
                f"metadata.{field}",
                value,
                AssumptionSource.ASSUMED_DEFAULT,
                basis,
                Confidence.LOW,
                reference=f"outcome:{outcome.value}",
                impact=1,
            )


def _resolve_environment(
    data: dict[str, Any],
    inputs: MooringResolverInputs,
    ledger: AssumptionLedger,
) -> None:
    env = _nested(data, "environment")
    water = _nested(env, "water")
    if "depth" not in water:
        depth = inputs.water_depth or DEFAULT_WATER_DEPTH
        water["depth"] = depth
        if inputs.water_depth is None:
            ledger.record(
                "environment.water.depth",
                depth,
                AssumptionSource.ASSUMED_DEFAULT,
                "Default screening water depth",
                Confidence.LOW,
                reference="default",
                impact=4,
            )
    # Seabed is required; an unspecified stiffness defaults to rigid (0 -> rigid
    # in OrcaFlex), which is the conservative screening default.
    seabed = _nested(env, "seabed")
    if "stiffness" not in seabed:
        seabed["stiffness"] = {}
        ledger.record(
            "environment.seabed.stiffness",
            {},
            AssumptionSource.ASSUMED_DEFAULT,
            "Default seabed stiffness (rigid)",
            Confidence.LOW,
            reference="default",
            impact=2,
        )


def _resolve_mooring(
    data: dict[str, Any],
    inputs: MooringResolverInputs,
    ledger: AssumptionLedger,
) -> None:
    if "mooring" in data and data["mooring"]:
        return  # fully user-specified mooring -> leave as is (not ledgered)

    water_depth = _nested(data, "environment", "water")["depth"]

    num_lines = inputs.num_lines or DEFAULT_NUM_LINES
    if inputs.num_lines is None:
        ledger.record(
            "mooring.num_lines",
            num_lines,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default spread-mooring line count",
            Confidence.LOW,
            reference="default",
            impact=4,
        )

    diameter = inputs.chain_diameter or DEFAULT_CHAIN_DIAMETER
    if inputs.chain_diameter is None:
        ledger.record(
            "mooring.chain_diameter",
            diameter,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default studless chain diameter",
            Confidence.LOW,
            reference="default",
            impact=5,
        )

    grade = inputs.chain_grade or DEFAULT_CHAIN_GRADE
    if inputs.chain_grade is None:
        ledger.record(
            "mooring.chain_grade",
            grade,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default chain grade",
            Confidence.LOW,
            reference="default",
            impact=3,
        )

    fairlead_radius = inputs.fairlead_radius or DEFAULT_FAIRLEAD_RADIUS
    if inputs.fairlead_radius is None:
        ledger.record(
            "mooring.fairlead_radius",
            fairlead_radius,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default fairlead radius from origin",
            Confidence.LOW,
            reference="default",
            impact=3,
        )

    anchor_radius = inputs.anchor_radius or ANCHOR_RADIUS_DEPTH_FACTOR * water_depth
    if inputs.anchor_radius is None:
        ledger.record(
            "mooring.anchor_radius",
            anchor_radius,
            AssumptionSource.ESTIMATED_FROM_DATA,
            f"{ANCHOR_RADIUS_DEPTH_FACTOR}x water depth (typical scope)",
            Confidence.MEDIUM,
            reference="anchor_radius=factor*depth",
            impact=4,
        )

    straight = math.hypot(anchor_radius - fairlead_radius, water_depth)
    line_length = inputs.line_length or LINE_LENGTH_SLACK_FACTOR * straight
    if inputs.line_length is None:
        ledger.record(
            "mooring.line_length",
            round(line_length, 2),
            AssumptionSource.ESTIMATED_FROM_DATA,
            f"{LINE_LENGTH_SLACK_FACTOR}x straight anchor-fairlead distance",
            Confidence.MEDIUM,
            reference="line_length=slack*straight",
            impact=4,
        )

    vessel = inputs.vessel_name or DEFAULT_VESSEL_NAME
    if inputs.vessel_name is None:
        ledger.record(
            "mooring.vessel_name",
            vessel,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default vessel name for fairleads",
            Confidence.LOW,
            reference="default",
            impact=2,
        )

    lines = []
    for i in range(num_lines):
        azimuth = math.radians(i * 360.0 / num_lines)
        cos_a, sin_a = math.cos(azimuth), math.sin(azimuth)
        segment: dict[str, Any] = {
            "type": "chain",
            "diameter": diameter,
            "length": round(line_length, 3),
            "grade": grade,
        }
        line: dict[str, Any] = {
            "name": f"line_{i + 1}",
            "segments": [segment],
            "anchor": {
                "type": "anchor",
                "position": [
                    round(anchor_radius * cos_a, 3),
                    round(anchor_radius * sin_a, 3),
                    -water_depth,
                ],
            },
            "fairlead": {
                "type": "fairlead",
                "position": [
                    round(fairlead_radius * cos_a, 3),
                    round(fairlead_radius * sin_a, 3),
                    DEFAULT_FAIRLEAD_Z,
                ],
                "vessel": vessel,
            },
        }
        if inputs.pretension is not None:
            line["pretension"] = inputs.pretension
        lines.append(line)

    data["mooring"] = {"lines": lines}


def _resolve_simulation(
    data: dict[str, Any],
    outcome: Outcome,
    ledger: AssumptionLedger,
) -> None:
    sim = _nested(data, "simulation")
    if "stages" not in sim:
        stages = list(_OUTCOME_SIMULATION[outcome]["stages"])
        sim["stages"] = stages
        ledger.record(
            "simulation.stages",
            stages,
            AssumptionSource.ASSUMED_DEFAULT,
            f"Default simulation stages for {outcome.value}",
            Confidence.LOW,
            reference=f"outcome:{outcome.value}",
            impact=3,
        )


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _nested(data: dict[str, Any], *keys: str) -> dict[str, Any]:
    node = data
    for key in keys:
        child = node.get(key)
        if not isinstance(child, dict):
            child = {}
            node[key] = child
        node = child
    return node


def _deep_copy(data: dict[str, Any]) -> dict[str, Any]:
    import copy

    return copy.deepcopy(data)


__all__ = [
    "Outcome",
    "OUTCOME_DESCRIPTIONS",
    "MOORING_OUTCOMES",
    "MooringResolverInputs",
    "resolve",
]
