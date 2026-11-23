"""Inverse resolver for OrcaFlex ``ProjectInputSpec`` (outcome -> complete spec).

The OrcaFlex counterpart of the diffraction ``resolver.resolve`` (digitalmodel
#622 / #1096). Given an analysis *outcome* and sparse inputs, it fills a complete,
``ModularModelGenerator``-ready ``ProjectInputSpec`` and records every value it
*assumed* (rather than received) in an :class:`AssumptionLedger` -- the
"no silent assumptions" contract.

It covers three license-free model families, each dispatched by outcome:

* **mooring** -- spread-mooring strength / pretension.
* **riser** -- static configuration / wave dynamic (#1095).
* **pipeline** -- on-bottom stability / installation lay (#1095).

    Outcome.RISER_STATIC + sparse inputs
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
from typing import Any, Optional, Union

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
    RISER_STATIC = "riser_static"
    RISER_DYNAMIC = "riser_dynamic"
    PIPELINE_ONBOTTOM = "pipeline_onbottom"
    PIPELINE_LAY = "pipeline_lay"


# Per-outcome simulation profile (stages = list of stage durations in seconds).
# Static / equilibrium checks are static-only ([single stage]); dynamic checks
# add a build-up stage ahead of the main wave/lay dynamic stage.
_OUTCOME_SIMULATION: dict[Outcome, dict[str, Any]] = {
    Outcome.MOORING_STRENGTH: {"stages": [8, 16]},
    Outcome.MOORING_PRETENSION: {"stages": [10]},
    Outcome.RISER_STATIC: {"stages": [10]},
    Outcome.RISER_DYNAMIC: {"stages": [8, 16]},
    Outcome.PIPELINE_ONBOTTOM: {"stages": [10]},
    Outcome.PIPELINE_LAY: {"stages": [8, 16]},
}

MOORING_OUTCOMES: frozenset[Outcome] = frozenset(
    {Outcome.MOORING_STRENGTH, Outcome.MOORING_PRETENSION}
)
RISER_OUTCOMES: frozenset[Outcome] = frozenset(
    {Outcome.RISER_STATIC, Outcome.RISER_DYNAMIC}
)
PIPELINE_OUTCOMES: frozenset[Outcome] = frozenset(
    {Outcome.PIPELINE_ONBOTTOM, Outcome.PIPELINE_LAY}
)

OUTCOME_DESCRIPTIONS: dict[Outcome, str] = {
    Outcome.MOORING_STRENGTH: (
        "Spread-mooring strength check (static + storm dynamic stage) -- peak "
        "line tensions vs MBL."
    ),
    Outcome.MOORING_PRETENSION: (
        "Mooring pretension / static equilibrium check (static stage only)."
    ),
    Outcome.RISER_STATIC: (
        "Riser static configuration check (static stage only) -- effective "
        "tension and curvature along the catenary."
    ),
    Outcome.RISER_DYNAMIC: (
        "Riser wave dynamic check (static + wave dynamic stage) -- dynamic "
        "tension and bending at the hang-off and touchdown."
    ),
    Outcome.PIPELINE_ONBOTTOM: (
        "Pipeline on-bottom stability check (static stage only) -- seabed "
        "contact and lateral stability."
    ),
    Outcome.PIPELINE_LAY: (
        "Pipeline installation lay check (static + lay dynamic stage) -- "
        "sag-bend / over-bend response during laydown."
    ),
}

# Per-family (structure label, default model name).
_FAMILY_DEFAULTS: dict[str, tuple[str, str]] = {
    "mooring": ("mooring", "mooring_system"),
    "riser": ("riser", "riser_system"),
    "pipeline": ("pipeline", "pipeline_system"),
}

# ---------------------------------------------------------------------------
# Reference defaults used when a value is not supplied (each is ledgered).
# ---------------------------------------------------------------------------
DEFAULT_WATER_DEPTH = 100.0  # m

# Mooring
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

# Steel reference properties used to estimate riser line-type stiffness/mass.
STEEL_DENSITY = 7.85  # te/m3
STEEL_YOUNGS_MODULUS = 2.1e8  # kN/m2 (210 GPa)

# Riser
DEFAULT_RISER_OUTER_DIAMETER = 0.30  # m (~12 in SCR)
DEFAULT_RISER_WALL_THICKNESS = 0.025  # m
DEFAULT_RISER_SEGMENT_LENGTH = 10.0  # m (FE target, <= 50)
DEFAULT_RISER_CONFIGURATION = "catenary"
DEFAULT_RISER_HANGOFF_RADIUS = 25.0  # m horizontal offset of the hang-off
DEFAULT_RISER_HANGOFF_Z = -10.0  # m below the free surface
# Horizontal touchdown layback as a multiple of water depth (catenary scope).
RISER_LAYBACK_DEPTH_FACTOR = 1.0
# Arc length as a multiple of the straight hang-off-to-touchdown distance.
RISER_LENGTH_SLACK_FACTOR = 1.2

# Pipeline
DEFAULT_PIPELINE_OUTER_DIAMETER = 0.508  # m (20 in)
DEFAULT_PIPELINE_WALL_THICKNESS = 0.0254  # m (1 in)
DEFAULT_PIPELINE_MATERIAL = "X65"
DEFAULT_PIPELINE_LENGTH = 2000.0  # m
DEFAULT_PIPELINE_SEGMENT_LENGTH = 10.0  # m (mesh target, <= 100)
DEFAULT_PIPELINE_CORROSION_THICKNESS = 0.003  # m (FBE)
DEFAULT_PIPELINE_CORROSION_DENSITY = 1.3  # te/m3


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


class RiserResolverInputs(BaseModel):
    """Sparse inputs accepted by the riser inverse resolver."""

    name: Optional[str] = None
    water_depth: Optional[float] = Field(default=None, gt=0)
    outer_diameter: Optional[float] = Field(default=None, gt=0)
    wall_thickness: Optional[float] = Field(default=None, gt=0)
    length: Optional[float] = Field(default=None, gt=0)
    configuration: Optional[str] = None
    vessel_name: Optional[str] = None
    top_position: Optional[list[float]] = Field(
        default=None, min_length=3, max_length=3
    )
    bottom_position: Optional[list[float]] = Field(
        default=None, min_length=3, max_length=3
    )
    # Escape hatch: a fully/partly specified ProjectInputSpec dict to merge.
    partial_spec: Optional[dict[str, Any]] = None


class PipelineResolverInputs(BaseModel):
    """Sparse inputs accepted by the pipeline inverse resolver."""

    name: Optional[str] = None
    water_depth: Optional[float] = Field(default=None, gt=0)
    outer_diameter: Optional[float] = Field(default=None, gt=0)
    wall_thickness: Optional[float] = Field(default=None, gt=0)
    length: Optional[float] = Field(default=None, gt=0)
    material: Optional[str] = None
    segment_length: Optional[float] = Field(default=None, gt=0)
    # Escape hatch: a fully/partly specified ProjectInputSpec dict to merge.
    partial_spec: Optional[dict[str, Any]] = None


ResolverInputs = Union[
    MooringResolverInputs, RiserResolverInputs, PipelineResolverInputs
]

_FAMILY_INPUT_MODELS: dict[str, type[BaseModel]] = {
    "mooring": MooringResolverInputs,
    "riser": RiserResolverInputs,
    "pipeline": PipelineResolverInputs,
}


def _family_of(outcome: Outcome) -> str:
    """Return the model family ('mooring' / 'riser' / 'pipeline') for an outcome."""
    if outcome in MOORING_OUTCOMES:
        return "mooring"
    if outcome in RISER_OUTCOMES:
        return "riser"
    if outcome in PIPELINE_OUTCOMES:
        return "pipeline"
    raise ValueError(f"Unsupported outcome: {outcome}")  # pragma: no cover


def resolve(
    outcome: Outcome | str,
    inputs: ResolverInputs | dict[str, Any] | None = None,
) -> tuple[ProjectInputSpec, AssumptionLedger]:
    """Resolve an outcome + sparse inputs into a complete ProjectInputSpec."""
    outcome = Outcome(outcome)
    family = _family_of(outcome)
    inputs_cls = _FAMILY_INPUT_MODELS[family]

    if isinstance(inputs, inputs_cls):
        resolver_inputs = inputs
    elif isinstance(
        inputs,
        (MooringResolverInputs, RiserResolverInputs, PipelineResolverInputs),
    ):
        raise ValueError(
            f"Inputs of type {type(inputs).__name__} do not match outcome "
            f"family '{family}' (expected {inputs_cls.__name__})"
        )
    else:
        resolver_inputs = inputs_cls.model_validate(inputs or {})

    ledger = AssumptionLedger()
    data: dict[str, Any] = _deep_copy(resolver_inputs.partial_spec or {})

    _resolve_metadata(data, resolver_inputs, outcome, family, ledger)
    _resolve_environment(data, resolver_inputs, ledger)
    if family == "mooring":
        _resolve_mooring(data, resolver_inputs, ledger)
    elif family == "riser":
        _resolve_riser(data, resolver_inputs, ledger)
    else:
        _resolve_pipeline(data, resolver_inputs, ledger)
    _resolve_simulation(data, outcome, ledger)

    spec = ProjectInputSpec.model_validate(data)
    return spec, ledger


# ---------------------------------------------------------------------------
# Section resolvers
# ---------------------------------------------------------------------------


def _resolve_metadata(
    data: dict[str, Any],
    inputs: ResolverInputs,
    outcome: Outcome,
    family: str,
    ledger: AssumptionLedger,
) -> None:
    structure, default_name = _FAMILY_DEFAULTS[family]
    meta = _nested(data, "metadata")
    if "name" not in meta:
        meta["name"] = inputs.name or default_name
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
        ("structure", structure, f"{family.capitalize()} model type"),
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
    inputs: ResolverInputs,
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


def _resolve_riser(
    data: dict[str, Any],
    inputs: RiserResolverInputs,
    ledger: AssumptionLedger,
) -> None:
    if "riser" in data and data["riser"]:
        return  # fully user-specified riser -> leave as is (not ledgered)

    water_depth = _nested(data, "environment", "water")["depth"]

    outer_diameter = inputs.outer_diameter or DEFAULT_RISER_OUTER_DIAMETER
    if inputs.outer_diameter is None:
        ledger.record(
            "riser.outer_diameter",
            outer_diameter,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default steel-catenary-riser outer diameter",
            Confidence.LOW,
            reference="default",
            impact=5,
        )

    wall_thickness = inputs.wall_thickness or DEFAULT_RISER_WALL_THICKNESS
    if inputs.wall_thickness is None:
        ledger.record(
            "riser.wall_thickness",
            wall_thickness,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default riser wall thickness",
            Confidence.LOW,
            reference="default",
            impact=4,
        )

    inner_diameter = round(outer_diameter - 2.0 * wall_thickness, 6)
    if inner_diameter <= 0:
        raise ValueError(
            f"wall_thickness ({wall_thickness}) too large for outer_diameter "
            f"({outer_diameter}); inner diameter would be {inner_diameter}"
        )

    # Section properties estimated from a steel pipe of the resolved geometry.
    area = math.pi / 4.0 * (outer_diameter**2 - inner_diameter**2)
    second_moment = math.pi / 64.0 * (outer_diameter**4 - inner_diameter**4)
    mass_per_length = round(STEEL_DENSITY * area, 6)
    bending_stiffness = round(STEEL_YOUNGS_MODULUS * second_moment, 3)
    axial_stiffness = round(STEEL_YOUNGS_MODULUS * area, 3)
    for field, value, basis in (
        ("riser.mass_per_length", mass_per_length, "Steel pipe mass from geometry"),
        ("riser.bending_stiffness", bending_stiffness, "E*I from steel geometry"),
        ("riser.axial_stiffness", axial_stiffness, "E*A from steel geometry"),
    ):
        ledger.record(
            field,
            value,
            AssumptionSource.ESTIMATED_FROM_DATA,
            basis,
            Confidence.MEDIUM,
            reference="steel pipe: rho=7.85 te/m3, E=210 GPa",
            impact=4,
        )

    configuration = inputs.configuration or DEFAULT_RISER_CONFIGURATION
    if inputs.configuration is None:
        ledger.record(
            "riser.configuration",
            configuration,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default riser configuration",
            Confidence.LOW,
            reference="default",
            impact=3,
        )

    vessel = inputs.vessel_name or DEFAULT_VESSEL_NAME
    if inputs.vessel_name is None:
        ledger.record(
            "riser.vessel_name",
            vessel,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default vessel name for hang-off",
            Confidence.LOW,
            reference="default",
            impact=2,
        )

    if inputs.top_position is not None:
        top_position = [float(v) for v in inputs.top_position]
    else:
        top_position = [DEFAULT_RISER_HANGOFF_RADIUS, 0.0, DEFAULT_RISER_HANGOFF_Z]
        ledger.record(
            "riser.top_position",
            top_position,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default hang-off position below the vessel",
            Confidence.LOW,
            reference="default",
            impact=3,
        )

    if inputs.bottom_position is not None:
        bottom_position = [float(v) for v in inputs.bottom_position]
    else:
        layback = top_position[0] + RISER_LAYBACK_DEPTH_FACTOR * water_depth
        bottom_position = [round(layback, 3), 0.0, -water_depth]
        ledger.record(
            "riser.bottom_position",
            bottom_position,
            AssumptionSource.ESTIMATED_FROM_DATA,
            f"Touchdown {RISER_LAYBACK_DEPTH_FACTOR}x water depth downrange",
            Confidence.MEDIUM,
            reference="bottom_x=top_x+factor*depth",
            impact=4,
        )

    straight = math.hypot(
        bottom_position[0] - top_position[0],
        bottom_position[2] - top_position[2],
    )
    length = inputs.length or RISER_LENGTH_SLACK_FACTOR * straight
    if inputs.length is None:
        length = round(length, 3)
        ledger.record(
            "riser.length",
            length,
            AssumptionSource.ESTIMATED_FROM_DATA,
            f"{RISER_LENGTH_SLACK_FACTOR}x straight hang-off-touchdown distance",
            Confidence.MEDIUM,
            reference="length=slack*straight",
            impact=4,
        )

    segment_length = DEFAULT_RISER_SEGMENT_LENGTH
    ledger.record(
        "riser.segment_length",
        segment_length,
        AssumptionSource.ASSUMED_DEFAULT,
        "Default FE segment length",
        Confidence.LOW,
        reference="default",
        impact=2,
    )

    line_type_name = "riser_pipe"
    data["riser"] = {
        "vessel": {"name": vessel},
        "line_types": [
            {
                "name": line_type_name,
                "outer_diameter": outer_diameter,
                "inner_diameter": inner_diameter,
                "mass_per_length": mass_per_length,
                "bending_stiffness": bending_stiffness,
                "axial_stiffness": axial_stiffness,
            }
        ],
        "lines": [
            {
                "name": "riser_1",
                "configuration": configuration,
                "end_a": {
                    "type": "vessel",
                    "name": vessel,
                    "position": top_position,
                },
                "end_b": {
                    "type": "anchor",
                    "position": bottom_position,
                },
                "sections": [
                    {
                        "line_type": line_type_name,
                        "length": length,
                        "segment_length": segment_length,
                    }
                ],
            }
        ],
    }


def _resolve_pipeline(
    data: dict[str, Any],
    inputs: PipelineResolverInputs,
    ledger: AssumptionLedger,
) -> None:
    if "pipeline" in data and data["pipeline"]:
        return  # fully user-specified pipeline -> leave as is (not ledgered)

    pipeline_name = inputs.name or "pipeline_1"
    if inputs.name is None:
        ledger.record(
            "pipeline.name",
            pipeline_name,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default pipeline name",
            Confidence.LOW,
            reference="default",
            impact=1,
        )

    material = inputs.material or DEFAULT_PIPELINE_MATERIAL
    if inputs.material is None:
        ledger.record(
            "pipeline.material",
            material,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default line-pipe steel grade",
            Confidence.LOW,
            reference="default",
            impact=3,
        )

    outer_diameter = inputs.outer_diameter or DEFAULT_PIPELINE_OUTER_DIAMETER
    if inputs.outer_diameter is None:
        ledger.record(
            "pipeline.outer_diameter",
            outer_diameter,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default pipeline outer diameter",
            Confidence.LOW,
            reference="default",
            impact=5,
        )

    wall_thickness = inputs.wall_thickness or DEFAULT_PIPELINE_WALL_THICKNESS
    if inputs.wall_thickness is None:
        ledger.record(
            "pipeline.wall_thickness",
            wall_thickness,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default pipeline wall thickness",
            Confidence.LOW,
            reference="default",
            impact=5,
        )
    if wall_thickness >= outer_diameter / 2.0:
        raise ValueError(
            f"wall_thickness ({wall_thickness}) must be < half outer_diameter "
            f"({outer_diameter / 2.0})"
        )

    length = inputs.length or DEFAULT_PIPELINE_LENGTH
    if inputs.length is None:
        ledger.record(
            "pipeline.length",
            length,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default screening pipeline length",
            Confidence.LOW,
            reference="default",
            impact=3,
        )

    segment_length = inputs.segment_length or DEFAULT_PIPELINE_SEGMENT_LENGTH
    if inputs.segment_length is None:
        ledger.record(
            "pipeline.segment_length",
            segment_length,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default mesh segment length",
            Confidence.LOW,
            reference="default",
            impact=2,
        )

    corrosion = {
        "thickness": DEFAULT_PIPELINE_CORROSION_THICKNESS,
        "density": DEFAULT_PIPELINE_CORROSION_DENSITY,
    }
    ledger.record(
        "pipeline.coatings.corrosion",
        corrosion,
        AssumptionSource.ASSUMED_DEFAULT,
        "Default FBE corrosion coating",
        Confidence.LOW,
        reference="default",
        impact=2,
    )

    data["pipeline"] = {
        "name": pipeline_name,
        "material": material,
        "dimensions": {
            "outer_diameter": outer_diameter,
            "wall_thickness": wall_thickness,
        },
        "coatings": {"corrosion": corrosion},
        "segments": [
            {
                "type": "pipe_main",
                "length": length,
                "segment_length": segment_length,
            }
        ],
    }


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
    "RISER_OUTCOMES",
    "PIPELINE_OUTCOMES",
    "MooringResolverInputs",
    "RiserResolverInputs",
    "PipelineResolverInputs",
    "resolve",
]
