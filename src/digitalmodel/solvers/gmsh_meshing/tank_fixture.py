"""Public synthetic L-tank fixture contract for the Gmsh/OpenFOAM bridge."""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping

import yaml


EXPECTED_FRAME = {
    "x": "transverse_breadth",
    "y": "vertical_up",
    "z": "longitudinal_span_roll_axis",
}


def _section(data: Mapping[str, Any], name: str) -> Mapping[str, Any]:
    value = data.get(name)
    if not isinstance(value, Mapping):
        raise ValueError(f"{name} must be a mapping")
    return value


def _positive(section: Mapping[str, Any], name: str) -> float:
    value = section.get(name)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a positive finite number")
    result = float(value)
    if not math.isfinite(result) or result <= 0.0:
        raise ValueError(f"{name} must be a positive finite number")
    return result


@dataclass(frozen=True)
class TankDimensions:
    breadth: float
    height: float
    length: float
    longitudinal_leg_width: float
    transverse_leg_length: float


@dataclass(frozen=True)
class MemberDimensions:
    height: float
    thickness: float
    edge_clearance: float


@dataclass(frozen=True)
class LongitudinalOpening:
    vertical_radius: float
    span_radius: float


@dataclass(frozen=True)
class TransverseOpening:
    transverse_radius: float
    vertical_radius: float


@dataclass(frozen=True)
class TankFixtureSpec:
    """Validated SI and frame-locked inputs for the synthetic fixture."""

    schema_version: int
    length_unit: str
    tank: TankDimensions
    members: MemberDimensions
    longitudinal_opening: LongitudinalOpening
    transverse_opening: TransverseOpening
    element_size: float

    @classmethod
    def from_mapping(cls, data: Mapping[str, Any]) -> "TankFixtureSpec":
        if data.get("schema_version") != 1:
            raise ValueError("schema_version must equal 1")
        if data.get("length_unit") != "m":
            raise ValueError("length_unit must be 'm'")
        if data.get("frame") != EXPECTED_FRAME:
            raise ValueError("frame must use x-transverse/y-up/z-roll-axis")
        tank_data = _section(data, "tank")
        member_data = _section(data, "members")
        openings = _section(data, "openings")
        mesh_data = _section(data, "mesh")
        spec = cls(
            schema_version=1,
            length_unit="m",
            tank=_parse_tank(tank_data),
            members=_parse_members(member_data),
            longitudinal_opening=_parse_longitudinal_opening(openings),
            transverse_opening=_parse_transverse_opening(openings),
            element_size=_positive(mesh_data, "element_size"),
        )
        spec._validate_geometry()
        return spec

    @property
    def frame(self) -> dict[str, str]:
        return dict(EXPECTED_FRAME)

    @property
    def longitudinal_member_span(self) -> float:
        return (
            self.tank.length
            - self.tank.transverse_leg_length
            - 2.0 * self.members.edge_clearance
        )

    @property
    def transverse_member_span(self) -> float:
        return (
            self.tank.breadth
            - self.tank.longitudinal_leg_width
            - 2.0 * self.members.edge_clearance
        )

    @property
    def gross_volume(self) -> float:
        tank = self.tank
        plan_area = (
            tank.longitudinal_leg_width * tank.length
            + tank.breadth * tank.transverse_leg_length
            - tank.longitudinal_leg_width * tank.transverse_leg_length
        )
        return tank.height * plan_area

    @property
    def expected_fluid_volume(self) -> float:
        member = self.members
        longitudinal_area = (
            member.height * self.longitudinal_member_span
            - math.pi
            * self.longitudinal_opening.vertical_radius
            * self.longitudinal_opening.span_radius
        )
        transverse_area = (
            member.height * self.transverse_member_span
            - math.pi
            * self.transverse_opening.transverse_radius
            * self.transverse_opening.vertical_radius
        )
        return self.gross_volume - member.thickness * (
            longitudinal_area + transverse_area
        )

    def _validate_geometry(self) -> None:
        tank = self.tank
        member = self.members
        if tank.longitudinal_leg_width >= tank.breadth:
            raise ValueError("longitudinal_leg_width must be less than breadth")
        if tank.transverse_leg_length >= tank.length:
            raise ValueError("transverse_leg_length must be less than length")
        if member.height >= tank.height:
            raise ValueError("member height must be less than tank height")
        if self.longitudinal_member_span <= 0.0:
            raise ValueError("member edge_clearance leaves no longitudinal span")
        if self.transverse_member_span <= 0.0:
            raise ValueError("member edge_clearance leaves no transverse span")
        self._validate_openings()

    def _validate_openings(self) -> None:
        half_height = 0.5 * self.members.height
        longitudinal = self.longitudinal_opening
        transverse = self.transverse_opening
        if (
            longitudinal.vertical_radius >= half_height
            or longitudinal.span_radius >= 0.5 * self.longitudinal_member_span
        ):
            raise ValueError("longitudinal opening must fit inside its member")
        if (
            transverse.vertical_radius >= half_height
            or transverse.transverse_radius >= 0.5 * self.transverse_member_span
        ):
            raise ValueError("transverse opening must fit inside its member")

    def scaled(self, factor: float) -> "TankFixtureSpec":
        if not math.isfinite(factor) or factor <= 0.0:
            raise ValueError("scale factor must be positive and finite")
        tank = self.tank
        member = self.members
        longitudinal = self.longitudinal_opening
        transverse = self.transverse_opening
        return TankFixtureSpec(
            schema_version=1,
            length_unit="m",
            tank=TankDimensions(*(factor * value for value in tank.__dict__.values())),
            members=MemberDimensions(
                *(factor * value for value in member.__dict__.values())
            ),
            longitudinal_opening=LongitudinalOpening(
                factor * longitudinal.vertical_radius,
                factor * longitudinal.span_radius,
            ),
            transverse_opening=TransverseOpening(
                factor * transverse.transverse_radius,
                factor * transverse.vertical_radius,
            ),
            element_size=factor * self.element_size,
        )


def _parse_tank(data: Mapping[str, Any]) -> TankDimensions:
    return TankDimensions(
        breadth=_positive(data, "breadth"),
        height=_positive(data, "height"),
        length=_positive(data, "length"),
        longitudinal_leg_width=_positive(data, "longitudinal_leg_width"),
        transverse_leg_length=_positive(data, "transverse_leg_length"),
    )


def _parse_members(data: Mapping[str, Any]) -> MemberDimensions:
    return MemberDimensions(
        height=_positive(data, "height"),
        thickness=_positive(data, "thickness"),
        edge_clearance=_positive(data, "edge_clearance"),
    )


def _parse_longitudinal_opening(
    openings: Mapping[str, Any],
) -> LongitudinalOpening:
    data = _section(openings, "longitudinal")
    return LongitudinalOpening(
        vertical_radius=_positive(data, "vertical_radius"),
        span_radius=_positive(data, "span_radius"),
    )


def _parse_transverse_opening(
    openings: Mapping[str, Any],
) -> TransverseOpening:
    data = _section(openings, "transverse")
    return TransverseOpening(
        transverse_radius=_positive(data, "transverse_radius"),
        vertical_radius=_positive(data, "vertical_radius"),
    )


def load_tank_fixture_spec(path: Path | str) -> TankFixtureSpec:
    source = Path(path)
    if not source.is_file():
        raise FileNotFoundError(f"tank fixture input not found: {source}")
    loaded = yaml.safe_load(source.read_text(encoding="utf-8"))
    if not isinstance(loaded, Mapping):
        raise ValueError("tank fixture input must contain a mapping")
    return TankFixtureSpec.from_mapping(loaded)


from ._tank_fixture_gmsh import (  # noqa: E402
    FixtureBuildSummary,
    Msh2Contract,
    build_tank_fixture,
    inspect_msh2,
)

__all__ = [
    "EXPECTED_FRAME",
    "FixtureBuildSummary",
    "Msh2Contract",
    "TankFixtureSpec",
    "build_tank_fixture",
    "inspect_msh2",
    "load_tank_fixture_spec",
]
