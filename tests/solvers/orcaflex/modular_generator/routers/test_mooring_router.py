"""Tests for MooringRouter -- TDD unit tests.

All tests are pure dict assertions, no OrcFxAPI or solver required.
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema.mooring import (
    ChainGrade,
    MooringEndpoint,
    MooringLine,
    MooringSegment,
    MooringSegmentType,
    MooringSystem,
)
from digitalmodel.solvers.orcaflex.modular_generator.routers.mooring_router import (
    CHAIN_DATABASE,
    MooringRouter,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _chain_segment(
    diameter: float = 0.084,
    length: float = 400,
    grade: ChainGrade = ChainGrade.R3,
    segment_length: float = 10.0,
) -> MooringSegment:
    return MooringSegment(
        type=MooringSegmentType.CHAIN,
        diameter=diameter,
        length=length,
        grade=grade,
        segment_length=segment_length,
    )


def _wire_segment(
    diameter: float = 0.089,
    length: float = 300,
    breaking_load: float = 5000,
    segment_length: float = 10.0,
) -> MooringSegment:
    return MooringSegment(
        type=MooringSegmentType.WIRE_ROPE,
        diameter=diameter,
        length=length,
        breaking_load=breaking_load,
        segment_length=segment_length,
    )


def _anchor(x: float = -500, y: float = 0, z: float = -100) -> MooringEndpoint:
    return MooringEndpoint(type="anchor", position=[x, y, z])


def _fairlead(vessel: str = "FPSO", x: float = 10, y: float = 0, z: float = -10) -> MooringEndpoint:
    return MooringEndpoint(type="fairlead", position=[x, y, z], vessel=vessel)


def _simple_mooring_line(name: str = "ML1", pretension: float | None = None) -> MooringLine:
    return MooringLine(
        name=name,
        segments=[_chain_segment()],
        anchor=_anchor(),
        fairlead=_fairlead(),
        pretension=pretension,
    )


def _chain_wire_chain_line(name: str = "ML1") -> MooringLine:
    return MooringLine(
        name=name,
        segments=[
            _chain_segment(diameter=0.084, length=200, grade=ChainGrade.R3),
            _wire_segment(diameter=0.089, length=400, breaking_load=5000),
            _chain_segment(diameter=0.084, length=50, grade=ChainGrade.R3),
        ],
        anchor=_anchor(),
        fairlead=_fairlead(),
    )


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


class TestMooringRouter:
    @pytest.fixture()
    def router(self) -> MooringRouter:
        return MooringRouter()

    def test_single_chain_segment_produces_one_linetype(self, router):
        """Single chain mooring line -> 1 line type."""
        system = MooringSystem(lines=[_simple_mooring_line()])
        result = router.route(system)

        assert len(result["line_types"]) == 1
        lt = result["line_types"][0]
        assert lt["name"] == "chain_84mm_R3"
        assert lt["properties"]["Category"] == "General"

    def test_chain_wire_chain_produces_three_sections(self, router):
        """Chain-wire-chain line -> 3 sections in line definition."""
        system = MooringSystem(lines=[_chain_wire_chain_line()])
        result = router.route(system)

        line = result["lines"][0]
        sections = line["properties"]["LineType, Length, NumberOfSegments"]
        assert len(sections) == 3
        assert sections[0][0] == "chain_84mm_R3"
        assert sections[1][0] == "wire_89mm"
        assert sections[2][0] == "chain_84mm_R3"

    def test_linetype_deduplication_across_lines(self, router):
        """Same segment spec on two lines -> single shared line type."""
        system = MooringSystem(
            lines=[
                _simple_mooring_line(name="ML1"),
                MooringLine(
                    name="ML2",
                    segments=[_chain_segment()],
                    anchor=MooringEndpoint(type="anchor", position=[-500, 200, -100]),
                    fairlead=_fairlead(x=10, y=200),
                ),
            ]
        )
        result = router.route(system)

        assert len(result["line_types"]) == 1
        assert len(result["lines"]) == 2

    def test_chain_grade_r3_properties_from_database(self, router):
        """R3 chain -> correct MBL and mass from CHAIN_DATABASE."""
        seg = _chain_segment(diameter=0.084, grade=ChainGrade.R3)
        lt = router._segment_to_linetype(seg)

        d_mm = 84
        db = CHAIN_DATABASE["R3"]
        expected_mbl = db["mbl_coeff"] * d_mm ** 2
        expected_mass = db["mass_coeff"] * d_mm ** 2

        assert lt["properties"]["AllowableTension"] == pytest.approx(expected_mbl, rel=1e-6)
        assert lt["properties"]["MassPerUnitLength"] == pytest.approx(expected_mass, rel=1e-6)

    def test_wire_rope_properties_from_diameter_and_mbl(self, router):
        """Wire rope -> EA derived from MBL * ea_ratio."""
        seg = _wire_segment(diameter=0.089, breaking_load=5000)
        lt = router._segment_to_linetype(seg)

        assert lt["name"] == "wire_89mm"
        # EA = ea_ratio * MBL
        expected_ea = 0.45 * 5000
        assert lt["properties"]["EA"] == pytest.approx(expected_ea, rel=1e-6)
        assert lt["properties"]["AllowableTension"] == 5000

    def test_anchor_connection_maps_to_end_a_fixed(self, router):
        """Anchor endpoint -> EndAConnection = 'Fixed'."""
        system = MooringSystem(lines=[_simple_mooring_line()])
        result = router.route(system)

        line = result["lines"][0]
        assert line["properties"]["EndAConnection"] == "Fixed"

    def test_fairlead_connection_maps_to_vessel_reference(self, router):
        """Fairlead with vessel -> EndBConnection = vessel name."""
        system = MooringSystem(lines=[_simple_mooring_line()])
        result = router.route(system)

        line = result["lines"][0]
        assert line["properties"]["EndBConnection"] == "FPSO"

    def test_pretension_creates_winch(self, router):
        """Line with pretension -> winch on vessel, line End B -> winch."""
        system = MooringSystem(lines=[_simple_mooring_line(pretension=1500)])
        result = router.route(system)

        assert "winches" in result
        assert len(result["winches"]) == 1
        winch = result["winches"][0]
        assert winch["name"] == "Winch_ML1"
        assert winch["properties"]["Tension"] == 1500
        assert winch["properties"]["WinchControl"] == "Specified tension"
        # Winch sits on the vessel
        assert winch["properties"]["Connection"] == "FPSO"
        # Line End B routes through the winch
        line = result["lines"][0]
        assert line["properties"]["EndBConnection"] == "Winch_ML1"
        assert line["properties"]["EndBX"] == 0
        assert line["properties"]["EndBY"] == 0
        assert line["properties"]["EndBZ"] == 0

    def test_no_pretension_no_winch(self, router):
        """Line without pretension -> no winches key or empty."""
        system = MooringSystem(lines=[_simple_mooring_line(pretension=None)])
        result = router.route(system)

        assert "winches" not in result or len(result.get("winches", [])) == 0

    def test_output_dict_compatible_with_generic_model_schema(self, router):
        """Router output must have keys that GenericModel accepts."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.generic import GenericModel

        system = MooringSystem(lines=[_chain_wire_chain_line()])
        result = router.route(system)

        # Convert to GenericModel-compatible format
        gm_input = {}
        if "line_types" in result:
            gm_input["line_types"] = [
                {"name": lt["name"], "properties": lt["properties"]}
                for lt in result["line_types"]
            ]
        if "lines" in result:
            gm_input["lines"] = [
                {"name": ln["name"], "properties": ln["properties"]}
                for ln in result["lines"]
            ]
        if "winches" in result:
            gm_input["winches"] = [
                {"name": w["name"], "properties": w["properties"]}
                for w in result["winches"]
            ]

        # Should not raise validation errors
        gm = GenericModel(**gm_input)
        assert len(gm.line_types) == 2  # chain + wire
        assert len(gm.lines) == 1
