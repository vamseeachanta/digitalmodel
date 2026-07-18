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


def _polyester_segment(
    diameter: float = 0.140,
    length: float = 500,
    breaking_load: float = 7200,
    segment_length: float = 10.0,
) -> MooringSegment:
    return MooringSegment(
        type=MooringSegmentType.POLYESTER,
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
        """R3 chain -> correct MBL and mass from CHAIN_DATABASE.

        Previously this test mirrored the buggy MBL = coeff * d^2 formula
        (no DNV-OS-E302 (44 - 0.08d) factor, coefficients shifted one
        grade); it now pins independently computed values.
        """
        seg = _chain_segment(diameter=0.084, grade=ChainGrade.R3)
        lt = router._segment_to_linetype(seg)

        d_mm = 84
        # DNV-OS-E302: MBL = 0.0223 * d^2 * (44 - 0.08d) = 5866 kN for 84mm R3
        expected_mbl = 0.0223 * d_mm ** 2 * (44 - 0.08 * d_mm)
        expected_mass = CHAIN_DATABASE["R3"]["mass_coeff"] * d_mm ** 2

        assert lt["properties"]["AllowableTension"] == pytest.approx(expected_mbl, rel=1e-6)
        assert lt["properties"]["AllowableTension"] == pytest.approx(5866, rel=1e-3)
        assert lt["properties"]["MassPerUnitLength"] == pytest.approx(expected_mass, rel=1e-6)

    def test_chain_r3_76mm_mbl_matches_dnv_os_e302(self, router):
        """76mm R3 chain MBL must be ~4884 kN per DNV-OS-E302 (not 144 kN).

        Regression for the shifted grade coefficients and missing
        (44 - 0.08d) factor: the buggy formula gave 0.0249 * 76^2 = 144 kN.
        """
        seg = _chain_segment(diameter=0.076, grade=ChainGrade.R3)
        lt = router._segment_to_linetype(seg)

        assert lt["properties"]["AllowableTension"] == pytest.approx(4884, rel=1e-3)

    def test_chain_grade_coefficients_match_dnv_os_e302(self):
        """Grade coefficients must match DNV-OS-E302 (not shifted by one)."""
        expected = {
            "R3": 0.0223,
            "R3S": 0.0249,
            "R4": 0.0274,
            "R4S": 0.0304,
            "R5": 0.0320,
        }
        actual = {g: CHAIN_DATABASE[g]["mbl_coeff"] for g in expected}
        assert actual == expected

    def test_chain_ea_matches_curated_library_component(self, router):
        """Chain EA = 0.854e8 * d^2 kN (d in m), not 10x stiffer.

        Reference: docs/domains/orcaflex/library/line_types/chain_76mm_r4.yml
        has EA = 490000 kN for 76mm chain.  The buggy formula gave 4.93e6 kN.
        """
        seg = _chain_segment(diameter=0.076, grade=ChainGrade.R4)
        lt = router._segment_to_linetype(seg)

        assert lt["properties"]["EA"] == pytest.approx(490000, rel=0.02)

    def test_wire_rope_properties_from_diameter_and_mbl(self, router):
        """Wire rope -> EA derived from diameter, MBL passed through.

        Previously this test pinned the buggy EA = 0.45 * MBL default
        (>200% elongation at break); EA is now diameter-based:
        EA = 0.78e8 * d^2 kN (d in m), calibrated to the curated
        wire_rope_*.yml library components.
        """
        seg = _wire_segment(diameter=0.089, breaking_load=5000)
        lt = router._segment_to_linetype(seg)

        assert lt["name"] == "wire_89mm"
        expected_ea = 0.78 * 89 ** 2 * 100
        assert lt["properties"]["EA"] == pytest.approx(expected_ea, rel=1e-6)
        assert lt["properties"]["AllowableTension"] == 5000

    def test_wire_ea_matches_curated_library_component(self, router):
        """76mm wire EA ~ 450000 kN per curated wire_rope_76mm.yml.

        Regression: the buggy EA = 0.45 * MBL default gave EA of the same
        order as MBL (~2 orders of magnitude too soft).
        """
        seg = _wire_segment(diameter=0.076, breaking_load=4950)
        lt = router._segment_to_linetype(seg)

        assert lt["properties"]["EA"] == pytest.approx(450000, rel=0.02)
        # Sanity: wire EA must be tens of times MBL, not a fraction of it
        assert lt["properties"]["EA"] > 10 * 4950

    def test_polyester_ea_matches_curated_library_component(self, router):
        """140mm polyester EA ~ 140000 kN per curated polyester_rope_140mm.yml.

        Regression: the buggy EA = 0.15 * MBL default gave 1080 kN for a
        7200 kN MBL rope.
        """
        seg = _polyester_segment(diameter=0.140, breaking_load=7200)
        lt = router._segment_to_linetype(seg)

        assert lt["properties"]["EA"] == pytest.approx(140000, rel=0.03)
        assert lt["properties"]["EA"] > 10 * 7200

    def test_axial_stiffness_override_still_wins(self, router):
        """Explicit axial_stiffness overrides the derived EA defaults."""
        seg = MooringSegment(
            type=MooringSegmentType.WIRE_ROPE,
            diameter=0.076,
            length=300,
            breaking_load=4950,
            axial_stiffness=555000,
        )
        lt = router._segment_to_linetype(seg)
        assert lt["properties"]["EA"] == 555000

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
        """Line with pretension -> winch spans vessel and line End B.

        Previously this test pinned the invalid topology (line End B
        connected to the winch object, winch with a scalar Connection);
        OrcaFlex lines cannot connect to winches and winches need >= 2
        connection points (see builders/winch_builder.py for the proven
        list form).
        """
        system = MooringSystem(lines=[_simple_mooring_line(pretension=1500)])
        result = router.route(system)

        assert "winches" in result
        assert len(result["winches"]) == 1
        winch = result["winches"][0]
        assert winch["name"] == "Winch_ML1"
        assert winch["properties"]["Tension"] == 1500
        assert winch["properties"]["WinchControl"] == "Specified tension"
        # Winch spans two connection points: vessel and line End B
        assert winch["properties"]["Connection"] == ["FPSO", "ML1"]
        assert winch["properties"]["ConnectionPoint"] == [[10, 0, -10], [0, 0, 0]]
        assert winch["properties"]["ConnectionzRelativeTo"] == [None, "End B"]
        # Line End B stays on the vessel fairlead (never on the winch object)
        line = result["lines"][0]
        assert line["properties"]["EndBConnection"] == "FPSO"
        assert line["properties"]["EndBX"] == 10
        assert line["properties"]["EndBY"] == 0
        assert line["properties"]["EndBZ"] == -10

    def test_pretension_line_never_references_winch_object(self, router):
        """Regression: no generated line may use a winch name as a connection."""
        system = MooringSystem(
            lines=[
                _simple_mooring_line(name="ML1", pretension=500),
                _simple_mooring_line(name="ML2", pretension=800),
            ]
        )
        result = router.route(system)

        winch_names = {w["name"] for w in result["winches"]}
        for line in result["lines"]:
            props = line["properties"]
            assert props["EndAConnection"] not in winch_names
            assert props["EndBConnection"] not in winch_names

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
