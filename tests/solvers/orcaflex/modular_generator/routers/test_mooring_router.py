"""Tests for MooringRouter -- TDD unit tests.

All tests are pure dict assertions, no OrcFxAPI or solver required.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
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
    POLYESTER_DEFAULTS,
    WIRE_ROPE_DEFAULTS,
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


# ---------------------------------------------------------------------------
# Rope mass per unit length against the curated library components (#3981)
# ---------------------------------------------------------------------------

_LIBRARY_LINE_TYPES = (
    Path(__file__).resolve().parents[5] / "docs/domains/orcaflex/library/line_types"
)


@pytest.mark.parametrize(
    ("seg_type", "library_file", "diameter", "library_mass", "relative_tolerance"),
    [
        (MooringSegmentType.WIRE_ROPE, "wire_rope_64mm.yml", 0.064, 0.018, 0.01),
        (MooringSegmentType.WIRE_ROPE, "wire_rope_76mm.yml", 0.076, 0.025, 0.01),
        (MooringSegmentType.WIRE_ROPE, "wire_rope_84mm.yml", 0.084, 0.031, 0.01),
        (MooringSegmentType.POLYESTER, "polyester_rope_120mm.yml", 0.120, 0.008, 0.02),
        (MooringSegmentType.POLYESTER, "polyester_rope_140mm.yml", 0.140, 0.011, 0.02),
        (MooringSegmentType.POLYESTER, "polyester_rope_160mm.yml", 0.160, 0.014, 0.02),
    ],
)
def test_rope_mass_matches_curated_library_component(
    seg_type, library_file, diameter, library_mass, relative_tolerance
):
    """Library mass agreement within 1 % for wire rope and 2 % for polyester.

    Regression: the wire and polyester coefficients gave 0.196 te/m for 76 mm
    wire (library 0.025 te/m) and 0.115 te/m for 120 mm polyester (library
    0.008 te/m). The pinned masses in te/m come from the library YAMLs
    cited in the router diff of fa970678f.
    """
    library = yaml.safe_load((_LIBRARY_LINE_TYPES / library_file).read_text(encoding="utf-8"))
    assert library["MassPerUnitLength"] == library_mass
    seg = MooringSegment(type=seg_type, diameter=diameter, length=100)
    lt = MooringRouter()._segment_to_linetype(seg)
    assert lt["properties"]["MassPerUnitLength"] == pytest.approx(
        library_mass, rel=relative_tolerance
    )


@pytest.fixture
def mixed_mooring_spec():
    line = MooringLine(
        name="ML1",
        segments=[_chain_segment(), _wire_segment(diameter=0.076), _polyester_segment()],
        anchor=_anchor(),
        fairlead=_fairlead(),
    )
    return ProjectInputSpec(
        metadata={
            "name": "mixed", "description": "Mixed mooring line",
            "structure": "mooring", "operation": "inplace",
        },
        environment={"water": {"depth": 100}, "seabed": {"stiffness": {"normal": 100}}},
        equipment={"vessel": {
            "name": "FPSO",
            "properties": {
                "loa": 100, "beam": 20, "draft": 6, "gmt": 2,
                "cog": [0, 0, 0], "gyration_radii": [8, 25, 25],
            },
        }},
        mooring=MooringSystem(lines=[line]),
    )


def _generate_model(spec, directory):
    ModularModelGenerator.from_spec(spec).generate(directory)
    master = yaml.safe_load((directory / "master.yml").read_text(encoding="utf-8"))
    model = {"master.yml": master}
    for entry in master:
        path = entry["includefile"]
        model[path] = yaml.safe_load((directory / path).read_text(encoding="utf-8"))
    return model


def test_mixed_mooring_changes_only_rope_masses(mixed_mooring_spec, tmp_path, monkeypatch):
    # Before fa970678f, mass in te/m = coefficient * nominal diameter in mm squared.
    old_wire_coeff = 0.0340e-3
    old_polyester_coeff = 0.0080e-3
    with monkeypatch.context() as patch:
        patch.setitem(WIRE_ROPE_DEFAULTS, "mass_coeff", old_wire_coeff)
        patch.setitem(POLYESTER_DEFAULTS, "mass_coeff", old_polyester_coeff)
        before = _generate_model(mixed_mooring_spec, tmp_path / "before")
    after = _generate_model(mixed_mooring_spec, tmp_path / "after")

    objects = before["includes/20_generic_objects.yml"]
    line_types = {lt["Name"]: lt for lt in objects["LineTypes"]}
    assert [row[0] for row in objects["Lines"][0]["LineType, Length, NumberOfSegments"]] == [
        "chain_84mm_R3", "wire_76mm", "polyester_140mm",
    ]
    assert line_types["chain_84mm_R3"]["MassPerUnitLength"] == pytest.approx(0.0219e-3 * 84**2)
    assert line_types["wire_76mm"]["MassPerUnitLength"] == pytest.approx(old_wire_coeff * 76**2)
    assert line_types["polyester_140mm"]["MassPerUnitLength"] == pytest.approx(
        old_polyester_coeff * 140**2
    )

    # Permit just these two changes in the master and its referenced includes.
    line_types["wire_76mm"]["MassPerUnitLength"] = pytest.approx(0.025, rel=0.01)
    line_types["polyester_140mm"]["MassPerUnitLength"] = pytest.approx(0.011, rel=0.02)
    assert after == before


def test_explicit_line_type_mass_overrides_computed_mass(mixed_mooring_spec, tmp_path):
    # MooringSegment supplies the computed property. GenericLineType exposes
    # the explicit mass_per_length input when a user customizes a routed model.
    generic = MooringRouter().route(mixed_mooring_spec.mooring)
    wire = next(lt for lt in generic["line_types"] if lt["name"] == "wire_76mm")
    assert wire["properties"]["MassPerUnitLength"] == pytest.approx(0.025, rel=0.01)
    wire["mass_per_length"] = 0.040
    spec = ProjectInputSpec(
        metadata=mixed_mooring_spec.metadata,
        environment=mixed_mooring_spec.environment,
        equipment=mixed_mooring_spec.equipment,
        generic=generic,
    )
    model = _generate_model(spec, tmp_path / "override")
    emitted = model["includes/20_generic_objects.yml"]["LineTypes"]
    assert next(lt for lt in emitted if lt["Name"] == "wire_76mm")["MassPerUnitLength"] == 0.040
