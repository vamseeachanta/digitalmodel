# ABOUTME: Unit tests for APDLReader — APDL .inp file material and section parsing
# ABOUTME: TDD Red phase: tests written before implementation

"""Tests for apdl_reader — APDLReader.parse_materials and parse_sections."""

from pathlib import Path

import pytest

from digitalmodel.ansys.apdl_reader import APDLReader
from digitalmodel.ansys.models import APDLMaterial, APDLSection

FIXTURES = Path(__file__).parent / "fixtures"
SEWOL_INP = FIXTURES / "sewol_excerpt.inp"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _reader() -> APDLReader:
    return APDLReader()


# ---------------------------------------------------------------------------
# parse_materials — basic parsing
# ---------------------------------------------------------------------------

class TestParseMaterialsBasic:
    def test_returns_list(self):
        mats = _reader().parse_materials(SEWOL_INP)
        assert isinstance(mats, list)

    def test_sewol_returns_two_materials(self):
        mats = _reader().parse_materials(SEWOL_INP)
        assert len(mats) == 2

    def test_steel_material_has_correct_id(self):
        mats = _reader().parse_materials(SEWOL_INP)
        steel = next(m for m in mats if m.mat_id == 1)
        assert steel.mat_id == 1

    def test_steel_elastic_modulus_mpa(self):
        mats = _reader().parse_materials(SEWOL_INP)
        steel = next(m for m in mats if m.mat_id == 1)
        assert steel.elastic_modulus_mpa == pytest.approx(206000.0)

    def test_steel_poissons_ratio(self):
        mats = _reader().parse_materials(SEWOL_INP)
        steel = next(m for m in mats if m.mat_id == 1)
        assert steel.poissons_ratio == pytest.approx(0.3)

    def test_steel_density(self):
        mats = _reader().parse_materials(SEWOL_INP)
        steel = next(m for m in mats if m.mat_id == 1)
        assert steel.density_kg_mm3 == pytest.approx(7.85e-9)

    def test_second_material_id_two(self):
        mats = _reader().parse_materials(SEWOL_INP)
        al = next(m for m in mats if m.mat_id == 2)
        assert al.elastic_modulus_mpa == pytest.approx(70000.0)
        assert al.poissons_ratio == pytest.approx(0.33)

    def test_returns_apdl_material_instances(self):
        mats = _reader().parse_materials(SEWOL_INP)
        assert all(isinstance(m, APDLMaterial) for m in mats)


class TestParseMaterialsEdgeCases:
    def test_skips_comment_lines(self, tmp_path):
        """Lines starting with ! are comments and must be ignored."""
        inp = tmp_path / "comments.inp"
        inp.write_text("! MP,EX,1,206000\n! this is all comments\n")
        mats = _reader().parse_materials(inp)
        assert mats == []

    def test_empty_file_returns_empty_list(self, tmp_path):
        inp = tmp_path / "empty.inp"
        inp.write_text("")
        mats = _reader().parse_materials(inp)
        assert mats == []

    def test_no_mp_commands_returns_empty_list(self, tmp_path):
        inp = tmp_path / "no_mp.inp"
        inp.write_text("/PREP7\nFINISH\n")
        mats = _reader().parse_materials(inp)
        assert mats == []

    def test_accepts_path_object(self):
        mats = _reader().parse_materials(Path(SEWOL_INP))
        assert len(mats) == 2

    def test_accepts_string_path(self):
        mats = _reader().parse_materials(str(SEWOL_INP))
        assert len(mats) == 2


# ---------------------------------------------------------------------------
# parse_sections
# ---------------------------------------------------------------------------

class TestParseSections:
    def test_returns_list(self):
        secs = _reader().parse_sections(SEWOL_INP)
        assert isinstance(secs, list)

    def test_sewol_returns_two_sections(self):
        secs = _reader().parse_sections(SEWOL_INP)
        assert len(secs) == 2

    def test_section_one_is_beam_asec(self):
        secs = _reader().parse_sections(SEWOL_INP)
        s1 = next(s for s in secs if s.section_id == 1)
        assert s1.section_type == "BEAM"
        assert s1.section_subtype == "ASEC"

    def test_section_one_area(self):
        secs = _reader().parse_sections(SEWOL_INP)
        s1 = next(s for s in secs if s.section_id == 1)
        assert s1.area_mm2 == pytest.approx(1.963e+04)

    def test_section_one_iz(self):
        secs = _reader().parse_sections(SEWOL_INP)
        s1 = next(s for s in secs if s.section_id == 1)
        assert s1.iz_mm4 == pytest.approx(9.817e+07)

    def test_section_one_iy(self):
        secs = _reader().parse_sections(SEWOL_INP)
        s1 = next(s for s in secs if s.section_id == 1)
        assert s1.iy_mm4 == pytest.approx(4.909e+07)

    def test_section_two_exists(self):
        secs = _reader().parse_sections(SEWOL_INP)
        ids = [s.section_id for s in secs]
        assert 2 in ids

    def test_returns_apdl_section_instances(self):
        secs = _reader().parse_sections(SEWOL_INP)
        assert all(isinstance(s, APDLSection) for s in secs)

    def test_no_sections_returns_empty(self, tmp_path):
        inp = tmp_path / "no_sec.inp"
        inp.write_text("/PREP7\nMP,EX,1,206000\n")
        secs = _reader().parse_sections(inp)
        assert secs == []

    def test_secdata_blank_field_preserves_positional_order(self, tmp_path):
        """SECDATA,area,,Iy must not shift Iy into Iz position."""
        inp = tmp_path / "blank_field.inp"
        inp.write_text(
            "SECTYPE,5,BEAM,ASEC\n"
            "SECDATA,1000.0,,500.0,200.0\n"  # area=1000, Iz=None, Iy=500, J=200
        )
        secs = _reader().parse_sections(inp)
        assert len(secs) == 1
        s = secs[0]
        assert s.area_mm2 == pytest.approx(1000.0)
        assert s.iz_mm4 is None
        assert s.iy_mm4 == pytest.approx(500.0)
        assert s.j_mm4 == pytest.approx(200.0)
