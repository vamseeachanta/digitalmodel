#!/usr/bin/env python3
"""
ABOUTME: Tests for the MYSTRAN F06 parser against captured MYSTRAN 19.0.0
output fragments (displacements, SPC forces, HEXA8 stresses, errors).
"""

import pytest

from digitalmodel.solvers.mystran.result_parser import MystranResultParser


DISP_BLOCK = """
 OUTPUT FOR SUBCASE        1
 CANTILEVER CBAR TRIAL

                                                       D I S P L A C E M E N T S
                                              (in global coordinate system at each grid)
           GRID     COORD      T1            T2            T3            R1            R2            R3
                     SYS
              1        0  0.0           0.0           0.0           0.0           0.0           0.0
              2        0  0.0           1.636911E-05  0.0           0.0           0.0           1.250005E-04
              3        0  0.0           5.952405E-05  0.0           0.0           0.0           2.142866E-04
              4        0  0.0           1.205362E-04  0.0           0.0           0.0           2.678582E-04
              5        0  0.0           1.904770E-04  0.0           0.0           0.0           2.857154E-04
                         ------------- ------------- ------------- ------------- ------------- -------------
                MAX* :    0.0           1.904770E-04  0.0           0.0           0.0           2.857154E-04
                MIN* :    0.0           0.0           0.0           0.0           0.0           0.0

                ABS* :    0.0           1.904770E-04  0.0           0.0           0.0           2.857154E-04
                *for output set
"""

SPCF_BLOCK = """
 OUTPUT FOR SUBCASE        1
 CANTILEVER CBAR TRIAL

                                                          S P C   F O R C E S
                                              (in global coordinate system at each grid)
           GRID     COORD      T1            T2            T3            R1            R2            R3
                     SYS
              1        0  0.0          -1.000000E+03  0.0           0.0           0.0          -1.000000E+03


"""

BAR_STRESS_BLOCK = """
 OUTPUT FOR SUBCASE        1

             E L E M E N T   S T R E S S E S   I N   L O C A L   E L E M E N T   C O O R D I N A T E   S Y S T E M
                                          F O R   E L E M E N T   T Y P E   B A R
  Element      SA1           SA2           SA3           SA4          Axial         SA-Max        SA-Min      M.S.-T
     ID        SB1           SB2           SB3           SB4          Stress        SB-Max        SB-Min      M.S.-C

        1  0.0           0.0           0.0           0.0           0.0           0.0           0.0
           0.0           0.0           0.0           0.0                         0.0           0.0
          ------------- ------------- ------------- ------------- ------------- ------------- ------------- ---------
"""

HEX_STRESS_BLOCK = """
 OUTPUT FOR SUBCASE        1
 HEX CANTILEVER TRIAL

                                E L E M E N T   S T R E S S E S   I N   M A T E R I A L   C O O R D I N A T E   S Y S T E M
                                                       F O R   E L E M E N T   T Y P E   H E X A  8
    Elem  Location            Sigma-xx      Sigma-yy      Sigma-zz       Tau-xy        Tau-yz        Tau-zx       von Mises
     ID
        1  CENTER            1.716341E-08  5.293549E-09  3.891058E-09  2.161889E-08  1.094627E-09  1.000000E+05  1.732051E+05
           GRD       1       4.970469E+06  2.130201E+06  2.130201E+06  6.272921E+04  0.000000E+00  7.200670E+06  1.279171E+07
           GRD       2       4.970469E+06  2.130201E+06  2.130201E+06 -6.272921E+04  0.000000E+00  7.200670E+06  1.279171E+07
        2  CENTER           -6.229968E-09 -1.873266E-08  3.895333E-09  3.119688E-08  0.000000E+00  1.000000E+05  1.732051E+05
           GRD       5       1.071523E+06 -1.333039E+06 -7.845471E+04 -5.713024E+04 -6.272921E+05  2.974945E+06  5.663951E+06
                            ------------- ------------- ------------- ------------- ------------- ------------- -------------
                MAX* :       4.970469E+06  2.130201E+06  2.130201E+06  6.272921E+04  6.272921E+05  7.200670E+06  1.279171E+07
                MIN* :      -4.970469E+06 -2.130201E+06 -2.130201E+06 -6.272921E+04 -6.272921E+05 -7.000670E+06  1.732051E+05
"""

EPSILON_LINE = (
    " *INFORMATION: FOR INTERNAL SUBCASE NUMBER        1 EPSILON ERROR ESTIMATE"
    "            =  7.801759E-15 Based on U'*(K*U - P)/(U'*P)\n"
)

ERROR_LINE = " *ERROR  1964: PSOLID ENTRY HAD FIELD FOR IN BLANK WITH ELEMENT TYPE HEXA8\n"

FULL_F06 = EPSILON_LINE + DISP_BLOCK + SPCF_BLOCK + BAR_STRESS_BLOCK + HEX_STRESS_BLOCK


@pytest.fixture
def parser(tmp_path):
    (tmp_path / "job.F06").write_text(FULL_F06)
    return MystranResultParser("job", tmp_path)


class TestFileDiscovery:

    def test_uppercase_and_lowercase_extension(self, tmp_path):
        (tmp_path / "a.f06").write_text(DISP_BLOCK)
        p = MystranResultParser("a", tmp_path)
        # Case-insensitive filesystems (Windows) may report either spelling
        assert p.f06_path.name.lower() == "a.f06"
        assert p.f06_path.exists()
        assert len(p.parse_f06()["displacements"]) == 5

    def test_missing_f06_raises(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            MystranResultParser("nope", tmp_path).parse_f06()

    def test_find_output_none(self, tmp_path):
        assert MystranResultParser("nope", tmp_path).find_output("OP2") is None


class TestDisplacements:

    def test_all_grids_parsed(self, parser):
        d = parser.parse_f06()["displacements"]
        assert sorted(d) == [1, 2, 3, 4, 5]

    def test_values(self, parser):
        d = parser.get_displacement(5)
        assert d["t1"] == 0.0
        assert d["t2"] == pytest.approx(1.904770e-4)
        assert d["r3"] == pytest.approx(2.857154e-4)

    def test_max_component(self, parser):
        assert parser.get_max_displacement("t2") == pytest.approx(1.904770e-4)
        assert parser.get_max_displacement("t3") == 0.0
        assert parser.get_max_displacement("mag") == pytest.approx(1.904770e-4)

    def test_unknown_component_raises(self, parser):
        with pytest.raises(ValueError):
            parser.get_max_displacement("tx")

    def test_missing_grid_raises(self, parser):
        with pytest.raises(KeyError):
            parser.get_displacement(99)

    def test_max_min_summary_lines_not_treated_as_rows(self, parser):
        d = parser.parse_f06()["displacements"]
        assert 0 not in d
        assert len(d) == 5


class TestSpcForces:

    def test_spc_forces_parsed_without_summary_lines(self, parser):
        f = parser.parse_f06()["spc_forces"]
        assert list(f) == [1]
        assert f[1]["t2"] == pytest.approx(-1000.0)
        assert f[1]["r3"] == pytest.approx(-1000.0)

    def test_reaction_sum(self, parser):
        r = parser.get_reaction_sum()
        assert r["t2"] == pytest.approx(-1000.0)
        assert r["t1"] == 0.0


class TestStresses:

    def test_hexa8_block_parsed(self, parser):
        s = parser.parse_f06()["element_stresses"]
        assert "HEXA8" in s
        assert sorted(s["HEXA8"]) == [1, 2]

    def test_center_and_grid_rows(self, parser):
        e1 = parser.parse_f06()["element_stresses"]["HEXA8"][1]
        assert e1["center"]["tau_zx"] == pytest.approx(1.0e5)
        assert e1["center"]["von_mises"] == pytest.approx(1.732051e5)
        assert sorted(e1["grid"]) == [1, 2]
        assert e1["grid"][2]["tau_xy"] == pytest.approx(-6.272921e4)

    def test_header_keys(self, parser):
        row = parser.parse_f06()["element_stresses"]["HEXA8"][1]["center"]
        assert list(row) == [
            "sigma_xx", "sigma_yy", "sigma_zz", "tau_xy", "tau_yz", "tau_zx", "von_mises",
        ]

    def test_max_von_mises(self, parser):
        assert parser.get_max_von_mises() == pytest.approx(1.279171e7)
        assert parser.get_max_von_mises("HEXA8") == pytest.approx(1.279171e7)
        assert parser.get_max_von_mises("QUAD4") == 0.0

    def test_bar_block_is_skipped_gracefully(self, parser):
        s = parser.parse_f06()["element_stresses"]
        assert "BAR" not in s


class TestDiagnostics:

    def test_epsilon(self, parser):
        assert parser.parse_f06()["epsilon"] == pytest.approx(7.801759e-15)
        assert not parser.has_errors

    def test_errors_collected(self, tmp_path):
        (tmp_path / "bad.F06").write_text(ERROR_LINE + DISP_BLOCK)
        p = MystranResultParser("bad", tmp_path)
        s = p.parse_f06()
        assert len(s["errors"]) == 1
        assert "1964" in s["errors"][0]
        assert p.has_errors

    def test_epsilon_none_when_absent(self, tmp_path):
        (tmp_path / "x.F06").write_text(DISP_BLOCK)
        assert MystranResultParser("x", tmp_path).parse_f06()["epsilon"] is None

    def test_parse_text_directly(self):
        p = MystranResultParser("mem", "/nonexistent")
        s = p.parse_f06_text(DISP_BLOCK)
        assert len(s["displacements"]) == 5
        assert p.get_max_displacement("t2") == pytest.approx(1.904770e-4)

    def test_read_op2_missing_file(self, parser):
        pytest.importorskip("pyNastran")
        with pytest.raises(FileNotFoundError):
            parser.read_op2()
