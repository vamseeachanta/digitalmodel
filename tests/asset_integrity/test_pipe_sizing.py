"""
Tests for asset_integrity.custom.PipeSizing module.

Focus: pure-Python / math paths only.
- pipe_section_properties: Ao, Ai, Io, Ii, Jo, Ji, A, I, J
- sectionProperties helper
- geometry derivation when OD/ID/WT is missing
- get_fea_properties derived quantities (EI, EA, GJ, MassPerUnitLength)
- single-pipe get_pipe_system_properties

No OrcaFlex, AQWA, or external licence required.
"""

import math
import pytest


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_cfg(od: float, wt: float):
    """Return a minimal config dict for a single outer-pipe run."""
    id_ = od - 2 * wt
    return {
        "Outer_Pipe": {
            "Geometry": {
                "Nominal_OD": od,
                "Nominal_ID": id_,
                "Design_WT": wt,
            },
            "Material": {
                "Material": "steel",
                "Material_Grade": "X65",
            },
        },
        "Inner_Pipe": None,
        "Material": {
            "steel": {
                "E": 200e9,
                "Rho": 7850.0,
                "Poissionsratio": 0.3,
                "Grades": {
                    "X65": {
                        "SMYS": 448.0e6,
                        "SMUS": 531.0e6,
                    }
                },
            }
        },
    }


def _pipe_sizing(od, wt):
    from digitalmodel.asset_integrity.custom.PipeSizing import PipeSizing
    cfg = _make_cfg(od, wt)
    return PipeSizing(cfg), cfg


# ---------------------------------------------------------------------------
# pipe_section_properties — cross-section geometry
# ---------------------------------------------------------------------------


class TestPipeSectionProperties:
    """Verify cross-section areas, moments of inertia, and torsion constants."""

    def test_section_properties_keys_present(self):
        ps, cfg = _pipe_sizing(od=0.3239, wt=0.01430)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        for key in ("Ao", "Ai", "Io", "Ii", "Jo", "Ji", "A", "I", "J"):
            assert key in sp, f"Missing key: {key}"

    def test_outer_area_formula(self):
        od = 0.3239
        ps, cfg = _pipe_sizing(od=od, wt=0.01430)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        expected = (math.pi / 4) * od**2
        assert sp["Ao"] == pytest.approx(expected, rel=1e-6)

    def test_inner_area_formula(self):
        od, wt = 0.3239, 0.01430
        id_ = od - 2 * wt
        ps, cfg = _pipe_sizing(od=od, wt=wt)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        expected = (math.pi / 4) * id_**2
        assert sp["Ai"] == pytest.approx(expected, rel=1e-6)

    def test_annular_area_equals_outer_minus_inner(self):
        ps, cfg = _pipe_sizing(od=0.3239, wt=0.01430)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        assert sp["A"] == pytest.approx(sp["Ao"] - sp["Ai"], rel=1e-10)

    def test_moment_of_inertia_outer_formula(self):
        od = 0.5080
        ps, cfg = _pipe_sizing(od=od, wt=0.020)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        expected = (math.pi / 64) * od**4
        assert sp["Io"] == pytest.approx(expected, rel=1e-6)

    def test_section_moment_of_inertia_equals_Io_minus_Ii(self):
        ps, cfg = _pipe_sizing(od=0.5080, wt=0.020)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        assert sp["I"] == pytest.approx(sp["Io"] - sp["Ii"], rel=1e-10)

    def test_polar_moment_outer_is_twice_second_moment(self):
        ps, cfg = _pipe_sizing(od=0.4064, wt=0.015)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        assert sp["Jo"] == pytest.approx(2 * sp["Io"], rel=1e-10)

    def test_torsion_constant_equals_Jo_minus_Ji(self):
        ps, cfg = _pipe_sizing(od=0.4064, wt=0.015)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        assert sp["J"] == pytest.approx(sp["Jo"] - sp["Ji"], rel=1e-10)

    def test_all_properties_positive_for_valid_pipe(self):
        ps, cfg = _pipe_sizing(od=0.2731, wt=0.012)
        ps.pipe_properties("Outer_Pipe")
        sp = cfg["Outer_Pipe"]["section_properties"]["pipe"]
        for key in ("Ao", "Ai", "Io", "Ii", "Jo", "Ji", "A", "I", "J"):
            assert sp[key] > 0, f"Expected positive for {key}"

    def test_thicker_wall_increases_area(self):
        ps_thin, cfg_thin = _pipe_sizing(od=0.3239, wt=0.010)
        ps_thin.pipe_properties("Outer_Pipe")
        ps_thick, cfg_thick = _pipe_sizing(od=0.3239, wt=0.020)
        ps_thick.pipe_properties("Outer_Pipe")
        a_thin = cfg_thin["Outer_Pipe"]["section_properties"]["pipe"]["A"]
        a_thick = cfg_thick["Outer_Pipe"]["section_properties"]["pipe"]["A"]
        assert a_thick > a_thin

    def test_larger_od_increases_moment_of_inertia(self):
        ps_small, cfg_small = _pipe_sizing(od=0.2731, wt=0.012)
        ps_small.pipe_properties("Outer_Pipe")
        ps_large, cfg_large = _pipe_sizing(od=0.5080, wt=0.012)
        ps_large.pipe_properties("Outer_Pipe")
        i_small = cfg_small["Outer_Pipe"]["section_properties"]["pipe"]["I"]
        i_large = cfg_large["Outer_Pipe"]["section_properties"]["pipe"]["I"]
        assert i_large > i_small


# ---------------------------------------------------------------------------
# Geometry derivation (missing OD / ID / WT)
# ---------------------------------------------------------------------------


class TestGeometryDerivation:
    """Test that missing geometry parameters are computed from the other two."""

    def test_missing_id_derived_from_od_and_wt(self):
        from digitalmodel.asset_integrity.custom.PipeSizing import PipeSizing
        od, wt = 0.3239, 0.01430
        cfg = _make_cfg(od=od, wt=wt)
        cfg["Outer_Pipe"]["Geometry"]["Nominal_ID"] = None
        ps = PipeSizing(cfg)
        ps.pipe_properties("Outer_Pipe")
        expected_id = od - 2 * wt
        actual_id = cfg["Outer_Pipe"]["Geometry"]["Nominal_ID"]
        assert actual_id == pytest.approx(expected_id, rel=1e-10)

    def test_missing_od_derived_from_id_and_wt(self):
        from digitalmodel.asset_integrity.custom.PipeSizing import PipeSizing
        od, wt = 0.3239, 0.01430
        id_ = od - 2 * wt
        cfg = _make_cfg(od=od, wt=wt)
        cfg["Outer_Pipe"]["Geometry"]["Nominal_OD"] = None
        ps = PipeSizing(cfg)
        ps.pipe_properties("Outer_Pipe")
        expected_od = id_ + 2 * wt
        actual_od = cfg["Outer_Pipe"]["Geometry"]["Nominal_OD"]
        assert actual_od == pytest.approx(expected_od, rel=1e-10)

    def test_missing_wt_derived_from_od_and_id(self):
        from digitalmodel.asset_integrity.custom.PipeSizing import PipeSizing
        od, wt = 0.3239, 0.01430
        id_ = od - 2 * wt
        cfg = _make_cfg(od=od, wt=wt)
        cfg["Outer_Pipe"]["Geometry"]["Design_WT"] = None
        ps = PipeSizing(cfg)
        ps.pipe_properties("Outer_Pipe")
        expected_wt = (od - id_) / 2
        actual_wt = cfg["Outer_Pipe"]["Geometry"]["Design_WT"]
        assert actual_wt == pytest.approx(expected_wt, rel=1e-10)


# ---------------------------------------------------------------------------
# FEA-derived properties (EI, EA, GJ, MassPerUnitLength)
# ---------------------------------------------------------------------------


class TestFEAProperties:
    """Test elasticity and mass properties derived from section + material."""

    def _get_fea_props(self, od=0.3239, wt=0.01430):
        ps, cfg = _pipe_sizing(od=od, wt=wt)
        ps.pipe_properties("Outer_Pipe")
        return cfg["Outer_Pipe"]["section_properties"]["pipe"]

    def test_ei_is_e_times_i(self):
        od, wt = 0.3239, 0.01430
        sp = self._get_fea_props(od=od, wt=wt)
        e = 200e9
        assert sp["EI"] == pytest.approx(e * sp["I"], rel=1e-6)

    def test_ea_is_e_times_a(self):
        sp = self._get_fea_props()
        e = 200e9
        assert sp["EA"] == pytest.approx(e * sp["A"], rel=1e-6)

    def test_mass_per_unit_length_is_area_times_density(self):
        od, wt = 0.3239, 0.01430
        sp = self._get_fea_props(od=od, wt=wt)
        rho = 7850.0
        assert sp["MassPerUnitLength"] == pytest.approx(rho * sp["A"], rel=1e-6)

    def test_shear_modulus_derived_correctly(self):
        # G = E / (2*(1 + nu))
        sp = self._get_fea_props()
        e, nu = 200e9, 0.3
        g_expected = e / (2 * (1 + nu))
        gj_expected = g_expected * sp["J"]
        assert sp["GJ"] == pytest.approx(gj_expected, rel=1e-5)

    def test_smys_populated_from_material_grade(self):
        sp = self._get_fea_props()
        assert sp["SMYS"] == pytest.approx(448.0e6, rel=1e-6)

    def test_smus_populated_from_material_grade(self):
        sp = self._get_fea_props()
        assert sp["SMUS"] == pytest.approx(531.0e6, rel=1e-6)

    def test_poisson_ratio_populated(self):
        sp = self._get_fea_props()
        assert sp["PoissonRatio"] == pytest.approx(0.3, rel=1e-8)

    def test_e_stored_in_section_properties(self):
        sp = self._get_fea_props()
        assert sp["E"] == pytest.approx(200e9, rel=1e-6)

    def test_all_fea_properties_positive(self):
        sp = self._get_fea_props()
        for key in ("EI", "EA", "GJ", "MassPerUnitLength"):
            assert sp[key] > 0, f"Expected positive for {key}"


# ---------------------------------------------------------------------------
# get_pipe_system_properties (single outer pipe)
# ---------------------------------------------------------------------------


class TestGetPipeSystemProperties:
    """Test the top-level convenience method for single-pipe configurations."""

    def test_returns_equivalent_pipe_dict(self):
        ps, _ = _pipe_sizing(od=0.3239, wt=0.01430)
        result = ps.get_pipe_system_properties()
        assert "section_properties" in result

    def test_equivalent_pipe_inherits_outer_pipe_properties(self):
        od, wt = 0.3239, 0.01430
        ps, cfg = _pipe_sizing(od=od, wt=wt)
        result = ps.get_pipe_system_properties()
        sp = result["section_properties"]["pipe"]
        expected_a = (math.pi / 4) * (od**2 - (od - 2 * wt)**2)
        assert sp["A"] == pytest.approx(expected_a, rel=1e-5)

    def test_equivalent_pipe_ei_positive(self):
        ps, _ = _pipe_sizing(od=0.5080, wt=0.020)
        result = ps.get_pipe_system_properties()
        assert result["section_properties"]["pipe"]["EI"] > 0


# ---------------------------------------------------------------------------
# sectionProperties helper (internal)
# ---------------------------------------------------------------------------


class TestSectionPropertiesHelper:
    """Test the internal section property calculator for generic OD/ID."""

    def _call(self, od, id_):
        from digitalmodel.asset_integrity.custom.PipeSizing import PipeSizing
        cfg = _make_cfg(od=od, wt=(od - id_) / 2)
        ps = PipeSizing(cfg)
        data = {"OD": od, "ID": id_}
        return ps.sectionProperties(data)

    def test_area_formula(self):
        od, id_ = 0.4064, 0.3556
        result = self._call(od, id_)
        expected = (math.pi / 4) * (od**2 - id_**2)
        assert result["A"] == pytest.approx(expected, rel=1e-6)

    def test_inner_area_formula(self):
        od, id_ = 0.4064, 0.3556
        result = self._call(od, id_)
        expected = (math.pi / 4) * id_**2
        assert result["Ai"] == pytest.approx(expected, rel=1e-6)

    def test_outer_area_formula(self):
        od, id_ = 0.4064, 0.3556
        result = self._call(od, id_)
        expected = (math.pi / 4) * od**2
        assert result["Ao"] == pytest.approx(expected, rel=1e-6)

    def test_moment_of_inertia_formula(self):
        od, id_ = 0.4064, 0.3556
        result = self._call(od, id_)
        expected = (math.pi / 64) * (od**4 - id_**4)
        assert result["I"] == pytest.approx(expected, rel=1e-6)

    def test_all_results_positive(self):
        result = self._call(0.3239, 0.2953)
        for key in ("A", "Ai", "Ao", "I"):
            assert result[key] > 0

    def test_od_and_id_preserved_in_result(self):
        od, id_ = 0.3239, 0.2953
        result = self._call(od, id_)
        assert result["OD"] == od
        assert result["ID"] == id_
