"""
Comprehensive unit tests for PipeCapacity.py

Tests cover:
- Module-level helper functions (_extract_year_from_code, _resolve_pressure_value,
  _is_dnv_f101, _is_dnv_f201)
- DNVWallThickness class (design factors, material strengths, pressures,
  burst/collapse/propagation calculations)
- PipeCapacity class (load condition evaluation, pressure results update,
  specification-code-based dispatch, ASME B31 / API STD 2RD / API RP 1111 methods)
- CFR_30_Part_250 class (burst pressure and minimum thickness)
- API_RP_16Q class (instantiation and edition metadata)
- API_TR_5C3 class (collapse pressure formula)
- OtherMethodsTobeIncorporated static methods (ASME B31.4 / B31.8 internal pressure,
  longitudinal stress, equivalent stress)

Engineering values use realistic pipe dimensions (e.g., 12.75" OD, X65 steel).
"""

import math
import pytest

from digitalmodel.structural.pipe_capacity.custom.PipeCapacity import (
    _extract_year_from_code,
    _is_dnv_f101,
    _is_dnv_f201,
    _resolve_pressure_value,
    CFR_30_Part_250,
    DNVWallThickness,
    OtherMethodsTobeIncorporated,
    PipeCapacity,
)


# ---------------------------------------------------------------------------
# Helper function tests
# ---------------------------------------------------------------------------

class TestExtractYearFromCode:
    # NOTE: The source regex uses r"(19|20)\\d{2}" (double-escaped backslash
    # inside a raw string), which searches for the literal characters '\d'
    # rather than digit metacharacters.  As a result the function returns None
    # for all normal year strings like "2013".  The tests below document the
    # actual runtime behaviour.

    def test_normal_year_returns_none_due_to_regex_bug(self):
        # "2013" does not contain literal '\d', so no match
        assert _extract_year_from_code("DNV-OS-F101-2013") is None

    def test_st_variant_returns_none(self):
        assert _extract_year_from_code("DNVGL-ST-F101-2017") is None

    def test_year_2000_returns_none(self):
        assert _extract_year_from_code("DNV-F201-2000") is None

    def test_no_year_returns_none(self):
        assert _extract_year_from_code("DNV-OS-F101") is None

    def test_no_match_returns_none(self):
        assert _extract_year_from_code("ASME B31.4") is None

    def test_literal_backslash_dd_would_match_but_int_fails(self):
        # The regex r"(19|20)\\d{2}" matches "19" or "20" + literal '\' + 'dd'.
        # The matched group is e.g. "20\dd" which cannot be converted to int.
        # So even when the regex matches, int() raises ValueError.
        test_input = "code-20" + "\\" + "dd-end"  # contains "20\dd"
        with pytest.raises(ValueError):
            _extract_year_from_code(test_input)


class TestResolvePressureValue:

    def test_none_returns_zero(self):
        assert _resolve_pressure_value(None) == 0.0

    def test_numeric_value_returned(self):
        assert _resolve_pressure_value(1500.0) == 1500.0

    def test_integer_coerced_to_float(self):
        assert _resolve_pressure_value(1500) == 1500.0

    def test_dict_computes_fluid_column_pressure(self):
        # pressure = fluid_density * fluid_column * 12
        value = {"fluid_density": 0.4433, "fluid_column": 5000.0}
        expected = 0.4433 * 5000.0 * 12.0
        assert _resolve_pressure_value(value) == pytest.approx(expected)

    def test_dict_with_missing_keys_returns_zero(self):
        assert _resolve_pressure_value({}) == 0.0

    def test_dict_with_zero_column(self):
        value = {"fluid_density": 0.4433, "fluid_column": 0.0}
        assert _resolve_pressure_value(value) == 0.0


class TestIsDnvF101:

    def test_standard_code(self):
        assert _is_dnv_f101("DNV-OS-F101-2013") is True

    def test_st_variant(self):
        assert _is_dnv_f101("DNVGL-ST-F101-2017") is True

    def test_lowercase(self):
        assert _is_dnv_f101("dnv-os-f101-2013") is True

    def test_f201_is_not_f101(self):
        assert _is_dnv_f101("DNV-OS-F201-2010") is False

    def test_non_dnv_code(self):
        assert _is_dnv_f101("ASME B31.4") is False


class TestIsDnvF201:

    def test_standard_code(self):
        assert _is_dnv_f201("DNV-OS-F201-2010") is True

    def test_lowercase(self):
        assert _is_dnv_f201("dnv-os-f201-2010") is True

    def test_f101_is_not_f201(self):
        assert _is_dnv_f201("DNV-OS-F101-2013") is False

    def test_non_dnv_code(self):
        assert _is_dnv_f201("API STD 2RD-2013") is False


# ---------------------------------------------------------------------------
# Fixtures for DNVWallThickness
# ---------------------------------------------------------------------------

def _make_dnv_cfg(
    od=12.75,
    wt=0.625,
    corrosion=0.039,
    smys=65000.0,
    smus=77000.0,
    E=2.97e7,
    poisson=0.3,
    p_internal=10000.0,
    p_external=3000.0,
    p_min_internal=0.0,
    design_factors_overrides=None,
    pipe_flag="Outer_Pipe",
    temp_derating=1.0,
    spec_code="DNV-OS-F101-2013",
):
    """Build a minimal cfg dict for DNVWallThickness tests.

    Default values represent a realistic 12.75" OD, X65 steel subsea pipeline.
    """
    cfg = {
        pipe_flag: {
            "Geometry": {
                "Nominal_OD": od,
                "Design_WT": wt,
                "Corrosion_Allowance": corrosion,
            },
            "Material": {
                "SMYS": smys,
                "SMUS": smus,
                "E": E,
                "Poissons_Ratio": poisson,
            },
        },
        "Design": [
            {
                "InternalPressure": {pipe_flag: p_internal},
                "ExternalPressure": {pipe_flag: p_external},
                "MinimumInternalPressure": {pipe_flag: p_min_internal},
                "Load Condition": {pipe_flag: "internal_pressure"},
                "Code": [{pipe_flag: spec_code}],
                "Material": {
                    "temperature_derating": {
                        pipe_flag: {spec_code: temp_derating}
                    }
                },
            }
        ],
        "DesignFactors": {},
        "basename": "test_pipe",
        "Inner_Pipe": None,
    }
    if design_factors_overrides:
        cfg["DesignFactors"] = design_factors_overrides
    return cfg


# ---------------------------------------------------------------------------
# DNVWallThickness tests
# ---------------------------------------------------------------------------

class TestDNVWallThicknessDesignFactors:

    def test_default_factors_when_no_overrides(self):
        cfg = _make_dnv_cfg()
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        df = dnv.design_factors
        assert df["internal_pressure"]["gamma_m"] == 1.15
        assert df["internal_pressure"]["gamma_sc"] == 1.046
        assert df["internal_pressure"]["alpha_u"] == 0.96
        assert df["internal_pressure"]["alpha_fab"] == 1.0
        assert df["external_pressure"]["ovality"] == 0.005
        assert df["collapse_propagation"]["gamma_m"] == 1.15

    def test_override_by_exact_specification_code(self):
        overrides = {
            "DNV-OS-F101-2013": {
                "internal_pressure": {"gamma_m": 1.20}
            }
        }
        cfg = _make_dnv_cfg(design_factors_overrides=overrides)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv.design_factors["internal_pressure"]["gamma_m"] == 1.20
        # Other defaults preserved
        assert dnv.design_factors["internal_pressure"]["gamma_sc"] == 1.046

    def test_override_by_generic_key(self):
        overrides = {
            "DNV-OS-F101": {
                "external_pressure": {"ovality": 0.01}
            }
        }
        cfg = _make_dnv_cfg(design_factors_overrides=overrides)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        # exact code not found, but "DNV-OS-F101" generic key matches
        assert dnv.design_factors["external_pressure"]["ovality"] == 0.01

    def test_top_level_scalar_override_applied_to_matching_section(self):
        # gamma_m at the top level should be applied to internal_pressure section
        overrides = {
            "DNV-OS-F101-2013": {
                "gamma_m": 1.30
            }
        }
        cfg = _make_dnv_cfg(design_factors_overrides=overrides)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        # gamma_m exists in all three sections, so first matching section gets it
        assert dnv.design_factors["internal_pressure"]["gamma_m"] == 1.30


class TestDNVWallThicknessTemperatureDerating:

    def test_derating_from_cfg(self):
        cfg = _make_dnv_cfg(temp_derating=0.95)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._get_temperature_derating() == 0.95

    def test_default_derating_when_key_missing(self):
        cfg = _make_dnv_cfg()
        # Remove the temperature_derating entry to trigger KeyError
        del cfg["Design"][0]["Material"]["temperature_derating"]
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._get_temperature_derating() == 1.0


class TestDNVWallThicknessMaterialStrengths:

    def test_fy_and_fu_with_defaults(self):
        cfg = _make_dnv_cfg(smys=65000.0, smus=77000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        fy, fu = dnv._get_material_strengths()
        alpha_u = 0.96
        assert fy == pytest.approx(65000.0 * alpha_u * 1.0)
        assert fu == pytest.approx(77000.0 * alpha_u * 1.0)

    def test_fy_and_fu_with_temperature_derating(self):
        cfg = _make_dnv_cfg(smys=65000.0, smus=77000.0, temp_derating=0.9)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        fy, fu = dnv._get_material_strengths()
        alpha_u = 0.96
        assert fy == pytest.approx(65000.0 * alpha_u * 0.9)
        assert fu == pytest.approx(77000.0 * alpha_u * 0.9)


class TestDNVWallThicknessMaterialModulus:

    def test_modulus_and_poisson_from_cfg(self):
        cfg = _make_dnv_cfg(E=2.97e7, poisson=0.3)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        E, poisson = dnv._get_material_modulus()
        assert E == 2.97e7
        assert poisson == 0.3

    def test_poisson_fallback_keys(self):
        cfg = _make_dnv_cfg()
        mat = cfg["Outer_Pipe"]["Material"]
        del mat["Poissons_Ratio"]
        mat["PoissonRatio"] = 0.28
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        _, poisson = dnv._get_material_modulus()
        assert poisson == 0.28


class TestDNVWallThicknessPressures:

    def test_numeric_pressures(self):
        cfg = _make_dnv_cfg(p_internal=10000.0, p_external=3000.0, p_min_internal=500.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        pi, pe, pmin = dnv._get_pressures()
        assert pi == 10000.0
        assert pe == 3000.0
        assert pmin == 500.0

    def test_dict_pressure_with_fluid_column(self):
        cfg = _make_dnv_cfg()
        cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = {
            "fluid_density": 0.4433,
            "fluid_column": 5000.0,
        }
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        pi, pe, pmin = dnv._get_pressures()
        assert pi == pytest.approx(0.4433 * 5000.0 * 12.0)

    def test_min_internal_as_dict_with_pipe_flag_key(self):
        cfg = _make_dnv_cfg()
        cfg["Design"][0]["MinimumInternalPressure"] = {"Outer_Pipe": 200.0}
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        _, _, pmin = dnv._get_pressures()
        assert pmin == 200.0

    def test_internal_minus_external_clamped_to_zero(self):
        cfg = _make_dnv_cfg(p_internal=1000.0, p_external=5000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._get_internal_minus_external() == 0.0

    def test_internal_minus_external_positive(self):
        cfg = _make_dnv_cfg(p_internal=10000.0, p_external=3000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._get_internal_minus_external() == 7000.0

    def test_external_minus_min_internal(self):
        cfg = _make_dnv_cfg(p_external=5000.0, p_min_internal=1000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        delta, pmin = dnv._get_external_minus_min_internal()
        assert delta == 4000.0
        assert pmin == 1000.0


class TestDNVWallThicknessBurst:

    def test_burst_pressure_containment_allowable_positive_thickness(self):
        cfg = _make_dnv_cfg(od=12.75, smys=65000.0, smus=77000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t = 0.625
        p_allow = dnv._pressure_containment_allowable(t)
        # Manual calculation
        fy, fu = dnv._get_material_strengths()
        fcb = min(fy, fu / 1.15)
        D = 12.75
        gamma_m = 1.15
        gamma_sc = 1.046
        expected = (2.0 / math.sqrt(3.0)) * (2.0 * t / (D - t)) * fcb / (gamma_m * gamma_sc)
        assert p_allow == pytest.approx(expected, rel=1e-9)

    def test_burst_pressure_containment_allowable_zero_thickness(self):
        cfg = _make_dnv_cfg()
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._pressure_containment_allowable(0.0) == 0.0

    def test_get_burst_pressure_includes_external(self):
        cfg = _make_dnv_cfg(p_external=3000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t = 0.625
        p_burst = dnv.get_burst_pressure(t)
        p_allow = dnv._pressure_containment_allowable(t)
        assert p_burst == pytest.approx(p_allow + 3000.0)

    def test_get_burst_minimum_thickness_positive_design_pressure(self):
        cfg = _make_dnv_cfg(p_internal=10000.0, p_external=3000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_burst_minimum_thickness()
        assert t_min > 0.0
        # Verify: burst allowable at t_min should approximately equal p_design
        p_design = 7000.0
        p_allow = dnv._pressure_containment_allowable(t_min)
        assert p_allow == pytest.approx(p_design, rel=1e-6)

    def test_get_burst_minimum_thickness_zero_when_no_design_pressure(self):
        cfg = _make_dnv_cfg(p_internal=3000.0, p_external=5000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv.get_burst_minimum_thickness() == 0.0


class TestDNVWallThicknessCollapse:

    def test_collapse_pressure_returns_positive_for_valid_pipe(self):
        cfg = _make_dnv_cfg(od=12.75, E=2.97e7, poisson=0.3, smys=65000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        pc = dnv._collapse_pressure(0.625)
        assert pc is not None
        assert pc > 0.0

    def test_collapse_pressure_returns_none_for_zero_thickness(self):
        cfg = _make_dnv_cfg()
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._collapse_pressure(0.0) is None

    def test_collapse_pressure_returns_none_for_thickness_ge_od(self):
        cfg = _make_dnv_cfg(od=12.75)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._collapse_pressure(12.75) is None
        assert dnv._collapse_pressure(15.0) is None

    def test_collapse_pressure_returns_none_for_zero_modulus(self):
        cfg = _make_dnv_cfg(E=0.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._collapse_pressure(0.625) is None

    def test_collapse_pressure_increases_with_thickness(self):
        cfg = _make_dnv_cfg()
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        pc_thin = dnv._collapse_pressure(0.4)
        pc_thick = dnv._collapse_pressure(0.8)
        assert pc_thin is not None and pc_thick is not None
        assert pc_thick > pc_thin

    def test_get_collapse_pressure_includes_safety_and_min_internal(self):
        cfg = _make_dnv_cfg(p_min_internal=500.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        gamma_m = dnv.design_factors["external_pressure"]["gamma_m"]
        gamma_sc = dnv.design_factors["external_pressure"]["gamma_sc"]
        pc_raw = dnv._collapse_pressure(0.625)
        pc_allow = dnv.get_collapse_pressure(0.625)
        expected = pc_raw / (gamma_m * gamma_sc) + 500.0
        assert pc_allow == pytest.approx(expected)

    def test_get_collapse_minimum_thickness_positive(self):
        cfg = _make_dnv_cfg(p_external=5000.0, p_min_internal=0.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_collapse_minimum_thickness()
        assert t_min > 0.0

    def test_get_collapse_minimum_thickness_zero_when_no_external(self):
        cfg = _make_dnv_cfg(p_external=0.0, p_min_internal=500.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_collapse_minimum_thickness()
        assert t_min == 0.0

    def test_collapse_minimum_thickness_satisfies_target(self):
        cfg = _make_dnv_cfg(p_external=5000.0, p_min_internal=0.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_collapse_minimum_thickness()
        gamma_m = dnv.design_factors["external_pressure"]["gamma_m"]
        gamma_sc = dnv.design_factors["external_pressure"]["gamma_sc"]
        target = 5000.0 * gamma_m * gamma_sc
        pc = dnv._collapse_pressure(t_min)
        assert pc is not None
        # The bisection should converge to within a small tolerance
        assert pc >= target * 0.99


class TestDNVWallThicknessPropagation:

    def test_propagation_pressure_formula(self):
        cfg = _make_dnv_cfg(od=12.75, smys=65000.0, p_min_internal=200.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t = 0.625
        p_prop = dnv.get_propagation_pressure(t)
        fy, _ = dnv._get_material_strengths()
        gamma_m = 1.15
        gamma_sc = 1.046
        alpha_fab = 1.0
        D = 12.75
        expected_raw = 35.0 * fy * alpha_fab * ((t / D) ** 2.5)
        expected = expected_raw / (gamma_m * gamma_sc) + 200.0
        assert p_prop == pytest.approx(expected, rel=1e-9)

    def test_propagation_pressure_zero_yield(self):
        cfg = _make_dnv_cfg(smys=0.0, smus=0.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv.get_propagation_pressure(0.625) == 0.0

    def test_propagation_minimum_thickness(self):
        cfg = _make_dnv_cfg(p_external=5000.0, p_min_internal=0.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_propagation_minimum_thickness()
        assert t_min > 0.0
        # Verify round-trip: propagation pressure at t_min should match target
        fy, _ = dnv._get_material_strengths()
        gamma_m = 1.15
        gamma_sc = 1.046
        alpha_fab = 1.0
        D = 12.75
        target = 5000.0 * gamma_m * gamma_sc
        p_raw = 35.0 * fy * alpha_fab * ((t_min / D) ** 2.5)
        assert p_raw == pytest.approx(target, rel=1e-6)

    def test_propagation_minimum_thickness_zero_when_no_delta(self):
        cfg = _make_dnv_cfg(p_external=0.0, p_min_internal=500.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv.get_propagation_minimum_thickness() == 0.0


# ---------------------------------------------------------------------------
# PipeCapacity class tests
# ---------------------------------------------------------------------------

def _make_pipe_capacity_cfg_dnv(
    pipe_flag="Outer_Pipe",
    load_condition="internal_pressure",
    spec_code="DNV-OS-F101-2013",
    od=12.75,
    wt=0.625,
    corrosion=0.039,
    smys=65000.0,
    smus=77000.0,
    E=2.97e7,
    poisson=0.3,
    p_internal=10000.0,
    p_external=3000.0,
    p_min_internal=0.0,
    temp_derating=1.0,
):
    """Build a cfg dict for PipeCapacity tests using DNV codes."""
    cfg = {
        pipe_flag: {
            "Geometry": {
                "Nominal_OD": od,
                "Design_WT": wt,
                "Corrosion_Allowance": corrosion,
            },
            "Material": {
                "SMYS": smys,
                "SMUS": smus,
                "E": E,
                "Poissons_Ratio": poisson,
            },
        },
        "Design": [
            {
                "InternalPressure": {pipe_flag: p_internal},
                "ExternalPressure": {pipe_flag: p_external},
                "MinimumInternalPressure": {pipe_flag: p_min_internal},
                "Load Condition": {pipe_flag: load_condition},
                "Code": [{pipe_flag: spec_code}],
                "Material": {
                    "temperature_derating": {
                        pipe_flag: {spec_code: temp_derating}
                    }
                },
            }
        ],
        "DesignFactors": {},
        "basename": "test_pipe",
        "Inner_Pipe": None,
        "Outer_Pipe" if pipe_flag != "Outer_Pipe" else "_skip": None,
    }
    # Ensure both pipe flags exist at top level for evaluate_pipe_wall
    if "Outer_Pipe" not in cfg:
        cfg["Outer_Pipe"] = cfg[pipe_flag]
    if "Inner_Pipe" not in cfg:
        cfg["Inner_Pipe"] = None
    return cfg


class TestPipeCapacityInternalPressureDNV:

    def test_internal_pressure_dnv_produces_results(self):
        cfg = _make_pipe_capacity_cfg_dnv()
        pc = PipeCapacity(cfg)
        pc.internal_pressure("Outer_Pipe", "DNV-OS-F101-2013", 0)
        # Results should be stored in cfg under basename
        results = cfg["test_pipe"]["Outer_Pipe"]["internal_pressure"]["DNV-OS-F101-2013"]
        assert "thickness" in results
        assert "Design_WT_Max_Pressure" in results
        assert "minimum_thickness" in results
        # Should have both zero and with corrosion allowance
        assert "Zero Corrosion Allowance" in results["thickness"]
        assert "With Corrosion Allowance" in results["thickness"]

    def test_internal_pressure_minimum_thickness_increases_with_corrosion(self):
        cfg = _make_pipe_capacity_cfg_dnv(corrosion=0.1)
        pc = PipeCapacity(cfg)
        pc.internal_pressure("Outer_Pipe", "DNV-OS-F101-2013", 0)
        results = cfg["test_pipe"]["Outer_Pipe"]["internal_pressure"]["DNV-OS-F101-2013"]
        t_zero = results["minimum_thickness"]["Zero Corrosion Allowance"]
        t_corr = results["minimum_thickness"]["With Corrosion Allowance"]
        # With corrosion allowance, minimum thickness should be larger
        assert t_corr > t_zero


class TestPipeCapacityExternalPressureDNV:

    def test_external_pressure_dnv_produces_results(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            load_condition="external_pressure",
            p_internal=1000.0,
            p_external=5000.0,
        )
        pc = PipeCapacity(cfg)
        pc.external_pressure("Outer_Pipe", "DNV-OS-F101-2013", 0)
        results = cfg["test_pipe"]["Outer_Pipe"]["external_pressure"]["DNV-OS-F101-2013"]
        assert "thickness" in results
        assert "Design_WT_Max_Pressure" in results
        assert "minimum_thickness" in results


class TestPipeCapacityCollapsePropagationDNV:

    def test_collapse_propagation_dnv_produces_results(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            load_condition="collapse_propagation",
            p_internal=0.0,
            p_external=5000.0,
        )
        pc = PipeCapacity(cfg)
        pc.collapse_propagation("Outer_Pipe", "DNV-OS-F101-2013", 0)
        results = cfg["test_pipe"]["Outer_Pipe"]["collapse_propagation"]["DNV-OS-F101-2013"]
        assert "thickness" in results
        assert "minimum_thickness" in results


class TestPipeCapacityEvaluateLoadConditions:

    def test_evaluate_load_conditions_dispatches_internal_pressure(self):
        cfg = _make_pipe_capacity_cfg_dnv(load_condition="internal_pressure")
        pc = PipeCapacity(cfg)
        pc.evaluate_load_conditions(pipe_flag="Outer_Pipe")
        assert "test_pipe" in cfg
        assert "Outer_Pipe" in cfg["test_pipe"]

    def test_evaluate_load_conditions_dispatches_external_pressure(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            load_condition="external_pressure",
            p_external=5000.0,
        )
        pc = PipeCapacity(cfg)
        pc.evaluate_load_conditions(pipe_flag="Outer_Pipe")
        assert "external_pressure" in cfg["test_pipe"]["Outer_Pipe"]

    def test_evaluate_load_conditions_dispatches_collapse_propagation(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            load_condition="collapse_propagation",
            p_external=5000.0,
        )
        pc = PipeCapacity(cfg)
        pc.evaluate_load_conditions(pipe_flag="Outer_Pipe")
        assert "collapse_propagation" in cfg["test_pipe"]["Outer_Pipe"]


class TestPipeCapacityEvaluatePipeWall:

    def test_evaluate_pipe_wall_outer_only(self):
        cfg = _make_pipe_capacity_cfg_dnv()
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        assert "test_pipe" in cfg

    def test_evaluate_pipe_wall_with_inner_pipe(self):
        cfg = _make_pipe_capacity_cfg_dnv()
        # Add inner pipe config
        cfg["Inner_Pipe"] = {
            "Geometry": {
                "Nominal_OD": 10.75,
                "Design_WT": 0.5,
                "Corrosion_Allowance": 0.02,
            },
            "Material": {
                "SMYS": 65000.0,
                "SMUS": 77000.0,
                "E": 2.97e7,
                "Poissons_Ratio": 0.3,
            },
        }
        cfg["Design"][0]["InternalPressure"]["Inner_Pipe"] = 8000.0
        cfg["Design"][0]["ExternalPressure"]["Inner_Pipe"] = 2000.0
        cfg["Design"][0]["MinimumInternalPressure"]["Inner_Pipe"] = 0.0
        cfg["Design"][0]["Load Condition"]["Inner_Pipe"] = "internal_pressure"
        cfg["Design"][0]["Code"][0]["Inner_Pipe"] = "DNV-OS-F101-2013"
        cfg["Design"][0]["Material"]["temperature_derating"]["Inner_Pipe"] = {
            "DNV-OS-F101-2013": 1.0
        }
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        assert "test_pipe" in cfg


class TestPipeCapacityUpdatePressureResults:

    def test_update_pressure_results_stores_values(self):
        cfg = _make_pipe_capacity_cfg_dnv()
        pc = PipeCapacity(cfg)
        pc.update_pressure_results(
            minimum_thickness=0.5,
            pipe_flag="Outer_Pipe",
            pressure=12000.0,
            specification_code="DNV-OS-F101-2013",
            thickness=0.625,
            load_condition="internal_pressure",
            custom_tag="Zero Corrosion Allowance",
        )
        result = cfg["test_pipe"]["Outer_Pipe"]["internal_pressure"]["DNV-OS-F101-2013"]
        assert result["thickness"]["Zero Corrosion Allowance"] == 0.625
        assert result["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] == 12000.0
        assert result["minimum_thickness"]["Zero Corrosion Allowance"] == 0.5


# ---------------------------------------------------------------------------
# Specification-code-based evaluation dispatch
# ---------------------------------------------------------------------------

class TestInternalPressureCodeDispatch:

    def test_dnv_f101_dispatch(self):
        cfg = _make_pipe_capacity_cfg_dnv(spec_code="DNV-OS-F101-2013")
        pc = PipeCapacity(cfg)
        t_min, p = pc.internal_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "DNV-OS-F101-2013", 0.625
        )
        assert t_min > 0.0
        assert p > 0.0

    def test_dnv_f201_dispatch(self):
        cfg = _make_pipe_capacity_cfg_dnv(spec_code="DNV-OS-F201-2010")
        cfg["Design"][0]["Code"][0]["Outer_Pipe"] = "DNV-OS-F201-2010"
        cfg["Design"][0]["Material"]["temperature_derating"]["Outer_Pipe"] = {
            "DNV-OS-F201-2010": 1.0
        }
        pc = PipeCapacity(cfg)
        t_min, p = pc.internal_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "DNV-OS-F201-2010", 0.625
        )
        assert t_min > 0.0
        assert p > 0.0


class TestExternalPressureCodeDispatch:

    def test_dnv_f101_dispatch(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            load_condition="external_pressure",
            p_external=5000.0,
        )
        pc = PipeCapacity(cfg)
        t_min, p = pc.external_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "DNV-OS-F101-2013", 0.625
        )
        assert t_min > 0.0
        assert p > 0.0


class TestCollapsePropagationCodeDispatch:

    def test_dnv_f101_dispatch(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            load_condition="collapse_propagation",
            p_external=5000.0,
        )
        pc = PipeCapacity(cfg)
        t_min, p = pc.collapse_propagation_specification_code_based_evaluation(
            0, "Outer_Pipe", "DNV-OS-F101-2013", 0.625
        )
        assert t_min > 0.0
        assert p > 0.0


# ---------------------------------------------------------------------------
# ASME B31 Barlow equation tests
# ---------------------------------------------------------------------------

def _make_asme_b31_cfg(
    pipe_flag="Outer_Pipe",
    spec_code="ASME B31.4",
    od=12.75,
    wt=0.375,
    smys=52000.0,
    smus=66000.0,
    p_internal=1480.0,
    p_external=0.0,
    design_factor=0.72,
    weld_factor=1.0,
    temp_derating=1.0,
    d_over_t_transition=30.0,
):
    """Build a cfg dict for ASME B31 Barlow equation tests.

    Default: 12.75" OD, X52 steel, 0.375" WT onshore pipeline.
    """
    pipe_data = {
        "Geometry": {
            "Nominal_OD": od,
            "Design_WT": wt,
            "Corrosion_Allowance": 0.0,
        },
        "Material": {
            "SMYS": smys,
            "SMUS": smus,
            "WeldFactor": {"Seamless": weld_factor},
        },
    }
    cfg = {
        pipe_flag: pipe_data,
        "Design": [
            {
                "InternalPressure": {pipe_flag: p_internal},
                "ExternalPressure": {pipe_flag: p_external},
                "Load Condition": {pipe_flag: "internal_pressure"},
                "Code": [{pipe_flag: spec_code}],
                "Material": {
                    "temperature_derating": {
                        pipe_flag: {spec_code: temp_derating}
                    }
                },
            }
        ],
        "DesignFactors": {
            spec_code: {
                "internal_pressure": design_factor,
                "D_over_T_Trasition_Ratio": d_over_t_transition,
            }
        },
        "basename": "test_pipe",
        "Inner_Pipe": None,
    }
    # Ensure Outer_Pipe key exists if pipe_flag is Outer_Pipe
    if pipe_flag != "Outer_Pipe":
        cfg["Outer_Pipe"] = None
    return cfg


class TestBurstPressureModifiedBarlow:

    def test_thin_wall_formula_d_over_t_ge_30(self):
        # D/t = 12.75/0.375 = 34 >= 30, so thin-wall formula
        cfg = _make_asme_b31_cfg(od=12.75, wt=0.375, d_over_t_transition=30.0)
        pc = PipeCapacity(cfg)
        t = 0.375
        p = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, t
        )
        smys = 52000.0
        design_factor = 0.72
        weld_factor = 1.0
        temp_derating = 1.0
        stress_hoop = smys * design_factor * weld_factor * temp_derating
        expected = 2 * t * stress_hoop / 12.75 + 0.0  # p_external = 0
        assert p == pytest.approx(expected)

    def test_thick_wall_formula_d_over_t_lt_30(self):
        # D/t = 12.75/1.0 = 12.75 < 30, so thick-wall formula
        cfg = _make_asme_b31_cfg(od=12.75, wt=1.0, d_over_t_transition=30.0)
        pc = PipeCapacity(cfg)
        t = 1.0
        p = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, t
        )
        smys = 52000.0
        design_factor = 0.72
        weld_factor = 1.0
        temp_derating = 1.0
        stress_hoop = smys * design_factor * weld_factor * temp_derating
        expected = 2 * t * stress_hoop / (12.75 - t) + 0.0
        assert p == pytest.approx(expected)


class TestBurstMinThicknessModifiedBarlow:

    def test_thin_wall_d_over_t_ge_30(self):
        cfg = _make_asme_b31_cfg(
            od=12.75, wt=0.375, p_internal=1480.0, p_external=0.0,
        )
        pc = PipeCapacity(cfg)
        t_min = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        smys = 52000.0
        design_factor = 0.72
        stress_hoop = smys * design_factor * 1.0 * 1.0
        expected = 1480.0 * 12.75 / (2 * stress_hoop)
        assert t_min == pytest.approx(expected)

    def test_thick_wall_d_over_t_lt_30(self):
        cfg = _make_asme_b31_cfg(
            od=12.75, wt=1.0, p_internal=5000.0, p_external=0.0,
        )
        pc = PipeCapacity(cfg)
        t_min = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        smys = 52000.0
        design_factor = 0.72
        stress_hoop = smys * design_factor * 1.0 * 1.0
        delta_p = 5000.0
        expected = delta_p * 12.75 / (2 * stress_hoop + delta_p)
        assert t_min == pytest.approx(expected)


# ---------------------------------------------------------------------------
# API STD 2RD tests
# ---------------------------------------------------------------------------

def _make_api_std_2rd_cfg(
    od=12.75,
    wt=0.625,
    smys=65000.0,
    smus=77000.0,
    p_internal=10000.0,
    p_external=3000.0,
    Fd=0.67,
    k=0.45,
):
    pipe_flag = "Outer_Pipe"
    spec_code = "API STD 2RD-2013"
    pipe_data = {
        "Geometry": {
            "Nominal_OD": od,
            "Design_WT": wt,
            "Corrosion_Allowance": 0.0,
        },
        "Material": {
            "SMYS": smys,
            "SMUS": smus,
        },
    }
    cfg = {
        pipe_flag: pipe_data,
        "Design": [
            {
                "InternalPressure": {pipe_flag: p_internal},
                "ExternalPressure": {pipe_flag: p_external},
                "Load Condition": {pipe_flag: "internal_pressure"},
                "Code": [{pipe_flag: spec_code}],
                "Material": {
                    "temperature_derating": {
                        pipe_flag: {spec_code: 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            spec_code: {
                "internal_pressure": {
                    "Fd": Fd,
                    "k": {"API 5L": k},
                },
            }
        },
        "basename": "test_pipe",
        "Inner_Pipe": None,
    }
    return cfg


class TestBurstPressureAPISTD2RD:

    def test_burst_pressure_formula(self):
        cfg = _make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        t = 0.625
        p = pc.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, t
        )
        Fd = 0.67
        k = 0.45
        smys = 65000.0
        smus = 77000.0
        od = 12.75
        expected = Fd * k * (smys + smus) * math.log(od / (od - 2 * t))
        assert p == pytest.approx(expected)


class TestBurstMinThicknessAPISTD2RD:

    def test_minimum_thickness_formula(self):
        cfg = _make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        t_min = pc.evaluate_burst_minimum_thickness_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0
        )
        Fd = 0.67
        k = 0.45
        smys = 65000.0
        smus = 77000.0
        od = 12.75
        p_diff = 10000.0 - 3000.0
        burst_by = p_diff / k / Fd / (smys + smus)
        expected = 0.5 * (od - od / math.exp(burst_by))
        assert t_min == pytest.approx(expected)


# ---------------------------------------------------------------------------
# API RP 1111 tests
# ---------------------------------------------------------------------------

def _make_api_rp_1111_cfg(
    od=12.75,
    wt=0.625,
    smys=65000.0,
    smus=77000.0,
    p_internal=10000.0,
    p_external=3000.0,
    Fd=0.90,
    Fp=0.80,
    d_over_t_transition=15.0,
):
    pipe_flag = "Outer_Pipe"
    spec_code = "API RP 1111-2009"
    pipe_data = {
        "Geometry": {
            "Nominal_OD": od,
            "Design_WT": wt,
            "Corrosion_Allowance": 0.0,
        },
        "Material": {
            "SMYS": smys,
            "SMUS": smus,
        },
    }
    cfg = {
        pipe_flag: pipe_data,
        "Design": [
            {
                "InternalPressure": {pipe_flag: p_internal},
                "ExternalPressure": {pipe_flag: p_external},
                "Load Condition": {pipe_flag: "internal_pressure"},
                "Code": [{pipe_flag: spec_code}],
                "Material": {
                    "temperature_derating": {
                        pipe_flag: {spec_code: 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            spec_code: {
                "internal_pressure": {"Fd": Fd},
                "collapse_propagation": {"Fp": Fp},
                "D_over_T_Trasition_Ratio": d_over_t_transition,
            }
        },
        "basename": "test_pipe",
        "Inner_Pipe": None,
    }
    return cfg


class TestBurstPressureAPIRP1111:

    def test_thick_wall_d_over_t_le_transition(self):
        # D/t = 12.75/0.625 = 20.4, transition = 25 => thick-wall (<=)
        cfg = _make_api_rp_1111_cfg(od=12.75, wt=0.625, d_over_t_transition=25.0)
        pc = PipeCapacity(cfg)
        t = 0.625
        p = pc.evaluate_burst_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, t
        )
        Fd = 0.90
        smys = 65000.0
        smus = 77000.0
        od = 12.75
        expected = Fd * 0.45 * (smys + smus) * math.log(od / (od - 2 * t))
        assert p == pytest.approx(expected)

    def test_thin_wall_d_over_t_gt_transition(self):
        # D/t = 12.75/0.625 = 20.4, transition = 15 => thin-wall (>)
        cfg = _make_api_rp_1111_cfg(od=12.75, wt=0.625, d_over_t_transition=15.0)
        pc = PipeCapacity(cfg)
        t = 0.625
        p = pc.evaluate_burst_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, t
        )
        Fd = 0.90
        smys = 65000.0
        smus = 77000.0
        od = 12.75
        expected = Fd * 0.90 * (smys + smus) * (t / (od - t))
        assert p == pytest.approx(expected)


class TestBurstMinThicknessAPIRP1111:

    def test_thick_wall_d_over_t_le_transition(self):
        cfg = _make_api_rp_1111_cfg(
            od=12.75, wt=0.625, d_over_t_transition=25.0,
            p_internal=10000.0, p_external=3000.0,
        )
        pc = PipeCapacity(cfg)
        t_min = pc.evaluate_burst_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        assert t_min > 0.0

    def test_thin_wall_d_over_t_gt_transition(self):
        cfg = _make_api_rp_1111_cfg(
            od=12.75, wt=0.625, d_over_t_transition=15.0,
            p_internal=10000.0, p_external=3000.0,
        )
        pc = PipeCapacity(cfg)
        t_min = pc.evaluate_burst_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        Fd = 0.90
        smys = 65000.0
        smus = 77000.0
        od = 12.75
        p_diff = 7000.0
        burst_by = p_diff / 0.90 / Fd / (smys + smus)
        expected = burst_by * od / (1 + burst_by)
        assert t_min == pytest.approx(expected)


# ---------------------------------------------------------------------------
# API RP 1111 collapse propagation tests
# ---------------------------------------------------------------------------

class TestCollapsePropagationAPIRP1111:

    def test_propagation_pressure_formula(self):
        cfg = _make_api_rp_1111_cfg(
            od=12.75, wt=0.625, Fp=0.80,
        )
        cfg["Design"][0]["Load Condition"]["Outer_Pipe"] = "collapse_propagation"
        pc = PipeCapacity(cfg)
        t = 0.625
        p = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, t
        )
        smys = 65000.0
        Pp = 24.0 * smys * ((t / 12.75) ** 2.4)
        expected = Pp * 0.80
        assert p == pytest.approx(expected)

    def test_propagation_min_thickness_with_fluid_column(self):
        cfg = _make_api_rp_1111_cfg(od=12.75, wt=0.625, Fp=0.80)
        cfg["Design"][0]["Load Condition"]["Outer_Pipe"] = "collapse_propagation"
        cfg["Design"][0]["ExternalPressure"]["Outer_Pipe"] = {
            "fluid_density": 0.4433,
            "fluid_column": 5000.0,
        }
        pc = PipeCapacity(cfg)
        t_min = pc.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        assert t_min > 0.0

    def test_propagation_min_thickness_with_none_external(self):
        cfg = _make_api_rp_1111_cfg(od=12.75, wt=0.625, Fp=0.80)
        cfg["Design"][0]["Load Condition"]["Outer_Pipe"] = "collapse_propagation"
        cfg["Design"][0]["ExternalPressure"]["Outer_Pipe"] = None
        pc = PipeCapacity(cfg)
        # When external pressure is None, Po remains None, causing TypeError in division
        with pytest.raises(TypeError):
            pc.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
                "Outer_Pipe", "API RP 1111-2009", 0
            )


# ---------------------------------------------------------------------------
# CFR_30_Part_250 tests
# ---------------------------------------------------------------------------

def _make_cfr_cfg(
    od=12.75,
    wt=0.625,
    smys=52000.0,
    p_internal=2000.0,
    p_external=0.0,
    design_factor=0.60,
    weld_factor=1.0,
    temp_derating=1.0,
):
    pipe_flag = "Outer_Pipe"
    spec_code = "30 CFR Part 250"
    cfg = {
        pipe_flag: {
            "Geometry": {"Nominal_OD": od, "Design_WT": wt},
            "Material": {
                "SMYS": smys,
                "WeldFactor": {"Seamless": weld_factor},
            },
        },
        "Design": [
            {
                "InternalPressure": {pipe_flag: p_internal},
                "ExternalPressure": {pipe_flag: p_external},
                "Load Condition": {pipe_flag: "internal_pressure"},
                "Material": {
                    "temperature_derating": {
                        pipe_flag: {spec_code: temp_derating}
                    }
                },
            }
        ],
        "DesignFactors": {
            spec_code: {
                "internal_pressure": {"Fd": design_factor},
            }
        },
        "basename": "test_pipe",
    }
    return cfg


class TestCFR30Part250:

    def test_burst_pressure(self):
        cfg = _make_cfr_cfg()
        cfr = CFR_30_Part_250(cfg)
        p = cfr.get_burst_pressure("Outer_Pipe", 0)
        smys = 52000.0
        Fd = 0.60
        weld = 1.0
        temp = 1.0
        stress = smys * Fd * weld * temp
        t = 0.625
        od = 12.75
        expected = 2 * t * stress / od + 0.0
        assert p == pytest.approx(expected)

    def test_burst_minimum_thickness(self):
        cfg = _make_cfr_cfg()
        cfr = CFR_30_Part_250(cfg)
        t_min = cfr.get_burst_minimum_thickness("Outer_Pipe", 0)
        smys = 52000.0
        Fd = 0.60
        stress = smys * Fd * 1.0 * 1.0
        p_diff = 2000.0 - 0.0
        expected = p_diff * 12.75 / 2 / stress
        assert t_min == pytest.approx(expected)

    def test_burst_pressure_with_external_pressure(self):
        cfg = _make_cfr_cfg(p_internal=5000.0, p_external=1000.0)
        cfr = CFR_30_Part_250(cfg)
        p = cfr.get_burst_pressure("Outer_Pipe", 0)
        smys = 52000.0
        Fd = 0.60
        stress = smys * Fd * 1.0 * 1.0
        t = 0.625
        od = 12.75
        expected = 2 * t * stress / od + 1000.0
        assert p == pytest.approx(expected)


# ---------------------------------------------------------------------------
# API_RP_16Q tests (limited - delegates to VonMises_Pipe which needs complex cfg)
# ---------------------------------------------------------------------------

class TestAPIRP16Q:

    def test_instantiation(self):
        import datetime

        try:
            from digitalmodel.structural.pipe_capacity.custom.PipeCapacity import API_RP_16Q
        except ImportError:
            pytest.skip("API_RP_16Q not importable")

        cfg = {}
        api = API_RP_16Q(cfg)
        assert api.edition == "second"
        assert api.release_date == datetime.date(2017, 4, 1)


# ---------------------------------------------------------------------------
# API_TR_5C3 tests
# ---------------------------------------------------------------------------

class TestAPITR5C3:

    def _make_5c3_cfg(self, od=13.375, id_=12.415, wt=0.48, p_ext=5000.0, p_int=0.0):
        pipe_flag = "Outer_Pipe"
        cfg = {
            pipe_flag: {
                "Geometry": {
                    "Nominal_OD": od,
                    "Nominal_ID": id_,
                    "Design_WT": wt,
                },
            },
            "Design": [
                {
                    "ExternalPressure": {pipe_flag: p_ext},
                    "InternalPressure": {pipe_flag: p_int},
                }
            ],
        }
        return cfg

    def test_instantiation(self):
        import datetime

        try:
            from digitalmodel.structural.pipe_capacity.custom.PipeCapacity import API_TR_5C3
        except ImportError:
            pytest.skip("API_TR_5C3 not importable")

        cfg = self._make_5c3_cfg()
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        assert api.edition == "second"
        assert api.release_date == datetime.date(2018, 6, 1)
        assert api.D_o == 13.375
        assert api.t == 0.48

    def test_collapse_pressure_formula(self):
        try:
            from digitalmodel.structural.pipe_capacity.custom.PipeCapacity import API_TR_5C3
        except ImportError:
            pytest.skip("API_TR_5C3 not importable")

        cfg = self._make_5c3_cfg(od=13.375, wt=0.48)
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        pe = api.get_collapse_pressure()
        d_over_t = 13.375 / 0.48
        expected = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)
        assert pe == pytest.approx(expected)

    def test_collapse_pressure_with_custom_thickness(self):
        try:
            from digitalmodel.structural.pipe_capacity.custom.PipeCapacity import API_TR_5C3
        except ImportError:
            pytest.skip("API_TR_5C3 not importable")

        cfg = self._make_5c3_cfg(od=13.375, wt=0.48)
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        pe = api.get_collapse_pressure(t=0.6)
        d_over_t = 13.375 / 0.6
        expected = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)
        assert pe == pytest.approx(expected)


# ---------------------------------------------------------------------------
# OtherMethodsTobeIncorporated tests (static-like methods)
# ---------------------------------------------------------------------------

class TestASMEB314InternalPressure:

    def _base_data(self, d_over_t_ge_30=True):
        if d_over_t_ge_30:
            D, t = 12.75, 0.375  # D/t = 34
        else:
            D, t = 12.75, 1.0  # D/t = 12.75
        return {
            "S": 52000.0,
            "t": t,
            "D": D,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
        }

    def test_thin_wall_case(self):
        data = self._base_data(d_over_t_ge_30=True)
        result = OtherMethodsTobeIncorporated.ASMEB314InternalPressure(data)
        assert "MaximumDesignPressure" in result
        assert "MinimumWallThickness_Pressure" in result
        stress = data["S"] * data["F"] * data["WeldFactor"] * data["T"]
        expected_t = data["Pi"] * data["D"] / (2 * stress)
        assert result["MinimumWallThickness_Pressure"] == pytest.approx(expected_t)

    def test_thick_wall_case(self):
        data = self._base_data(d_over_t_ge_30=False)
        result = OtherMethodsTobeIncorporated.ASMEB314InternalPressure(data)
        stress = data["S"] * data["F"] * data["WeldFactor"] * data["T"]
        expected_t = data["Pi"] * data["D"] / (2 * stress + data["Pi"])
        assert result["MinimumWallThickness_Pressure"] == pytest.approx(expected_t)


class TestASMEB314LongitudinalStress:

    def test_restrained_condition(self):
        data = {
            "S": 52000.0,
            "t": 0.375,
            "D": 12.75,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
            "Condition": "Restrained",
            "E": 2.97e7,
            "Alpha": 6.5e-6,
            "T1": 150.0,
            "T2": 60.0,
            "Poissionsratio": 0.3,
        }
        result = OtherMethodsTobeIncorporated.ASMEB314LogitudinalStress(data)
        assert result is not None
        assert "Stress_Elongation" in result
        assert "Stress_Hoop" in result
        assert "MinimumWallThickness_Longitudinal" in result

    def test_non_restrained_returns_none(self):
        data = {
            "Condition": "Unrestrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB314LogitudinalStress(data)
        assert result is None


class TestASMEB314EquivalentStress:

    def test_restrained_condition(self):
        data = {
            "S": 52000.0,
            "t": 0.375,
            "D": 12.75,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
            "Condition": "Restrained",
            "E": 2.97e7,
            "Alpha": 6.5e-6,
            "T1": 150.0,
            "T2": 60.0,
            "Poissionsratio": 0.3,
        }
        result = OtherMethodsTobeIncorporated.ASMEB314EquivalentStress(data)
        assert result is not None
        assert "MinimumWallThickness_Equivalent" in result
        assert "Stress_Equivalent" in result

    def test_non_restrained_returns_none(self):
        data = {"Condition": "Unrestrained"}
        result = OtherMethodsTobeIncorporated.ASMEB314EquivalentStress(data)
        assert result is None


class TestASMEB318InternalPressure:

    def _base_data(self, d_over_t_ge_30=True):
        if d_over_t_ge_30:
            D, t = 12.75, 0.375
        else:
            D, t = 12.75, 1.0
        return {
            "S": 52000.0,
            "t": t,
            "D": D,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
        }

    def test_thin_wall_case(self):
        data = self._base_data(d_over_t_ge_30=True)
        result = OtherMethodsTobeIncorporated.ASMEB318InternalPressure(data)
        assert "MaximumDesignPressure" in result
        assert "MinimumWallThickness" in result

    def test_thick_wall_case(self):
        data = self._base_data(d_over_t_ge_30=False)
        result = OtherMethodsTobeIncorporated.ASMEB318InternalPressure(data)
        assert result["MinimumWallThickness"] > 0.0


class TestASMEB318LongitudinalStress:

    def test_restrained_thin_wall(self):
        data = {
            "S": 52000.0,
            "t": 0.375,
            "D": 12.75,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
            "Condition": "Restrained",
            "E": 2.97e7,
            "Alpha": 6.5e-6,
            "T1": 150.0,
            "T2": 60.0,
            "Poissionsratio": 0.3,
        }
        result = OtherMethodsTobeIncorporated.ASMEB318LogitudinalStress(data)
        assert result is not None
        assert "MinimumWallThickness_Longitudinal" in result

    def test_restrained_thick_wall(self):
        data = {
            "S": 52000.0,
            "t": 1.0,
            "D": 12.75,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
            "Condition": "Restrained",
            "E": 2.97e7,
            "Alpha": 6.5e-6,
            "T1": 150.0,
            "T2": 60.0,
            "Poissionsratio": 0.3,
        }
        result = OtherMethodsTobeIncorporated.ASMEB318LogitudinalStress(data)
        assert result is not None

    def test_non_restrained_returns_none(self):
        data = {"Condition": "Unrestrained"}
        result = OtherMethodsTobeIncorporated.ASMEB318LogitudinalStress(data)
        assert result is None


class TestASMEB318EquivalentStress:

    def test_restrained_condition(self):
        data = {
            "S": 52000.0,
            "t": 0.375,
            "D": 12.75,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 2000.0,
            "Po": 0.0,
            "Condition": "Restrained",
            "E": 2.97e7,
            "Alpha": 6.5e-6,
            "T1": 150.0,
            "T2": 60.0,
            "Poissionsratio": 0.3,
        }
        result = OtherMethodsTobeIncorporated.ASMEB318EquivalentStress(data)
        assert result is not None
        assert "MinimumWallThickness_Equivalent" in result
        assert "Stress_Equivalent" in result

    def test_non_restrained_returns_none(self):
        data = {"Condition": "Unrestrained"}
        result = OtherMethodsTobeIncorporated.ASMEB318EquivalentStress(data)
        assert result is None


# ---------------------------------------------------------------------------
# Edge case / cross-cutting tests
# ---------------------------------------------------------------------------

class TestDNVEdgeCases:

    def test_inner_pipe_flag(self):
        cfg = _make_dnv_cfg(pipe_flag="Inner_Pipe")
        cfg["Inner_Pipe"] = cfg.pop("Inner_Pipe", None) or cfg.get("Inner_Pipe")
        # Re-make with explicit inner pipe
        cfg = {
            "Inner_Pipe": {
                "Geometry": {"Nominal_OD": 10.75, "Design_WT": 0.5, "Corrosion_Allowance": 0.02},
                "Material": {"SMYS": 65000.0, "SMUS": 77000.0, "E": 2.97e7, "Poissons_Ratio": 0.3},
            },
            "Design": [
                {
                    "InternalPressure": {"Inner_Pipe": 8000.0},
                    "ExternalPressure": {"Inner_Pipe": 2000.0},
                    "MinimumInternalPressure": {"Inner_Pipe": 0.0},
                    "Load Condition": {"Inner_Pipe": "internal_pressure"},
                    "Material": {"temperature_derating": {"Inner_Pipe": {"DNV-OS-F101-2013": 1.0}}},
                }
            ],
            "DesignFactors": {},
        }
        dnv = DNVWallThickness(cfg, "Inner_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_burst_minimum_thickness()
        assert t_min > 0.0

    def test_large_diameter_pipe(self):
        # 24" OD, 1.0" WT, X70 steel
        cfg = _make_dnv_cfg(od=24.0, wt=1.0, smys=70000.0, smus=82000.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_min = dnv.get_burst_minimum_thickness()
        assert t_min > 0.0
        pc = dnv._collapse_pressure(1.0)
        assert pc is not None and pc > 0.0

    def test_high_external_pressure_deepwater(self):
        # Simulating 10,000 ft water depth, ~4440 psi external
        cfg = _make_dnv_cfg(p_internal=0.0, p_external=4440.0, p_min_internal=0.0)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        t_collapse = dnv.get_collapse_minimum_thickness()
        t_prop = dnv.get_propagation_minimum_thickness()
        # Propagation thickness is typically larger than collapse thickness
        # for deepwater scenarios
        assert t_collapse > 0.0
        assert t_prop > 0.0


class TestDNVWallThicknessNominalOD:

    def test_get_nominal_od(self):
        cfg = _make_dnv_cfg(od=12.75)
        dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2013")
        assert dnv._get_nominal_od() == 12.75


class TestPipeCapacityF201:

    def test_external_pressure_f201_dispatch(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            spec_code="DNV-OS-F201-2010",
            load_condition="external_pressure",
            p_external=5000.0,
        )
        cfg["Design"][0]["Code"][0]["Outer_Pipe"] = "DNV-OS-F201-2010"
        cfg["Design"][0]["Material"]["temperature_derating"]["Outer_Pipe"] = {
            "DNV-OS-F201-2010": 1.0
        }
        pc = PipeCapacity(cfg)
        t_min, p = pc.external_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "DNV-OS-F201-2010", 0.625
        )
        assert t_min > 0.0
        assert p > 0.0

    def test_collapse_propagation_f201_dispatch(self):
        cfg = _make_pipe_capacity_cfg_dnv(
            spec_code="DNV-OS-F201-2010",
            load_condition="collapse_propagation",
            p_external=5000.0,
        )
        cfg["Design"][0]["Code"][0]["Outer_Pipe"] = "DNV-OS-F201-2010"
        cfg["Design"][0]["Material"]["temperature_derating"]["Outer_Pipe"] = {
            "DNV-OS-F201-2010": 1.0
        }
        pc = PipeCapacity(cfg)
        t_min, p = pc.collapse_propagation_specification_code_based_evaluation(
            0, "Outer_Pipe", "DNV-OS-F201-2010", 0.625
        )
        assert t_min > 0.0
        assert p > 0.0
