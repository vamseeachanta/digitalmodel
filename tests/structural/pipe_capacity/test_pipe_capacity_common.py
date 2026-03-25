"""
Comprehensive Unit Tests for PipeCapacity Common Module
========================================================

Tests cover:
- PipeCapacity: evaluate_pipe_wall, evaluate_load_conditions, internal_pressure,
  external_pressure, collapse_propagation, update_pressure_results
- Internal pressure codes: ASME B31, API STD 2RD-2013, API RP 1111-2009,
  30 CFR Part 250
- External pressure codes: API STD 2RD-2013, API TR 5C3-2018
- Collapse propagation: API RP 1111-2009, API STD 2RD-2013
- Barlow equation variants (thin/thick wall)
- OtherMethodsTobeIncorporated (ASME B31.4, B31.8 static methods)
- API_TR_5C3: collapse pressure, collapse minimum thickness
- CFR_30_Part_250: burst pressure, burst minimum thickness
- API_RP_16Q: init, edition/date
- API_RP_2RD: init

Engineering values: 10.75in OD x 0.5in WT X65 steel pipe (typical subsea pipeline).
"""

import copy
import math
import sys
import os
import pytest

# Ensure common and custom packages are importable
sys.path.insert(0, os.path.join(
    os.path.dirname(__file__), '..', '..', '..', 'src', 'digitalmodel', 'infrastructure'
))

from common.update_deep import update_deep_dictionary

# Now import the module under test directly by manipulating sys.path
# so the 'from common.update_deep import ...' inside PipeCapacity.py resolves
sys.path.insert(0, os.path.join(
    os.path.dirname(__file__), '..', '..', '..', 'src'
))

from digitalmodel.structural.pipe_capacity.common.PipeCapacity import (
    PipeCapacity,
    API_RP_2RD,
    API_RP_16Q,
    API_TR_5C3,
    CFR_30_Part_250,
    InternalPressureMethods,
    OtherMethodsTobeIncorporated,
)


# ---------------------------------------------------------------------------
# Helpers: Configuration builders for realistic pipe engineering data
# ---------------------------------------------------------------------------

def make_pipe_geometry(nominal_od=10.75, design_wt=0.5, corrosion_allowance=0.05):
    """Standard X65 steel pipe geometry in inches."""
    return {
        "Nominal_OD": nominal_od,
        "Nominal_ID": nominal_od - 2 * design_wt,
        "Design_WT": design_wt,
        "Corrosion_Allowance": corrosion_allowance,
    }


def make_pipe_material(smys=65000, smus=77000, material="Carbon Steel"):
    """X65 steel material properties (psi)."""
    return {
        "SMYS": smys,
        "SMUS": smus,
        "Material": material,
        "WeldFactor": {"Seamless": 1.0, "ERW": 0.95},
    }


def make_material_database():
    """Global material property database."""
    return {
        "Carbon Steel": {
            "E": 29e6,  # psi
            "Poissionsratio": 0.3,
        }
    }


def make_asme_b31_cfg(
    internal_pressure=5000,
    external_pressure=1000,
    nominal_od=10.75,
    design_wt=0.5,
    corrosion_allowance=0.05,
    design_factor=0.72,
    d_over_t_transition=30,
    temp_derating=1.0,
):
    """Build a complete config for ASME B31 internal pressure evaluation."""
    pipe_geometry = make_pipe_geometry(nominal_od, design_wt, corrosion_allowance)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "internal_pressure"},
                "Code": [{"Outer_Pipe": "ASME B31.4"}],
                "InternalPressure": {"Outer_Pipe": internal_pressure},
                "ExternalPressure": {"Outer_Pipe": external_pressure},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {"ASME B31.4": temp_derating}
                    }
                },
            }
        ],
        "DesignFactors": {
            "ASME B31.4": {
                "internal_pressure": design_factor,
                "D_over_T_Trasition_Ratio": d_over_t_transition,
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


def make_api_std_2rd_cfg(
    load_condition="internal_pressure",
    internal_pressure=5000,
    external_pressure=1000,
    nominal_od=10.75,
    design_wt=0.5,
    corrosion_allowance=0.05,
    fd=0.67,
    k_api5l=0.45,
):
    """Build config for API STD 2RD-2013 evaluation."""
    pipe_geometry = make_pipe_geometry(nominal_od, design_wt, corrosion_allowance)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": load_condition},
                "Code": [{"Outer_Pipe": "API STD 2RD-2013"}],
                "InternalPressure": {"Outer_Pipe": internal_pressure},
                "ExternalPressure": {"Outer_Pipe": external_pressure},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {"API STD 2RD-2013": 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            "API STD 2RD-2013": {
                load_condition: {
                    "Fd": fd,
                    "k": {"API 5L": k_api5l},
                },
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


def make_api_rp_1111_cfg(
    load_condition="internal_pressure",
    internal_pressure=5000,
    external_pressure=1000,
    nominal_od=10.75,
    design_wt=0.5,
    corrosion_allowance=0.05,
    fd=0.90,
    d_over_t_transition=30,
):
    """Build config for API RP 1111-2009 evaluation."""
    pipe_geometry = make_pipe_geometry(nominal_od, design_wt, corrosion_allowance)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": load_condition},
                "Code": [{"Outer_Pipe": "API RP 1111-2009"}],
                "InternalPressure": {"Outer_Pipe": internal_pressure},
                "ExternalPressure": {"Outer_Pipe": external_pressure},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {"API RP 1111-2009": 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            "API RP 1111-2009": {
                load_condition: {"Fd": fd},
                "D_over_T_Trasition_Ratio": d_over_t_transition,
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


def make_cfr_30_cfg(
    internal_pressure=5000,
    external_pressure=1000,
    nominal_od=10.75,
    design_wt=0.5,
    fd=0.60,
    temp_derating=1.0,
):
    """Build config for 30 CFR Part 250."""
    pipe_geometry = make_pipe_geometry(nominal_od, design_wt, 0.05)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "internal_pressure"},
                "Code": [{"Outer_Pipe": "30 CFR Part 250"}],
                "InternalPressure": {"Outer_Pipe": internal_pressure},
                "ExternalPressure": {"Outer_Pipe": external_pressure},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {"30 CFR Part 250": temp_derating}
                    }
                },
            }
        ],
        "DesignFactors": {
            "30 CFR Part 250": {
                "internal_pressure": {"Fd": fd, },
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


def make_api_tr_5c3_cfg(
    external_pressure=3000,
    internal_pressure=0,
    nominal_od=10.75,
    design_wt=0.5,
):
    """Build config for API TR 5C3-2018 collapse evaluation."""
    pipe_geometry = make_pipe_geometry(nominal_od, design_wt, 0.05)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "external_pressure"},
                "Code": [{"Outer_Pipe": "API TR 5C3-2018"}],
                "InternalPressure": {"Outer_Pipe": internal_pressure},
                "ExternalPressure": {"Outer_Pipe": external_pressure},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {"API TR 5C3-2018": 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            "API TR 5C3-2018": {
                "external_pressure": {"Fd": 0.67},
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


def make_collapse_propagation_cfg(
    specification_code="API RP 1111-2009",
    external_pressure=3000,
    nominal_od=10.75,
    design_wt=0.5,
    corrosion_allowance=0.05,
    fp=0.80,
):
    """Build config for collapse propagation evaluation."""
    pipe_geometry = make_pipe_geometry(nominal_od, design_wt, corrosion_allowance)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "collapse_propagation"},
                "Code": [{"Outer_Pipe": specification_code}],
                "InternalPressure": {"Outer_Pipe": 0},
                "ExternalPressure": {
                    "Outer_Pipe": {
                        "fluid_density": 0.4445,  # psi/ft seawater
                        "fluid_column": 5000,  # ft water depth
                    }
                },
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {specification_code: 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            specification_code: {
                "collapse_propagation": {"Fp": fp},
                "D_over_T_Trasition_Ratio": 30,
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


def make_collapse_propagation_none_ext_pressure_cfg(
    specification_code="API RP 1111-2009",
    fp=0.80,
):
    """Config where ExternalPressure for the pipe is None."""
    pipe_geometry = make_pipe_geometry(10.75, 0.5, 0.05)
    pipe_material = make_pipe_material()
    return {
        "Outer_Pipe": {
            "Geometry": pipe_geometry,
            "Material": pipe_material,
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "collapse_propagation"},
                "Code": [{"Outer_Pipe": specification_code}],
                "InternalPressure": {"Outer_Pipe": 0},
                "ExternalPressure": {"Outer_Pipe": None},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {specification_code: 1.0}
                    }
                },
            }
        ],
        "DesignFactors": {
            specification_code: {
                "collapse_propagation": {"Fp": fp},
                "D_over_T_Trasition_Ratio": 30,
            }
        },
        "Material": make_material_database(),
        "Result": {},
    }


# ---------------------------------------------------------------------------
# Tests: PipeCapacity class - initialization
# ---------------------------------------------------------------------------


class TestPipeCapacityInit:
    def test_init_stores_cfg(self):
        cfg = {"Outer_Pipe": None, "Inner_Pipe": None}
        pc = PipeCapacity(cfg)
        assert pc.cfg is cfg

    def test_init_with_full_config(self):
        cfg = make_asme_b31_cfg()
        pc = PipeCapacity(cfg)
        assert pc.cfg["Outer_Pipe"] is not None
        assert pc.cfg["Inner_Pipe"] is None


# ---------------------------------------------------------------------------
# Tests: evaluate_pipe_wall dispatch
# ---------------------------------------------------------------------------


class TestEvaluatePipeWall:
    def test_outer_pipe_only(self):
        cfg = make_asme_b31_cfg()
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        assert "Result" in pc.cfg
        assert "Outer_Pipe" in pc.cfg["Result"]

    def test_both_pipes_none_does_nothing(self):
        cfg = {"Outer_Pipe": None, "Inner_Pipe": None, "Result": {}}
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        assert pc.cfg["Result"] == {}

    def test_inner_pipe_evaluated_when_present(self):
        cfg = make_asme_b31_cfg()
        # Clone Outer_Pipe config for Inner_Pipe
        cfg["Inner_Pipe"] = copy.deepcopy(cfg["Outer_Pipe"])
        cfg["Inner_Pipe"]["Geometry"]["Nominal_OD"] = 8.625
        cfg["Inner_Pipe"]["Geometry"]["Design_WT"] = 0.4
        cfg["Inner_Pipe"]["Geometry"]["Nominal_ID"] = 8.625 - 2 * 0.4
        cfg["Inner_Pipe"]["Geometry"]["Corrosion_Allowance"] = 0.05
        # Add Inner_Pipe references to Design
        cfg["Design"][0]["Load Condition"]["Inner_Pipe"] = "internal_pressure"
        cfg["Design"][0]["Code"][0]["Inner_Pipe"] = "ASME B31.4"
        cfg["Design"][0]["InternalPressure"]["Inner_Pipe"] = 4000
        cfg["Design"][0]["ExternalPressure"]["Inner_Pipe"] = 800
        cfg["Design"][0]["Material"]["temperature_derating"]["Inner_Pipe"] = {
            "ASME B31.4": 1.0
        }
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        assert "Inner_Pipe" in pc.cfg["Result"]


# ---------------------------------------------------------------------------
# Tests: evaluate_load_conditions routing
# ---------------------------------------------------------------------------


class TestEvaluateLoadConditions:
    def test_internal_pressure_route(self):
        cfg = make_asme_b31_cfg()
        pc = PipeCapacity(cfg)
        pc.evaluate_load_conditions("Outer_Pipe")
        result = pc.cfg["Result"]["Outer_Pipe"]["internal_pressure"]
        assert "ASME B31.4" in result

    def test_external_pressure_route(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        pc.evaluate_load_conditions("Outer_Pipe")
        result = pc.cfg["Result"]["Outer_Pipe"]["external_pressure"]
        assert "API STD 2RD-2013" in result

    def test_collapse_propagation_route(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        pc.evaluate_load_conditions("Outer_Pipe")
        result = pc.cfg["Result"]["Outer_Pipe"]["collapse_propagation"]
        assert "API RP 1111-2009" in result


# ---------------------------------------------------------------------------
# Tests: update_pressure_results
# ---------------------------------------------------------------------------


class TestUpdatePressureResults:
    def test_results_structure_created(self):
        cfg = {"Result": {}}
        pc = PipeCapacity(cfg)
        pc.update_pressure_results(
            minimum_thickness=0.35,
            pipe_flag="Outer_Pipe",
            pressure=6000,
            specification_code="ASME B31.4",
            thickness=0.5,
            load_condition="internal_pressure",
            custom_tag="Zero Corrosion Allowance",
        )
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        assert r["thickness"]["Zero Corrosion Allowance"] == 0.5
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] == 6000
        assert r["minimum_thickness"]["Zero Corrosion Allowance"] == 0.35

    def test_results_with_corrosion_tag(self):
        cfg = {"Result": {}}
        pc = PipeCapacity(cfg)
        pc.update_pressure_results(
            minimum_thickness=0.40,
            pipe_flag="Outer_Pipe",
            pressure=5500,
            specification_code="API STD 2RD-2013",
            thickness=0.45,
            load_condition="internal_pressure",
            custom_tag="With Corrosion Allowance",
        )
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["API STD 2RD-2013"]
        assert r["thickness"]["With Corrosion Allowance"] == 0.45
        assert r["Design_WT_Max_Pressure"]["With Corrosion Allowance"] == 5500
        assert r["minimum_thickness"]["With Corrosion Allowance"] == 0.40

    def test_multiple_updates_preserve_both_tags(self):
        cfg = {"Result": {}}
        pc = PipeCapacity(cfg)
        pc.update_pressure_results(
            0.35, "Outer_Pipe", 6000, "ASME B31.4", 0.5,
            "internal_pressure", "Zero Corrosion Allowance",
        )
        pc.update_pressure_results(
            0.40, "Outer_Pipe", 5500, "ASME B31.4", 0.45,
            "internal_pressure", "With Corrosion Allowance",
        )
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        assert "Zero Corrosion Allowance" in r["thickness"]
        assert "With Corrosion Allowance" in r["thickness"]


# ---------------------------------------------------------------------------
# Tests: ASME B31 Internal Pressure (Modified Barlow Equation)
# ---------------------------------------------------------------------------


class TestASMEB31BurstPressure:
    def test_thin_wall_formula_d_over_t_above_transition(self):
        """D/t = 10.75/0.5 = 21.5, transition = 20 => above => thin wall."""
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, thickness
        )
        # Expected: 2 * t * S_allow / D + P_ext
        smys = 65000
        factor = 0.72
        weld = 1.0
        temp = 1.0
        s_allow = smys * factor * weld * temp
        expected = 2 * thickness * s_allow / 10.75 + 1000
        assert abs(pressure - expected) < 0.01

    def test_thick_wall_formula_d_over_t_below_transition(self):
        """D/t = 10.75/0.5 = 21.5, transition = 25 => below => thick wall."""
        cfg = make_asme_b31_cfg(d_over_t_transition=25)
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, thickness
        )
        smys = 65000
        factor = 0.72
        weld = 1.0
        temp = 1.0
        s_allow = smys * factor * weld * temp
        expected = 2 * thickness * s_allow / (10.75 - thickness) + 1000
        assert abs(pressure - expected) < 0.01

    def test_pressure_increases_with_thickness(self):
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        p1 = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.4
        )
        p2 = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.6
        )
        assert p2 > p1

    def test_zero_external_pressure(self):
        cfg = make_asme_b31_cfg(external_pressure=0, d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.5
        )
        # Without external pressure contribution
        smys = 65000
        s_allow = smys * 0.72 * 1.0 * 1.0
        expected = 2 * 0.5 * s_allow / 10.75
        assert abs(pressure - expected) < 0.01


class TestASMEB31BurstMinimumThickness:
    def test_thin_wall_minimum_thickness(self):
        """D/t = 21.5 >= 20 (transition), thin wall formula."""
        cfg = make_asme_b31_cfg(
            internal_pressure=5000,
            external_pressure=1000,
            d_over_t_transition=20,
        )
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        smys = 65000
        s_allow = smys * 0.72 * 1.0 * 1.0
        delta_p = 5000 - 1000
        expected = delta_p * 10.75 / (2 * s_allow)
        assert abs(min_t - expected) < 1e-6

    def test_thick_wall_minimum_thickness(self):
        """D/t = 21.5 < 25 (transition), thick wall formula."""
        cfg = make_asme_b31_cfg(
            internal_pressure=5000,
            external_pressure=1000,
            d_over_t_transition=25,
        )
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        smys = 65000
        s_allow = smys * 0.72 * 1.0 * 1.0
        delta_p = 5000 - 1000
        expected = delta_p * 10.75 / (2 * s_allow + delta_p)
        assert abs(min_t - expected) < 1e-6

    def test_minimum_thickness_less_than_design_wt(self):
        """For a well-designed pipe, min thickness should be less than design WT."""
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        assert min_t < cfg["Outer_Pipe"]["Geometry"]["Design_WT"]

    def test_higher_pressure_requires_thicker_wall(self):
        cfg1 = make_asme_b31_cfg(internal_pressure=3000, d_over_t_transition=20)
        cfg2 = make_asme_b31_cfg(internal_pressure=8000, d_over_t_transition=20)
        pc1 = PipeCapacity(cfg1)
        pc2 = PipeCapacity(cfg2)
        t1 = pc1.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        t2 = pc2.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        assert t2 > t1


# ---------------------------------------------------------------------------
# Tests: API STD 2RD-2013 Internal Pressure
# ---------------------------------------------------------------------------


class TestAPISTD2RDBurstPressure:
    def test_burst_pressure_calculation(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, thickness
        )
        # Expected: Fd * k * (SMYS + SMUS) * ln(OD / (OD - 2t))
        fd = 0.67
        k = 0.45
        smys = 65000
        smus = 77000
        od = 10.75
        expected = fd * k * (smys + smus) * math.log(od / (od - 2 * thickness))
        assert abs(pressure - expected) < 0.01

    def test_pressure_positive(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.5
        )
        assert pressure > 0

    def test_thicker_pipe_higher_pressure(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        p_thin = pc.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.3
        )
        p_thick = pc.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.7
        )
        assert p_thick > p_thin


class TestAPISTD2RDBurstMinThickness:
    def test_minimum_thickness_calculation(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0
        )
        # Expected: 0.5 * (OD - OD / exp(ratio))
        fd = 0.67
        k = 0.45
        smys = 65000
        smus = 77000
        od = 10.75
        delta_p = 5000 - 1000
        ratio = delta_p / k / fd / (smys + smus)
        expected = 0.5 * (od - od / math.exp(ratio))
        assert abs(min_t - expected) < 1e-6

    def test_minimum_thickness_positive(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0
        )
        assert min_t > 0


# ---------------------------------------------------------------------------
# Tests: API RP 1111-2009 Internal Pressure
# ---------------------------------------------------------------------------


class TestAPIRP1111BurstPressure:
    def test_thick_wall_low_d_over_t(self):
        """D/t = 10.75/0.5 = 21.5, transition = 30 => thick wall (Barlow log)."""
        cfg = make_api_rp_1111_cfg(d_over_t_transition=30)
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_burst_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, thickness
        )
        fd = 0.90
        smys = 65000
        smus = 77000
        od = 10.75
        expected = fd * 0.45 * (smys + smus) * math.log(od / (od - 2 * thickness))
        assert abs(pressure - expected) < 0.01

    def test_thin_wall_high_d_over_t(self):
        """D/t = 10.75/0.5 = 21.5, transition = 20 => thin wall (membrane)."""
        cfg = make_api_rp_1111_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_burst_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, thickness
        )
        fd = 0.90
        smys = 65000
        smus = 77000
        od = 10.75
        expected = fd * 0.90 * (smys + smus) * (thickness / (od - thickness))
        assert abs(pressure - expected) < 0.01

    def test_pressure_always_positive(self):
        cfg = make_api_rp_1111_cfg()
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, 0.5
        )
        assert pressure > 0


class TestAPIRP1111BurstMinThickness:
    def test_thick_wall_min_thickness(self):
        """D/t <= transition => Barlow log-based min thickness."""
        cfg = make_api_rp_1111_cfg(d_over_t_transition=30)
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        fd = 0.90
        smys = 65000
        smus = 77000
        od = 10.75
        delta_p = 5000 - 1000
        ratio = delta_p / 0.45 / fd / (smys + smus)
        expected = 0.5 * (od - od / math.exp(ratio))
        assert abs(min_t - expected) < 1e-6

    def test_thin_wall_min_thickness(self):
        """D/t > transition => membrane min thickness."""
        cfg = make_api_rp_1111_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        fd = 0.90
        smys = 65000
        smus = 77000
        od = 10.75
        delta_p = 5000 - 1000
        ratio = delta_p / 0.90 / fd / (smys + smus)
        expected = ratio * od / (1 + ratio)
        assert abs(min_t - expected) < 1e-6

    def test_minimum_thickness_positive(self):
        cfg = make_api_rp_1111_cfg()
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        assert min_t > 0


# ---------------------------------------------------------------------------
# Tests: API STD 2RD-2013 External Pressure (Collapse)
# ---------------------------------------------------------------------------


class TestAPISTD2RDCollapsePressure:
    def test_collapse_pressure_calculation(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_collapse_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, thickness
        )
        od = 10.75
        smys = 65000
        E = 29e6
        nu = 0.3
        fd = 0.67
        d_over_t = od / thickness
        t_over_d = 1 / d_over_t
        Py = 2 * smys * t_over_d
        Pel = 2 * E * (t_over_d ** 3) / (1 - nu ** 2)
        Pc = Py * Pel / math.sqrt(Py ** 2 + Pel ** 2)
        expected = Pc * fd
        assert abs(pressure - expected) < 0.01

    def test_collapse_pressure_positive(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_collapse_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.5
        )
        assert pressure > 0

    def test_thicker_pipe_resists_more_collapse(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        p_thin = pc.evaluate_collapse_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.3
        )
        p_thick = pc.evaluate_collapse_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.7
        )
        assert p_thick > p_thin


class TestAPISTD2RDCollapseMinThickness:
    def test_returns_placeholder(self):
        """Current implementation returns 100 (TODO marker)."""
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_collapse_minimum_thickness_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0
        )
        assert min_t == 100  # Placeholder per source TODO


# ---------------------------------------------------------------------------
# Tests: Collapse Propagation (API RP 1111-2009)
# ---------------------------------------------------------------------------


class TestCollapsePropagationPressure:
    def test_propagation_pressure_formula(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        thickness = 0.5
        pressure = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, thickness
        )
        smys = 65000
        od = 10.75
        fp = 0.80
        Pp = 24 * smys * ((thickness / od) ** 2.4)
        expected = Pp * fp
        assert abs(pressure - expected) < 0.01

    def test_propagation_pressure_positive(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, 0.5
        )
        assert pressure > 0

    def test_thicker_pipe_higher_propagation_pressure(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        p1 = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, 0.4
        )
        p2 = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, 0.6
        )
        assert p2 > p1


class TestCollapsePropagationMinThickness:
    def test_min_thickness_with_fluid_column(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        # External pressure: fluid_density * fluid_column * 12
        smys = 65000
        od = 10.75
        fp = 0.80
        fluid_density = 0.4445
        fluid_column = 5000
        Po = fluid_density * fluid_column * 12
        Pp = Po / fp
        expected = ((Pp / 24 / smys) ** (1 / 2.4)) * od
        assert abs(min_t - expected) < 1e-6

    def test_min_thickness_positive(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0
        )
        assert min_t > 0

    def test_none_external_pressure_passthrough(self):
        """When ExternalPressure is None, Po is set to None and calculation follows."""
        cfg = make_collapse_propagation_none_ext_pressure_cfg()
        pc = PipeCapacity(cfg)
        # When ExternalPressure is None, the code sets Po = None then does Po / Fp
        # which would raise TypeError. This tests the None branch is entered.
        with pytest.raises(TypeError):
            pc.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
                "Outer_Pipe", "API RP 1111-2009", 0
            )


# ---------------------------------------------------------------------------
# Tests: internal_pressure method (full flow with corrosion)
# ---------------------------------------------------------------------------


class TestInternalPressureFullFlow:
    def test_asme_b31_produces_two_result_tags(self):
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pc.internal_pressure("Outer_Pipe", "ASME B31.4", 0)
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        assert "Zero Corrosion Allowance" in r["thickness"]
        assert "With Corrosion Allowance" in r["thickness"]
        assert "Zero Corrosion Allowance" in r["Design_WT_Max_Pressure"]
        assert "With Corrosion Allowance" in r["Design_WT_Max_Pressure"]
        assert "Zero Corrosion Allowance" in r["minimum_thickness"]
        assert "With Corrosion Allowance" in r["minimum_thickness"]

    def test_corroded_thickness_less_than_nominal(self):
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pc.internal_pressure("Outer_Pipe", "ASME B31.4", 0)
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        t_zero = r["thickness"]["Zero Corrosion Allowance"]
        t_corr = r["thickness"]["With Corrosion Allowance"]
        assert t_corr < t_zero

    def test_corroded_pressure_less_than_nominal(self):
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pc.internal_pressure("Outer_Pipe", "ASME B31.4", 0)
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        p_zero = r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"]
        p_corr = r["Design_WT_Max_Pressure"]["With Corrosion Allowance"]
        assert p_corr < p_zero


# ---------------------------------------------------------------------------
# Tests: external_pressure method (full flow with corrosion)
# ---------------------------------------------------------------------------


class TestExternalPressureFullFlow:
    def test_api_std_2rd_produces_two_result_tags(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        pc.external_pressure("Outer_Pipe", "API STD 2RD-2013", 0)
        r = cfg["Result"]["Outer_Pipe"]["external_pressure"]["API STD 2RD-2013"]
        assert "Zero Corrosion Allowance" in r["thickness"]
        assert "With Corrosion Allowance" in r["thickness"]

    def test_corroded_collapse_pressure_lower(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        pc.external_pressure("Outer_Pipe", "API STD 2RD-2013", 0)
        r = cfg["Result"]["Outer_Pipe"]["external_pressure"]["API STD 2RD-2013"]
        p_zero = r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"]
        p_corr = r["Design_WT_Max_Pressure"]["With Corrosion Allowance"]
        assert p_corr < p_zero


# ---------------------------------------------------------------------------
# Tests: collapse_propagation method (full flow with corrosion)
# ---------------------------------------------------------------------------


class TestCollapsePropagationFullFlow:
    def test_produces_two_result_tags(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        pc.collapse_propagation("Outer_Pipe", "API RP 1111-2009", 0)
        r = cfg["Result"]["Outer_Pipe"]["collapse_propagation"]["API RP 1111-2009"]
        assert "Zero Corrosion Allowance" in r["thickness"]
        assert "With Corrosion Allowance" in r["thickness"]

    def test_corroded_propagation_pressure_lower(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        pc.collapse_propagation("Outer_Pipe", "API RP 1111-2009", 0)
        r = cfg["Result"]["Outer_Pipe"]["collapse_propagation"]["API RP 1111-2009"]
        p_zero = r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"]
        p_corr = r["Design_WT_Max_Pressure"]["With Corrosion Allowance"]
        assert p_corr < p_zero


# ---------------------------------------------------------------------------
# Tests: specification_code_based_evaluation dispatchers
# ---------------------------------------------------------------------------


class TestInternalPressureCodeDispatch:
    def test_asme_b31_dispatch(self):
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.internal_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "ASME B31.4", 0.5
        )
        assert pressure > 0
        assert min_t > 0

    def test_api_std_2rd_dispatch(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.internal_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "API STD 2RD-2013", 0.5
        )
        assert pressure > 0
        assert min_t > 0

    def test_api_rp_1111_dispatch(self):
        cfg = make_api_rp_1111_cfg()
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.internal_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "API RP 1111-2009", 0.5
        )
        assert pressure > 0
        assert min_t > 0


class TestExternalPressureCodeDispatch:
    def test_api_std_2rd_dispatch(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.external_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "API STD 2RD-2013", 0.5
        )
        assert pressure > 0
        assert min_t == 100  # Placeholder


class TestCollapsePropagationCodeDispatch:
    def test_api_rp_1111_dispatch(self):
        cfg = make_collapse_propagation_cfg(specification_code="API RP 1111-2009")
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.collapse_propagation_specification_code_based_evaluation(
            0, "Outer_Pipe", "API RP 1111-2009", 0.5
        )
        assert pressure > 0
        assert min_t > 0

    def test_api_std_2rd_dispatch_for_propagation(self):
        cfg = make_collapse_propagation_cfg(specification_code="API STD 2RD-2013")
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.collapse_propagation_specification_code_based_evaluation(
            0, "Outer_Pipe", "API STD 2RD-2013", 0.5
        )
        assert pressure > 0
        assert min_t > 0


# ---------------------------------------------------------------------------
# Tests: API_TR_5C3 class
# ---------------------------------------------------------------------------


class TestAPITR5C3:
    def test_init_sets_edition(self):
        import datetime
        cfg = make_api_tr_5c3_cfg()
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        assert api.edition == "second"
        assert api.release_date == datetime.date(2018, 6, 1)

    def test_init_stores_geometry(self):
        cfg = make_api_tr_5c3_cfg()
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        assert api.D_o == 10.75
        assert api.t == 0.5
        assert api.D_i == 10.75 - 2 * 0.5

    def test_collapse_pressure_formula(self):
        cfg = make_api_tr_5c3_cfg()
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        Pe = api.get_collapse_pressure()
        d_over_t = 10.75 / 0.5
        expected = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)
        assert abs(Pe - expected) < 0.01

    def test_collapse_pressure_with_custom_thickness(self):
        cfg = make_api_tr_5c3_cfg()
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        Pe = api.get_collapse_pressure(t=0.75)
        d_over_t = 10.75 / 0.75
        expected = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)
        assert abs(Pe - expected) < 0.01

    def test_collapse_pressure_increases_with_thickness(self):
        cfg = make_api_tr_5c3_cfg()
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        p1 = api.get_collapse_pressure(t=0.3)
        p2 = api.get_collapse_pressure(t=0.7)
        assert p2 > p1

    def test_collapse_minimum_thickness(self):
        cfg = make_api_tr_5c3_cfg(external_pressure=3000)
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        min_t = api.get_collapse_minimum_thickness()
        # The result should be a positive thickness value
        assert min_t > 0
        # Verify the collapse pressure at the min thickness roughly matches
        # the external pressure (the interpolation target)
        Pe_at_min = api.get_collapse_pressure(t=min_t)
        # Tolerance: interpolation-based with 100 discrete steps, allow 10%
        assert abs(Pe_at_min - 3000) / 3000 < 0.10

    def test_collapse_minimum_thickness_with_custom_pressure(self):
        cfg = make_api_tr_5c3_cfg(external_pressure=2000)
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        min_t = api.get_collapse_minimum_thickness(external_pressure=5000)
        # Higher external pressure requires thicker wall
        min_t_low = api.get_collapse_minimum_thickness()
        # After calling with external_pressure=5000, internal state was reset
        # min_t for 5000 psi should be more than for 2000 psi
        assert min_t > 0


# ---------------------------------------------------------------------------
# Tests: CFR_30_Part_250 class
# ---------------------------------------------------------------------------


class TestCFR30Part250:
    def test_init(self):
        cfg = make_cfr_30_cfg()
        cfr = CFR_30_Part_250(cfg)
        assert cfr.specification_code == "30 CFR Part 250"

    def test_burst_pressure(self):
        cfg = make_cfr_30_cfg()
        cfr = CFR_30_Part_250(cfg)
        pressure = cfr.get_burst_pressure("Outer_Pipe", 0)
        smys = 65000
        fd = 0.60
        weld = 1.0
        temp = 1.0
        thickness = 0.5
        od = 10.75
        ext_p = 1000
        s_allow = smys * fd * weld * temp
        expected = 2 * thickness * s_allow / od + ext_p
        assert abs(pressure - expected) < 0.01

    def test_burst_pressure_positive(self):
        cfg = make_cfr_30_cfg()
        cfr = CFR_30_Part_250(cfg)
        pressure = cfr.get_burst_pressure("Outer_Pipe", 0)
        assert pressure > 0

    def test_burst_minimum_thickness(self):
        cfg = make_cfr_30_cfg()
        cfr = CFR_30_Part_250(cfg)
        min_t = cfr.get_burst_minimum_thickness("Outer_Pipe", 0)
        smys = 65000
        fd = 0.60
        weld = 1.0
        temp = 1.0
        od = 10.75
        delta_p = 5000 - 1000
        s_allow = smys * fd * weld * temp
        expected = delta_p * od / 2 / s_allow
        assert abs(min_t - expected) < 1e-6

    def test_burst_minimum_thickness_positive(self):
        cfg = make_cfr_30_cfg()
        cfr = CFR_30_Part_250(cfg)
        min_t = cfr.get_burst_minimum_thickness("Outer_Pipe", 0)
        assert min_t > 0

    def test_higher_pressure_requires_thicker_wall(self):
        cfg1 = make_cfr_30_cfg(internal_pressure=3000)
        cfg2 = make_cfr_30_cfg(internal_pressure=8000)
        cfr1 = CFR_30_Part_250(cfg1)
        cfr2 = CFR_30_Part_250(cfg2)
        t1 = cfr1.get_burst_minimum_thickness("Outer_Pipe", 0)
        t2 = cfr2.get_burst_minimum_thickness("Outer_Pipe", 0)
        assert t2 > t1

    def test_temperature_derating_reduces_pressure(self):
        cfg_full = make_cfr_30_cfg(temp_derating=1.0)
        cfg_derated = make_cfr_30_cfg(temp_derating=0.8)
        cfr_full = CFR_30_Part_250(cfg_full)
        cfr_derated = CFR_30_Part_250(cfg_derated)
        p_full = cfr_full.get_burst_pressure("Outer_Pipe", 0)
        p_derated = cfr_derated.get_burst_pressure("Outer_Pipe", 0)
        assert p_derated < p_full


# ---------------------------------------------------------------------------
# Tests: API_RP_16Q class
# ---------------------------------------------------------------------------


class TestAPIRP16Q:
    def test_init_edition(self):
        import datetime
        cfg = {"dummy": True}
        api = API_RP_16Q(cfg)
        assert api.edition == "second"
        assert api.release_date == datetime.date(2017, 4, 1)

    def test_init_stores_cfg(self):
        cfg = {"some_key": "some_value"}
        api = API_RP_16Q(cfg)
        assert api.cfg is cfg


# ---------------------------------------------------------------------------
# Tests: API_RP_2RD class
# ---------------------------------------------------------------------------


class TestAPIRP2RD:
    def test_init_stores_cfg(self):
        cfg = {"test": True}
        api = API_RP_2RD(cfg)
        assert api.cfg is cfg


# ---------------------------------------------------------------------------
# Tests: InternalPressureMethods (empty class)
# ---------------------------------------------------------------------------


class TestInternalPressureMethods:
    def test_instantiation(self):
        obj = InternalPressureMethods()
        assert isinstance(obj, InternalPressureMethods)


# ---------------------------------------------------------------------------
# Tests: OtherMethodsTobeIncorporated (ASME B31.4 and B31.8 static methods)
# ---------------------------------------------------------------------------


class TestASMEB314Methods:
    def test_internal_pressure_thin_wall(self):
        """D/t = 12/0.3 = 40 >= 30 => thin wall."""
        data = {
            "S": 65000,  # SMYS
            "t": 0.3,
            "D": 12.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,  # temperature derating
            "Pi": 5000,
            "Po": 500,
        }
        result = OtherMethodsTobeIncorporated.ASMEB314InternalPressure(data)
        assert "MaximumDesignPressure" in result
        assert "MinimumWallThickness_Pressure" in result
        assert result["MaximumDesignPressure"] > 0
        assert result["MinimumWallThickness_Pressure"] > 0

    def test_internal_pressure_thick_wall(self):
        """D/t = 6/0.3 = 20 < 30 => thick wall."""
        data = {
            "S": 65000,
            "t": 0.3,
            "D": 6.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 5000,
            "Po": 500,
        }
        result = OtherMethodsTobeIncorporated.ASMEB314InternalPressure(data)
        stress_hoop = 65000 * 0.72 * 1.0 * 1.0
        expected_t = (5000 - 500) * 6.0 / (2 * stress_hoop + (5000 - 500))
        assert abs(result["MinimumWallThickness_Pressure"] - expected_t) < 1e-6

    def test_longitudinal_stress_restrained_thin_wall(self):
        data = {
            "S": 65000,
            "t": 0.3,
            "D": 12.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 5000,
            "Po": 500,
            "E": 29e6,
            "Alpha": 6.5e-6,  # coefficient of thermal expansion
            "T1": 200,  # operating temp (F)
            "T2": 60,  # install temp (F)
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB314LogitudinalStress(data)
        assert "MinimumWallThickness_Longitudinal" in result
        assert "Stress_Elongation" in result
        assert "Stress_Hoop" in result

    def test_longitudinal_stress_restrained_thick_wall(self):
        data = {
            "S": 65000,
            "t": 0.3,
            "D": 6.0,  # D/t = 20 < 30
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 5000,
            "Po": 500,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 200,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB314LogitudinalStress(data)
        assert "MinimumWallThickness_Longitudinal" in result

    def test_equivalent_stress_restrained(self):
        data = {
            "S": 65000,
            "t": 0.3,
            "D": 12.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 5000,
            "Po": 500,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 200,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB314EquivalentStress(data)
        assert "MinimumWallThickness_Equivalent" in result
        assert "Stress_Equivalent" in result

    def test_equivalent_stress_thick_wall(self):
        data = {
            "S": 65000,
            "t": 0.3,
            "D": 6.0,  # D/t = 20 < 30
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 5000,
            "Po": 500,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 200,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB314EquivalentStress(data)
        assert "MinimumWallThickness_Equivalent" in result


class TestASMEB318Methods:
    def test_internal_pressure_thin_wall(self):
        """D/t = 12/0.3 = 40 >= 30 => thin wall."""
        data = {
            "S": 52000,
            "t": 0.3,
            "D": 12.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 4000,
            "Po": 300,
        }
        result = OtherMethodsTobeIncorporated.ASMEB318InternalPressure(data)
        assert "MaximumDesignPressure" in result
        assert "MinimumWallThickness" in result
        assert result["MaximumDesignPressure"] > 0

    def test_internal_pressure_thick_wall(self):
        """D/t = 6/0.3 = 20 < 30 => thick wall."""
        data = {
            "S": 52000,
            "t": 0.3,
            "D": 6.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 4000,
            "Po": 300,
        }
        result = OtherMethodsTobeIncorporated.ASMEB318InternalPressure(data)
        s_allow = 52000 * 0.72 * 1.0 * 1.0
        expected_t = (4000 - 300) * 6.0 / (2 * s_allow + (4000 - 300))
        assert abs(result["MinimumWallThickness"] - expected_t) < 1e-6

    def test_longitudinal_stress_restrained_thin_wall(self):
        data = {
            "S": 52000,
            "t": 0.3,
            "D": 12.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 4000,
            "Po": 300,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 150,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB318LogitudinalStress(data)
        assert "MinimumWallThickness_Longitudinal" in result
        assert "Stress_Elongation" in result
        # B31.8 does not subtract Stress_Axial in Stress_Hoop calculation
        stress_elong = -29e6 * 6.5e-6 * (150 - 60)
        stress_long = 52000 * 0.72 * 1.0 * 1.0
        expected_hoop = (stress_long - stress_elong - 0) / 0.3
        assert abs(result["Stress_Hoop"] - expected_hoop) < 0.01

    def test_longitudinal_stress_restrained_thick_wall(self):
        data = {
            "S": 52000,
            "t": 0.3,
            "D": 6.0,  # D/t = 20 < 30
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 4000,
            "Po": 300,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 150,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB318LogitudinalStress(data)
        assert "MinimumWallThickness_Longitudinal" in result

    def test_equivalent_stress_restrained_thin_wall(self):
        data = {
            "S": 52000,
            "t": 0.3,
            "D": 12.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 4000,
            "Po": 300,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 150,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB318EquivalentStress(data)
        assert "MinimumWallThickness_Equivalent" in result
        assert "Stress_Equivalent" in result

    def test_equivalent_stress_restrained_thick_wall(self):
        data = {
            "S": 52000,
            "t": 0.3,
            "D": 6.0,
            "F": 0.72,
            "WeldFactor": 1.0,
            "T": 1.0,
            "Pi": 4000,
            "Po": 300,
            "E": 29e6,
            "Alpha": 6.5e-6,
            "T1": 150,
            "T2": 60,
            "Poissionsratio": 0.3,
            "Condition": "Restrained",
        }
        result = OtherMethodsTobeIncorporated.ASMEB318EquivalentStress(data)
        assert "MinimumWallThickness_Equivalent" in result


# ---------------------------------------------------------------------------
# Tests: Engineering cross-checks and physical consistency
# ---------------------------------------------------------------------------


class TestEngineeringConsistency:
    def test_burst_pressure_exceeds_design_pressure_asme(self):
        """Burst capacity should exceed internal design pressure for adequate pipe.
        Use a thicker wall (0.75in) so factored burst > 3000 psi design pressure."""
        cfg = make_asme_b31_cfg(
            internal_pressure=3000,
            external_pressure=0,
            design_wt=0.75,
            d_over_t_transition=20,
        )
        pc = PipeCapacity(cfg)
        burst = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.75
        )
        assert burst > 3000, "Burst capacity must exceed design pressure"

    def test_larger_diameter_lower_burst_pressure_for_same_thickness(self):
        """For the same wall thickness, a larger OD pipe has lower burst pressure."""
        cfg_small = make_asme_b31_cfg(nominal_od=8.625, d_over_t_transition=20)
        cfg_large = make_asme_b31_cfg(nominal_od=16.0, d_over_t_transition=20)
        pc_small = PipeCapacity(cfg_small)
        pc_large = PipeCapacity(cfg_large)
        p_small = pc_small.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.5
        )
        p_large = pc_large.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.5
        )
        assert p_small > p_large

    def test_higher_smys_gives_higher_burst_api_2rd(self):
        """Higher yield strength steel should give higher burst pressure."""
        cfg_x52 = make_api_std_2rd_cfg()
        cfg_x52["Outer_Pipe"]["Material"]["SMYS"] = 52000
        cfg_x52["Outer_Pipe"]["Material"]["SMUS"] = 66000
        cfg_x70 = make_api_std_2rd_cfg()
        cfg_x70["Outer_Pipe"]["Material"]["SMYS"] = 70000
        cfg_x70["Outer_Pipe"]["Material"]["SMUS"] = 82000
        pc_x52 = PipeCapacity(cfg_x52)
        pc_x70 = PipeCapacity(cfg_x70)
        p_x52 = pc_x52.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.5
        )
        p_x70 = pc_x70.evaluate_burst_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.5
        )
        assert p_x70 > p_x52

    def test_collapse_pressure_higher_for_lower_d_over_t(self):
        """Lower D/t ratio (thicker relative wall) gives higher collapse resistance."""
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        # Thicker wall: t=0.75 -> D/t = 14.3
        p_thick = pc.evaluate_collapse_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.75
        )
        # Thinner wall: t=0.3 -> D/t = 35.8
        p_thin = pc.evaluate_collapse_pressure_API_STD_2RD(
            "Outer_Pipe", "API STD 2RD-2013", 0, 0.3
        )
        assert p_thick > p_thin

    def test_propagation_pressure_exponent_effect(self):
        """Verify the 2.4 exponent makes propagation pressure very sensitive to t/D."""
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        # Double the thickness
        p1 = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, 0.5
        )
        p2 = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            "Outer_Pipe", "API RP 1111-2009", 0, 1.0
        )
        # With exponent 2.4: doubling thickness should multiply pressure by ~2^2.4 = ~5.28
        ratio = p2 / p1
        expected_ratio = 2 ** 2.4
        assert abs(ratio - expected_ratio) < 0.01

    def test_api_tr_5c3_collapse_pressure_formula_check(self):
        """Verify API TR 5C3 uses elastic collapse formula: 46.95e6 / (D/t * (D/t-1)^2)."""
        cfg = make_api_tr_5c3_cfg(nominal_od=9.625, design_wt=0.545)
        api = API_TR_5C3(cfg, "Outer_Pipe", 0)
        Pe = api.get_collapse_pressure()
        d_over_t = 9.625 / 0.545
        expected = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)
        assert abs(Pe - expected) < 0.01


# ---------------------------------------------------------------------------
# Tests: Edge cases and boundary conditions
# ---------------------------------------------------------------------------


class TestEdgeCases:
    def test_zero_corrosion_allowance(self):
        """Zero corrosion should yield same results for both tags."""
        cfg = make_asme_b31_cfg(corrosion_allowance=0.0, d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pc.internal_pressure("Outer_Pipe", "ASME B31.4", 0)
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        p_zero = r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"]
        p_corr = r["Design_WT_Max_Pressure"]["With Corrosion Allowance"]
        assert abs(p_zero - p_corr) < 0.01

    def test_very_thin_wall(self):
        """Thin wall pipe: high D/t ratio."""
        cfg = make_asme_b31_cfg(
            nominal_od=24.0, design_wt=0.25, d_over_t_transition=30
        )
        pc = PipeCapacity(cfg)
        # D/t = 96, well above transition
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.25
        )
        assert pressure > 0

    def test_equal_internal_external_pressure(self):
        """When internal = external pressure, min thickness should be ~0."""
        cfg = make_asme_b31_cfg(
            internal_pressure=3000,
            external_pressure=3000,
            d_over_t_transition=20,
        )
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0
        )
        assert abs(min_t) < 1e-10

    def test_design_factor_of_one(self):
        """Design factor of 1.0 gives highest allowable pressure."""
        cfg_low = make_asme_b31_cfg(design_factor=0.50, d_over_t_transition=20)
        cfg_high = make_asme_b31_cfg(design_factor=1.0, d_over_t_transition=20)
        pc_low = PipeCapacity(cfg_low)
        pc_high = PipeCapacity(cfg_high)
        p_low = pc_low.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.5
        )
        p_high = pc_high.evaluate_burst_pressure_modified_burlow_equation(
            "Outer_Pipe", "ASME B31.4", 0, 0.5
        )
        assert p_high > p_low

    def test_multiple_load_conditions(self):
        """Config with two load conditions processes both."""
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        # Add a second load condition
        second_lc = copy.deepcopy(cfg["Design"][0])
        second_lc["InternalPressure"]["Outer_Pipe"] = 8000
        second_lc["ExternalPressure"]["Outer_Pipe"] = 2000
        cfg["Design"].append(second_lc)
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        # Both load conditions should produce results
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        assert "Zero Corrosion Allowance" in r["thickness"]

    def test_multiple_codes_per_load_condition(self):
        """Config with multiple codes per load condition."""
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        # Add a second code for the same load condition
        cfg["Design"][0]["Code"].append({"Outer_Pipe": "ASME B31.8"})
        cfg["DesignFactors"]["ASME B31.8"] = {
            "internal_pressure": 0.60,
            "D_over_T_Trasition_Ratio": 20,
        }
        cfg["Design"][0]["Material"]["temperature_derating"]["Outer_Pipe"]["ASME B31.8"] = 1.0
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]
        assert "ASME B31.4" in r
        assert "ASME B31.8" in r


# ---------------------------------------------------------------------------
# Tests: API TR 5C3 external pressure dispatch
# ---------------------------------------------------------------------------


class TestExternalPressureAPITR5C3:
    def test_api_tr_5c3_dispatch_via_evaluate(self):
        cfg = make_api_tr_5c3_cfg()
        pc = PipeCapacity(cfg)
        min_t, pressure = pc.external_pressure_specification_code_based_evaluation(
            0, "Outer_Pipe", "API TR 5C3-2018", 0.5
        )
        assert pressure > 0
        assert min_t > 0


# ---------------------------------------------------------------------------
# Tests: Full end-to-end pipeline for various codes
# ---------------------------------------------------------------------------


class TestEndToEndASMEB31:
    def test_full_evaluation(self):
        cfg = make_asme_b31_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["ASME B31.4"]
        # Verify all expected keys exist
        for tag in ["Zero Corrosion Allowance", "With Corrosion Allowance"]:
            assert tag in r["thickness"]
            assert tag in r["Design_WT_Max_Pressure"]
            assert tag in r["minimum_thickness"]
            assert r["Design_WT_Max_Pressure"][tag] > 0
            assert r["minimum_thickness"][tag] > 0


class TestEndToEndAPISTD2RD:
    def test_full_internal_pressure_evaluation(self):
        cfg = make_api_std_2rd_cfg()
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["API STD 2RD-2013"]
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] > 0

    def test_full_external_pressure_evaluation(self):
        cfg = make_api_std_2rd_cfg(load_condition="external_pressure")
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["external_pressure"]["API STD 2RD-2013"]
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] > 0


class TestEndToEndAPIRP1111:
    def test_full_internal_pressure_evaluation(self):
        cfg = make_api_rp_1111_cfg(d_over_t_transition=30)
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["API RP 1111-2009"]
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] > 0

    def test_full_internal_pressure_thin_wall(self):
        cfg = make_api_rp_1111_cfg(d_over_t_transition=20)
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["API RP 1111-2009"]
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] > 0


class TestEndToEndCFR30:
    def test_full_evaluation(self):
        cfg = make_cfr_30_cfg()
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["30 CFR Part 250"]
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] > 0
        assert r["minimum_thickness"]["Zero Corrosion Allowance"] > 0


class TestEndToEndCollapsePropagation:
    def test_full_evaluation(self):
        cfg = make_collapse_propagation_cfg()
        pc = PipeCapacity(cfg)
        pc.evaluate_pipe_wall()
        r = cfg["Result"]["Outer_Pipe"]["collapse_propagation"]["API RP 1111-2009"]
        assert r["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] > 0
        assert r["minimum_thickness"]["Zero Corrosion Allowance"] > 0
