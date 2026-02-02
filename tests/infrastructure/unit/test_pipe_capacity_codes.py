import pytest

from digitalmodel.pipe_capacity.custom import PipeCapacity as pc_module


def _base_cfg(spec_code):
    return {
        "Outer_Pipe": {
            "Geometry": {
                "Nominal_OD": 12.75,
                "Nominal_ID": 11.75,
                "Design_WT": 0.5,
                "Corrosion_Allowance": 0.0,
            },
            "Material": {
                "SMYS": 65000.0,
                "SMUS": 77000.0,
                "E": 30000000.0,
                "Poissionsratio": 0.3,
                "WeldFactor": {"Seamless": 1.0},
            },
        },
        "Inner_Pipe": None,
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "internal_pressure"},
                "InternalPressure": {"Outer_Pipe": 1000.0},
                "ExternalPressure": {"Outer_Pipe": 100.0},
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {spec_code: 1.0, "30 CFR Part 250": 1.0}
                    }
                },
                "BendingMoment": 0.0,
                "AxialForce": None,
                "EffectiveTension": 0.0,
                "allowable_stress_to_yield_ratio": 0.67,
            }
        ],
        "DesignFactors": {},
    }


def test_asme_b31_burst_pressure_increases_with_thickness():
    spec_code = "ASME B31.4"
    cfg = _base_cfg(spec_code)
    cfg["DesignFactors"][spec_code] = {
        "internal_pressure": 0.72,
        "D_over_T_Trasition_Ratio": 30,
    }

    pc = pc_module.PipeCapacity(cfg)
    p_low = pc.evaluate_burst_pressure_modified_burlow_equation(
        "Outer_Pipe", spec_code, 0, 0.4
    )
    p_high = pc.evaluate_burst_pressure_modified_burlow_equation(
        "Outer_Pipe", spec_code, 0, 0.6
    )

    assert p_high > p_low


def test_asme_b31_min_thickness_increases_with_pressure():
    spec_code = "ASME B31.4"
    cfg = _base_cfg(spec_code)
    cfg["DesignFactors"][spec_code] = {
        "internal_pressure": 0.72,
        "D_over_T_Trasition_Ratio": 30,
    }
    pc = pc_module.PipeCapacity(cfg)

    cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = 800.0
    t_low = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
        "Outer_Pipe", spec_code, 0
    )

    cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = 1600.0
    t_high = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
        "Outer_Pipe", spec_code, 0
    )

    assert t_high > t_low


def test_api_std_2rd_min_thickness_increases_with_pressure():
    spec_code = "API STD 2RD-2013 Section 5"
    cfg = _base_cfg(spec_code)
    cfg["DesignFactors"][spec_code] = {
        "internal_pressure": {"Fd": 0.6, "k": {"API 5L": 0.45}},
        "external_pressure": {"Fd": 0.7},
    }
    pc = pc_module.PipeCapacity(cfg)

    cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = 600.0
    t_low = pc.evaluate_burst_minimum_thickness_API_STD_2RD("Outer_Pipe", spec_code, 0)

    cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = 1200.0
    t_high = pc.evaluate_burst_minimum_thickness_API_STD_2RD("Outer_Pipe", spec_code, 0)

    assert t_high > t_low


def test_api_rp_1111_burst_pressure_increases_with_thickness():
    spec_code = "API RP 1111-2009 Pipelines"
    cfg = _base_cfg(spec_code)
    cfg["DesignFactors"][spec_code] = {
        "internal_pressure": {"Fd": 0.67},
        "D_over_T_Trasition_Ratio": 30,
    }
    pc = pc_module.PipeCapacity(cfg)

    p_low = pc.evaluate_burst_pressure_API_RP_1111("Outer_Pipe", spec_code, 0, 0.4)
    p_high = pc.evaluate_burst_pressure_API_RP_1111("Outer_Pipe", spec_code, 0, 0.6)

    assert p_high > p_low


def test_api_rp_16q_routing(monkeypatch):
    class Dummy16Q:
        def __init__(self, cfg):
            self.cfg = cfg

        def get_burst_pressure(self, pipe_flag, load_condition_index):
            return 123.0

        def get_burst_minimum_thickness(self, pipe_flag, load_condition_index):
            return 0.75

    spec_code = "API RP 16Q-2017"
    cfg = _base_cfg(spec_code)

    monkeypatch.setattr(pc_module, "API_RP_16Q", Dummy16Q)
    pc = pc_module.PipeCapacity(cfg)
    t_min, p = pc.internal_pressure_specification_code_based_evaluation(
        0, "Outer_Pipe", spec_code, 0.5
    )

    assert p == 123.0
    assert t_min == 0.75


def test_cfr_30_part_250_min_thickness_increases_with_pressure():
    spec_code = "30 CFR Part 250"
    cfg = _base_cfg(spec_code)
    cfg["DesignFactors"][spec_code] = {"internal_pressure": {"Fd": 0.6}}
    pc = pc_module.PipeCapacity(cfg)

    cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = 700.0
    t_low, _ = pc.internal_pressure_specification_code_based_evaluation(
        0, "Outer_Pipe", spec_code, 0.5
    )

    cfg["Design"][0]["InternalPressure"]["Outer_Pipe"] = 1400.0
    t_high, _ = pc.internal_pressure_specification_code_based_evaluation(
        0, "Outer_Pipe", spec_code, 0.5
    )

    assert t_high > t_low


def test_api_tr_5c3_collapse_thickness_increases_with_external_pressure():
    spec_code = "API TR 5C3-2018"
    cfg = _base_cfg(spec_code)
    cfg["Design"][0]["Load Condition"]["Outer_Pipe"] = "external_pressure"
    pc = pc_module.PipeCapacity(cfg)

    cfg["Design"][0]["ExternalPressure"]["Outer_Pipe"] = 2000.0
    t_low, _ = pc.external_pressure_specification_code_based_evaluation(
        0, "Outer_Pipe", spec_code, 0.5
    )

    cfg["Design"][0]["ExternalPressure"]["Outer_Pipe"] = 4000.0
    t_high, _ = pc.external_pressure_specification_code_based_evaluation(
        0, "Outer_Pipe", spec_code, 0.5
    )

    assert t_high > t_low
