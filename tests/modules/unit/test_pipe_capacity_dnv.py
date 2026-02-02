from digitalmodel.pipe_capacity.custom.PipeCapacity import DNVWallThickness


def _make_cfg(
    p_internal=1440.0,
    p_external=0.0,
    min_internal=0.0,
    spec_code="DNV-OS-F101-2010",
    design_factors=None,
):
    return {
        "basename": "pipe_capacity",
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
                "InternalPressure": {"Outer_Pipe": p_internal},
                "ExternalPressure": {"Outer_Pipe": p_external},
                "MinimumInternalPressure": {"Outer_Pipe": min_internal},
                "Material": {
                    "temperature_derating": {"Outer_Pipe": {spec_code: 1.0}}
                },
                "Code": [{"Outer_Pipe": spec_code}],
            }
        ],
        "DesignFactors": design_factors or {},
    }


def test_dnv_burst_minimum_thickness_increases_with_pressure():
    cfg_low = _make_cfg(p_internal=1000.0)
    dnv_low = DNVWallThickness(cfg_low, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_low = dnv_low.get_burst_minimum_thickness()

    cfg_high = _make_cfg(p_internal=2000.0)
    dnv_high = DNVWallThickness(cfg_high, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_high = dnv_high.get_burst_minimum_thickness()

    assert t_high > t_low


def test_dnv_collapse_minimum_thickness_increases_with_external_pressure():
    cfg_low = _make_cfg(p_external=500.0)
    cfg_low["Design"][0]["Load Condition"]["Outer_Pipe"] = "external_pressure"
    dnv_low = DNVWallThickness(cfg_low, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_low = dnv_low.get_collapse_minimum_thickness()

    cfg_high = _make_cfg(p_external=1000.0)
    cfg_high["Design"][0]["Load Condition"]["Outer_Pipe"] = "external_pressure"
    dnv_high = DNVWallThickness(cfg_high, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_high = dnv_high.get_collapse_minimum_thickness()

    assert t_high > t_low


def test_dnv_burst_thickness_respects_design_factors():
    cfg_base = _make_cfg(p_internal=1500.0)
    dnv_base = DNVWallThickness(cfg_base, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_base = dnv_base.get_burst_minimum_thickness()

    cfg_override = _make_cfg(
        p_internal=1500.0,
        design_factors={
            "DNV-OS-F101-2010": {
                "internal_pressure": {"gamma_sc": 1.30, "gamma_m": 1.15}
            }
        },
    )
    dnv_override = DNVWallThickness(
        cfg_override, "Outer_Pipe", 0, "DNV-OS-F101-2010"
    )
    t_override = dnv_override.get_burst_minimum_thickness()

    assert t_override > t_base


def test_dnv_external_pressure_dict_input():
    cfg = _make_cfg(
        p_external={"fluid_density": 0.037, "fluid_column": 300.0},
    )
    cfg["Design"][0]["Load Condition"]["Outer_Pipe"] = "external_pressure"
    dnv = DNVWallThickness(cfg, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_min = dnv.get_collapse_minimum_thickness()

    assert t_min > 0.0


def test_dnv_propagation_thickness_increases_with_external_pressure():
    cfg_low = _make_cfg(p_external=400.0, min_internal=0.0)
    cfg_low["Design"][0]["Load Condition"]["Outer_Pipe"] = "collapse_propagation"
    dnv_low = DNVWallThickness(cfg_low, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_low = dnv_low.get_propagation_minimum_thickness()

    cfg_high = _make_cfg(p_external=800.0, min_internal=0.0)
    cfg_high["Design"][0]["Load Condition"]["Outer_Pipe"] = "collapse_propagation"
    dnv_high = DNVWallThickness(cfg_high, "Outer_Pipe", 0, "DNV-OS-F101-2010")
    t_high = dnv_high.get_propagation_minimum_thickness()

    assert t_high > t_low
