import logging
import math
import re

from assetutilities.common.update_deep import update_deep_dictionary


def _extract_year_from_code(specification_code):
    match = re.search(r"(19|20)\\d{2}", specification_code)
    if match:
        return int(match.group(0))
    return None


def _resolve_pressure_value(value):
    if value is None:
        return 0.0
    if isinstance(value, dict):
        return float(value.get("fluid_density", 0.0)) * float(
            value.get("fluid_column", 0.0)
        ) * 12.0
    return float(value)


def _is_dnv_f101(specification_code):
    code = specification_code.upper()
    return "F101" in code and "DNV" in code


def _is_dnv_f201(specification_code):
    code = specification_code.upper()
    return "F201" in code and "DNV" in code


class DNVWallThickness:
    def __init__(self, cfg, pipe_flag, load_condition_index, specification_code):
        self.cfg = cfg
        self.pipe_flag = pipe_flag
        self.load_condition_index = load_condition_index
        self.specification_code = specification_code
        self.design_factors = self._resolve_design_factors()

    def _resolve_design_factors(self):
        defaults = {
            "internal_pressure": {
                "gamma_m": 1.15,
                "gamma_sc": 1.046,
                "alpha_u": 0.96,
                "alpha_fab": 1.0,
                "alpha_mpt": 0.75,
                "alpha_spt": 1.0,
            },
            "external_pressure": {
                "gamma_m": 1.15,
                "gamma_sc": 1.046,
                "alpha_fab": 1.0,
                "ovality": 0.005,
            },
            "collapse_propagation": {
                "gamma_m": 1.15,
                "gamma_sc": 1.046,
                "alpha_fab": 1.0,
            },
        }

        design_factors = self.cfg.get("DesignFactors", {})
        year = _extract_year_from_code(self.specification_code)
        year_keys = []
        if year is not None:
            year_keys = [
                f"DNV-OS-F101-{year}",
                f"DNV-ST-F101-{year}",
                f"DNVGL-ST-F101-{year}",
                f"DNV-F101-{year}",
                f"DNV-OS-F201-{year}",
                f"DNV-F201-{year}",
            ]
        candidate_keys = [
            self.specification_code,
            *year_keys,
            "DNV-OS-F101",
            "DNV-ST-F101",
            "DNVGL-ST-F101",
            "DNV-F101",
            "DNV-OS-F201",
            "DNV-F201",
        ]
        for key in candidate_keys:
            if key in design_factors:
                overrides = design_factors[key]
                for section in defaults:
                    if section in overrides and isinstance(overrides[section], dict):
                        defaults[section].update(overrides[section])
                for top_key, top_value in overrides.items():
                    if top_key in defaults:
                        continue
                    for section in defaults:
                        if top_key in defaults[section]:
                            defaults[section][top_key] = top_value
                break

        return defaults

    def _get_temperature_derating(self):
        try:
            return float(
                self.cfg["Design"][self.load_condition_index]["Material"][
                    "temperature_derating"
                ][self.pipe_flag][self.specification_code]
            )
        except KeyError:
            return 1.0

    def _get_material_strengths(self):
        alpha_u = self.design_factors["internal_pressure"]["alpha_u"]
        temp_derating = self._get_temperature_derating()
        smys = float(self.cfg[self.pipe_flag]["Material"]["SMYS"])
        smus = float(self.cfg[self.pipe_flag]["Material"]["SMUS"])
        fy = smys * alpha_u * temp_derating
        fu = smus * alpha_u * temp_derating
        return fy, fu

    def _get_material_modulus(self):
        material = self.cfg[self.pipe_flag]["Material"]
        E = float(material.get("E", 0.0))
        poisson = material.get(
            "Poissons_Ratio",
            material.get("Poissionsratio", material.get("PoissonRatio", 0.3)),
        )
        return E, float(poisson)

    def _get_pressures(self):
        design = self.cfg["Design"][self.load_condition_index]
        p_internal = _resolve_pressure_value(design["InternalPressure"][self.pipe_flag])
        p_external = _resolve_pressure_value(design["ExternalPressure"][self.pipe_flag])
        min_internal = design.get("MinimumInternalPressure")
        if isinstance(min_internal, dict):
            min_internal = min_internal.get(self.pipe_flag)
        min_internal = _resolve_pressure_value(min_internal)
        return p_internal, p_external, min_internal

    def _get_nominal_od(self):
        return float(self.cfg[self.pipe_flag]["Geometry"]["Nominal_OD"])

    def _get_internal_minus_external(self):
        p_internal, p_external, _ = self._get_pressures()
        return max(0.0, p_internal - p_external)

    def _get_external_minus_min_internal(self):
        _, p_external, p_internal_min = self._get_pressures()
        return max(0.0, p_external - p_internal_min), p_internal_min

    def _pressure_containment_allowable(self, thickness):
        if thickness <= 0.0:
            return 0.0
        D = self._get_nominal_od()
        fy, fu = self._get_material_strengths()
        fcb = min(fy, fu / 1.15)
        gamma_m = self.design_factors["internal_pressure"]["gamma_m"]
        gamma_sc = self.design_factors["internal_pressure"]["gamma_sc"]
        return (
            (2.0 / math.sqrt(3.0))
            * (2.0 * thickness / (D - thickness))
            * fcb
            / (gamma_m * gamma_sc)
        )

    def get_burst_pressure(self, thickness):
        _, p_external, _ = self._get_pressures()
        return self._pressure_containment_allowable(thickness) + p_external

    def get_burst_minimum_thickness(self):
        p_design = self._get_internal_minus_external()
        if p_design <= 0.0:
            return 0.0
        D = self._get_nominal_od()
        fy, fu = self._get_material_strengths()
        fcb = min(fy, fu / 1.15)
        gamma_m = self.design_factors["internal_pressure"]["gamma_m"]
        gamma_sc = self.design_factors["internal_pressure"]["gamma_sc"]
        strength_term = (2.0 / math.sqrt(3.0)) * fcb / (gamma_m * gamma_sc)
        if strength_term <= 0.0:
            return 0.0
        return (p_design * D) / (p_design + 2.0 * strength_term)

    def _collapse_pressure(self, thickness):
        if thickness <= 0.0:
            return None
        D = self._get_nominal_od()
        if thickness >= D:
            return None
        E, poisson = self._get_material_modulus()
        fy, _ = self._get_material_strengths()
        if E <= 0.0 or fy <= 0.0:
            return None
        ovality = self.design_factors["external_pressure"]["ovality"]
        alpha_fab = self.design_factors["external_pressure"]["alpha_fab"]
        d_over_t = D / thickness
        t_over_d = thickness / D
        pel = 2.0 * E * (t_over_d**3) / (1.0 - poisson**2)
        pp = 2.0 * fy * alpha_fab * t_over_d

        b = -pel
        c = -(pp**2 + pp * pel * ovality * d_over_t)
        d = pel * (pp**2)
        u = (-(b**2) / 3.0 + c) / 3.0
        v = (2.0 * b**3 / 27.0 - b * c / 3.0 + d) / 2.0
        if u >= 0.0:
            return None
        cos_arg = -v / math.sqrt(-u**3)
        cos_arg = max(-1.0, min(1.0, cos_arg))
        phi = math.acos(cos_arg)
        y = -2.0 * math.sqrt(-u) * math.cos(phi / 3.0 + 60.0 * math.pi / 180.0)
        return y - b / 3.0

    def get_collapse_pressure(self, thickness):
        _, _, p_internal_min = self._get_pressures()
        gamma_m = self.design_factors["external_pressure"]["gamma_m"]
        gamma_sc = self.design_factors["external_pressure"]["gamma_sc"]
        pc = self._collapse_pressure(thickness)
        if pc is None:
            return 0.0
        return pc / (gamma_m * gamma_sc) + p_internal_min

    def get_collapse_minimum_thickness(self):
        p_delta, p_internal_min = self._get_external_minus_min_internal()
        gamma_m = self.design_factors["external_pressure"]["gamma_m"]
        gamma_sc = self.design_factors["external_pressure"]["gamma_sc"]
        target = p_delta * gamma_m * gamma_sc
        if target <= 0.0:
            return 0.0

        D = self._get_nominal_od()
        t_min = max(1e-6, 0.001 * D)
        t_max = 0.45 * D
        for _ in range(60):
            t_mid = 0.5 * (t_min + t_max)
            pc = self._collapse_pressure(t_mid)
            if pc is None:
                t_min = t_mid
                continue
            if pc >= target:
                t_max = t_mid
            else:
                t_min = t_mid
        return t_max

    def get_propagation_pressure(self, thickness):
        _, _, p_internal_min = self._get_pressures()
        gamma_m = self.design_factors["collapse_propagation"]["gamma_m"]
        gamma_sc = self.design_factors["collapse_propagation"]["gamma_sc"]
        alpha_fab = self.design_factors["collapse_propagation"]["alpha_fab"]
        D = self._get_nominal_od()
        fy, _ = self._get_material_strengths()
        if fy <= 0.0 or alpha_fab <= 0.0 or D <= 0.0:
            return 0.0
        p_prop = 35.0 * fy * alpha_fab * ((thickness / D) ** 2.5)
        return p_prop / (gamma_m * gamma_sc) + p_internal_min

    def get_propagation_minimum_thickness(self):
        p_delta, _ = self._get_external_minus_min_internal()
        gamma_m = self.design_factors["collapse_propagation"]["gamma_m"]
        gamma_sc = self.design_factors["collapse_propagation"]["gamma_sc"]
        alpha_fab = self.design_factors["collapse_propagation"]["alpha_fab"]
        D = self._get_nominal_od()
        fy, _ = self._get_material_strengths()
        target = p_delta * gamma_m * gamma_sc
        if target <= 0.0 or fy <= 0.0 or alpha_fab <= 0.0:
            return 0.0
        return D * ((target / (35.0 * fy * alpha_fab)) ** (1.0 / 2.5))


class PipeCapacity:
    def __init__(self, cfg):
        self.cfg = cfg

    def evaluate_pipe_wall(self):
        if self.cfg["Outer_Pipe"] != None:
            self.evaluate_load_conditions(pipe_flag="Outer_Pipe")
        if self.cfg["Inner_Pipe"] != None:
            self.evaluate_load_conditions(pipe_flag="Inner_Pipe")

    def evaluate_load_conditions(self, pipe_flag):
        for load_condition_index in range(0, len(self.cfg["Design"])):
            for code_index in range(
                0, len(self.cfg["Design"][load_condition_index]["Code"])
            ):
                specification_code = self.cfg["Design"][load_condition_index]["Code"][
                    code_index
                ][pipe_flag]
                logging.info(
                    "Finished processing results for code: {0}".format(
                        specification_code
                    )
                )
                load_condition = self.cfg["Design"][load_condition_index][
                    "Load Condition"
                ][pipe_flag]
                if load_condition == "internal_pressure":
                    self.internal_pressure(
                        pipe_flag, specification_code, load_condition_index
                    )
                if load_condition == "external_pressure":
                    self.external_pressure(
                        pipe_flag, specification_code, load_condition_index
                    )
                if load_condition == "collapse_propagation":
                    self.collapse_propagation(
                        pipe_flag, specification_code, load_condition_index
                    )

    def internal_pressure(self, pipe_flag, specification_code, load_condition_index):
        thickness = self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]

        (
            minimum_thickness,
            pressure,
        ) = self.internal_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness
        )
        self.update_pressure_results(
            minimum_thickness,
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag="Zero Corrosion Allowance",
        )

        thickness = (
            self.cfg[pipe_flag]["Geometry"]["Design_WT"]
            - self.cfg[pipe_flag]["Geometry"]["Corrosion_Allowance"]
        )
        (
            minimum_thickness,
            pressure,
        ) = self.internal_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness
        )

        self.update_pressure_results(
            minimum_thickness + self.cfg[pipe_flag]["Geometry"]["Corrosion_Allowance"],
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag="With Corrosion Allowance",
        )

    def collapse_propagation(self, pipe_flag, specification_code, load_condition_index):
        thickness = self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]

        (
            minimum_thickness,
            pressure,
        ) = self.collapse_propagation_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness
        )
        self.update_pressure_results(
            minimum_thickness,
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag="Zero Corrosion Allowance",
        )

        thickness = (
            self.cfg[pipe_flag]["Geometry"]["Design_WT"]
            - self.cfg[pipe_flag]["Geometry"]["Corrosion_Allowance"]
        )
        (
            minimum_thickness,
            pressure,
        ) = self.collapse_propagation_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness
        )

        self.update_pressure_results(
            minimum_thickness + self.cfg[pipe_flag]["Geometry"]["Corrosion_Allowance"],
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag="With Corrosion Allowance",
        )

    def external_pressure(self, pipe_flag, specification_code, load_condition_index):
        thickness = self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]

        (
            minimum_thickness,
            pressure,
        ) = self.external_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness
        )
        self.update_pressure_results(
            minimum_thickness,
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag="Zero Corrosion Allowance",
        )

        thickness = (
            self.cfg[pipe_flag]["Geometry"]["Design_WT"]
            - self.cfg[pipe_flag]["Geometry"]["Corrosion_Allowance"]
        )
        (
            minimum_thickness,
            pressure,
        ) = self.external_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness
        )

        self.update_pressure_results(
            minimum_thickness + self.cfg[pipe_flag]["Geometry"]["Corrosion_Allowance"],
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag="With Corrosion Allowance",
        )

    def update_pressure_results(
        self,
        minimum_thickness,
        pipe_flag,
        pressure,
        specification_code,
        thickness,
        load_condition,
        custom_tag,
    ):

        update_deep_dictionary(
            self.cfg,
            {
                self.cfg["basename"]: {
                    pipe_flag: {
                        load_condition: {
                            specification_code: {"thickness": {custom_tag: thickness}}
                        }
                    }
                }
            },
        )
        update_deep_dictionary(
            self.cfg,
            {
                self.cfg["basename"]: {
                    pipe_flag: {
                        load_condition: {
                            specification_code: {
                                "Design_WT_Max_Pressure": {custom_tag: pressure}
                            }
                        }
                    }
                }
            },
        )
        update_deep_dictionary(
            self.cfg,
            {
                self.cfg["basename"]: {
                    pipe_flag: {
                        load_condition: {
                            specification_code: {
                                "minimum_thickness": {custom_tag: minimum_thickness}
                            }
                        }
                    }
                }
            },
        )

    def internal_pressure_specification_code_based_evaluation(
        self, load_condition_index, pipe_flag, specification_code, thickness
    ):
        if "ASME B31" in specification_code:
            pressure = self.evaluate_burst_pressure_modified_burlow_equation(
                pipe_flag, specification_code, load_condition_index, thickness
            )
            minimum_thickness = (
                self.evaluate_burst_minimum_thickness_modified_burlow_equation(
                    pipe_flag, specification_code, load_condition_index
                )
            )

        if "API STD 2RD-2013" in specification_code:
            pressure = self.evaluate_burst_pressure_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index, thickness
            )
            minimum_thickness = self.evaluate_burst_minimum_thickness_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index
            )

        if "API RP 1111-2009" in specification_code:
            pressure = self.evaluate_burst_pressure_API_RP_1111(
                pipe_flag, specification_code, load_condition_index, thickness
            )
            minimum_thickness = self.evaluate_burst_minimum_thickness_API_RP_1111(
                pipe_flag, specification_code, load_condition_index
            )

        if "API RP 16Q-2017" in specification_code:
            api_rp_16q = API_RP_16Q(self.cfg)
            pressure = api_rp_16q.get_burst_pressure(pipe_flag, load_condition_index)
            minimum_thickness = api_rp_16q.get_burst_minimum_thickness(
                pipe_flag, load_condition_index
            )

        if "30 CFR Part 250" in specification_code:
            cfr_30_part_250 = CFR_30_Part_250(self.cfg)
            pressure = cfr_30_part_250.get_burst_pressure(
                pipe_flag, load_condition_index
            )
            minimum_thickness = cfr_30_part_250.get_burst_minimum_thickness(
                pipe_flag, load_condition_index
            )

        if _is_dnv_f101(specification_code) or _is_dnv_f201(specification_code):
            dnv = DNVWallThickness(
                self.cfg, pipe_flag, load_condition_index, specification_code
            )
            pressure = dnv.get_burst_pressure(thickness)
            minimum_thickness = dnv.get_burst_minimum_thickness()

        return minimum_thickness, pressure

    def evaluate_burst_pressure_API_STD_2RD(
        self, pipe_flag, specification_code, load_condition_index, thickness
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        pressure = (
            self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
            * self.cfg["DesignFactors"][specification_code][load_condition]["k"][
                "API 5L"
            ]
            * (
                self.cfg[pipe_flag]["Material"]["SMYS"]
                + self.cfg[pipe_flag]["Material"]["SMUS"]
            )
            * math.log(
                self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                / (self.cfg[pipe_flag]["Geometry"]["Nominal_OD"] - 2 * thickness)
            )
        )
        return pressure

    def evaluate_burst_pressure_API_RP_1111(
        self, pipe_flag, specification_code, load_condition_index, thickness
    ):
        d_over_t = (
            self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        )
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]

        if (
            d_over_t
            <= self.cfg["DesignFactors"][specification_code]["D_over_T_Trasition_Ratio"]
        ):
            pressure = (
                self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
                * 0.45
                * (
                    self.cfg[pipe_flag]["Material"]["SMYS"]
                    + self.cfg[pipe_flag]["Material"]["SMUS"]
                )
                * math.log(
                    self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                    / (self.cfg[pipe_flag]["Geometry"]["Nominal_OD"] - 2 * thickness)
                )
            )
        else:
            pressure = (
                self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
                * 0.90
                * (
                    self.cfg[pipe_flag]["Material"]["SMYS"]
                    + self.cfg[pipe_flag]["Material"]["SMUS"]
                )
                * (
                    thickness
                    / (self.cfg[pipe_flag]["Geometry"]["Nominal_OD"] - thickness)
                )
            )

        return pressure

    def evaluate_burst_minimum_thickness_API_RP_1111(
        self, pipe_flag, specification_code, load_condition_index
    ):
        d_over_t = (
            self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        )
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]

        if (
            d_over_t
            <= self.cfg["DesignFactors"][specification_code]["D_over_T_Trasition_Ratio"]
        ):
            burst_by_factored_strength = (
                (
                    self.cfg["Design"][load_condition_index]["InternalPressure"][
                        pipe_flag
                    ]
                    - self.cfg["Design"][load_condition_index]["ExternalPressure"][
                        pipe_flag
                    ]
                )
                / 0.45
                / self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
                / (
                    self.cfg[pipe_flag]["Material"]["SMYS"]
                    + self.cfg[pipe_flag]["Material"]["SMUS"]
                )
            )

            minimum_thickness = 0.5 * (
                self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                - self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                / math.exp(burst_by_factored_strength)
            )
        else:
            burst_by_factored_strength = (
                (
                    self.cfg["Design"][load_condition_index]["InternalPressure"][
                        pipe_flag
                    ]
                    - self.cfg["Design"][load_condition_index]["ExternalPressure"][
                        pipe_flag
                    ]
                )
                / 0.90
                / self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
                / (
                    self.cfg[pipe_flag]["Material"]["SMYS"]
                    + self.cfg[pipe_flag]["Material"]["SMUS"]
                )
            )

            minimum_thickness = (
                burst_by_factored_strength
                * self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                / (1 + burst_by_factored_strength)
            )

        return minimum_thickness

    def evaluate_burst_minimum_thickness_API_STD_2RD(
        self, pipe_flag, specification_code, load_condition_index
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        burst_by_factored_strength = (
            (
                self.cfg["Design"][load_condition_index]["InternalPressure"][pipe_flag]
                - self.cfg["Design"][load_condition_index]["ExternalPressure"][
                    pipe_flag
                ]
            )
            / self.cfg["DesignFactors"][specification_code][load_condition]["k"][
                "API 5L"
            ]
            / self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
            / (
                self.cfg[pipe_flag]["Material"]["SMYS"]
                + self.cfg[pipe_flag]["Material"]["SMUS"]
            )
        )

        minimum_thickness = 0.5 * (
            self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            - self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / math.exp(burst_by_factored_strength)
        )

        return minimum_thickness

    def evaluate_burst_pressure_modified_burlow_equation(
        self, pipe_flag, specification_code, load_condition_index, thickness
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        stress_hoop_allowable = (
            self.cfg[pipe_flag]["Material"]["SMYS"]
            * self.cfg["DesignFactors"][specification_code][load_condition]
            * self.cfg[pipe_flag]["Material"]["WeldFactor"]["Seamless"]
            * self.cfg["Design"][load_condition_index]["Material"][
                "temperature_derating"
            ][pipe_flag][specification_code]
        )
        d_over_t = (
            self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        )

        if (
            d_over_t
            >= self.cfg["DesignFactors"][specification_code]["D_over_T_Trasition_Ratio"]
        ):
            pressure = (
                2
                * thickness
                * stress_hoop_allowable
                / self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                + self.cfg["Design"][load_condition_index]["ExternalPressure"][
                    pipe_flag
                ]
            )
        else:
            pressure = (
                2
                * thickness
                * stress_hoop_allowable
                / (self.cfg[pipe_flag]["Geometry"]["Nominal_OD"] - thickness)
                + self.cfg["Design"][load_condition_index]["ExternalPressure"][
                    pipe_flag
                ]
            )

        return pressure

    def evaluate_burst_minimum_thickness_modified_burlow_equation(
        self, pipe_flag, specification_code, load_condition_index
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        stress_hoop = (
            self.cfg[pipe_flag]["Material"]["SMYS"]
            * self.cfg["DesignFactors"][specification_code][load_condition]
            * self.cfg[pipe_flag]["Material"]["WeldFactor"]["Seamless"]
            * self.cfg["Design"][load_condition_index]["Material"][
                "temperature_derating"
            ][pipe_flag][specification_code]
        )

        d_over_t = (
            self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        )
        # TODO Need itreation to ensure new d_over_t still satisfies the criteria
        if (
            d_over_t
            >= self.cfg["DesignFactors"][specification_code]["D_over_T_Trasition_Ratio"]
        ):
            minimum_thickness = (
                (
                    self.cfg["Design"][load_condition_index]["InternalPressure"][
                        pipe_flag
                    ]
                    - self.cfg["Design"][load_condition_index]["ExternalPressure"][
                        pipe_flag
                    ]
                )
                * self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                / (2 * stress_hoop)
            )
        else:
            minimum_thickness = (
                (
                    self.cfg["Design"][load_condition_index]["InternalPressure"][
                        pipe_flag
                    ]
                    - self.cfg["Design"][load_condition_index]["ExternalPressure"][
                        pipe_flag
                    ]
                )
                * self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
                / (
                    2 * stress_hoop
                    + (
                        self.cfg["Design"][load_condition_index]["InternalPressure"][
                            pipe_flag
                        ]
                        - self.cfg["Design"][load_condition_index]["ExternalPressure"][
                            pipe_flag
                        ]
                    )
                )
            )

        return minimum_thickness

    def external_pressure_specification_code_based_evaluation(
        self, load_condition_index, pipe_flag, specification_code, thickness
    ):
        if "API STD 2RD-2013" in specification_code:
            pressure = self.evaluate_collapse_pressure_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index, thickness
            )
            minimum_thickness = self.evaluate_collapse_minimum_thickness_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index
            )

        if "API TR 5C3-2018" in specification_code:
            api_tr_5c3 = API_TR_5C3(self.cfg, pipe_flag, load_condition_index)
            pressure = api_tr_5c3.get_collapse_pressure()
            minimum_thickness = api_tr_5c3.get_collapse_minimum_thickness()

        if _is_dnv_f101(specification_code) or _is_dnv_f201(specification_code):
            dnv = DNVWallThickness(
                self.cfg, pipe_flag, load_condition_index, specification_code
            )
            pressure = dnv.get_collapse_pressure(thickness)
            minimum_thickness = dnv.get_collapse_minimum_thickness()

        return minimum_thickness, pressure

    def evaluate_collapse_pressure_API_STD_2RD(
        self, pipe_flag, specification_code, load_condition_index, thickness
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]

        d_over_t = self.cfg[pipe_flag]["Geometry"]["Nominal_OD"] / thickness
        t_over_d = 1 / d_over_t
        material = self.cfg[pipe_flag]["Material"]["Material"]
        E = self.cfg["Material"][material]["E"]
        Poissionsratio = self.cfg["Material"][material]["Poissionsratio"]

        Py = 2 * self.cfg[pipe_flag]["Material"]["SMYS"] * t_over_d
        Pel = 2 * E * (t_over_d**3) / (1 - Poissionsratio**2)
        Pc = Py * Pel / (math.sqrt(Py**2 + Pel**2))
        Pe = Pc * self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]

        return Pe

    def evaluate_collapse_minimum_thickness_API_STD_2RD(
        self, pipe_flag, specification_code, load_condition_index
    ):
        d_over_t = (
            self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        )
        t_over_d = 1 / d_over_t
        material = self.cfg[pipe_flag]["Material"]["Material"]
        E = self.cfg["Material"][material]["E"]
        Poissionsratio = self.cfg["Material"][material]["Poissionsratio"]
        # TODO Calculation to be completed.
        return 100

    def collapse_propagation_specification_code_based_evaluation(
        self, load_condition_index, pipe_flag, specification_code, thickness
    ):
        if (
            "API STD 2RD-2013" in specification_code
            or "API RP 1111-2009" in specification_code
        ):
            pressure = self.evaluate_collapse_propagation_pressure_API_RP_1111(
                pipe_flag, specification_code, load_condition_index, thickness
            )
            minimum_thickness = (
                self.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
                    pipe_flag, specification_code, load_condition_index
                )
            )

        if _is_dnv_f101(specification_code) or _is_dnv_f201(specification_code):
            dnv = DNVWallThickness(
                self.cfg, pipe_flag, load_condition_index, specification_code
            )
            pressure = dnv.get_propagation_pressure(thickness)
            minimum_thickness = dnv.get_propagation_minimum_thickness()

        return minimum_thickness, pressure

    def evaluate_collapse_propagation_pressure_API_RP_1111(
        self, pipe_flag, specification_code, load_condition_index, thickness
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        Pp = (
            24
            * self.cfg[pipe_flag]["Material"]["SMYS"]
            * ((thickness / self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]) ** 2.4)
        )

        pressure = (
            Pp * self.cfg["DesignFactors"][specification_code][load_condition]["Fp"]
        )

        return pressure

    def evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
        self, pipe_flag, specification_code, load_condition_index
    ):
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        if (
            self.cfg["Design"][load_condition_index]["ExternalPressure"][pipe_flag]
            == None
        ):
            Po = self.cfg["Design"][load_condition_index]["ExternalPressure"][pipe_flag]
        else:
            Po = (
                self.cfg["Design"][load_condition_index]["ExternalPressure"][pipe_flag][
                    "fluid_density"
                ]
                * self.cfg["Design"][load_condition_index]["ExternalPressure"][
                    pipe_flag
                ]["fluid_column"]
                * 12
            )

        Pp = Po / self.cfg["DesignFactors"][specification_code][load_condition]["Fp"]
        minimum_thickness = (
            (Pp / 24 / self.cfg[pipe_flag]["Material"]["SMYS"]) ** (1 / 2.4)
        ) * self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]

        return minimum_thickness


class InternalPressureMethods:
    pass


class OtherMethodsTobeIncorporated:
    # TODO LogitudinalStress, EquivalentStress
    def ASMEB314InternalPressure(data):
        DesignPressure = (
            2
            * data["S"]
            * data["t"]
            / data["D"]
            * data["F"]
            * data["WeldFactor"]
            * data["T"]
        )
        Stress_Hoop = data["S"] * data["F"] * data["WeldFactor"] * data["T"]

        if data["D"] / data["t"] >= 30:
            ThicknessMinimum_Pressure = (
                (data["Pi"] - data["Po"]) * data["D"] / (2 * Stress_Hoop)
            )
        else:
            ThicknessMinimum_Pressure = (
                (data["Pi"] - data["Po"])
                * data["D"]
                / (2 * Stress_Hoop + (data["Pi"] - data["Po"]))
            )

        data.update(
            {
                "MaximumDesignPressure": DesignPressure,
                "MinimumWallThickness_Pressure": ThicknessMinimum_Pressure,
            }
        )
        return data

    def ASMEB314LogitudinalStress(data):
        if data["Condition"] == "Restrained":
            Stress_Elongation = -data["E"] * data["Alpha"] * (data["T1"] - data["T2"])
            Stress_Longitudinal = data["S"] * data["F"] * data["WeldFactor"] * data["T"]
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Hoop = (
                Stress_Longitudinal - Stress_Elongation - Stress_Moment - Stress_Axial
            ) / data["Poissionsratio"]

            DbytRatio = data["D"] / data["t"]
            if DbytRatio >= 30:
                ThicknessMinimum_Longitudinal = (
                    (data["Pi"] - data["Po"]) * data["D"] / (2 * Stress_Hoop)
                )
            else:
                ThicknessMinimum_Longitudinal = (
                    (data["Pi"] - data["Po"])
                    * data["D"]
                    / (2 * Stress_Hoop + (data["Pi"] - data["Po"]))
                )

            data.update(
                {
                    "MinimumWallThickness_Longitudinal": ThicknessMinimum_Longitudinal,
                    "Stress_Elongation": Stress_Elongation,
                    "Stress_Logitudinal": Stress_Longitudinal,
                    "Stress_Hoop": Stress_Hoop,
                    "Stress_Axial": Stress_Axial,
                    "Stress_Moment": Stress_Moment,
                }
            )

            return data

    def ASMEB314EquivalentStress(data):
        if data["Condition"] == "Restrained":
            Stress_Elongation = -data["E"] * data["Alpha"] * (data["T1"] - data["T2"])
            Stress_Equivalent = data["S"] * data["F"] * data["WeldFactor"] * data["T"]
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Torsion = 0
            Stress_Hoop = (
                (
                    -1
                    * math.sqrt(Stress_Equivalent**2 / 2 - Stress_Torsion)
                    / (data["Poissionsratio"] - 1)
                )
                - Stress_Elongation
                - Stress_Moment
            ) / (1 + data["Poissionsratio"])

            DbytRatio = data["D"] / data["t"]
            if DbytRatio >= 30:
                ThicknessMinimum_Equivalent = (
                    (data["Pi"] - data["Po"]) * data["D"] / (2 * Stress_Hoop)
                )
            else:
                ThicknessMinimum_Equivalent = (
                    (data["Pi"] - data["Po"])
                    * data["D"]
                    / (2 * Stress_Hoop + (data["Pi"] - data["Po"]))
                )

            data.update(
                {
                    "MinimumWallThickness_Equivalent": ThicknessMinimum_Equivalent,
                    "Stress_Elongation": Stress_Elongation,
                    "Stress_Hoop": Stress_Hoop,
                    "Stress_Axial": Stress_Axial,
                    "Stress_Moment": Stress_Moment,
                    "Stress_Equivalent": Stress_Equivalent,
                }
            )

            return data

    def ASMEB318InternalPressure(data):
        DesignPressure = (
            2
            * data["S"]
            * data["t"]
            / data["D"]
            * data["F"]
            * data["WeldFactor"]
            * data["T"]
        )

        DbytRatio = data["D"] / data["t"]
        if DbytRatio >= 30:
            ThicknessMinimum_Pressure = (
                (data["Pi"] - data["Po"])
                * data["D"]
                / (2 * data["S"] * data["F"] * data["WeldFactor"] * data["T"])
            )
        else:
            ThicknessMinimum_Pressure = (
                (data["Pi"] - data["Po"])
                * data["D"]
                / (
                    2 * data["S"] * data["F"] * data["WeldFactor"] * data["T"]
                    + (data["Pi"] - data["Po"])
                )
            )

        data.update(
            {
                "MaximumDesignPressure": DesignPressure,
                "MinimumWallThickness": ThicknessMinimum_Pressure,
            }
        )

        return data

    def ASMEB318LogitudinalStress(data):
        if data["Condition"] == "Restrained":
            Stress_Elongation = -data["E"] * data["Alpha"] * (data["T1"] - data["T2"])
            Stress_Longitudinal = data["S"] * data["F"] * data["WeldFactor"] * data["T"]
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Hoop = (
                Stress_Longitudinal - Stress_Elongation - Stress_Moment
            ) / data["Poissionsratio"]

            DbytRatio = data["D"] / data["t"]
            if DbytRatio >= 30:
                ThicknessMinimum_Longitudinal = (
                    (data["Pi"] - data["Po"]) * data["D"] / (2 * Stress_Hoop)
                )
            else:
                ThicknessMinimum_Longitudinal = (
                    (data["Pi"] - data["Po"])
                    * data["D"]
                    / (2 * Stress_Hoop + (data["Pi"] - data["Po"]))
                )

            data.update(
                {
                    "MinimumWallThickness_Longitudinal": ThicknessMinimum_Longitudinal,
                    "Stress_Elongation": Stress_Elongation,
                    "Stress_Logitudinal": Stress_Longitudinal,
                    "Stress_Hoop": Stress_Hoop,
                    "Stress_Axial": Stress_Axial,
                    "Stress_Moment": Stress_Moment,
                }
            )

            return data

    def ASMEB318EquivalentStress(data):
        if data["Condition"] == "Restrained":
            Stress_Elongation = -data["E"] * data["Alpha"] * (data["T1"] - data["T2"])
            Stress_Equivalent = data["S"] * data["F"] * data["WeldFactor"] * data["T"]
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Torsion = 0
            Stress_Hoop = (
                (
                    -1
                    * math.sqrt(Stress_Equivalent**2 / 2 - Stress_Torsion)
                    / (data["Poissionsratio"] - 1)
                )
                - Stress_Elongation
                - Stress_Moment
            ) / (1 + data["Poissionsratio"])

            DbytRatio = data["D"] / data["t"]
            if DbytRatio >= 30:
                ThicknessMinimum_Equivalent = (
                    (data["Pi"] - data["Po"]) * data["D"] / (2 * Stress_Hoop)
                )
            else:
                ThicknessMinimum_Equivalent = (
                    (data["Pi"] - data["Po"])
                    * data["D"]
                    / (2 * Stress_Hoop + (data["Pi"] - data["Po"]))
                )

            data.update(
                {
                    "MinimumWallThickness_Equivalent": ThicknessMinimum_Equivalent,
                    "Stress_Elongation": Stress_Elongation,
                    "Stress_Hoop": Stress_Hoop,
                    "Stress_Axial": Stress_Axial,
                    "Stress_Moment": Stress_Moment,
                    "Stress_Equivalent": Stress_Equivalent,
                }
            )

            return data


class API_RP_2RD:
    def __init__(self, cfg):
        self.cfg = cfg


class API_RP_16Q:
    def __init__(self, cfg):
        import datetime

        self.cfg = cfg
        self.edition = "second"
        self.release_date = datetime.date(2017, 4, 1)

    def get_burst_minimum_thickness(self, pipe_flag, load_condition_index):
        vm = VonMises_Pipe(self.cfg, pipe_flag, load_condition_index)
        minimum_thickness = vm.get_burst_minimum_thickness()

        return minimum_thickness

    def get_burst_pressure(self, pipe_flag, load_condition_index):
        vm = VonMises_Pipe(self.cfg, pipe_flag, load_condition_index)
        pressure = vm.get_burst_pressure()

        return pressure


class API_TR_5C3:
    def __init__(self, cfg, pipe_flag, load_condition_index):
        import datetime

        self.cfg = cfg
        self.edition = "second"
        self.release_date = datetime.date(2018, 6, 1)
        self.init_calculate_required_properties(pipe_flag, load_condition_index)

    def init_calculate_required_properties(self, pipe_flag, load_condition_index):
        self.pipe_flag = pipe_flag
        self.D_o = self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
        self.D_i = self.cfg[pipe_flag]["Geometry"]["Nominal_ID"]
        self.t = self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        self.loads = self.cfg["Design"][load_condition_index]
        self.P_o = self.loads["ExternalPressure"][pipe_flag]
        self.P_i = self.loads["InternalPressure"][pipe_flag]

    def get_collapse_pressure(self, t=None):
        if t == None:
            t = self.t

        d_over_t = self.D_o / t
        Pe = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)

        return Pe

    def get_collapse_minimum_thickness(self, external_pressure=None):
        import numpy as np

        if external_pressure != None:
            self.P_o = external_pressure

        self.d_over_t = self.D_o / self.t
        d_over_t = []
        collapse_pressure = []
        number_of_steps = 100
        parameter_attribute = "d_over_t"
        # TODO Set attr to run it further. Generalize the code for reuse as common code.
        parameter_min = self.d_over_t / 2
        parameter_max = self.d_over_t * 30
        for step in range(1, number_of_steps):
            self.d_over_t = (
                parameter_min + (parameter_max - parameter_min) / number_of_steps * step
            )
            d_over_t.append(self.d_over_t)
            collapse_pressure.append(
                self.get_collapse_pressure(self.D_o / self.d_over_t)
            )

        x = self.loads["ExternalPressure"][self.pipe_flag]
        xp = collapse_pressure
        fp = d_over_t
        result = np.interp(x, xp[::-1], fp[::-1])
        target_d_over_t = float(result)
        min_wall_thickness_at_limit = self.D_o / target_d_over_t

        self.P_o = self.loads["ExternalPressure"][self.pipe_flag]
        self.d_over_t = self.D_o / self.t

        return min_wall_thickness_at_limit


class VonMises_Pipe:
    def __init__(self, cfg, pipe_flag, load_condition_index):
        self.cfg = cfg
        self.init_calculate_required_properties(pipe_flag, load_condition_index)
        self.vm_stress = self.get_stress()

    def init_calculate_required_properties(self, pipe_flag, load_condition_index):
        self.pipe_flag = pipe_flag
        self.D_o = self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
        self.D_i = self.cfg[pipe_flag]["Geometry"]["Nominal_ID"]
        self.t = self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        self.loads = self.cfg["Design"][load_condition_index]
        self.P_o = self.loads["ExternalPressure"][pipe_flag]
        self.P_i = self.loads["InternalPressure"][pipe_flag]

        self.section_properties = self.cfg.equivalent_pipe["section_properties"]["pipe"]

        self.M = self.loads["BendingMoment"] * 8.850746 * 1000
        if self.loads["AxialForce"] is None:
            self.T_eff = self.loads["EffectiveTension"] * 0.224809 * 1000
            self.T = self.T_eff + self.P_i * self.D_i - self.P_o * self.D_o
        elif self.loads["EffectiveTension"] is None:
            self.T = self.loads["AxialForce"] * 0.224809 * 1000
            self.T_eff = (
                self.loads["AxialForce"] - self.P_i * self.D_i + self.P_o * self.D_o
            )
        else:
            raise (
                "Set Axial force or Effective tension to None to proceed with calculation"
            )

    def get_stress(self):
        import copy

        from common.data import AttributeDict
        from custom.PipeSizing import PipeSizing

        cfg_temp = AttributeDict(copy.deepcopy(self.cfg))
        cfg_temp["Outer_Pipe"]["Geometry"]["Design_WT"] = self.t
        cfg_temp["Outer_Pipe"]["Geometry"]["Nominal_ID"] = (
            cfg_temp["Outer_Pipe"]["Geometry"]["Nominal_OD"]
            - 2 * cfg_temp["Outer_Pipe"]["Geometry"]["Design_WT"]
        )
        self.D_i = cfg_temp["Outer_Pipe"]["Geometry"]["Nominal_ID"]
        Pipe = PipeSizing(cfg_temp)
        Pipe.evaluate_pipe_system_properties()
        section_properties = cfg_temp["Outer_Pipe"]["section_properties"]["pipe"]

        import math

        sigma_radial = (
            -1 * (self.P_o * self.D_o + self.P_i * self.D_i) / (self.D_o + self.D_i)
        )
        sigma_hoop = (self.P_i - self.P_o) * self.D_o / (2 * self.t) - self.P_i
        sigma_axial_addition = self.T / section_properties["A"] + (
            self.M / 2 / section_properties["I"]
        ) * (self.D_o - self.t)
        sigma_axial_subtraction = self.T / section_properties["A"] - (
            self.M / 2 / section_properties["I"]
        ) * (self.D_o - self.t)
        sigma_under_root_addition = (
            (sigma_radial - sigma_hoop) ** 2
            + (sigma_hoop - sigma_axial_addition) ** 2
            + (sigma_axial_addition - sigma_radial) ** 2
        )
        sigma_under_root_subtraction = (
            (sigma_radial - sigma_hoop) ** 2
            + (sigma_hoop - sigma_axial_subtraction) ** 2
            + (sigma_axial_subtraction - sigma_radial) ** 2
        )

        sigma_under_root = max(sigma_under_root_addition, sigma_under_root_subtraction)
        sigma = math.sqrt(sigma_under_root / 2)

        self.D_i = self.cfg["Outer_Pipe"]["Geometry"]["Nominal_ID"]

        return sigma

    def get_burst_minimum_thickness(self, internal_pressure=None):
        import numpy as np

        if internal_pressure != None:
            self.P_i = internal_pressure

        t = []
        sigma = []
        number_of_steps = 30
        parameter_attribute = "t"
        # TODO Set attr to run it further. Generalize the code for reuse as common code.
        parameter_min = self.t / 8
        parameter_max = self.t * 2
        for step in range(1, number_of_steps):
            self.t = (
                parameter_min + (parameter_max - parameter_min) / number_of_steps * step
            )
            t.append(self.t)
            sigma.append(self.get_stress())

        x = (
            self.loads["allowable_stress_to_yield_ratio"]
            * self.section_properties["SMYS"]
        )
        xp = sigma
        fp = t
        result = np.interp(x, xp[::-1], fp[::-1])
        min_wall_thickness_at_limit = float(result)

        self.P_o = self.loads["ExternalPressure"][self.pipe_flag]
        self.t = self.cfg[self.pipe_flag]["Geometry"]["Design_WT"]

        # TODO send results to DB
        return min_wall_thickness_at_limit

    def get_burst_pressure(self, t=None):
        import numpy as np

        if t != None:
            self.t = t

        P_i = []
        sigma = []
        if self.P_i > 200:
            number_of_steps = 30
            parameter_min = self.P_i / 2
            parameter_max = self.P_i * 2
        else:
            number_of_steps = 100
            parameter_min = 0
            parameter_max = (
                25000  # Currently at 20 k designs for typical offshore design
            )
        for step in range(1, number_of_steps):
            self.P_i = parameter_min + (
                parameter_max - parameter_min
            ) / number_of_steps * (step - 1)
            P_i.append(self.P_i)
            sigma.append(self.get_stress())

        x = (
            self.loads["allowable_stress_to_yield_ratio"]
            * self.section_properties["SMYS"]
        )
        xp = sigma
        fp = P_i
        result = np.interp(x, xp, fp)
        burst_pressure_at_limit = float(result)

        self.P_i = self.loads["InternalPressure"][self.pipe_flag]
        self.t = self.cfg[self.pipe_flag]["Geometry"]["Design_WT"]

        # TODO expose these results and include them in graphs

        return burst_pressure_at_limit


class CFR_30_Part_250:
    def __init__(self, cfg):

        self.cfg = cfg
        self.specification_code = "30 CFR Part 250"

    def get_burst_minimum_thickness(self, pipe_flag, load_condition_index):
        specification_code = self.specification_code
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        internal_pressure = self.cfg["Design"][load_condition_index][
            "InternalPressure"
        ][pipe_flag]
        external_pressure = self.cfg["Design"][load_condition_index][
            "ExternalPressure"
        ][pipe_flag]
        stress_hoop_allowable = (
            self.cfg[pipe_flag]["Material"]["SMYS"]
            * self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
            * self.cfg[pipe_flag]["Material"]["WeldFactor"]["Seamless"]
            * self.cfg["Design"][load_condition_index]["Material"][
                "temperature_derating"
            ][pipe_flag][specification_code]
        )

        minimum_thickness = (
            (internal_pressure - external_pressure)
            * self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            / 2
            / stress_hoop_allowable
        )

        return minimum_thickness

    def get_burst_pressure(self, pipe_flag, load_condition_index):
        specification_code = self.specification_code
        load_condition = self.cfg["Design"][load_condition_index]["Load Condition"][
            pipe_flag
        ]
        thickness = self.cfg[pipe_flag]["Geometry"]["Design_WT"]
        stress_hoop_allowable = (
            self.cfg[pipe_flag]["Material"]["SMYS"]
            * self.cfg["DesignFactors"][specification_code][load_condition]["Fd"]
            * self.cfg[pipe_flag]["Material"]["WeldFactor"]["Seamless"]
            * self.cfg["Design"][load_condition_index]["Material"][
                "temperature_derating"
            ][pipe_flag][specification_code]
        )

        pressure = (
            2
            * thickness
            * stress_hoop_allowable
            / self.cfg[pipe_flag]["Geometry"]["Nominal_OD"]
            + self.cfg["Design"][load_condition_index]["ExternalPressure"][pipe_flag]
        )

        return pressure
