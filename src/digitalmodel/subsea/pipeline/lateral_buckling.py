# Standard library imports
import logging
import os

# Third party imports
import pandas as pd
from assetutilities.common.visualization.visualization_templates_matplotlib import (
    VisualizationTemplates,
)
from assetutilities.engine import engine as au_engine
from scipy import interpolate

# Reader imports
from digitalmodel.subsea.pipeline.buckling_common import CommonBucklingCaculations

viz_templates = VisualizationTemplates()
cbc = CommonBucklingCaculations()


class LateralBuckling:
    def __init__(self):
        pass

    def run(self, cfg):
        friction_force = self.get_friction_force(cfg)
        cfg["pipeline"]["friction_force"] = friction_force
        lateral_buckling_df = self.get_lateral_buckling(cfg)

        self.save_results(cfg, lateral_buckling_df)

    def save_results(self, cfg, lateral_buckling_df):
        file_name = cfg["Analysis"]["file_name_for_overwrite"] + "_lateral_buckling.csv"
        file_name = os.path.join(cfg["Analysis"]["result_folder"], file_name)
        lateral_buckling_df.to_csv(file_name, index=False)

        csv_groups = [{"file_name": file_name, "label": ""}]
        self.save_plots(cfg, csv_groups)

    def save_plots(self, cfg, csv_groups):
        self.save_temperature_plot(cfg, csv_groups.copy())
        self.save_buckling_force_plot_imp(cfg, csv_groups.copy())
        self.save_buckling_force_plot_met(cfg, csv_groups.copy())
        self.save_buckling_stress_plot_imp(cfg, csv_groups.copy())
        self.save_buckling_stress_plot_met(cfg, csv_groups.copy())
        self.save_buckling_strain_plot(cfg, csv_groups.copy())

    def get_friction_force(self, cfg):
        pipe_properties = cfg["pipeline"]["pipe_properties"]
        system_properties = cfg["pipeline"]["system_properties"]

        mass_in_water = system_properties["mass"]["water"]["with_internal_fluid"]
        friction_cfg = cfg["pipeline"]["soil"]

        axial_friction_force = (
            mass_in_water * friction_cfg["axial_breakout_friction_coeff"]
        )
        lateral_friction_force = (
            mass_in_water * friction_cfg["lateral_breakout_friction_coeff"]
        )

        friction = {"axial": axial_friction_force, "lateral": lateral_friction_force}

        return friction

    def get_lateral_buckling(self, cfg):

        df = pd.DataFrame()
        length = cfg["pipeline"]["length"] * 12 / 0.3048

        df = cbc.assign_mesh(df, length)
        df = cbc.get_water_depth(cfg, df)
        df = cbc.get_differential_temp(cfg, df)
        df = cbc.get_internal_pressure(cfg, df)
        df = cbc.get_external_pressure(cfg, df)

        df = self.get_longitudinal_force_and_stress(cfg, df)

        df = cbc.get_circumferential_stress(cfg, df)

        df = self.get_longitudinal_strain_and_thermal_stress(cfg, df)

        df = cbc.get_longitudinal_stress_fully_restrained(cfg, df)

        lateral_buckling, df = self.calculateLateralBuckling(cfg, df)
        cfg["pipeline"]["lateral_buckling"] = lateral_buckling

        return df

    def calculateLateralBuckling(self, cfg, lateral_buckling_df):
        length_array = list(lateral_buckling_df["length"])

        anchor_length = self.get_anchor_length(cfg, lateral_buckling_df)
        anchor_length_start = anchor_length["start"] / 0.0254
        anchor_length_end = anchor_length["end"] / 0.0254

        # Buckling Check
        pipe_properties = cfg["pipeline"]["pipe_properties"]
        system_properties = cfg["pipeline"]["system_properties"]
        friction = cfg["pipeline"]["friction_force"]
        tension_cfg = cfg["pipeline"]["tension"]

        pressure = pipe_properties[0]["internal_fluid"]["pressure"]
        A = pipe_properties[0]["section"]["A"]
        Ai = pipe_properties[0]["section"]["Ai"]

        E = pipe_properties[0]["material"]["E"]
        ThermalExpansionCoefficient = pipe_properties[0]["material"][
            "ThermalExpansionCoefficient"
        ]
        Poissonsratio = pipe_properties[0]["material"]["Poissonsratio"]

        differential_temperature = lateral_buckling_df["differential_temperature"]

        fully_restrained_axial_force_term1 = tension_cfg[
            "lay_tension"
        ] - pressure * Ai * (1 - 2 * Poissonsratio)
        fully_restrained_axial_force = [
            fully_restrained_axial_force_term1
            - A * E * ThermalExpansionCoefficient * item
            for item in differential_temperature
        ]
        lateral_buckling_df[
            "fully_restrained_axial_force"
        ] = fully_restrained_axial_force

        effective_axial_force_array = []
        for idx in range(0, len(length_array)):
            if length_array[idx] < anchor_length_start:
                effective_axial_force = (
                    -friction["axial"] * length_array[idx] + tension_cfg["start"]
                )
            elif length_array[idx] < anchor_length_end:
                effective_axial_force = fully_restrained_axial_force[idx]
            else:
                effective_axial_force = (
                    -friction["axial"] * (length_array[-1] - length_array[idx])
                    + tension_cfg["end"]
                )
            effective_axial_force_array.append(effective_axial_force)
        lateral_buckling_df["effective_axial_force"] = effective_axial_force_array
        x_values = length_array
        y_values = effective_axial_force_array
        f = interpolate.interp1d(x_values, y_values)
        effective_axial_force_length_start = round(float(f(anchor_length_start)), 3)
        effective_axial_force_length_end = round(float(f(anchor_length_end)), 3)
        effective_axial_load = {
            "anchor_start": round(effective_axial_force_length_start / 1000, 3),
            "anchor_end": round(effective_axial_force_length_end / 1000, 3),
        }

        I = pipe_properties[0]["section"]["I"]
        critical_buckling_load = (
            0.65 * 2.26 * (E * A) ** 0.25 * (E * I) ** 0.25 * friction["lateral"] ** 0.5
        )

        route_curve_radius = cfg["pipeline"]["route"]["curve_radius"] / 0.0254
        critical_buckling_load_for_route = friction["lateral"] * route_curve_radius
        min_critical_buckling_load = -min(
            critical_buckling_load, critical_buckling_load_for_route
        )
        min_critical_buckling_load_array = [min_critical_buckling_load] * len(
            length_array
        )
        lateral_buckling_df[
            "min_critical_buckling_load"
        ] = min_critical_buckling_load_array

        if abs(effective_axial_force_length_start) <= abs(
            min_critical_buckling_load
        ) and abs(effective_axial_force_length_end) <= abs(min_critical_buckling_load):
            lateral_buckling_check = "Pass"
        else:
            lateral_buckling_check = "Fail"

        lateral_buckling = {
            "anchor_length": anchor_length,
            "min_critical_buckling_load": min_critical_buckling_load,
            "effective_axial_load": effective_axial_load,
            "lateral_buckling_check": lateral_buckling_check,
            "fully_restrained_axial_force": {
                "L=0": round(fully_restrained_axial_force[0] / 1000, 3),
                "L=L": round(fully_restrained_axial_force[-1] / 1000, 3),
            },
        }

        logging.info(f"Lateral Buckling Check: {lateral_buckling_check}")
        logging.info(f"Lateral Buckling summary: {lateral_buckling}")

        return lateral_buckling, lateral_buckling_df

    def get_anchor_length(self, cfg, lateral_buckling_df):
        # Anchor Length
        length_array = list(lateral_buckling_df["length"])

        pipe_properties = cfg["pipeline"]["pipe_properties"]
        system_properties = cfg["pipeline"]["system_properties"]
        friction = cfg["pipeline"]["friction_force"]
        tension_cfg = cfg["pipeline"]["tension"]

        pressure = pipe_properties[0]["internal_fluid"]["pressure"]
        A = pipe_properties[0]["section"]["A"]
        Ai = pipe_properties[0]["section"]["Ai"]

        E = pipe_properties[0]["material"]["E"]
        ThermalExpansionCoefficient = pipe_properties[0]["material"][
            "ThermalExpansionCoefficient"
        ]
        Poissonsratio = pipe_properties[0]["material"]["Poissonsratio"]

        longitudinal_stress_hot_end = lateral_buckling_df["longitudinal_stress_hot_end"]
        longitudinal_stress_cold_end = lateral_buckling_df[
            "longitudinal_stress_cold_end"
        ]
        longitudinal_stress_mid_zone = lateral_buckling_df[
            "longitudinal_stress_mid_zone"
        ]

        for idx in range(0, len(length_array)):
            if longitudinal_stress_hot_end[idx] < longitudinal_stress_mid_zone[idx]:
                anchor_length_start = length_array[idx]
            else:
                anchor_length_start = length_array[-1] * 2

            if longitudinal_stress_cold_end[idx] < longitudinal_stress_mid_zone[idx]:
                anchor_length_end = length_array[idx]
            else:
                anchor_length_end = length_array[0] - 1

        anchor_length = {"start": anchor_length_start, "end": anchor_length_end}

        y_values = length_array
        zipped_array = list(
            zip(longitudinal_stress_hot_end, longitudinal_stress_mid_zone)
        )
        x_values = [item[0] - item[1] for item in zipped_array]
        f = interpolate.interp1d(x_values, y_values, fill_value="extrapolate")
        length_start_0 = round(float(f(0)), 3)
        zipped_array = list(
            zip(longitudinal_stress_cold_end, longitudinal_stress_mid_zone)
        )
        x_values = [item[0] - item[1] for item in zipped_array]
        f = interpolate.interp1d(x_values, y_values, fill_value="extrapolate")
        length_end_0 = round(float(f(0)), 3)

        reference_length = (
            friction["axial"] * length_array[-1]
            + tension_cfg["start"]
            - tension_cfg["end"]
        ) / (2 * friction["axial"])
        if length_start_0 > length_array[-1] or length_start_0 < length_array[0]:
            length_start = reference_length
        else:
            length_start = length_start_0
        if length_end_0 < length_array[0] or length_end_0 > length_array[-1]:
            length_end = length_start
        else:
            length_end = length_end_0
        anchor_length = {
            "start": round(length_start * 0.0254, 1),
            "end": round(length_end * 0.0254, 1),
        }

        return anchor_length

    def get_longitudinal_strain_and_thermal_stress(self, cfg, lateral_buckling_df):
        pipe_properties = cfg["pipeline"]["pipe_properties"]
        system_properties = cfg["pipeline"]["system_properties"]
        friction = cfg["pipeline"]["friction_force"]
        tension_cfg = cfg["pipeline"]["tension"]

        pressure = pipe_properties[0]["internal_fluid"]["pressure"]
        A = pipe_properties[0]["section"]["A"]
        Ai = pipe_properties[0]["section"]["Ai"]

        E = pipe_properties[0]["material"]["E"]
        ThermalExpansionCoefficient = pipe_properties[0]["material"][
            "ThermalExpansionCoefficient"
        ]
        Poissonsratio = pipe_properties[0]["material"]["Poissonsratio"]

        circumferential_stress_array = lateral_buckling_df["circumferential_stress"]

        differential_temperature = lateral_buckling_df["differential_temperature"]
        longitudinal_stress_hot_end = lateral_buckling_df["longitudinal_stress_hot_end"]
        longitudinal_stress_cold_end = lateral_buckling_df[
            "longitudinal_stress_cold_end"
        ]

        zipped_array = list(
            zip(
                longitudinal_stress_hot_end,
                circumferential_stress_array,
                differential_temperature,
            )
        )
        longitudinal_strain_hot_end = [
            (item[0] - item[1] * Poissonsratio) / E
            + ThermalExpansionCoefficient * item[2]
            for item in zipped_array
        ]
        lateral_buckling_df["longitudinal_strain_hot_end"] = longitudinal_strain_hot_end

        zipped_array = list(
            zip(
                longitudinal_stress_cold_end,
                circumferential_stress_array,
                differential_temperature,
            )
        )
        longitudinal_strain_cold_end = [
            (item[0] - item[1] * Poissonsratio) / E
            + ThermalExpansionCoefficient * item[2]
            for item in zipped_array
        ]
        lateral_buckling_df[
            "longitudinal_strain_cold_end"
        ] = longitudinal_strain_cold_end

        thermal_stress_array = [
            -E * ThermalExpansionCoefficient * item for item in differential_temperature
        ]
        lateral_buckling_df["thermal_stress"] = thermal_stress_array

        return lateral_buckling_df

    def get_longitudinal_force_and_stress(self, cfg, lateral_buckling_df):

        length_array = list(lateral_buckling_df["length"])

        pipe_properties = cfg["pipeline"]["pipe_properties"]
        system_properties = cfg["pipeline"]["system_properties"]
        friction = cfg["pipeline"]["friction_force"]
        tension_cfg = cfg["pipeline"]["tension"]

        pressure = pipe_properties[0]["internal_fluid"]["pressure"]
        A_system = system_properties["A"]
        A = pipe_properties[0]["section"]["A"]
        Ai = pipe_properties[0]["section"]["Ai"]

        logitudinal_force_term1 = (
            tension_cfg["start"] - tension_cfg["lay_tension"] + pressure * Ai
        )
        longitudinal_force_hot_end = [
            logitudinal_force_term1 - friction["axial"] * (item - length_array[0])
            for item in length_array
        ]
        lateral_buckling_df["longitudinal_force_hot_end"] = longitudinal_force_hot_end
        longitudinal_stress_hot_end = [
            item / A_system for item in longitudinal_force_hot_end
        ]
        lateral_buckling_df["longitudinal_stress_hot_end"] = longitudinal_stress_hot_end

        logitudinal_force_term1 = (
            tension_cfg["end"] - tension_cfg["lay_tension"] + pressure * Ai
        )
        longitudinal_force_cold_end = [
            logitudinal_force_term1 - friction["axial"] * (length_array[-1] - item)
            for item in length_array
        ]
        lateral_buckling_df["longitudinal_force_cold_end"] = longitudinal_force_cold_end
        longitudinal_stress_cold_end = [
            item / A for item in longitudinal_force_cold_end
        ]
        lateral_buckling_df[
            "longitudinal_stress_cold_end"
        ] = longitudinal_stress_cold_end

        return lateral_buckling_df

    def get_mesh(self, length):
        no_of_elements = 100
        element_array = list(range(0, no_of_elements + 1))
        length_factor_array = [item / no_of_elements for item in element_array]
        length_array = [item * length for item in length_factor_array]

        mesh = {
            "element": element_array,
            "length_factor": length_factor_array,
            "length": length_array,
        }

        return mesh

    def get_differential_temp(self, cfg, mesh):
        temperature_cfg = cfg["pipeline"]["crossection"][0]["temperature"]

        x_values = temperature_cfg["length_factor"]
        y_values = temperature_cfg["temperature"]

        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = mesh["length_factor"]
        differential_temperature_array = [
            round(float(f(item)), 3) for item in length_factor_array
        ]

        return differential_temperature_array

    def save_temperature_plot(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())

        plot_yml["data"]["groups"] = csv_groups
        columns = {"x": ["length"], "y": ["differential_temperature"]}
        plot_yml["master_settings"]["groups"]["columns"] = columns

        transform = [{"column": "length", "scale": 0.0254, "shift": 0}]
        plot_yml["master_settings"]["groups"]["transform"] = transform

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_temperature",
            "title": "Temperature along length",
            "xlabel": "Length (m)",
            "ylabel": "Differential Temperature (deg C)",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_force_plot_imp(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())

        plot_yml["data"]["groups"] = csv_groups

        columns = {
            "x": ["length"],
            "y": [
                "longitudinal_force_hot_end",
                "longitudinal_force_cold_end",
                "fully_restrained_axial_force",
                "effective_axial_force",
                "min_critical_buckling_load",
            ],
        }
        plot_yml["master_settings"]["groups"]["columns"] = columns

        transform = [
            {"column": "length", "scale": 0.0254, "shift": 0},
            {"column": "longitudinal_force_hot_end", "scale": 0.001, "shift": 0},
            {"column": "longitudinal_force_cold_end", "scale": 0.001, "shift": 0},
            {"column": "fully_restrained_axial_force", "scale": 0.001, "shift": 0},
            {"column": "effective_axial_force", "scale": 0.001, "shift": 0},
            {"column": "min_critical_buckling_load", "scale": 0.001, "shift": 0},
        ]
        plot_yml["master_settings"]["groups"]["transform"] = transform

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_force_imp",
            "title": "Force along length",
            "xlabel": "Length (m)",
            "ylabel": "Force (kips)",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_force_plot_met(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())

        plot_yml["data"]["groups"] = csv_groups

        columns = {
            "x": ["length"],
            "y": [
                "longitudinal_force_hot_end",
                "longitudinal_force_cold_end",
                "fully_restrained_axial_force",
                "effective_axial_force",
                "min_critical_buckling_load",
            ],
        }
        plot_yml["master_settings"]["groups"]["columns"] = columns

        transform = [
            {"column": "length", "scale": 0.0254, "shift": 0},
            {"column": "longitudinal_force_hot_end", "scale": 0.004449816, "shift": 0},
            {"column": "longitudinal_force_cold_end", "scale": 0.004449816, "shift": 0},
            {
                "column": "fully_restrained_axial_force",
                "scale": 0.004449816,
                "shift": 0,
            },
            {"column": "effective_axial_force", "scale": 0.004449816, "shift": 0},
            {"column": "min_critical_buckling_load", "scale": 0.004449816, "shift": 0},
        ]
        plot_yml["master_settings"]["groups"]["transform"] = transform

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_force_met",
            "title": "Force along length",
            "xlabel": "Length (m)",
            "ylabel": "Force (kN)",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_stress_plot_imp(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())

        plot_yml["data"]["groups"] = csv_groups

        columns = {
            "x": ["length"],
            "y": [
                "longitudinal_stress_hot_end",
                "longitudinal_stress_cold_end",
                "circumferential_stress",
                "thermal_stress",
                "longitudinal_stress_mid_zone",
            ],
        }
        plot_yml["master_settings"]["groups"]["columns"] = columns

        length_in_to_m = 0.0254
        stress_psi_to_ksi = 0.001
        transform = [
            {"column": "length", "scale": length_in_to_m, "shift": 0},
            {
                "column": "longitudinal_stress_hot_end",
                "scale": stress_psi_to_ksi,
                "shift": 0,
            },
            {
                "column": "longitudinal_stress_cold_end",
                "scale": stress_psi_to_ksi,
                "shift": 0,
            },
            {
                "column": "circumferential_stress",
                "scale": stress_psi_to_ksi,
                "shift": 0,
            },
            {"column": "thermal_stress", "scale": stress_psi_to_ksi, "shift": 0},
            {
                "column": "longitudinal_stress_mid_zone",
                "scale": stress_psi_to_ksi,
                "shift": 0,
            },
        ]
        plot_yml["master_settings"]["groups"]["transform"] = transform

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_stress_imp",
            "title": "Stress along length",
            "xlabel": "Length (m)",
            "ylabel": "Stress (ksi)",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_stress_plot_met(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())

        plot_yml["data"]["groups"] = csv_groups

        columns = {
            "x": ["length"],
            "y": [
                "longitudinal_stress_hot_end",
                "longitudinal_stress_cold_end",
                "circumferential_stress",
                "thermal_stress",
                "longitudinal_stress_mid_zone",
            ],
        }
        plot_yml["master_settings"]["groups"]["columns"] = columns

        length_in_to_m = 0.0254
        stress_psi_to_mpa = 0.00689476
        transform = [
            {"column": "length", "scale": length_in_to_m, "shift": 0},
            {
                "column": "longitudinal_stress_hot_end",
                "scale": stress_psi_to_mpa,
                "shift": 0,
            },
            {
                "column": "longitudinal_stress_cold_end",
                "scale": stress_psi_to_mpa,
                "shift": 0,
            },
            {
                "column": "circumferential_stress",
                "scale": stress_psi_to_mpa,
                "shift": 0,
            },
            {"column": "thermal_stress", "scale": stress_psi_to_mpa, "shift": 0},
            {
                "column": "longitudinal_stress_mid_zone",
                "scale": stress_psi_to_mpa,
                "shift": 0,
            },
        ]
        plot_yml["master_settings"]["groups"]["transform"] = transform

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_stress_met",
            "title": "Stress along length",
            "xlabel": "Length (m)",
            "ylabel": "Stress (MPa)",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_strain_plot(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())

        plot_yml["data"]["groups"] = csv_groups

        columns = {
            "x": ["length"],
            "y": ["longitudinal_strain_hot_end", "longitudinal_strain_cold_end"],
        }
        plot_yml["master_settings"]["groups"]["columns"] = columns

        transform = [
            {"column": "length", "scale": 0.0254, "shift": 0},
            {"column": "longitudinal_strain_hot_end", "scale": 1, "shift": 0},
            {"column": "longitudinal_strain_cold_end", "scale": 1, "shift": 0},
        ]
        plot_yml["master_settings"]["groups"]["transform"] = transform

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_strain",
            "title": "Strain along length",
            "xlabel": "Length (m)",
            "ylabel": "Strain",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)
