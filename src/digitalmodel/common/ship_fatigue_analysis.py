import os
import logging
import math
import statistics
import logging
import pandas as pd
import numpy as np

from assetutilities.common.utilities import add_cwd_to_filename
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData
from assetutilities.common.visualizations import Visualization
from digitalmodel.common.fatigue_analysis import FatigueAnalysis

read_data = ReadData()
fatigue_analysis = FatigueAnalysis()


class ShipFatigueAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg["inputs"]["files"]["lcf"]["file_type"] == "seasam_xtract":
            cfg = self.get_fatigue_states_and_stress_range(cfg)
        if cfg["inputs"]["calculation"] == "abs_combined_fatigue":
            df = self.get_lcf_fatigue_damage(cfg)
            df = self.get_abs_combined_fatigue(df)
            self.save_combined_fatigue_as_csv(cfg, df)

        return cfg

    def get_lcf_fatigue_damage(self, cfg):
        df = self.get_fatigue_damage_output_df(cfg["ship_design"]["stress_output"])

        cfg_fat_dam = fatigue_analysis.get_default_cfg()
        for idx in range(0, len(df)):
            cfg_fat_dam["inputs"].update(
                {
                    "SN": [
                        {
                            "s": df.iloc[idx]["delta_stress"],
                            "n_cycles": df.iloc[idx]["n_cycles"],
                            "thickness": df.iloc[idx]["thickness"],
                        }
                    ]
                }
            )
            cfg_fat_dam = fatigue_analysis.router(cfg_fat_dam)
            df.loc[idx, "lcf_damage"] = cfg_fat_dam["fatigue_analysis"]["damage"]

        return df

    def get_abs_combined_fatigue(self, df):
        delta = 0.02
        for idx in range(0, len(df)):
            lcf_damage = df.iloc[idx]["lcf_damage"]
            wave_damage = df.iloc[idx]["wave_damage"]
            combined_damage_numerator = (
                lcf_damage**2
                + wave_damage**2
                + 2 * delta * lcf_damage * wave_damage
            )
            combined_damage_denominator = math.sqrt(lcf_damage**2 + wave_damage**2)
            combined_damage = combined_damage_numerator / combined_damage_denominator
            df.loc[idx, "combined_damage"] = combined_damage

        return df

    def save_combined_fatigue_as_csv(self, cfg, df):
        file_name = os.path.join(
            cfg["Analysis"]["result_folder"],
            cfg["Analysis"]["file_name"] + "_combined_fatigue.csv",
        )
        df.to_csv(file_name, index=False)

    def get_fatigue_states_and_stress_range(self, cfg):
        fatigue_states = cfg["inputs"]["files"]["lcf"]["fatigue_states"]

        for fatigue_state in fatigue_states:
            if not os.path.isdir(fatigue_state):
                cwd = os.getcwd()
                fatigue_states[0] = add_cwd_to_filename(fatigue_state, cwd)
                if not os.path.isdir(fatigue_state):
                    raise ValueError(f"Invalid directory: {fatigue_states[0]}")

        state_0_files = self.get_files_in_directory(fatigue_states[0])
        state_1_files = self.get_files_in_directory(fatigue_states[1])

        fatigue_state_pairs = self.get_fatigue_state_pairs(state_0_files, state_1_files)

        stress_output = self.get_stress_ranges(cfg, fatigue_state_pairs)
        self.save_stress_output_as_csv(cfg, stress_output)

        cfg.update({"ship_design": {"stress_output": stress_output}})

        return cfg

    def save_stress_output_as_csv(self, cfg, stress_output):
        df = self.get_stress_output_df(stress_output)
        sress_output_file_name = os.path.join(
            cfg["Analysis"]["result_folder"],
            cfg["Analysis"]["file_name"] + "_stress_output.csv",
        )
        df.to_csv(sress_output_file_name, index=False)

    def get_fatigue_damage_output_df(self, stress_output):
        df = self.get_fatigue_df_definition()

        for fatigue_state_pair in stress_output:
            basename = list(fatigue_state_pair.keys())[0]

            if fatigue_state_pair[basename]["status"] == "Pass":
                for by_type in ["coordinate", "element"]:
                    for idx in range(0, len(fatigue_state_pair[basename][by_type])):
                        coordinate = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ]["coordinate"]
                        element = fatigue_state_pair[basename][by_type][idx]["state_0"][
                            "element"
                        ]["element"]
                        delta_stress = fatigue_state_pair[basename][by_type][idx][
                            "delta_stress"
                        ]
                        thickness = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["thickness"]
                        stress_method = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["stress_method"]
                        fatigue_curve = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["fatigue_curve"]
                        n_cycles = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["n_cycles"]
                        wave_damage = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["wave_damage"]
                        lcf_damage = np.nan
                        combined_damage = np.nan

            else:
                coordinate = np.nan
                element = np.nan
                delta_stress = np.nan
                thickness = np.nan
                stress_method = np.nan
                fatigue_curve = np.nan
                n_cycles = np.nan
                wave_damage = np.nan
                lcf_damage = np.nan
                combined_damage = np.nan

            df.loc[len(df)] = [
                basename,
                coordinate,
                element,
                delta_stress,
                fatigue_state_pair[basename]["status"],
                thickness,
                stress_method,
                fatigue_curve,
                n_cycles,
                lcf_damage,
                wave_damage,
                combined_damage,
            ]

        return df

    def get_stress_output_df(self, stress_output):
        df = self.get_stress_df_definition()

        for fatigue_state_pair in stress_output:
            basename = list(fatigue_state_pair.keys())[0]

            if fatigue_state_pair[basename]["status"] == "Pass":
                for by_type in ["coordinate", "element"]:
                    for idx in range(0, len(fatigue_state_pair[basename][by_type])):
                        coordinate = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ]["coordinate"]
                        element = fatigue_state_pair[basename][by_type][idx]["state_0"][
                            "element"
                        ]["element"]
                        delta_stress = fatigue_state_pair[basename][by_type][idx][
                            "delta_stress"
                        ]
                        thickness = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["thickness"]
                        stress_method = fatigue_state_pair[basename][by_type][idx][
                            "state_0"
                        ][by_type]["stress_method"]
            else:
                coordinate = np.nan
                element = np.nan
                delta_stress = np.nan
                thickness = np.nan
                stress_method = np.nan

            df.loc[len(df)] = [
                basename,
                coordinate,
                element,
                delta_stress,
                fatigue_state_pair[basename]["status"],
                thickness,
                stress_method,
            ]

        return df

    def get_stress_df_definition(self):
        columns = (
            ["basename"]
            + ["coordinate", "element"]
            + ["delta_stress"]
            + ["status"]
            + ["thickness", "stress_method"]
        )

        df = pd.DataFrame(columns=columns)

        return df

    def get_fatigue_df_definition(self):
        columns = (
            ["basename"]
            + ["coordinate", "element"]
            + ["delta_stress"]
            + ["status"]
            + ["thickness", "stress_method", "fatigue_curve", "n_cycles"]
            + ["lcf_damage", "wave_damage", "combined_damage"]
        )

        df = pd.DataFrame(columns=columns)

        return df

    def get_stress_ranges(self, cfg, fatigue_state_pairs):
        stress_output = []
        for fatigue_state_pair in fatigue_state_pairs:
            logging.info(
                f"Getting stress data for fatigue pair: {fatigue_state_pair['basename']}"
            )
            try:
                fatigue_state_pair = self.read_files_and_get_data_as_df(
                    fatigue_state_pair
                )
                stress_data_output_by_pair = self.get_stress_data_for_pair(
                    cfg, fatigue_state_pair
                )
                status = "Pass"

            except Exception as e:
                logging.warning(
                    f"   Could not get stress data for pair: {fatigue_state_pair['basename']} due to error: {e}"
                )

                stress_data_output_by_pair = {}
                status = "Fail"

            stress_data_output_by_pair.update({"status": status})
            stress_output.append(
                {fatigue_state_pair["basename"]: stress_data_output_by_pair}
            )

        return stress_output

    def get_stress_data_for_pair(self, cfg, fatigue_state_pair):
        stress_data_output_by_pair = {}
        if cfg["inputs"]["files"]["locations"]["coordinate"]["flag"]:
            logging.info("   Getting stress data by coordinates ... START")
            coordinate_data = cfg["inputs"]["files"]["locations"]["coordinate"]["data"]

            coordinate_output_array = []
            for coordinate in coordinate_data:
                coordinate_output = {}
                stress_range = []
                for label in ["state_0", "state_1"]:
                    df = fatigue_state_pair[label + "_df"]

                    stress_output = self.get_stress_data_for_coordinate(
                        coordinate, df, label
                    )
                    coordinate_output.update({label: stress_output})
                    if coordinate["stress_method"] == "mean":
                        stress_range.append(stress_output["S_mean"])
                    elif coordinate["stress_method"] == "max":
                        stress_range.append(stress_output["S_max"])
                    else:
                        raise ValueError(
                            f"Invalid stress method: {coordinate['stress_method']}"
                        )

                delta_stress = stress_range[1] - stress_range[0]
                coordinate_output.update(
                    {"delta_stress": round(float(delta_stress), 2)}
                )
                coordinate_output_array.append(coordinate_output)

            stress_data_output_by_pair.update({"coordinate": coordinate_output_array})
            logging.info("   Getting stress data by coordinates ... COMPLETE")

        if cfg["inputs"]["files"]["locations"]["element"]["flag"]:
            logging.info("   Getting stress data by elements ... START")
            element_data = cfg["inputs"]["files"]["locations"]["element"]["data"]

            element_output_array = []
            for element_dict in element_data:
                element_output = {}
                stress_range = []
                for label in ["state_0", "state_1"]:
                    df = fatigue_state_pair[label + "_df"]

                    stress_output = self.get_stress_data_for_element(
                        element_dict, df, label
                    )
                    element_output.update({label: stress_output})
                    if element_dict["stress_method"] == "mean":
                        stress_range.append(stress_output["S_mean"])
                    elif element_dict["stress_method"] == "max":
                        stress_range.append(stress_output["S_max"])
                    else:
                        raise ValueError(
                            f"Invalid stress method: {element_dict['stress_method']}"
                        )

                delta_stress = stress_range[1] - stress_range[0]
                element_output.update({"delta_stress": round(float(delta_stress), 2)})
                element_output_array.append(element_output)

            stress_data_output_by_pair.update({"element": element_output_array})
            logging.info("   Getting stress data by elements ... COMPLETE")

        return stress_data_output_by_pair

    def get_stress_data_for_coordinate(self, coordinate, df, label):
        logging.info(
            f"      Getting stress data, state: {label}, coordinate: {coordinate['coordinate']}"
        )
        stress_output = {}

        df_filter = df[
            (df["x_min"] <= coordinate["x"])
            & (df["x_max"] >= coordinate["x"])
            & (df["y_min"] <= coordinate["y"])
            & (df["y_max"] >= coordinate["y"])
            & (df["z_min"] <= coordinate["z"])
            & (df["z_max"] >= coordinate["z"])
        ]
        stress_output.update({"coordinate": coordinate.copy()})
        stress_output.update({"element": df_filter["Element"].to_list()})
        stress_output.update({"S_mean": round(float(df_filter["S_mean"].mean()), 2)})
        stress_output.update({"S_max": round(float(df_filter["S_max"].max()), 2)})
        stress_output.update({"label": label})

        return stress_output

    def get_stress_data_for_element(self, element_dict, df, label):
        logging.info(
            f"      Getting stress data for state: {label}, element: {element_dict['element']}"
        )
        stress_output = {}

        df_filter = df[df["Element"] == element_dict["element"]]

        x = statistics.mean(df_filter["x_min"].tolist() + df_filter["x_max"].tolist())
        y = statistics.mean(df_filter["y_min"].tolist() + df_filter["y_max"].tolist())
        z = statistics.mean(df_filter["z_min"].tolist() + df_filter["z_max"].tolist())
        coordinate = {
            "x": round(float(x), 2),
            "y": round(float(y), 2),
            "z": round(float(z), 2),
        }
        stress_output.update({"coordinate": coordinate})

        stress_output.update({"element": element_dict.copy()})
        stress_output.update({"S_mean": round(float(df_filter["S_mean"].mean()), 2)})
        stress_output.update({"S_max": round(float(df_filter["S_max"].mean()), 2)})
        stress_output.update({"label": label})

        return stress_output

    def read_files_and_get_data_as_df(self, fatigue_state_pair):
        status = {}
        for state in ["state_0", "state_1"]:
            state_df = self.read_seasam_xtract(fatigue_state_pair[state])
            transformed_df = self.transform_df_for_stress_analysis(state_df)
            fatigue_state_pair.update({(state + "_df"): transformed_df})
            if len(transformed_df) > 0:
                status.update({state + "_data": "Pass"})
            else:
                status.update({state + "_data": "Fail"})
        fatigue_state_pair.update({"status": status})

        return fatigue_state_pair

    def transform_df_for_stress_analysis(self, df):
        # Convert columns to float
        df.replace("N/A", np.nan, inplace=True)
        for column in df.columns:
            try:
                df[column] = df[column].astype(float)
            except:
                logging.debug(f"      Could not convert column {column} to float")

        add_columns = ["x_min", "x_max", "y_min", "y_max", "z_min", "S_mean", "S_max"]
        for column in add_columns:
            df[column] = np.nan

        for df_row in range(0, len(df)):
            df.loc[df_row, "x_min"] = min(
                df.iloc[df_row]["X-coord(1)"],
                df.iloc[df_row]["X-coord(2)"],
                df.iloc[df_row]["X-coord(3)"],
                df.iloc[df_row]["X-coord(4)"],
                df.iloc[df_row]["X-coord(5)"],
                df.iloc[df_row]["X-coord(6)"],
                df.iloc[df_row]["X-coord(7)"],
                df.iloc[df_row]["X-coord(8)"],
            )
            df.loc[df_row, "x_max"] = max(
                df.iloc[df_row]["X-coord(1)"],
                df.iloc[df_row]["X-coord(2)"],
                df.iloc[df_row]["X-coord(3)"],
                df.iloc[df_row]["X-coord(4)"],
                df.iloc[df_row]["X-coord(5)"],
                df.iloc[df_row]["X-coord(6)"],
                df.iloc[df_row]["X-coord(7)"],
                df.iloc[df_row]["X-coord(8)"],
            )
            df.loc[df_row, "y_min"] = min(
                df.iloc[df_row]["Y-coord(1)"],
                df.iloc[df_row]["Y-coord(2)"],
                df.iloc[df_row]["Y-coord(3)"],
                df.iloc[df_row]["Y-coord(4)"],
                df.iloc[df_row]["Y-coord(5)"],
                df.iloc[df_row]["Y-coord(6)"],
                df.iloc[df_row]["Y-coord(7)"],
                df.iloc[df_row]["Y-coord(8)"],
            )
            df.loc[df_row, "y_max"] = max(
                df.iloc[df_row]["Y-coord(1)"],
                df.iloc[df_row]["Y-coord(2)"],
                df.iloc[df_row]["Y-coord(3)"],
                df.iloc[df_row]["Y-coord(4)"],
                df.iloc[df_row]["Y-coord(5)"],
                df.iloc[df_row]["Y-coord(6)"],
                df.iloc[df_row]["Y-coord(7)"],
                df.iloc[df_row]["Y-coord(8)"],
            )
            df.loc[df_row, "z_min"] = min(
                df.iloc[df_row]["Z-coord(1)"],
                df.iloc[df_row]["Z-coord(2)"],
                df.iloc[df_row]["Z-coord(3)"],
                df.iloc[df_row]["Z-coord(4)"],
                df.iloc[df_row]["Z-coord(5)"],
                df.iloc[df_row]["Z-coord(6)"],
                df.iloc[df_row]["Z-coord(7)"],
                df.iloc[df_row]["Z-coord(8)"],
            )
            df.loc[df_row, "z_max"] = max(
                df.iloc[df_row]["Z-coord(1)"],
                df.iloc[df_row]["Z-coord(2)"],
                df.iloc[df_row]["Z-coord(3)"],
                df.iloc[df_row]["Z-coord(4)"],
                df.iloc[df_row]["Z-coord(5)"],
                df.iloc[df_row]["Z-coord(6)"],
                df.iloc[df_row]["Z-coord(7)"],
                df.iloc[df_row]["Z-coord(8)"],
            )

            if "VONMISES(1)" in df.columns:
                stress_nomenclature = "VONMISES"
            elif "SIGXX(1)" in df.columns:
                stress_nomenclature = "SIGXX"
            elif "SIGYY(1)" in df.columns:
                stress_nomenclature = "SIGYY"
            elif "TAUXY(1)" in df.columns:
                stress_nomenclature = "TAUXY"
            else:
                raise ValueError("Could not find stress column")

            df.loc[df_row, "S_mean"] = statistics.mean(
                [
                    df.iloc[df_row][stress_nomenclature + "(1)"],
                    df.iloc[df_row][stress_nomenclature + "(2)"],
                    df.iloc[df_row][stress_nomenclature + "(3)"],
                    df.iloc[df_row][stress_nomenclature + "(4)"],
                    df.iloc[df_row][stress_nomenclature + "(5)"],
                    df.iloc[df_row][stress_nomenclature + "(6)"],
                    df.iloc[df_row][stress_nomenclature + "(7)"],
                    df.iloc[df_row][stress_nomenclature + "(8)"],
                ]
            )

            df.loc[df_row, "S_max"] = max(
                [
                    df.iloc[df_row][stress_nomenclature + "(1)"],
                    df.iloc[df_row][stress_nomenclature + "(2)"],
                    df.iloc[df_row][stress_nomenclature + "(3)"],
                    df.iloc[df_row][stress_nomenclature + "(4)"],
                    df.iloc[df_row][stress_nomenclature + "(5)"],
                    df.iloc[df_row][stress_nomenclature + "(6)"],
                    df.iloc[df_row][stress_nomenclature + "(7)"],
                    df.iloc[df_row][stress_nomenclature + "(8)"],
                ]
            )

        return df

    def read_seasam_xtract(self, seasam_xtract_file):
        cfg = {
            "io": seasam_xtract_file,
            "start_line": 6,
            "end_line": 6,
            "DataFrame": False,
        }

        column_data = (
            read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg)
        )
        columns = [data[0] for data in column_data]

        cfg = {
            "io": seasam_xtract_file,
            "start_line": 7,
            "columns": columns,
            "DataFrame": True,
        }

        df = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg)

        return df

    def get_fatigue_state_pairs(self, state_0_files, state_1_files):
        fatigue_state_pairs = []
        for state_0_index in range(0, len(state_0_files["basenames"])):
            basename_0 = state_0_files["basenames"][state_0_index]
            basename_index_1 = state_1_files["basenames"].index(basename_0)
            fatigue_state_pair = {
                "state_0": state_0_files["files"][state_0_index],
                "state_1": state_1_files["files"][basename_index_1],
                "basename": basename_0,
            }
            fatigue_state_pairs.append(fatigue_state_pair)

        return fatigue_state_pairs

    def get_files_in_directory(self, directory):
        from assetutilities.engine import engine

        cfg_file_management = {
            "basename": "file_management",
            "files": {
                "files_in_current_directory": {"flag": False, "directory": directory},
                "extension": "txt",
            },
        }
        cfg_result = engine(inputfile=None, cfg=cfg_file_management)
        files = cfg_result["file_management"]

        return files
