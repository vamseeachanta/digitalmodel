import os
import logging
import statistics
import logging
import pandas as pd
import numpy as np

from assetutilities.common.utilities import add_cwd_to_filename
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData
from assetutilities.common.visualizations import Visualization

read_data = ReadData()


class ShipFatigueAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg["inputs"]["files"]["lcf"]["file_type"] == "seasam_xtract":
            cfg = self.get_fatigue_states_and_stress_range(cfg)

        return cfg

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
        self.save_stress_output_as_csv(stress_output, cfg)

        cfg.update({"ship_design": {"stress_output": stress_output}})

        return cfg

    def save_stress_output_as_csv(self, stress_output, cfg):
        df = self.get_output_df_definition(cfg)

        for fatigue_state_pair in stress_output:
            df.loc[len(df)] = [np.nan] * len(list(df.columns))
            df['basename'].iloc[len(df)] = list(fatigue_state_pair.keys())[0]

            for by_type in ["coordinate", "element"]:
                for idx in range(0, len(fatigue_state_pair[basename][by_type])):
                    delta_stress = fatigue_state_pair[basename][by_type][0][
                        "delta_stress"
                    ]
                    element = fatigue_state_pair[basename]["coordinate"][0]["element"]

    def get_output_df_definition(self, cfg):
        columns = ["basename"]

        for by_type in ["coordinate", "element"]:
            if cfg["inputs"]["files"]["locations"][by_type]["flag"]:
                for idx in range(
                    0, len(cfg["inputs"]["files"]["locations"][by_type]["data"])
                ):
                    columns = columns + [
                        by_type + str(idx) + "_" + column
                        for column in [
                            "delta_stress",
                            "element",
                        ]
                    ]

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

            except Exception as e:
                logging.warning(
                    "   Could not get stress data for pair: {fatigue_state_pair['basename']} due to error: {e}"
                )

                stress_data_output_by_pair = {}

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
                    stress_range.append(stress_output["S"])

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
                    stress_range.append(stress_output["S"])

                delta_stress = stress_range[1] - stress_range[0]
                element_output.update({"delta_stress": round(float(delta_stress), 2)})
                element_output_array.append(element_output)

            stress_data_output_by_pair.update({"element": element_output_array})
            logging.info("   Getting stress data by elements ... COMPLETE")

        return stress_data_output_by_pair

    def get_stress_data_for_coordinate(self, coordinate, df, label):
        logging.info(
            f"      Getting stress data, state: {label}, coordinate: {coordinate}"
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
        stress_output.update({"S": round(float(df_filter["S"].mean()), 2)})
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
        stress_output.update({"S": round(float(df_filter["S"].mean()), 2)})
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

        add_columns = ["x_min", "x_max", "y_min", "y_max", "z_min", "S"]
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

            if True:
                df.loc[df_row, "S"] = statistics.mean(
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
            else:
                df.loc[df_row, "S"] = statistics.max(
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
