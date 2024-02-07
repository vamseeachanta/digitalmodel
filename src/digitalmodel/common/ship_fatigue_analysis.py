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
            cfg, stress_output = self.get_fatigue_states_and_stress_timetrace(cfg)
        if cfg["inputs"]["calculation"] == "abs_combined_fatigue":
            if cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
                df = self.get_lcf_timetrace_rainflow_and_fatigue_damage_P1_fast(cfg, stress_output)
            else:
                df = self.get_lcf_timetrace_rainflow_and_fatigue_damage(cfg)
                df = self.get_abs_combined_fatigue(df)
            self.save_combined_fatigue_as_csv(cfg, df)
            self.save_combined_fatigue_summary_as_csv(cfg, df)

        return cfg

    def get_lcf_sn_fatigue_damage(self, cfg):
        # TODO Not tested. May not work. DELETE by 01 March 2024.
        logging.info("Calculating LCF fatigue damage by SN cycles ... START")
        df = self.get_fatigue_damage_output_df_for_timetrace(
            cfg["ship_design"]["stress_output"], cfg
        )

        cfg_fat_dam = fatigue_analysis.get_default_sn_cfg()
        for idx in range(0, len(df)):
            cfg_fat_dam["inputs"].update(
                {
                    "SN": [
                        {
                            "s": df.iloc[idx]["stress_timetrace"],
                            "n_cycles": df.iloc[idx]["n_cycles"],
                            "thickness": df.iloc[idx]["thickness"],
                        }
                    ],
                    "fatigue_curve": df.iloc[idx]["fatigue_curve"],
                }
            )
            cfg_fat_dam = fatigue_analysis.router(cfg_fat_dam)
            df.loc[idx, "lcf_damage"] = cfg_fat_dam["fatigue_analysis"]["damage"]

        logging.info("Calculating LCF fatigue damage by SN cycles ... COMPLETE")

        return df

    def get_lcf_timetrace_rainflow_and_fatigue_damage_P1_fast(self, cfg, stress_output):
        logging.info("Calculating LCF fatigue damage by Timetraces ... START    P1 Fast")
        stress_df = self.get_stress_output_df_P1_fast(stress_output, cfg)
        df = pd.DataFrame(columns=["element", "lcf_damage"])
        df['element'] = stress_df['element']

        fatigue_curve = cfg['inputs']['files']['locations']['element']['settings']['fatigue_curve']
        n_traces = cfg['inputs']['files']['locations']['element']['settings']['n_traces']
        thickness = cfg['inputs']['files']['locations']['element']['settings']['thickness']
        
        for idx in range(0, len(stress_df)):

            if idx % 5000 == 0:
                logging.info(f"   Calculating LCF fatigue damage for elements: {idx} to {idx+1000} ...")

            cfg_fat_dam = fatigue_analysis.get_default_timetrace_cfg()
            cfg_fat_dam["inputs"].update(
                {
                    "timetraces": [
                        {
                            "s_trace": stress_df.iloc[idx]["stress_timetrace"],
                            "n_traces": n_traces,
                            "thickness": thickness,
                        }
                    ],
                    "fatigue_curve": fatigue_curve,
                }
            )

            cfg_fat_dam = fatigue_analysis.router(cfg_fat_dam)

            df.loc[idx, "lcf_damage"] = cfg_fat_dam["fatigue_analysis"]["total_damage"]

        logging.info("Calculating LCF fatigue damage  by Timetraces ... COMPLETE")

        return df

    def get_lcf_timetrace_rainflow_and_fatigue_damage(self, cfg):
        logging.info("Calculating LCF fatigue damage by Timetraces ... START")
        df = self.get_fatigue_damage_output_df_for_timetrace(
            cfg["ship_design"]["stress_output"], cfg
        )

        cfg_fat_dam = fatigue_analysis.get_default_timetrace_cfg()

        for idx in range(0, len(df)):
            if idx % 1000 == 0:
                logging.info(f"   Calculating LCF fatigue damage for elements: {idx} to {idx+1000} ...")
            if cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
                fatigue_curve = cfg['inputs']['files']['locations']['element']['settings']['fatigue_curve']
                n_traces = cfg['inputs']['files']['locations']['element']['settings']['n_traces']
                thickness = cfg['inputs']['files']['locations']['element']['settings']['thickness']
            else:
                df.loc[idx, "fatigue_curve"] = fatigue_curve
                n_traces = df.iloc[idx]["n_traces"]
                thickness = df.iloc[idx]["thickness"]

            cfg_fat_dam["inputs"].update(
                {
                    "timetraces": [
                        {
                            "s_trace": df.iloc[idx]["stress_timetrace"],
                            "n_traces": n_traces,
                            "thickness": thickness,
                        }
                    ],
                    "fatigue_curve": fatigue_curve,
                }
            )
            cfg_fat_dam = fatigue_analysis.router(cfg_fat_dam)
            df.loc[idx, "lcf_damage"] = cfg_fat_dam["fatigue_analysis"]["total_damage"]
            rainflow = [
                timetrace["rainflow"]
                for timetrace in cfg_fat_dam["fatigue_analysis"]["timetraces"]
            ]
            df.loc[idx, "rainflow"] = rainflow
            
            if not cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
                cfg.update({"fatigue_analysis": cfg_fat_dam["fatigue_analysis"]})

        logging.info("Calculating LCF fatigue damage  by Timetraces ... COMPLETE")

        return df

    def get_abs_combined_fatigue(self, df):
        logging.info("Calculating ABS combined fatigue ... START")
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

        logging.info("Calculating ABS combined fatigue ... COMPLETE")

        return df

    def save_combined_fatigue_as_csv(self, cfg, df):
        file_name = os.path.join(
            cfg["Analysis"]["result_folder"],
            cfg["Analysis"]["file_name"] + "_combined_fatigue.csv",
        )
        df.to_csv(file_name, index=False)

    def save_combined_fatigue_summary_as_csv(self, cfg, df):
        file_name = os.path.join(
            cfg["Analysis"]["result_folder"],
            cfg["Analysis"]["file_name"] + "_combined_fatigue_summary.csv",
        )
        df_summary = df[
            ["element", "lcf_damage"]
        ].copy()
        df_summary.to_csv(file_name, index=False)

    def get_fatigue_states_and_stress_timetrace(self, cfg):
        fatigue_states = cfg["inputs"]["files"]["lcf"]["fatigue_states"]

        if "directory" in fatigue_states[0]:
            state_files = self.get_state_files_by_directory(fatigue_states)
            fatigue_states_analysis = self.get_fatigue_states_by_directory(state_files)
        elif "file_name" in fatigue_states[0]:
            fatigue_states_analysis = self.get_fatigue_states_by_file_name(
                fatigue_states
            )

        stress_output = self.get_stress_timetrace(cfg, fatigue_states_analysis)

        self.save_stress_output_as_csv(cfg, stress_output)
        if not cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
            cfg.update({"ship_design": {"stress_output": stress_output}})

        return cfg, stress_output

    def get_fatigue_states_by_file_name(self, fatigue_states):
        state_files = []
        for fatigue_state_indx in range(0, len(fatigue_states)):
            valid_file_flag = False

            fatigue_state = fatigue_states[fatigue_state_indx]

            if not os.path.isfile(fatigue_state["file_name"]):
                cwd = os.getcwd()
                fatigue_state["file_name"] = add_cwd_to_filename(
                    fatigue_state["file_name"], cwd
                )
                fatigue_states[fatigue_state_indx] = fatigue_state
            else:
                valid_file_flag = True

            if not valid_file_flag and not os.path.isfile(fatigue_state["file_name"]):
                valid_file_flag = False
                raise ValueError(f"Invalid file: {fatigue_state['file_name']}")

            state_files.append(
                {
                    "state_file": fatigue_state["file_name"],
                    "label": fatigue_state["label"],
                }
            )

        fatigue_states_analysis = [
            {
                "state_files": state_files,
                "basename": None,
            }
        ]

        return fatigue_states_analysis

    def get_state_files_by_directory(self, fatigue_states):
        state_files = []
        for fatigue_state_indx in range(0, len(fatigue_states)):
            valid_directory_flag = False

            fatigue_state = fatigue_states[fatigue_state_indx]

            if not os.path.isdir(fatigue_state["directory"]):
                cwd = os.getcwd()
                fatigue_state["directory"] = add_cwd_to_filename(
                    fatigue_state["directory"], cwd
                )
                fatigue_states[fatigue_state_indx] = fatigue_state
            else:
                valid_directory_flag = True

            if not valid_directory_flag and not os.path.isdir(
                fatigue_state["directory"]
            ):
                valid_directory_flag = False
                raise ValueError(f"Invalid directory: {fatigue_state['directory']}")
            files = self.get_files_in_directory(fatigue_state["directory"])
            files.update({"label": fatigue_state["label"]})
            state_files.append(files)

        return state_files

    def save_stress_output_as_csv(self, cfg, stress_output):
        logging.info("Saving stress output as csv ... START")
        if cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
            df = self.get_stress_output_df_P1_fast(stress_output, cfg)
        else:
            df = self.get_stress_output_df(stress_output, cfg)
        sress_output_file_name = os.path.join(
            cfg["Analysis"]["result_folder"],
            cfg["Analysis"]["file_name"] + "_stress_output.csv",
        )
        df.to_csv(sress_output_file_name, index=False)
        
        logging.info("Saving stress output as csv ... COMPLETE")

    def get_fatigue_damage_output_df_for_timetrace(self, stress_output, cfg):
        df = self.get_fatigue_df_definition_for_timetrace()

        for fatigue_state_set in stress_output:
            basename = list(fatigue_state_set.keys())[0]

            labels = [
                fatigue_state["label"]
                for fatigue_state in cfg["inputs"]["files"]["lcf"]["fatigue_states"]
            ]
            if fatigue_state_set[basename]["status"] == "Pass":
                for by_type in ["coordinate", "element"]:
                    if by_type in fatigue_state_set[basename].keys():
                        for idx in range(0, len(fatigue_state_set[basename][by_type])):
                            coordinate = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ]["coordinate"]
                            if by_type == "coordinate":
                                element = fatigue_state_set[basename][by_type][idx][
                                    labels[0]
                                ]["element"]
                            elif by_type == "element":
                                element = fatigue_state_set[basename][by_type][idx][
                                    labels[0]
                                ]["element"]["element"]
                            stress_timetrace = fatigue_state_set[basename][by_type][
                                idx
                            ]["stress_timetrace"]
                            rainflow = np.nan

                            thickness = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["thickness"]
                            stress_method = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["stress_method"]
                            fatigue_curve = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["fatigue_curve"]
                            n_traces = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["n_traces"]
                            wave_damage = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["wave_damage"]
                            lcf_damage = np.nan
                            combined_damage = np.nan

                            df.loc[len(df)] = [
                                basename,
                                coordinate,
                                element,
                                stress_timetrace,
                                rainflow,
                                fatigue_state_set[basename]["status"],
                                thickness,
                                stress_method,
                                fatigue_curve,
                                n_traces,
                                lcf_damage,
                                wave_damage,
                                combined_damage,
                            ]

            else:
                coordinate = np.nan
                element = np.nan
                stress_timetrace = np.nan
                rainflow = np.nan
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
                    stress_timetrace,
                    rainflow,
                    fatigue_state_set[basename]["status"],
                    thickness,
                    stress_method,
                    fatigue_curve,
                    n_cycles,
                    lcf_damage,
                    wave_damage,
                    combined_damage,
                ]

        return df

    def get_stress_output_df_P1_fast(self, stress_output, cfg):

        df = pd.DataFrame(stress_output[0][None]['element'])

        return df

    def get_stress_output_df(self, stress_output, cfg):
        df = self.get_stress_df_definition()

        labels = [
            fatigue_state["label"]
            for fatigue_state in cfg["inputs"]["files"]["lcf"]["fatigue_states"]
        ]
        for fatigue_state_set in stress_output:
            basename = list(fatigue_state_set.keys())[0]

            if fatigue_state_set[basename]["status"] == "Pass":
                for by_type in ["coordinate", "element"]:
                    if by_type in fatigue_state_set[basename].keys():
                        for idx in range(0, len(fatigue_state_set[basename][by_type])):
                            coordinate = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ]["coordinate"]
                            if by_type == "coordinate":
                                element = fatigue_state_set[basename][by_type][idx][
                                    labels[0]
                                ]["element"]
                            elif by_type == "element":
                                element = fatigue_state_set[basename][by_type][idx][
                                    labels[0]
                                ]["element"]["element"]
                            stress_timetrace = fatigue_state_set[basename][by_type][
                                idx
                            ]["stress_timetrace"]
                            thickness = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["thickness"]
                            stress_method = fatigue_state_set[basename][by_type][idx][
                                labels[0]
                            ][by_type]["stress_method"]

                            df.loc[len(df)] = [
                                basename,
                                coordinate,
                                element,
                                stress_timetrace,
                                fatigue_state_set[basename]["status"],
                                thickness,
                                stress_method,
                            ]

            else:
                coordinate = np.nan
                element = np.nan
                stress_timetrace = np.nan
                thickness = np.nan
                stress_method = np.nan

                df.loc[len(df)] = [
                    basename,
                    coordinate,
                    element,
                    stress_timetrace,
                    fatigue_state_set[basename]["status"],
                    thickness,
                    stress_method,
                ]

        return df

    def get_stress_df_definition(self):
        columns = (
            ["basename"]
            + ["coordinate", "element"]
            + ["stress_timetrace"]
            + ["status"]
            + ["thickness", "stress_method"]
        )

        df = pd.DataFrame(columns=columns)

        return df

    def get_fatigue_df_definition_for_timetrace(self):
        columns = (
            ["basename"]
            + ["coordinate", "element"]
            + ["stress_timetrace"]
            + ["rainflow"]
            + ["status"]
            + ["thickness", "stress_method", "fatigue_curve", "n_traces"]
            + ["lcf_damage", "wave_damage", "combined_damage"]
        )

        df = pd.DataFrame(columns=columns)

        return df

    def get_stress_timetrace(self, cfg, fatigue_state_sets):
        stress_output = []
        for fatigue_state_set in fatigue_state_sets:
            logging.info(
                f"Getting stress data for fatigue pair: {fatigue_state_set['basename']}"
            )
            try:
                fatigue_state_set = self.read_files_and_get_data_as_df(
                    fatigue_state_set, cfg
                )
                if cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
                    stress_data_by_set = self.get_stress_data_by_set_P1_fast(cfg, fatigue_state_set)
                else:
                    stress_data_by_set = self.get_stress_data_by_set(cfg, fatigue_state_set)
                status = "Pass"

            except Exception as e:
                logging.warning(
                    f"   Could not get stress data for pair: {fatigue_state_set['basename']} due to error: {e}"
                )

                stress_data_by_set = {}
                status = "Fail"

            stress_data_by_set.update({"status": status})
            stress_output.append({fatigue_state_set["basename"]: stress_data_by_set})

        return stress_output

    def get_stress_data_by_set_P1_fast(self, cfg, fatigue_state_pair):
        stress_data_output_by_pair = {}
        labels = [
            fatigue_state_pair["state_files"][i]["label"]
            for i in range(0, len(fatigue_state_pair["state_files"]))
        ]

        logging.info("   Getting stress data by elements ... START")
        element_data = cfg["inputs"]["files"]["locations"]["element"]["data"]
        if element_data == "All":
            df = fatigue_state_pair[labels[0] + "_df"]
            element_data = list(df["Element"])
            element_data = [{"element": element} for element in element_data]

        element_output_array = []
        for element_dict in element_data:
            element_output = {}
            stress_timetrace = []
            for label in labels:
                df = fatigue_state_pair[label + "_df"]

                stress_output = self.get_stress_data_for_element_P1_fast(
                    element_dict, df, label
                )
                stress_timetrace.append(stress_output["S"])

            element_output.update({"stress_timetrace": stress_timetrace})
            element_output.update({"element": element_dict['element']})
            element_output_array.append(element_output)

        stress_data_output_by_pair.update({"element": element_output_array})
        logging.info("   Getting stress data by elements ... COMPLETE")

        return stress_data_output_by_pair

    def get_stress_data_by_set(self, cfg, fatigue_state_pair):
        stress_data_output_by_pair = {}

        labels = [
            fatigue_state_pair["state_files"][i]["label"]
            for i in range(0, len(fatigue_state_pair["state_files"]))
        ]

        if cfg["inputs"]["files"]["locations"]["coordinate"]["flag"]:
            logging.info("   Getting stress data by coordinates ... START")
            coordinate_data = cfg["inputs"]["files"]["locations"]["coordinate"]["data"]

            coordinate_output_array = []
            for coordinate in coordinate_data:
                coordinate_output = {}
                stress_timetrace = []
                for label in labels:
                    df = fatigue_state_pair[label + "_df"]

                    stress_output = self.get_stress_data_for_coordinate(
                        coordinate, df, label
                    )
                    coordinate_output.update({label: stress_output})
                    if coordinate["stress_method"] == "mean":
                        stress_timetrace.append(stress_output["S_mean"])
                    elif coordinate["stress_method"] == "max":
                        if stress_output["S_max"] < 0:
                            stress_timetrace.append(stress_output["S_min"])
                        else:
                            stress_timetrace.append(stress_output["S_max"])
                    else:
                        raise ValueError(
                            f"Invalid stress method: {coordinate['stress_method']}"
                        )

                coordinate_output.update({"stress_timetrace": stress_timetrace})
                coordinate_output_array.append(coordinate_output)

            stress_data_output_by_pair.update({"coordinate": coordinate_output_array})
            logging.info("   Getting stress data by coordinates ... COMPLETE")

        if cfg["inputs"]["files"]["locations"]["element"]["flag"]:
            logging.info("   Getting stress data by elements ... START")
            element_data = cfg["inputs"]["files"]["locations"]["element"]["data"]
            if element_data == "All":
                df = fatigue_state_pair[labels[0] + "_df"]
                element_data = list(df["Element"])
                element_data = [{"element": element} for element in element_data]

            element_output_array = []
            element_settings = cfg["inputs"]["files"]["locations"]["element"][
                "settings"
            ].copy()
            for element_dict in element_data:
                element_settings.update(element_dict)
                element_dict = element_settings.copy()
                element_output = {}
                stress_timetrace = []
                for label in labels:
                    df = fatigue_state_pair[label + "_df"]

                    stress_output = self.get_stress_data_for_element(
                        element_dict, df, label
                    )
                    element_output.update({label: stress_output})
                    if element_dict["stress_method"] == "mean":
                        stress_timetrace.append(stress_output["S_mean"])
                    elif element_dict["stress_method"] == "max":
                        if stress_output["S_max"] < 0:
                            stress_timetrace.append(stress_output["S_min"])
                        else:
                            stress_timetrace.append(stress_output["S_max"])
                    else:
                        raise ValueError(
                            f"Invalid stress method: {element_dict['stress_method']}"
                        )

                element_output.update({"stress_timetrace": stress_timetrace})
                element_output_array.append(element_output)

            stress_data_output_by_pair.update({"element": element_output_array})
            logging.info("   Getting stress data by elements ... COMPLETE")

        return stress_data_output_by_pair

    def get_stress_data_for_coordinate(self, coordinate, df, label):
        logging.info(
            f"      Getting stress data, state: {label}, coordinate: x {coordinate['x']}, y {coordinate['y']}, x {coordinate['z']}"
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
        stress_output.update({"S_mean": round(float(df_filter["S_mean"].mean()), 0)})
        stress_output.update({"S_max": round(float(df_filter["S_max"].max()), 0)})
        stress_output.update({"S_min": round(float(df_filter["S_min"].min()), 0)})
        stress_output.update({"label": label})

        return stress_output

    def get_stress_data_for_element_P1_fast(self, element_dict, df, label):
        # logging.info(
        #     f"      Getting stress data for state: {label}, element: {element_dict['element']}"
        # )

        df_filter = df[df["Element"] == element_dict["element"]]

        stress_output = {'S': round(float(df_filter["S"].mean()), 0), 'Element': element_dict["element"]}
        stress_output.update({"element": element_dict.copy()})

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
        stress_output.update({"S_mean": round(float(df_filter["S_mean"].mean()), 0)})
        stress_output.update({"S_max": round(float(df_filter["S_max"].max()), 0)})
        stress_output.update({"S_min": round(float(df_filter["S_min"].min()), 0)})
        stress_output.update({"label": label})

        return stress_output

    def read_files_and_get_data_as_df(self, fatigue_state_pair, cfg):
        status = {}
        for state_file in fatigue_state_pair["state_files"]:
            state_df = self.read_seasam_xtract(state_file["state_file"])
            if 'sesam_extract_type' in cfg['inputs'] and cfg['inputs']['sesam_extract_type'] == 'fast_optimum':
                transformed_df = self.transform_df_for_P1_stress_fast_process(state_df, state_file, cfg)
            else:
                transformed_df = self.transform_df_for_stress_analysis(state_df, state_file)
            fatigue_state_pair.update({(state_file["label"] + "_df"): transformed_df})
            if len(transformed_df) > 0:
                status.update({state_file["label"]: "Pass"})
            else:
                status.update({state_file["label"]: "Fail"})
        fatigue_state_pair.update({"status": status})

        return fatigue_state_pair

    def transform_df_for_P1_stress_fast_process(self, df, state_file, cfg):
        logging.info(
            f"      Transforming data for: {state_file['state_file']} ... START"
        )

        df.replace("N/A", np.nan, inplace=True)
        for column in df.columns:
            try:
                df[column] = df[column].astype(float)
            except:
                logging.debug(f"      Could not convert column {column} to float")

        stress_nomenclature = "P1"
        stress_method = cfg['inputs']['files']['locations']['element']['settings']['stress_method']

        df_stress = df[[stress_nomenclature + "(1)", stress_nomenclature + "(2)", stress_nomenclature + "(3)", stress_nomenclature + "(4)"]]
        if stress_method == "mean":
            df['S'] = round(df_stress.mean(axis=1), 0)
        elif stress_method == "max":
            df['S'] = round(df_stress.max(axis=1), 0)
        elif stress_method == "min":
            df['S'] = round(df_stress.min(axis=1), 0)
        else:
            raise ValueError(f"Stress summary method NOT implemented: {stress_method}")

        df = df[['Element', 'S']]

        logging.info(f"      Transforming data for: {state_file['state_file']} ... END")
        
        return df



    def transform_df_for_stress_analysis(self, df, state_file):
        logging.info(
            f"      Transforming data for: {state_file['state_file']} ... START"
        )

        # Convert columns to float
        if len(df.keys()) == 18:
            number_of_nodes = 4
        else:
            number_of_nodes = 8

        df.replace("N/A", np.nan, inplace=True)
        for column in df.columns:
            try:
                df[column] = df[column].astype(float)
            except:
                logging.debug(f"      Could not convert column {column} to float")

        add_columns = [
            "x_min",
            "x_max",
            "y_min",
            "y_max",
            "z_min",
            "S_mean",
            "S_max",
            "S_min",
        ]
        for column in add_columns:
            df[column] = np.nan

        if number_of_nodes == 4:
            df = self.get_df_with_4_nodes(df)
        elif number_of_nodes == 8:
            df = self.get_df_with_8_nodes(df)

        logging.info(f"      Transforming data for: {state_file['state_file']} ... END")
        return df

    def get_df_with_4_nodes(self, df):
        try:
            for df_row in range(0, len(df)):
                df.loc[df_row, "x_min"] = min(
                    df.iloc[df_row]["X-coord(1)"],
                    df.iloc[df_row]["X-coord(2)"],
                    df.iloc[df_row]["X-coord(3)"],
                    df.iloc[df_row]["X-coord(4)"],
                )
                df.loc[df_row, "x_max"] = max(
                    df.iloc[df_row]["X-coord(1)"],
                    df.iloc[df_row]["X-coord(2)"],
                    df.iloc[df_row]["X-coord(3)"],
                    df.iloc[df_row]["X-coord(4)"],
                )
                df.loc[df_row, "y_min"] = min(
                    df.iloc[df_row]["Y-coord(1)"],
                    df.iloc[df_row]["Y-coord(2)"],
                    df.iloc[df_row]["Y-coord(3)"],
                    df.iloc[df_row]["Y-coord(4)"],
                )
                df.loc[df_row, "y_max"] = max(
                    df.iloc[df_row]["Y-coord(1)"],
                    df.iloc[df_row]["Y-coord(2)"],
                    df.iloc[df_row]["Y-coord(3)"],
                    df.iloc[df_row]["Y-coord(4)"],
                )
                df.loc[df_row, "z_min"] = min(
                    df.iloc[df_row]["Z-coord(1)"],
                    df.iloc[df_row]["Z-coord(2)"],
                    df.iloc[df_row]["Z-coord(3)"],
                    df.iloc[df_row]["Z-coord(4)"],
                )
                df.loc[df_row, "z_max"] = max(
                    df.iloc[df_row]["Z-coord(1)"],
                    df.iloc[df_row]["Z-coord(2)"],
                    df.iloc[df_row]["Z-coord(3)"],
                    df.iloc[df_row]["Z-coord(4)"],
                )

                if "VONMISES(1)" in df.columns:
                    stress_nomenclature = "VONMISES"
                elif "SIGXX(1)" in df.columns:
                    stress_nomenclature = "SIGXX"
                elif "SIGYY(1)" in df.columns:
                    stress_nomenclature = "SIGYY"
                elif "TAUXY(1)" in df.columns:
                    stress_nomenclature = "TAUXY"
                elif "P1(1)" in df.columns:
                    stress_nomenclature = "P1"
                else:
                    raise ValueError("Could not find stress column")

                df.loc[df_row, "S_mean"] = statistics.mean(
                    [
                        df.iloc[df_row][stress_nomenclature + "(1)"],
                        df.iloc[df_row][stress_nomenclature + "(2)"],
                        df.iloc[df_row][stress_nomenclature + "(3)"],
                        df.iloc[df_row][stress_nomenclature + "(4)"],
                    ]
                )

                df.loc[df_row, "S_max"] = max(
                    [
                        df.iloc[df_row][stress_nomenclature + "(1)"],
                        df.iloc[df_row][stress_nomenclature + "(2)"],
                        df.iloc[df_row][stress_nomenclature + "(3)"],
                        df.iloc[df_row][stress_nomenclature + "(4)"],
                    ]
                )

                df.loc[df_row, "S_min"] = min(
                    [
                        df.iloc[df_row][stress_nomenclature + "(1)"],
                        df.iloc[df_row][stress_nomenclature + "(2)"],
                        df.iloc[df_row][stress_nomenclature + "(3)"],
                        df.iloc[df_row][stress_nomenclature + "(4)"],
                    ]
                )
        except Exception as e:
            logging.warning(f"Could not transform data: {e}")

        return df

    def get_df_with_8_nodes(self, df):
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

            df.loc[df_row, "S_min"] = min(
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
        logging.info(f"      Reading file: {seasam_xtract_file} ... START")
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

        logging.info(f"      Reading file: {seasam_xtract_file} ... END")
        return df

    def get_fatigue_states_by_directory(self, state_files):
        fatigue_states_analysis = []

        for state_file_index in range(0, len(state_files[0]["basenames"])):
            basename_first_set = state_files[0]["basenames"][state_file_index]
            state_files_for_basename = []
            for state_file_set in state_files:
                basename_index = state_file_set["basenames"].index(basename_first_set)
                state_file = state_file_set["files"][basename_index]
                state_files_for_basename.append(
                    {"state_file": state_file, "label": state_file_set["label"]}
                )

            fatigue_state = {
                "state_files": state_files_for_basename,
                "basename": basename_first_set,
            }
            fatigue_states_analysis.append(fatigue_state)

        return fatigue_states_analysis

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
