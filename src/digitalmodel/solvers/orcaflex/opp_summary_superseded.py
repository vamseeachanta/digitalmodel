# Standard library imports
import logging
import os

# Third party imports
import numpy as np
import pandas as pd
from assetutilities.common.data import SaveData, TransformData

# Reader imports
from digitalmodel.orcaflex.opp_range_graph import OPPRangeGraph
from digitalmodel.orcaflex.opp_time_series import OPPTimeSeries
from digitalmodel.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.orcaflex.orcaflex_utilities import OrcaflexUtilities

save_data = SaveData()
ou = OrcaflexUtilities()
of_objects = OrcaFlexObjects()
opp_ts = OPPTimeSeries()
opp_rg = OPPRangeGraph()


class OPPSummary_superseded:
    def process_summary_superseded(self, cfg):
        # TODO DELETE by end of WLNG project or 2024-02-01
        if "summary_settings" in cfg:
            SummaryDFAllFiles = self.process_summary_groups(cfg)
            self.SummaryDFAllFiles = SummaryDFAllFiles
            self.save_summary_superseded(cfg, self.SummaryDFAllFiles, self.load_matrix)

    def process_summary_groups_superseded(self, cfg):
        # TODO DELETE by end of WLNG project or 2024-02-01

        summary_cfg = cfg["summary_settings"].copy()
        SummaryDFAllFiles = [pd.DataFrame()] * len(summary_cfg["groups"])

        for SummaryIndex in range(0, len(cfg["summary_settings"]["groups"])):
            if "filename_pattern" in cfg["summary_settings"]["groups"][SummaryIndex]:
                filename_pattern = cfg["summary_settings"]["groups"][SummaryIndex][
                    "filename_pattern"
                ]
                if filename_pattern is not None:
                    cfg["file_management"]["files"]["files_in_current_directory"][
                        "filename_pattern"
                    ] = filename_pattern

            if "directory" in cfg["summary_settings"]["groups"][SummaryIndex]:
                directory = cfg["summary_settings"]["groups"][SummaryIndex]["directory"]
                if directory is not None:
                    cfg["file_management"]["files"]["files_in_current_directory"][
                        "directory"
                    ] = directory

            ou.file_management(cfg)
            self.load_matrix = ou.get_load_matrix_with_filenames(cfg)

            sim_files = cfg.file_management["input_files"]["sim"]

            for fileIndex in range(0, len(sim_files)):
                file_name = sim_files[fileIndex]
                self.fileIndex = fileIndex
                model = ou.get_model_from_filename(
                    file_name=file_name, load_matrix=self.load_matrix
                )
                FileDescription = "Description"
                FileObjectName = "Dummy_Object"
                print("Post-processing file: {}".format(file_name))

                SimulationFileName = ou.get_SimulationFileName(file_name)

                try:
                    SummaryDFAllFiles[SummaryIndex] = self.postProcessSummary(
                        model,
                        SummaryDFAllFiles[SummaryIndex],
                        SummaryIndex,
                        FileDescription,
                        FileObjectName,
                        SimulationFileName,
                        fileIndex,
                        cfg,
                    )
                except Exception as e:
                    logging.info(str(e))
                    raise Exception("Error in post processing")

        return SummaryDFAllFiles

    def postProcessSummary_superseded(
        self,
        model,
        SummaryDF,
        SummaryIndex,
        FileDescription,
        FileObjectName,
        FileName,
        fileIndex,
        cfg,
    ):
        # TODO DELETE by end of WLNG project or 2024-02-01
        summary_group_cfg = cfg["summary_settings"]["groups"][SummaryIndex]
        if SummaryDF.empty:
            columns = self.get_summary_df_columns(summary_group_cfg)
            pd.options.mode.chained_assignment = None
            SummaryDF = pd.DataFrame(columns=columns)
            pd.reset_option("mode.chained_assignment")

        summary_from_sim_file = []
        summary_from_sim_file.append(FileName)
        summary_from_sim_file.append(FileDescription)

        loading_condition_array = self.get_loading_condition_array(fileIndex)

        if model is not None:
            for SummaryColumnIndex in range(0, len(summary_group_cfg["Columns"])):
                summary_group_item_cfg = summary_group_cfg["Columns"][
                    SummaryColumnIndex
                ]
                try:
                    output_value = self.process_summary_by_model_and_cfg_item(
                        model, summary_group_item_cfg
                    )
                    summary_from_sim_file.append(output_value)
                except Exception as e:
                    summary_from_sim_file.append(None)
                    print(str(e))
                    print(f"Summary Group {summary_group_item_cfg}  in post processing")

            if np.nan in summary_from_sim_file:
                print(
                    "Summary Incomplete for : '{}' in simulation file: '{}'".format(
                        summary_group_cfg["SummaryFileName"], FileDescription
                    )
                )
                if ArcLengthArray is None:
                    print(
                        "          Arc length is {}. Define appropriate arc length value or range".format(
                            ArcLengthArray
                        )
                    )
                elif len(ArcLengthArray) == 0:
                    print(
                        "          Arc length is {}. Define appropriate arc length value or range".format(
                            ArcLengthArray
                        )
                    )
                elif len(ArcLengthArray) == 1:
                    print(
                        "          Node may not be in arc length exact position for Arc length {}. Provide a range of approx. element length".format(
                            ArcLengthArray
                        )
                    )
                elif len(ArcLengthArray) == 2:
                    print(
                        "          Node may not be in arc length range position for Arc length {}. Check range and fe element length".format(
                            ArcLengthArray
                        )
                    )
                print(
                    "          (or) Simulation failed for : {} before postprocess Time {}".format(
                        FileDescription, SimulationPeriod
                    )
                )
        else:
            summary_from_sim_file = summary_from_sim_file + [None] * len(
                summary_group_cfg["Columns"]
            )

        pd.options.mode.chained_assignment = None
        SummaryDF.loc[len(SummaryDF)] = loading_condition_array + summary_from_sim_file
        pd.reset_option("mode.chained_assignment")

        return SummaryDF

    def get_loading_condition_array_superseded(self, file_index):
        # TODO DELETE by end of WLNG project or 2024-02-01
        if self.load_matrix is None:
            loading_condition_array = []
        else:
            loading_condition_array = self.load_matrix.iloc[file_index].to_list()
        return loading_condition_array

    def save_summary_superseded(self, cfg, SummaryDFAllFiles, load_matrix):
        if not cfg.orcaflex["postprocess"]["summary"]["flag"]:
            print("No analysis summary per user request")
            return None

        summary_groups = cfg["summary_settings"]["groups"]
        SummaryFileNameArray = []
        for SummaryIndex in range(0, len(summary_groups)):
            summary_group_cfg = summary_groups[SummaryIndex]
            SummaryFileName = summary_group_cfg["SummaryFileName"]
            summary_value_columns = self.get_summary_value_columns(summary_group_cfg)
            summary_column_count = len(summary_value_columns)
            SummaryFileNameArray.append(SummaryFileName)
            SummaryDF = SummaryDFAllFiles[SummaryIndex]
            summaryDF_temp = SummaryDF.iloc[
                :,
                (len(SummaryDF.columns) - summary_column_count) : len(
                    SummaryDF.columns
                ),
            ]

            loadng_condition_array = []
            if load_matrix is not None:
                loadng_condition_array = [None] * len(load_matrix.columns)

            if len(summaryDF_temp) > 0:
                if "AddMeanToSummary" in cfg["summary_settings"].keys():
                    summaryDF_temp_numeric = summaryDF_temp.apply(
                        pd.to_numeric, errors="coerce"
                    )
                    result_array = ["Mean", "Mean"] + summaryDF_temp_numeric.mean(
                        axis=0
                    ).tolist()
                    SummaryDF.loc[len(SummaryDF)] = (
                        loadng_condition_array + result_array
                    )
                if "AddMinimumToSummary" in cfg["summary_settings"].keys():
                    result_array = ["Minimum", "Minimum"] + summaryDF_temp_numeric.min(
                        axis=0
                    ).tolist()
                    SummaryDF.loc[len(SummaryDF)] = (
                        loadng_condition_array + result_array
                    )
                if "AddMaximumToSummary" in cfg["summary_settings"].keys():
                    result_array = ["Maximum", "Maximum"] + summaryDF_temp_numeric.max(
                        axis=0
                    ).tolist()
                    SummaryDF.loc[len(SummaryDF)] = (
                        loadng_condition_array + result_array
                    )

            try:
                decimalArray = pd.Series(
                    [0, 0, 0, 2, 0], index=SummaryDF.columns.values
                )
                SummaryDF = SummaryDF.round(decimalArray)
            except Exception:
                SummaryDF = SummaryDF.round(2)

        self.save_summary_to_csv(SummaryFileNameArray, cfg)
        self.saveSummaryToNewExcel(SummaryFileNameArray, cfg)
        self.injectSummaryToNewExcel(SummaryFileNameArray, cfg)

        cfg[cfg["basename"]].update({"summary_groups": len(summary_groups)})
        print(f"Processed summary files: {len(summary_groups)}")

    def save_summary_to_csv_superseded(self, SummaryFileNameArray, cfg):
        cfg[cfg["basename"]].update({"summary": {}})

        for group_idx in range(0, len(SummaryFileNameArray)):
            df = self.SummaryDFAllFiles[group_idx]
            file_name = os.path.join(
                cfg["Analysis"]["result_folder"],
                cfg["Analysis"]["file_name"]
                + "_"
                + SummaryFileNameArray[group_idx]
                + ".csv",
            )

            df.to_csv(file_name, index=False)

            result_dict = {SummaryFileNameArray[group_idx]: df.to_dict()}
            cfg[cfg["basename"]]["summary"].update(result_dict)

    def injectSummaryToNewExcel_superseded(self, SummaryFileNameArray, cfg):
        for group_idx in range(0, len(SummaryFileNameArray)):
            summary_group_cfg = cfg["summary_settings"]["groups"][group_idx]
            if (
                "inject_into" in summary_group_cfg
                and summary_group_cfg["inject_into"]["flag"]
            ):
                inject_into_file = summary_group_cfg["inject_into"]["filename"]
                file_name = os.path.join(
                    cfg["Analysis"]["analysis_root_folder"], inject_into_file
                )
                if not os.path.isfile(file_name):
                    raise Exception(
                        f"Inject Into File {file_name} not found for writing summary data"
                    )

                sheetname = summary_group_cfg["inject_into"]["sheetname"]
                if sheetname is None:
                    sheetname = SummaryFileNameArray[group_idx]

                df = self.SummaryDFAllFiles[group_idx]
                cfg_save_to_existing_workbook = {
                    "template_file_name": file_name,
                    "sheetname": sheetname,
                    "saved_file_name": file_name,
                    "if_sheet_exists": "replace",
                    "df": df,
                }
                save_data.df_to_sheet_in_existing_workbook(
                    cfg_save_to_existing_workbook
                )

    def saveSummaryToNewExcel_superseded(self, SummaryFileNameArray, cfg):
        if len(self.SummaryDFAllFiles) > 0:
            customdata = {
                "FileName": os.path.join(
                    cfg["Analysis"]["result_folder"],
                    cfg["Analysis"]["file_name"] + ".xlsx",
                ),
                "SheetNames": SummaryFileNameArray,
                "thin_border": True,
            }
            save_data.DataFrameArray_To_xlsx_openpyxl(
                self.SummaryDFAllFiles, customdata
            )

    def transform_output_superseded(self, cfg):
        trans_data = TransformData()
        trans_data.get_transformed_data(cfg)

        return cfg
