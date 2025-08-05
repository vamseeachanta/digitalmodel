# Standard library imports
import logging
import os
from pathlib import Path

# Third party imports
import pandas as pd

# Reader imports
from digitalmodel.common.ETL_components import ETL_components
from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects

try:
    # Third party imports
    import OrcFxAPI
except:
    logging.debug("OrcFxAPI not available")


of_objects = OrcaFlexObjects()


class OPPTimeSeries:
    def __init__(self) -> None:
        pass

    def router(self, cfg):

        if "time_series_settings" in cfg:
            if cfg["time_series_settings"]["data"]:
                self.get_time_series_data(cfg)

            if cfg["time_series_settings"]["histogram"]:
                histogram_for_file = self.post_process_histograms(model, FileObjectName)
                histogram_all_files.append(histogram_for_file)

                self.prepare_histogram_bins_for_output()

            if cfg["time_series_settings"]["summation"]:
                self.generate_cummulative_histograms()
                self.save_histograms()

        if cfg["orcaflex"]["postprocess"]["RAOs"]["flag"]:
            self.post_process_RAOs(model, FileObjectName)

    def get_time_series_data(self, cfg, model_dict, file_name):
        model = model_dict["model"]
        time_series_cfg_output = {"groups": []}

        ts_groups = cfg["time_series_settings"]["groups"]
        for ts_group in ts_groups:
            group_label = ts_group["Label"]
            df = pd.DataFrame()
            for ts_cfg in ts_group["Columns"]:
                if ts_cfg["SimulationPeriod"][1] is None:
                    ts_cfg["SimulationPeriod"][1] = model_dict["stop_time"]
                    logging.debug(
                        f"SimulationPeriod[1] set to {model_dict['stop_time']}"
                    )
                if ts_cfg["SimulationPeriod"][1] is None:
                    ts_cfg["SimulationPeriod"][1] = model_dict["stop_time"]
                    logging.debug(
                        f"SimulationPeriod[1] set to {model_dict['stop_time']}"
                    )
                ts_label = ts_cfg["Label"]
                try:
                    time_series, times = self.get_time_series_from_orcaflex_run(
                        model_dict, ts_cfg
                    )
                    if "time" not in list(df.columns):
                        df["time"] = times
                    df[ts_label] = time_series
                except:
                    logging.error(
                        f"Error getting time series data for {ts_label} in group {group_label}"
                    )
                    df["time"] = []
                    df[ts_label] = []
                    continue
            # Save by filename
            file_name_stem = Path(file_name).stem
            output_file_name = os.path.join(
                cfg["Analysis"]["result_folder"],
                file_name_stem + "_" + group_label + ".csv",
            )

            csv_decimal = 6
            if "csv_decimal" in cfg.orcaflex["postprocess"]["time_series"]:
                csv_decimal = cfg.orcaflex["postprocess"]["time_series"]["csv_decimal"]

            df.round(csv_decimal).to_csv(output_file_name, index=False)
            time_series_cfg_output["groups"].append(
                {"label": group_label, "data": output_file_name}
            )

        basename = cfg["meta"]["basename"]
        cfg_output = {"time_series": time_series_cfg_output, "file_name": file_name}
        if "time_series" not in cfg[basename]:
            cfg[basename]["time_series"] = []
        else:
            cfg[basename]["time_series"].append(cfg_output)

    def get_time_series_from_orcaflex_run(self, model_dict, cfg_time_series):
        """Gets time series data from an OrcaFlex run

        Args:
            model: OrcaFlex model object
            cfg_time_series: Configuration dictionary containing time series settings

        Returns:
            Time series data from the OrcaFlex analysis
        """

        (
            OrcFXAPIObject,
            TimePeriod,
            arclengthRange,
            objectExtra,
            VariableName,
            Statistic_Type,
        ) = of_objects.get_orcaflex_objects(model_dict, cfg_time_series)
        model = model_dict["model"]

        if OrcFXAPIObject is not None:
            times = model.SampleTimes(TimePeriod)
            time_series = OrcFXAPIObject.TimeHistory(
                VariableName, TimePeriod, objectExtra=objectExtra
            )
        else:
            time_series = []
            times = []

        return time_series, times

    def get_TimeHistory(
        self, model_dict, OrcFXAPIObject, TimePeriod, objectExtra, VariableName
    ):
        output = None
        if OrcFXAPIObject is None:
            logging.warning(f"OrcFXAPIObject is None for TimeHistory with VariableName: {VariableName}")
            return output
        
        model_objects = of_objects.get_model_objects(model_dict["model"])
        object_df = model_objects["object_df"]
        if OrcFXAPIObject.name in list(object_df["ObjectName"]):
            try:
                if OrcFXAPIObject is not None:
                    if objectExtra is None:
                        output = OrcFXAPIObject.TimeHistory(VariableName, TimePeriod)
                    else:
                        output = OrcFXAPIObject.TimeHistory(
                            VariableName, TimePeriod, objectExtra
                        )
            except Exception as e:
                logging.error(str(e))
                # raise Exception(f"Error in TimeHistory: {str(e)}")
        return output

    def post_process_RAOs(self, model, FileObjectName):
        cfg_raos = self.cfg["RAOs"].copy()
        for rao_set in cfg_raos["sets"]:

            cfg_time_series_reference = self.get_cfg_time_series_reference(
                cfg_raos, rao_set
            )
            reference_time_series = self.get_time_series_from_orcaflex_run(
                model, cfg_time_series_reference
            )

            output_time_series_array = self.get_time_series_output(
                cfg_raos, rao_set, model
            )

            self.get_RAOs_from_time_series_data(
                output_time_series_array, reference_time_series, cfg_raos, rao_set
            )

    def get_cfg_fft(self, cfg_fft_common, cfg_fft_custom):
        cfg_fft = cfg_fft_common.copy()
        cfg_fft.update(cfg_fft_custom)

        return cfg_fft

    def get_RAOs_from_time_series_data(
        self, output_time_series_array, reference_time_series, cfg_raos, rao_set
    ):

        RAO_df_Labels = ["frequency"] + rao_set["time_series"]["Output"][
            "Variable_Label"
        ]
        RAO_df_columns = self.get_detailed_RAO_df_columns(RAO_df_Labels)
        RAO_ArcLength_df = pd.DataFrame(columns=RAO_df_columns)

        for ArcLength_index in range(0, len(output_time_series_array)):
            ArcLength_Label = rao_set["time_series"]["Output"]["Label"][ArcLength_index]
            RAO_df_array_Labels = [item["Label"] for item in self.RAO_df_array]
            if ArcLength_Label not in RAO_df_array_Labels:
                self.RAO_df_array.append(
                    {"Label": ArcLength_Label, "RAO_df": RAO_ArcLength_df.copy()}
                )

        cfg_fft_common = cfg_raos["fft"].copy()
        cfg_fft_custom = rao_set["fft"].copy()
        cfg_fft = self.get_cfg_fft(cfg_fft_common, cfg_fft_custom)

        for ArcLength_index in range(0, len(output_time_series_array)):
            ArcLength_Label = rao_set["time_series"]["Output"]["Label"][ArcLength_index]
            RAO_df_array_Labels = [item["Label"] for item in self.RAO_df_array]
            RAO_df_array_index = RAO_df_array_Labels.index(ArcLength_Label)
            df = self.RAO_df_array[RAO_df_array_index]["RAO_df"].copy()
            Variable_RAO_df = pd.DataFrame(columns=RAO_df_columns)
            for Variable_index in range(
                0, len(output_time_series_array[ArcLength_index])
            ):
                time_series = output_time_series_array[ArcLength_index][Variable_index]
                RAO_raw, RAO_filtered = self.get_RAO(
                    time_series, reference_time_series, cfg_fft
                )
                if Variable_index == 0:
                    Variable_RAO_df["frequency"] = RAO_filtered["frequency"]
                    Variable_RAO_df = self.assign_file_info_to_Variable_df(
                        Variable_RAO_df, self.fileIndex
                    )
                RAO_df_column_label = rao_set["time_series"]["Output"][
                    "Variable_Label"
                ][Variable_index]
                Variable_RAO_df[RAO_df_column_label] = RAO_filtered["complex"]

            df = pd.concat([df, Variable_RAO_df])
            self.RAO_df_array[RAO_df_array_index]["RAO_df"] = df.copy()

    def assign_file_info_to_Variable_df(self, Variable_RAO_df, fileIndex):
        df_temp = self.load_matrix.iloc[fileIndex].copy()
        keys = list(df_temp.keys())
        for key in keys:
            Variable_RAO_df[key] = df_temp[key]

        return Variable_RAO_df

    def get_RAO(self, signal, excitation, cfg_fft):
        ts_comp = TimeSeriesComponents({"default": {"analysis": {"fft": cfg_fft}}})
        RAO_raw, RAO_filtered = ts_comp.get_RAO(
            signal=signal, excitation=excitation, cfg_rao=cfg_fft
        )

        return RAO_raw, RAO_filtered

    def get_time_series_output(self, cfg_raos, rao_set, model):
        cfg_common = cfg_raos["time_series"].copy()
        cfg_group = rao_set["time_series"].copy()
        cfg_time_series_array = self.get_cfg_time_series_custom(cfg_group, cfg_common)
        output_time_series_array = []
        for ArcLength_index in range(0, len(cfg_time_series_array)):
            output_ArcLength_time_series_array = []
            for Variable_index in range(0, len(cfg_time_series_array[ArcLength_index])):
                cfg_Variable = cfg_time_series_array[ArcLength_index][Variable_index]
                Variable_time_series = self.get_time_series_from_orcaflex_run(
                    model, cfg_Variable
                )
                output_ArcLength_time_series_array.append(Variable_time_series)
            output_time_series_array.append(output_ArcLength_time_series_array)

        return output_time_series_array

    def get_cfg_time_series_reference(self, cfg_raos, rao_set):
        cfg_common = cfg_raos["time_series"].copy()
        cfg_group = rao_set["time_series"].copy()
        reference_cfg = cfg_common.copy()
        reference_cfg.update(cfg_group["Reference"])
        return reference_cfg

    def get_cfg_time_series_custom(self, cfg_group, cfg_common):
        cfg_time_series_array = []
        cfg_time_series = cfg_common.copy()
        cfg_time_series.update(cfg_group["Output"])
        for ArcLength_index in range(0, len(cfg_group["Output"]["ArcLength"])):
            cfg_time_series_variable_array = []
            ArcLength = cfg_group["Output"]["ArcLength"][ArcLength_index]
            cfg_time_series.update({"ArcLength": ArcLength})
            ObjectName = cfg_group["Output"].get("ObjectName", None)
            if ObjectName is not None:
                cfg_time_series.update({"ObjectName": ObjectName})

            for Variable_index in range(0, len(cfg_group["Output"]["Variable"])):
                Variable = cfg_group["Output"]["Variable"][Variable_index]
                cfg_time_series.update({"Variable": Variable})
                cfg_time_series_variable_array.append(cfg_time_series.copy())

            cfg_time_series_array.append(cfg_time_series_variable_array.copy())

        return cfg_time_series_array

    def prepare_histogram_bins_for_output(self):
        rain_flow_settings = self.cfg["default"]["Analysis"]["rain_flow"]
        start_range = rain_flow_settings["range"][0]
        end_range = rain_flow_settings["range"][1]
        no_of_bins = rain_flow_settings["bins"]
        bins = list(np.linspace(start_range, end_range, no_of_bins + 1))
        self.detailed_histograms_array = []
        no_of_files = len(self.simulation_filenames)
        for time_series_index in range(0, len(self.cfg.time_series)):
            time_series_cfg = self.cfg.time_series[time_series_index]
            SummaryDF = None
            AddSummaryColumnsArray = []
            if (
                (len(self.cfg.Summary) > 0)
                and time_series_cfg.__contains__("AddSummaryToHistograms")
                and time_series_cfg["AddSummaryToHistograms"]
            ):
                SummaryDF = self.SummaryDFAllFiles[time_series_index]
                Summary_cfg = self.cfg.Summary[time_series_index]
                AddSummaryColumnsArray = [
                    item["Label"] for item in Summary_cfg["Columns"]
                ]
            columns = self.get_detailed_histogram_df_columns(
                rain_flow_settings, AddSummaryColumnsArray
            )
            detailed_histograms = pd.DataFrame(columns=columns)
            for file_index in range(0, no_of_files):
                histogram = self.HistogramAllFiles[file_index][time_series_index]
                SimulationDuration = self.get_SimulationDuration(file_index)
                seastate_probability = self.get_seastate_probability(file_index)
                if (len(histogram) > 0) and (seastate_probability > 0):
                    loading_condition_array = self.get_loading_condition_array(
                        file_index
                    )
                    AddSummary_array = self.get_AddSummary_array(
                        SummaryDF, file_index, AddSummaryColumnsArray
                    )
                    for bin_index in range(0, no_of_bins):
                        bin_range_array = [bins[bin_index], bins[bin_index + 1]]
                        histogram_cycles = histogram[bin_index]
                        histogram_cycles_per_year = (
                            histogram_cycles * (365.25 * 24 * 3600) / SimulationDuration
                        )
                        histogram_cycles_per_year_with_probability = (
                            histogram_cycles_per_year * seastate_probability
                        )
                        histogram_cycle_array = [
                            histogram_cycles,
                            histogram_cycles_per_year,
                            histogram_cycles_per_year_with_probability,
                        ]
                        histogram_bin_array = bin_range_array + histogram_cycle_array
                        df_row = (
                            loading_condition_array
                            + histogram_bin_array
                            + AddSummary_array
                        )
                        detailed_histograms.loc[len(detailed_histograms)] = df_row
            Label = time_series_cfg["Label"]

            self.detailed_histograms_array.append(
                {"label": Label, "data": detailed_histograms}
            )

    def generate_cummulative_histograms(self):

        self.all_histogram_file_dfs = []
        self.probability_array = []
        self.simulation_duration_array = []
        self.file_label_array = []
        bins = list(
            range(
                self.cfg["default"]["rain_flow"]["range"][0],
                self.cfg["default"]["rain_flow"]["range"][1],
                self.cfg["default"]["rain_flow"]["bins"],
            )
        )
        cummulative_histogram_df = pd.DataFrame(bins, columns=["bins"])
        for time_series_index in range(0, len(self.HistogramAllFiles[0])):
            label = self.histogram_labels[time_series_index]
            cummulative_histogram_df[label] = pd.Series([0] * len(bins))

        for file_index in range(0, len(self.cfg["Analysis"]["input_files"]["no_ext"])):
            file_label = self.simulation_Labels[file_index]["Label"]
            self.file_label_array.append(file_label)
            self.probability_array.append(self.cfg["Files"][file_index]["probability"])
            self.simulation_duration_array.append(
                self.cfg["Files"][file_index]["simulation_duration"]
            )
            histogram_file_df = pd.DataFrame(bins, columns=["bins"])
            for time_series_index in range(0, len(self.HistogramAllFiles[0])):
                label = self.histogram_labels[time_series_index]
                histogram_file_df[label] = pd.Series(
                    self.HistogramAllFiles[file_index][time_series_index]
                    * (365.25 * 24 * 3600)
                    / self.simulation_duration_array[file_index],
                    index=histogram_file_df.index,
                )
                cummulative_histogram_df[label] = (
                    cummulative_histogram_df[label]
                    + histogram_file_df[label]
                    * self.probability_array[file_index]
                    / 100
                )
            self.all_histogram_file_dfs.append(histogram_file_df.copy())

        self.file_label_array.append("cummulative_histograms")
        self.all_histogram_file_dfs.append(cummulative_histogram_df.copy())

        self.qa_histograms()
        self.file_label_array.append("histograms_qa")
        self.all_histogram_file_dfs.append(self.sum_df)

    def qa_histograms(self):
        etl_components = ETL_components(cfg=None)
        self.sum_df = etl_components.get_sum_df_from_df_array(
            self.all_histogram_file_dfs
        )

    def save_histograms(self):
        customdata = {
            "FileName": self.cfg["Analysis"]["result_folder"]
            + self.cfg["Analysis"]["file_name"]
            + "_histograms.xlsx",
            "SheetNames": self.file_label_array,
            "thin_border": True,
        }
        save_data.DataFrameArray_To_xlsx_openpyxl(
            self.all_histogram_file_dfs, customdata
        )

    def post_process_histograms(self, model, FileObjectName):
        histogram_array = []
        self.histogram_labels = []

        for time_series_index in range(0, len(self.cfg["time_series"])):
            cfg_temp = self.cfg["time_series"][time_series_index]
            self.histogram_labels.append(cfg_temp["Label"])
            histogram_object = self.get_histogram_object_from_orcaflex_run(
                FileObjectName, cfg_temp, model
            )
            histogram_array.append(histogram_object)

        return histogram_array

    def get_histogram_object_from_orcaflex_run(
        self, FileObjectName, cfg_histogram, model
    ):

        # Read Object
        OrcFXAPIObject = model[FileObjectName]
        SimulationPeriod = cfg_histogram["SimulationPeriod"]
        TimePeriod = of_objects.get_TimePeriodObject(SimulationPeriod)
        VariableName = cfg_histogram["Variable"]
        ArcLength = cfg_histogram["ArcLength"][0]
        RadialPos_text = cfg_histogram["RadialPos"]
        if RadialPos_text == "Outer":
            RadialPos = 1
        elif RadialPos_text == "Inner":
            RadialPos = 0
        Theta = cfg_histogram["Theta"]
        objectExtra = OrcFxAPI.oeLine(
            ArcLength=ArcLength, RadialPos=RadialPos, Theta=Theta
        )
        rain_flow_half_cycles = OrcFXAPIObject.RainflowHalfCycles(
            VariableName, TimePeriod, objectExtra=objectExtra
        )
        bins = self.cfg["default"]["Analysis"]["rain_flow"]["bins"]
        rainflow_range = self.cfg["default"]["Analysis"]["rain_flow"]["range"]
        histogram_object = np.histogram(
            rain_flow_half_cycles,
            bins=bins,
            range=(rainflow_range[0], rainflow_range[1]),
        )

        return histogram_object[0]

    def get_detailed_histogram_df_columns(
        self, rain_flow_settings, AddSummaryColumnsArray
    ):
        if self.load_matrix is None:
            load_matrix_columns = []
        else:
            load_matrix_columns = self.load_matrix.columns.to_list()
        if (
            rain_flow_settings.__contains__("BinLabels")
            and rain_flow_settings["BinLabels"] is not None
        ):
            BinLabels = rain_flow_settings["BinLabels"]
        else:
            BinLabels = ["BinStart", "BinEnd"]
        cycle_labels = [
            "Halfcycles_per_simulation",
            "Halfcycles_per_yr",
            "Halfcycles_per_yr_with_probability",
        ]
        rainflow_labels = BinLabels + cycle_labels
        columns = load_matrix_columns + rainflow_labels + AddSummaryColumnsArray
        return columns

    def get_detailed_RAO_df_columns(self, RAO_Columns):
        if self.load_matrix is None:
            load_matrix_columns = []
        else:
            load_matrix_columns = self.load_matrix.columns.to_list()
        columns = load_matrix_columns + RAO_Columns
        return columns
