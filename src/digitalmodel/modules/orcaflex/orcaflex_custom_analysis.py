from assetutilities.common.utilities import is_file_valid_func

# TODO Refactor
class OrcaFlexCustomAnalysis:
    def __init__(self) -> None:
        pass

    def perform_simulations(self):
        self.process_fea()

        self.save_data()

        try:
            file_type = self.cfg["default"]["Analysis"]["Analyze"]["file_type"]
        except:
            file_type = None

        if file_type is not None and file_type in ["script", "batch_script"]:
            print("Processing orcaflex script/batch files for orcaflex input files")
            self.process_scripts()

    def get_files_for_analysis(
        self,
        analysis_type,
        fileIndex,
        input_files_with_extension,
        input_files_without_extension,
    ):
        filename = self.simulation_filenames[fileIndex]
        analysis_root_folder = self.cfg["Analysis"]["analysis_root_folder"]
        file_is_valid, filename = is_file_valid_func(filename, analysis_root_folder)
        filename_components = filename.split(".")
        filename_without_extension = filename.replace("." + filename_components[-1], "")
        if len(filename_components) > 1:
            input_files_with_extension.append(filename)
            input_files_without_extension.append(filename_without_extension)
        elif os.path.isfile(filename_without_extension + ".yml"):
            input_files_without_extension.append(filename_without_extension)
            input_files_with_extension.append(
                input_files_without_extension[fileIndex] + ".yml"
            )
        elif os.path.isfile(filename_without_extension + ".dat"):
            input_files_without_extension.append(filename_without_extension)
            input_files_with_extension.append(
                input_files_without_extension[fileIndex] + ".dat"
            )
        else:
            print("File not found: {0}".format(filename))

        if self.cfg["orcaflex"]["analysis"]["simulation"]:
            analysis_type.append("simulation")
        if self.cfg["orcaflex"]["analysis"]["static"]:
            analysis_type.append("statics")
        if self.cfg["orcaflex"]["iterate"]["flag"]:
            analysis_type.append("iterate")

    def process_fea(self):
        exts = list(self.cfg.file_management["input_files"].keys())

        static_flag = self.cfg["orcaflex"]["analysis"]["static"]
        simulation_flag = self.cfg["orcaflex"]["analysis"]["simulation"]
        iterate_flag = self.cfg["orcaflex"]["analysis"]["iterate"]["flag"]
        save_sim_flag = self.cfg["orcaflex"]["analysis"]["save_sim"]
        save_dat_flag = self.cfg["orcaflex"]["analysis"]["save_dat"]

        for fileIndex in range(
            0, len(self.cfg["file_management"]["input_files"][exts[0]])
        ):
            filename_with_ext = self.cfg["file_management"]["input_files"][exts[0]][
                fileIndex
            ]
            filename_without_extension_dict = fm.get_filename_without_extension(
                filename_with_ext
            )
            filename_without_ext = filename_without_extension_dict["with_path"]

            if iterate_flag:
                iterate_to_target_value_flag = self.cfg["orcaflex"]["analysis"][
                    "iterate"
                ]["to_target_value"]

            model = OrcFxAPI.Model()
            try:
                logging.info(
                    f"Cleaning input file for potential stale data for {filename_with_ext} .. START"
                )
                model = self.clean_model(model, filename_with_ext, filename_without_ext)
            except Exception as e:
                logging.info(
                    f"Cleaning input file for potential stale data for {filename_with_ext} .. FAIL"
                )
                logging.info(str(e))

            try:
                logging.info(f"Loading input file ... {filename_with_ext} ... START")
                model.LoadData(filename_with_ext)
                logging.info(f"Loading input file ... {filename_with_ext} COMPLETE")
            except Exception as e:
                simulation_flag = False
                iterate_flag = False
                logging.info(str(e))
                raise ImportError(f"Load data for {filename_with_ext} ... FAIL")

            if static_flag or iterate_flag:
                model, run_success_flag = self.run_static_analysis(
                    filename_with_ext, model
                )
                if run_success_flag:
                    logging.info("Run statics  ....  SUCCESS")
                else:
                    logging.info("Run statics  ....  FAIL")
                    simulation_flag = (
                        save_sim_flag
                    ) = save_dat_flag = iterate_to_target_value_flag = False

            if simulation_flag:
                model.RunSimulation()
                logging.info("Run simulation successful")

            if save_sim_flag:
                try:
                    model.SaveSimulation(filename_without_ext + ".sim")
                    logging.info("Save simulation successful")
                except:
                    print("Save simulation.. FAILED")

            if save_dat_flag:
                try:
                    model.SaveData(filename_without_ext + ".dat")
                    logging.info("Save data file      ... SUCCESS")
                except:
                    logging.info("Save data file      ... FAILED")

            if iterate_flag and iterate_to_target_value_flag:
                iterate_cfg = self.cfg["default"]["Analysis"]["Analyze"].copy()
                iterate_cfg.update({"filename_without_ext": filename_without_ext})
                self.iterate_to_target_value(model, iterate_cfg)

        print(
            f"Analysis done for {len(self.cfg['file_management']['input_files'][exts[0]])} input files"
        )

    def clean_model(self, model, filename_with_ext, filename_without_ext):
        clean_model = model

        UseCalculatedPositions_cfg = self.cfg["orcaflex"]["analysis"][
            "UseCalculatedPositions_cfg"
        ].copy()
        if (
            UseCalculatedPositions_cfg["flag"]
            and UseCalculatedPositions_cfg["clean_StaleCalculatedPositions"]
        ):
            file_yml = ymlInput(filename_with_ext)
            clean_file = {"BaseFile": file_yml["BaseFile"]}
            save_data.saveDataYaml(clean_file, filename_without_ext)
            clean_model = OrcFxAPI.Model()
            clean_model.LoadData(filename_with_ext)

        return clean_model

    def run_static_analysis(self, filename_with_ext, model):
        print(f"Static analysis for {filename_with_ext} ... .... ")
        try:
            print("First analysis ......")
            model.CalculateStatics()
            print("First analysis ... PASS")
            self.save_model_with_calculated_positions(filename_with_ext, model)
            model = self.analysis_with_calculated_positions(model)
            run_success_flag = True
        except:
            run_success_flag = False
            print("First static analysis for ... FAIL")

        return model, run_success_flag

    def analysis_with_calculated_positions(self, model):
        try:
            model.CalculateStatics()
        except:
            print("Analysis with calculated positions ... FAIL")

        return model

    def save_model_with_calculated_positions(self, filename_with_ext, model):
        calculated_cfg = self.cfg["orcaflex"]["iterate"][
            "UseCalculatedPositions"
        ].copy()

        if calculated_cfg["SetLinesToUserSpecifiedStartingShape"]:
            model.UseCalculatedPositions(SetLinesToUserSpecifiedStartingShape=True)
        elif calculated_cfg["UseStaticLineEndOrientations"]:
            model.UseCalculatedPositions(UseStaticLineEndOrientations=True)
        else:
            model.UseCalculatedPositions()
        calculated_positions_filename = filename_with_ext
        if not self.cfg["orcaflex"]["iterate"]["overwrite_data"]:
            calculated_positions_filename = (
                os.path.splitext(filename_with_ext)[0]
                + "_calculated_all"
                + os.path.splitext(filename_with_ext)[1]
            )
        model.SaveData(calculated_positions_filename)

    def iterate_to_target_value(self, model, iterate_cfg):
        iterations_df = pd.DataFrame(columns=["variable", "output"])
        model.CalculateStatics()
        model.SaveSimulation(iterate_cfg["filename_without_ext"] + ".sim")

        target_value = iterate_cfg["iterate"]["column"]["target_value"]
        current_iteration = 1
        column_cfg = iterate_cfg["iterate"]["column"].copy()

        update_cfg = {
            "model_file": iterate_cfg["filename_without_ext"]
            + "."
            + iterate_cfg["file_type"]
        }
        model_file = OrderedDict(ymlInput(update_cfg["model_file"]))
        variable_current_value = model_file["Lines"]["Umbilical"]["Length[4]"]
        output_current_value = round(
            self.process_summary_by_model_and_cfg(model, column_cfg), 3
        )
        iterations_df.loc[len(iterations_df)] = [
            variable_current_value,
            output_current_value,
        ]

        while (
            abs(output_current_value - target_value) > 0.2
            and current_iteration < iterate_cfg["iterate"]["column"]["max_iterations"]
        ):

            current_iteration += 1

            variable_current_value = self.get_variable_current_value_for_iteration(
                iterations_df, variable_current_value, target_value
            )

            update_cfg.update({"variable_value": variable_current_value})

            model["Umbilical"].Length[3] = variable_current_value
            # ou.update_model(update_cfg)

            model.SaveData(update_cfg["model_file"])
            model.CalculateStatics()
            model.SaveSimulation(iterate_cfg["filename_without_ext"] + ".sim")
            output_current_value = round(
                self.process_summary_by_model_and_cfg(model, column_cfg), 3
            )

            iterations_df.loc[len(iterations_df)] = [
                variable_current_value,
                output_current_value,
            ]
            iterations_df.sort_values(by=["output"], inplace=True)

        print(f"Iterations done in {current_iteration} times")
        print(f"Current target value is: {output_current_value}")
        print(f"Variable current value is: {variable_current_value}")

    def get_variable_current_value_for_iteration(
        self, iterations_df, variable_current_value, target_value
    ):

        if len(iterations_df) > 1:
            xp = list(iterations_df.output)
            fp = list(iterations_df.variable)
            f = scipy.interpolate.interp1d(xp, fp, fill_value="extrapolate")
            variable_current_value = np.round(f(target_value), decimals=3)

        else:
            variable_current_value += 0.25

        return variable_current_value

    def save_data(self):
        save_data.saveDataYaml(
            self.cfg,
            self.cfg["Analysis"]["result_folder"] + self.cfg["Analysis"]["file_name"],
            False,
        )

        if (
            "postprocess" in self.cfg.default
            and self.cfg.default["postprocess"]["cummulative_histograms"]["flag"]
        ):
            histogram_data_array = [
                item["data"] for item in self.detailed_histograms_array
            ]
            label_array = [item["label"] for item in self.detailed_histograms_array]
            customdata = {
                "FileName": self.cfg["Analysis"]["result_folder"]
                + self.cfg["Analysis"]["file_name"]
                + "_histograms.xlsx",
                "SheetNames": label_array,
                "thin_border": True,
            }
            if len(histogram_data_array) > 0:
                save_data.DataFrameArray_To_xlsx_openpyxl(
                    histogram_data_array, customdata
                )

        if (
            "Analysis" in self.cfg.default
            and self.cfg.default["Analysis"].__contains__("RAOs")
            and self.cfg.default["Analysis"]["RAOs"]["flag"]
        ):
            self.save_RAOs()

    def process_scripts(self):
        model = OrcFxAPI.Model()
        for file_index in range(
            0, len(self.cfg["Analysis"]["input_files"]["with_ext"])
        ):
            file_name = self.cfg["Analysis"]["input_files"]["with_ext"][file_index]
            model.ProcessBatchScript(file_name)

        print("Ran script files successfully")
