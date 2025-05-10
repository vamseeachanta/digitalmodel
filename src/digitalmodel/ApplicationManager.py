# TODO Refer to AssetUtilities?
# Standard library imports
import datetime
import functools
import logging
import os
import pkgutil
import sys
from pathlib import Path

# Third party imports
import yaml
import numpy as np

# Reader imports
from assetutilities.common.data import AttributeDict
from assetutilities.common.data import SaveData
from assetutilities.common.set_logging import set_logging
from assetutilities.common.update_deep import update_deep_dictionary

save_data = SaveData()

def applicationTimer(func):
    # Standard library imports
    import functools

    @functools.wraps(func)
    def wrapper_applicationTimer(*args, **kwargs):
        # Standard library imports
        import time

        start_time = time.perf_counter()

        function_value = func(*args, **kwargs)

        end_time = time.perf_counter()
        run_time = end_time - start_time
        print(f"Finished {func.__name__!r} in {run_time:.2f} secs")

        return function_value

    return wrapper_applicationTimer


def setupApplicationRuns(func):

    @functools.wraps(func)
    def wrapper_applicationRuns(*args, **kwargs):
        # Standard library imports
        import logging

        app_runs = ApplicationRuns(basename=func.__name__)
        logging.info("  ******Set Up Application Runs .... ******")
        cfg = app_runs.configureApplication()
        kwargs["cfg"] = cfg

        # Standard library imports
        import time

        save_results = SaveApplicationResults()

        start_time = time.perf_counter()
        function_value = func(*args, **kwargs)
        end_time = time.perf_counter()
        run_time = end_time - start_time
        if (
            cfg.default.__contains__("data_source")
            and cfg.default["data_source"] == "db"
        ):
            save_results.saveApplicationResultsToDB(
                cfg=kwargs["cfg"], run_time=run_time, run_dict=None
            )

        if (
            cfg.default.__contains__("data_source")
            and cfg.default["data_source"] == "db"
        ):
            run_df = app_runs.getRuns()
            if (run_df is not None) and (len(run_df) > 0):
                for row_index in range(0, len(run_df)):
                    start_time = time.perf_counter()
                    run_dict = run_df.to_dict("records")[row_index]
                    cfg = app_runs.configureApplication(run_dict)
                    kwargs["cfg"] = cfg
                    function_value = func(*args, **kwargs)

                    end_time = time.perf_counter()
                    run_time = end_time - start_time
                    save_results.saveApplicationResultsToDB(
                        cfg=kwargs["cfg"], run_time=run_time, run_dict=run_dict
                    )

        logging.info("  ****** Application Runs .... COMPLETE ******")

        return function_value

    return wrapper_applicationRuns



class ConfigureApplicationInputs:

    def __init__(self):
        pass

    def configure(self, run_dict, library_name, basename, cfg_argv_dict):
        cfg = self.unify_application_and_default_and_custom_yamls(run_dict, library_name, basename, cfg_argv_dict)
        cfg = self.get_application_configuration_parameters(run_dict, basename, cfg)
        cfg = self.configure_overwrite_filenames(cfg)
        cfg = self.convert_cfg_to_attribute_dictionary(cfg)
        cfg = set_logging(cfg)

        logging.debug(cfg)

        return cfg

    def unify_application_and_default_and_custom_yamls(self, run_dict, library_name, basename, cfg_argv_dict):
        application_input_file_path = os.path.join(
            os.getcwd(), "src", library_name, "tests", "test_data", basename + ".yml")
        self.ApplicationInputFile = application_input_file_path
        self.get_custom_file()
        if not os.path.isfile(self.ApplicationInputFile):
            try:
                filename = os.path.join('base_configs', 'modules', basename, basename + '.yml')
                self.ApplicationInputFile = filename
                data = pkgutil.get_data(library_name, self.ApplicationInputFile)
            except Exception:
                raise FileNotFoundError(
                    "Application input file {0} not found".format(
                        self.ApplicationInputFile
                    )
                )
            self.ApplicationInputFile_dict = yaml.safe_load(data)

        # Get updated configuration file for Analysis
        cfg = self.generateYMLInput(run_dict, cfg_argv_dict)

        return cfg

    def get_custom_file(self, run_dict=None):

        try:
            if sys.argv[1] is not None:
                self.customYaml = sys.argv[1]
        except:
            self.customYaml = None
            print(
                "No update values file is provided. Running program default values "
                "from {0}".format(self.ApplicationInputFile)
            )

        if run_dict is not None:
            self.customYaml = None
            self.CustomInputs = run_dict.get("CustomInputs", None)
        else:
            self.CustomInputs = None

    def generateYMLInput(self, run_dict, cfg_argv_dict):

        if os.path.isfile(self.ApplicationInputFile):
            with open(self.ApplicationInputFile, "r") as ymlfile:
                cfg = yaml.load(ymlfile, Loader=yaml.Loader)
        else:
            cfg = self.ApplicationInputFile_dict

        if self.customYaml is not None:
            try:
                with open(self.customYaml, "r") as ymlfile:
                    cfgCustomValues = yaml.load(ymlfile, Loader=yaml.Loader)
                    default_yaml_file = cfgCustomValues.get("default_yaml", None)

                with open(self.customYaml) as fp:
                    custom_file_data = fp.read()
            except Exception as e:

                print("Update Input file could not be loaded successfully.")
                print("Error is : {}".format(e))
                print("Stopping program")
                sys.exit()
        elif self.CustomInputs is not None:

            custom_file_data = self.CustomInputs.replace("\\'", "'").replace(
                "\\n", "\n"
            )
            default_yaml_file = run_dict["DefaultInputFile"]
        else:
            custom_file_data = ""
            default_yaml_file = None

        if (self.customYaml is not None) or (self.CustomInputs is not None):
            if default_yaml_file is None:
                cfgDefaultAndCustomValues = yaml.load(
                    custom_file_data, Loader=yaml.Loader
                )
            else:
                with open(default_yaml_file) as fp:
                    default_file_data = fp.read()
                custom_and_default_yaml_data = (
                    custom_file_data + "\n" + default_file_data
                )
                cfgDefaultAndCustomValues = yaml.load(
                    custom_and_default_yaml_data, Loader=yaml.Loader
                )

            cfg = update_deep_dictionary(cfg, cfgDefaultAndCustomValues)
            print(f"Update default app configuration with contents in file {self.customYaml} ... DONE")
            print(f"Update default app configuration with dictionary content in:  ... START")
            print(f"\t{ cfg_argv_dict}")
            cfg = update_deep_dictionary(cfg, cfg_argv_dict)
            print(f"Update default app configuration with dictionary content ... FINISH")

        return cfg

    def get_application_configuration_parameters(self, run_dict, basename, cfg):

        application_start_time = datetime.datetime.now()

        if self.customYaml is not None:
            custom_file_name = os.path.split(self.customYaml)[1].split(".")[0]
            analysis_root_folder = os.path.split(self.customYaml)[0]
            if analysis_root_folder == "":
                analysis_root_folder = os.getcwd()
        elif self.CustomInputs is not None:
            custom_file_name = run_dict["RunName"]
            analysis_root_folder = os.path.join(os.getcwd(), "tests", "cfg", basename)
        else:
            custom_file_name = os.path.split(self.ApplicationInputFile)[1].split(".")[0]
            analysis_root_folder = os.getcwd()

        filename_label = cfg.get('meta', {}).get('label', None)
        if filename_label is not None:
            custom_file_name = custom_file_name + "_" + filename_label

        file_name = (
            custom_file_name + "_" + application_start_time.strftime("%Y%m%d_%Hh%Mm")
        )
        file_name_for_overwrite = custom_file_name

        result_folder_dict, cfg_with_fm = self.configure_result_folder(analysis_root_folder)

        log_folder = os.path.join(analysis_root_folder, "logs")
        if not os.path.exists(log_folder):
            os.mkdir(log_folder)

        cfg_array_file_names = None

        app_config_params = {
            "Analysis": {
                "basename": basename,
                "analysis_root_folder": analysis_root_folder,
                "file_name": file_name,
                "file_name_for_overwrite": file_name_for_overwrite,
                "log_folder": log_folder,
                "start_time": application_start_time,
                "cfg_array_file_names": cfg_array_file_names,
                "DefaultInputFile": cfg.get("default_yaml", None),
                "CustomInputFile": self.customYaml,
            }
        }

        app_config_params["Analysis"] = update_deep_dictionary(app_config_params["Analysis"], result_folder_dict)

        cfg = update_deep_dictionary(cfg, app_config_params)

        return cfg

    def configure_result_folder(self, analysis_root_folder, cfg_with_fm={}):

        if analysis_root_folder is None:
            analysis_root_folder = cfg_with_fm['Analysis']['analysis_root_folder']

        result_sub_folder = 'results'
        if len(cfg_with_fm) != 0 and 'file_management' in cfg_with_fm:
            result_sub_folder_cfg = cfg_with_fm['file_management'].get('output_directory', None)
            if result_sub_folder_cfg is not None:
                result_sub_folder = result_sub_folder_cfg

        result_folder = os.path.join(analysis_root_folder, result_sub_folder)
        if not os.path.exists(result_folder):
            os.mkdir(result_folder)

        result_data_folder = os.path.join(result_folder, "Data")
        if not os.path.exists(result_data_folder):
            os.mkdir(result_data_folder)

        result_plot_folder = os.path.join(result_folder, "Plot")
        if not os.path.exists(result_plot_folder):
            os.mkdir(result_plot_folder)

        result_folder_dict = {
            "result_folder": result_folder,
            "result_data_folder": result_data_folder,
            "result_plot_folder": result_plot_folder,
        }


        if len(cfg_with_fm) != 0:
            cfg_with_fm["Analysis"] = update_deep_dictionary(cfg_with_fm["Analysis"], result_folder_dict)

        return result_folder_dict, cfg_with_fm

    def configure_overwrite_filenames(self, cfg):
        if cfg["default"]["config"]["overwrite"]["output"] is True:
            cfg["Analysis"]["file_name"] = cfg["Analysis"][
                "file_name_for_overwrite"
            ]
        try:
            fe_folder = cfg["Analysis"].get("fe_folder", None)
            if fe_folder is None:
                cfg["Analysis"]["fe_folder"] = cfg["Analysis"][
                    "result_folder"
                ]
        except KeyError as e:
            logging.info("No fe_folder key in Analysis section of yml file")
            logging.info("Error is : {}".format(e))

        return cfg

    def convert_cfg_to_attribute_dictionary(self, cfg):
        cfg = AttributeDict(cfg)
        
        return cfg

    def validate_arguments_run_methods(self, inputfile):
        """
        Validate inputs for following run methods:
        - module (i.e. python -m digitalmodel input.yml "{'key':'value'}")
        - from python file (i.e. test_*.py)
        - from function call (i.e. engine(inputfile))
        
        """

        if len(sys.argv) > 1 and inputfile is not None:
            raise (
                Exception(
                    "2 Input files provided via arguments & function. Please provide only 1 file ... FAIL"
                )
            )

        cfg_argv_dict = {}
        if len(sys.argv) > 2:
            try:
                cfg_argv_dict_eval = eval(sys.argv[2])
            except Exception as e:
                print(sys.argv[2])
                print(f"Error: {e}")
                raise (ValueError(f"Check dictionary format provided in {sys.argv[2]} ... FAIL"))


            if isinstance(cfg_argv_dict_eval, dict):
                cfg_argv_dict = cfg_argv_dict_eval
            else:
                print("Dictionary not provided in sys.argv[2]. sys.arg vaules are:")
                print(sys.argv[2])

                print("System argument values are:")
                for item in sys.argv:
                    print(f"item : {item}")
                raise (ValueError(f"Check dictionary format provided in {sys.argv[2]} ... FAIL"))


        if len(sys.argv) > 1:
            if not os.path.isfile(sys.argv[1]):
                raise (FileNotFoundError(f"Input file {sys.argv[1]} not found ... FAIL"))
            else:
                inputfile = sys.argv[1]

        if len(sys.argv) <= 1:
            if not os.path.isfile(inputfile):
                raise (FileNotFoundError(f"Input file {inputfile} not found ... FAIL"))
            else:
                sys.argv.append(inputfile)

        return inputfile, cfg_argv_dict


    def save_cfg(self, cfg_base):
        output_dir = cfg_base.Analysis["analysis_root_folder"]

        filename = cfg_base.Analysis["file_name"]
        filename_path = os.path.join(output_dir, "results", filename)
        cfg_base = self.standardize_yml_data(cfg_base)

        save_data.saveDataYaml(cfg_base, filename_path, default_flow_style=False)
        return cfg_base
    
    def standardize_yml_data(self, data):
        """
        Recursively clean up the yml data structure to ensure readability and consistency.
        """
        if isinstance(data, dict):  # Process dictionaries
            return {key: self.standardize_yml_data(value) for key, value in data.items()}
        elif isinstance(data, list):  # Process lists
            return [self.standardize_yml_data(item) for item in data]
        elif isinstance(data, Path):  # Convert WindowsPath to string
            return str(data)
        elif isinstance(data, np.ndarray):  # Convert NumPy arrays to lists
            return data.tolist()
        elif isinstance(data, (np.integer, np.int32, np.int64)):  # Convert NumPy int to Python int
            return int(data)
        elif isinstance(data, (np.float32, np.float64)):  # Convert NumPy float to Python float
            return float(data)
        return data 
