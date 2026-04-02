import datetime
import os
import sys

from digitalmodel.infrastructure.utils.update_deep import update_deep_dictionary
from digitalmodel.infrastructure.utils.ymlInput import ymlInput

application_start_time = datetime.datetime.now()


def application_configuration(basename):
    """Build application configuration from YAML files and system arguments.

    Loads a default YAML configuration file based on the basename, optionally
    merges in an update YAML file provided as a command-line argument, and
    adds application configuration parameters (file names, folders, etc.).

    Args:
        basename: Base name used to locate the default YAML config file
            and to construct output file names.

    Returns:
        dict: The merged configuration dictionary containing application
            settings and parameters.
    """
    defaultYml = 'data_manager\\' + basename + '.yml'
    try:
        if (sys.argv[1] != None):
            updateYml = sys.argv[1]
            print("Updating default values with contents in file {0}".format(updateYml))
    except:
        updateYml = None
        print("No update values file is provided. Running program default values")
    # Get updated configuration file for Analysis
    cfg = ymlInput(defaultYml, updateYml)

    application_configuration_parameters = get_application_configuration_parameters(basename, defaultYml, updateYml)
    cfg = update_deep_dictionary(cfg, application_configuration_parameters)

    return cfg


def get_application_configuration_parameters(basename, defaultYml, updateYml):
    """Generate application configuration parameters including file paths and folders.

    Creates result and log directories if they do not exist, and builds
    a dictionary with file naming conventions and folder paths.

    Args:
        basename: Base name for the application.
        defaultYml: Path to the default YAML configuration file.
        updateYml: Path to the update YAML file, or None if not provided.

    Returns:
        dict: A dictionary with 'Analysis' key containing file_name,
            file_name_for_overwrite, result_folder, log_folder, and
            start_time entries.
    """
    if updateYml is not None:
        file_name = 'app_' + basename + '_' + os.path.split(updateYml)[1].split(
            '.')[0] + '_' + application_start_time.strftime('%Y%m%d_%Hh%Mm')
        file_name_for_overwrite = 'app_' + basename + '_' + os.path.split(updateYml)[1].split('.')[0]
        AnalysisRootFolder = os.path.split(updateYml)[0]
        result_folder = AnalysisRootFolder + '\\results\\'
        log_folder = AnalysisRootFolder + '\\logs\\'
    else:
        file_name = 'app_' + basename + '_' + os.path.split(defaultYml)[1].split(
            '.')[0] + '_' + application_start_time.strftime('%Y%m%d_%Hh%Mm')
        file_name_for_overwrite = 'app_' + basename + '_' + os.path.split(defaultYml)[1].split('.')[0]
        result_folder = os.getcwd() + '\\results\\'
        log_folder = os.getcwd() + '\\logs\\'

    if not os.path.exists(result_folder):
        os.mkdir(result_folder)
    if not os.path.exists(log_folder):
        os.mkdir(log_folder)

    result = {
        "Analysis": {
            "file_name": file_name,
            "file_name_for_overwrite": file_name_for_overwrite,
            "result_folder": result_folder,
            "log_folder": log_folder,
            "start_time": application_start_time
        }
    }

    return (result)
