import datetime
import os
import sys

from digitalmodel.infrastructure.common.update_deep import update_deep_dictionary
from digitalmodel.infrastructure.common.ymlInput import ymlInput

application_start_time = datetime.datetime.now()


def application_configuration(basename):
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
