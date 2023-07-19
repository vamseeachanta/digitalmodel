import os
import yaml
import datetime
import logging
import functools
import pkgutil

from digitalmodel.common.update_deep import update_deep_dictionary
from digitalmodel.common.data import AttributeDict
from digitalmodel.common.set_logging import set_logging
# from common.database import Database


def applicationTimer(func):
    import functools

    @functools.wraps(func)
    def wrapper_applicationTimer(*args, **kwargs):
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
        import logging
        app_runs = ApplicationRuns(basename=func.__name__)
        logging.info("  ******Set Up Application Runs .... ******")
        cfg = app_runs.configureApplication()
        kwargs['cfg'] = cfg

        import time
        save_results = SaveApplicationResults()

        start_time = time.perf_counter()
        function_value = func(*args, **kwargs)
        end_time = time.perf_counter()
        run_time = end_time - start_time
        if cfg.default.__contains__(
                'data_source') and cfg.default['data_source'] == 'db':
            save_results.saveApplicationResultsToDB(cfg=kwargs['cfg'],
                                                    run_time=run_time,
                                                    run_dict=None)

        if cfg.default.__contains__(
                'data_source') and cfg.default['data_source'] == 'db':
            run_df = app_runs.getRuns()
            if (run_df is not None) and (len(run_df) > 0):
                for row_index in range(0, len(run_df)):
                    start_time = time.perf_counter()
                    run_dict = run_df.to_dict('records')[row_index]
                    cfg = app_runs.configureApplication(run_dict)
                    kwargs['cfg'] = cfg
                    function_value = func(*args, **kwargs)

                    end_time = time.perf_counter()
                    run_time = end_time - start_time
                    save_results.saveApplicationResultsToDB(cfg=kwargs['cfg'],
                                                            run_time=run_time,
                                                            run_dict=run_dict)

        logging.info("  ****** Application Runs .... COMPLETE ******")

        return function_value

    return wrapper_applicationRuns


def decoratorWithArgumentExample(func=None, *, kw1=None):
    '''Decotrator with keyword argument Boiler Plate Example'''
    import functools

    @functools.wraps(func)
    def wrapper_decoratorWithArgumentExample(*args, **kwargs):
        cfg = func(*args, **kwargs)
        return cfg

    return wrapper_decoratorWithArgumentExample


class ApplicationRuns():

    def __init__(self, basename):
        self.basename = basename

    def configureApplication(self, run_dict=None):
        configure_app = ConfigureApplicationInputs(self.basename)
        self.cfg = configure_app.configure(run_dict)

        return self.cfg

    def getRuns(self):
        import pandas as pd
        run_df = pd.DataFrame()
        database_runs_flag = self.cfg.default.get('database_runs', None)
        if database_runs_flag is not None and database_runs_flag:
            self.addRunRecordsToDB()
            run_df = self.getApplicatonRunsFromDB()

        return run_df

    def addRunRecordsToDB(self):

        if self.cfg.__contains__('db'):
            db_properties = self.cfg.db
            self.dbe = Database(db_properties)
            self.dbe.set_up_db_connection(db_properties)

            run_file = os.path.join("tests\\cfg\\", self.basename, 'runs.csv')
            run_file_updated = os.path.join("tests\\cfg\\", self.basename,
                                            'runs_added_to_db.csv')

            ApplicationIDQuery = "SELECT TOP 1 ApplicationID from [dbo].[Application] WHERE ApplicationName = '{}'".format(
                self.basename)
            result_df = self.dbe.get_df_from_query(ApplicationIDQuery)
            self.ApplicationId = result_df['ApplicationID'].iloc[0]

            if os.path.isfile(run_file):
                import pandas as pd
                df = pd.read_csv(run_file, header='infer')
                df['ApplicationId'] = self.ApplicationId
                self.dbe.save_to_db(df, table_name='ApplicationRuns')
                logging.info("Runs records to add to db")
            else:
                logging.info("No Runs records to add to db")

    def getApplicatonRunsFromDB(self):
        ApplicationRunsQuery = "SELECT * FROM [master].[dbo].[ApplicationRuns] WHERE ApplicationId={} and RunStatus=0".format(
            self.ApplicationId)
        run_df = self.dbe.get_df_from_query(ApplicationRunsQuery)

        return run_df


class ConfigureApplicationInputs():

    def __init__(self, basename):
        self.basename = basename

    def configure(self, run_dict):
        self.unify_application_and_default_and_custom_yamls(run_dict=run_dict)
        self.get_application_configuration_parameters(run_dict=run_dict)
        self.configure_overwrite_filenames()
        self.convert_cfg_to_attribute_dictionary()

        set_logging(self.cfg)
        logging.info(self.cfg)

        return self.cfg

    def unify_application_and_default_and_custom_yamls(self, run_dict):
        self.ApplicationInputFile = os.getcwd(
        ) + '\\src\\digitalmodel\\tests\\test_data\\' + self.basename + '.yml'
        self.get_custom_file()
        if not os.path.isfile(self.ApplicationInputFile):
            try:
                self.ApplicationInputFile = 'tests/test_data/' + self.basename + "/" + self.basename + '.yml'
                data = pkgutil.get_data('digitalmodel',
                                        self.ApplicationInputFile)
            except:
                self.ApplicationInputFile = 'tests/test_data/' + self.basename + '.yml'
                data = pkgutil.get_data('digitalmodel',
                                        self.ApplicationInputFile)
            self.ApplicationInputFile_dict = yaml.safe_load(data)

        # Get updated configuration file for Analysis
        self.cfg = self.generateYMLInput(run_dict)

    def get_custom_file(self, run_dict=None):
        import sys

        try:
            if (sys.argv[1] != None):
                self.customYaml = sys.argv[1]
                print(
                    "Updating default values with contents in file {0}".format(
                        self.customYaml))
        except:
            self.customYaml = None
            print(
                "No update values file is provided. Running program default values "
                "from {0}".format(self.ApplicationInputFile))

        if run_dict is not None:
            self.customYaml = None
            self.CustomInputs = run_dict.get('CustomInputs', None)
        else:
            self.CustomInputs = None

    def generateYMLInput(self, run_dict):

        if os.path.isfile(self.ApplicationInputFile):
            with open(self.ApplicationInputFile, 'r') as ymlfile:
                cfg = yaml.load(ymlfile, Loader=yaml.Loader)
        else:
            cfg = self.ApplicationInputFile_dict

        if self.customYaml is not None:
            try:
                with open(self.customYaml, 'r') as ymlfile:
                    cfgCustomValues = yaml.load(ymlfile, Loader=yaml.Loader)
                    default_yaml_file = cfgCustomValues.get(
                        'default_yaml', None)

                with open(self.customYaml) as fp:
                    custom_file_data = fp.read()
            except Exception as e:
                import sys
                print("Update Input file could not be loaded successfully.")
                print("Error is : {}".format(e))
                print("Stopping program")
                sys.exit()
        elif self.CustomInputs is not None:
            import json
            custom_file_data = self.CustomInputs.replace("\\\'", "\'").replace(
                "\\n", "\n")
            default_yaml_file = run_dict['DefaultInputFile']
        else:
            custom_file_data = ''
            default_yaml_file = None

        if (self.customYaml is not None) or (self.CustomInputs is not None):
            if default_yaml_file is None:
                cfgDefaultAndCustomValues = yaml.load(custom_file_data,
                                                      Loader=yaml.Loader)
            else:
                with open(default_yaml_file) as fp:
                    default_file_data = fp.read()
                custom_and_default_yaml_data = custom_file_data + "\n" + default_file_data
                cfgDefaultAndCustomValues = yaml.load(
                    custom_and_default_yaml_data, Loader=yaml.Loader)

            cfg = update_deep_dictionary(cfg, cfgDefaultAndCustomValues)

        return cfg

    def get_application_configuration_parameters(self, run_dict=None):

        application_start_time = datetime.datetime.now()

        if self.customYaml is not None:
            custom_file_name = os.path.split(self.customYaml)[1].split('.')[0]
            AnalysisRootFolder = os.path.split(self.customYaml)[0]
        elif (self.CustomInputs is not None):
            custom_file_name = run_dict['RunName']
            AnalysisRootFolder = os.path.join(os.getcwd(), 'tests\cfg',
                                              self.basename)
        else:
            custom_file_name = os.path.split(
                self.ApplicationInputFile)[1].split('.')[0]
            AnalysisRootFolder = os.getcwd()

        file_name = 'app_' + self.basename + '_' + custom_file_name + '_' + application_start_time.strftime(
            '%Y%m%d_%Hh%Mm')
        file_name_for_overwrite = 'app_' + self.basename + '_' + custom_file_name
        result_folder = AnalysisRootFolder + '\\results\\'
        result_data_folder = result_folder + '\\Data'
        result_plot_folder = result_folder + '\\Plot'
        result_folder = AnalysisRootFolder + '\\results\\'
        log_folder = AnalysisRootFolder + '\\logs\\'

        if not os.path.exists(result_folder):
            os.mkdir(result_folder)
        if not os.path.exists(log_folder):
            os.mkdir(log_folder)
        if not os.path.exists(result_data_folder):
            os.mkdir(result_data_folder)
        if not os.path.exists(result_plot_folder):
            os.mkdir(result_plot_folder)

        cfg_array_file_names = None

        application_configuration_parameters = {
            "Analysis": {
                "basename": self.basename,
                "analysis_root_folder": AnalysisRootFolder,
                "file_name": file_name,
                "file_name_for_overwrite": file_name_for_overwrite,
                "result_folder": result_folder,
                "log_folder": log_folder,
                "start_time": application_start_time,
                'cfg_array_file_names': cfg_array_file_names,
                'DefaultInputFile': self.cfg.get('default_yaml', None),
                'CustomInputFile': self.customYaml
            }
        }

        self.cfg = update_deep_dictionary(self.cfg,
                                          application_configuration_parameters)

    def configure_overwrite_filenames(self):
        if self.cfg['default']['config']['overwrite']['output'] == True:
            self.cfg['Analysis']['file_name'] = self.cfg['Analysis'][
                'file_name_for_overwrite']
        try:
            if self.cfg['Analysis']['fe_folder'] == None:
                self.cfg['Analysis']['fe_folder'] = self.cfg['Analysis'][
                    'result_folder']
        except:
            pass

    def convert_cfg_to_attribute_dictionary(self):
        self.cfg = AttributeDict(self.cfg)


class SaveApplicationResults():

    def __init__(self):
        pass

    def saveApplicationResultsToDB(self, cfg, run_time, run_dict=None):
        import json

        import pandas as pd
        import yaml

        from common.database import Database

        try:
            db_properties = cfg.db
            dbe = Database(db_properties)
            dbe.set_up_db_connection(db_properties)

            columns = [
                'ApplicationId', 'ProjectId', 'RunName', 'RunStatus',
                'DefaultInputFile', 'CustomInputFile', 'CustomInputs',
                'AnalysisTime', 'RunTimeInSeconds'
            ]
            df = pd.DataFrame(columns=columns)

            ApplicationName = cfg.Analysis['basename']
            ApplicationIDQuery = "SELECT TOP 1 ApplicationID from [dbo].[Application] WHERE ApplicationName = '{}'".format(
                ApplicationName)
            result_df = dbe.get_df_from_query(ApplicationIDQuery)
            ApplicationId = result_df['ApplicationID'].iloc[0]

            ProjectName = cfg.get('ProjectName', 'DigitalTwinFeed')
            ProjectIDQuery = "SELECT TOP 1 ProjectId from [dbo].[Project] WHERE ProjectName = '{}'".format(
                ProjectName)
            result_df = dbe.get_df_from_query(ProjectIDQuery)
            ProjectId = result_df['ProjectId'].iloc[0]
            if run_dict is not None:
                RunName = run_dict['RunName']
            else:
                RunName = None

            RunStatus = 1
            DefaultInputFile = cfg.Analysis.get('DefaultInputFile', None)

            CustomInputFile = cfg.Analysis.get('CustomInputFile', None)

            if run_dict is None:
                if CustomInputFile is not None:
                    with open(CustomInputFile, 'r') as ymlfile:
                        try:
                            CustomInputs = json.dumps(yaml.load(
                                ymlfile, Loader=yaml.Loader),
                                                      default=str)
                        except:
                            CustomInputs = None
                else:
                    CustomInputs = None
            else:
                CustomInputs = run_dict['CustomInputs'].replace("\\\'",
                                                                "\'").replace(
                                                                    "\\n", "\n")

            AnalysisTime = cfg.Analysis['start_time']
            RunTimeInSeconds = run_time

            ApplicationRunQuery = """
                BEGIN TRAN
                UPDATE [ApplicationRuns]  SET RunStatus={0}, RunTimeInSeconds={1} WHERE ApplicationId={2} and ProjectId={3} and RunName = '{4}' and RunStatus=0
                IF @@rowcount = 0
                    BEGIN
                        INSERT INTO [ApplicationRuns] (ApplicationId,ProjectId,RunName,RunStatus,DefaultInputFile,CustomInputFile,CustomInputs,AnalysisTime,RunTimeInSeconds) 
                        VALUES ({2},{3},'{4}',{0},'{5}','{6}','{7}','{8}', {1})
                    END
                COMMIT TRAN
                """.format(RunStatus, RunTimeInSeconds, ApplicationId,
                           ProjectId, RunName, DefaultInputFile,
                           CustomInputFile, CustomInputs, AnalysisTime)

            dbe.executeNoDataQuery(ApplicationRunQuery)
        except:
            print("Encountered error while saving application result to db")
