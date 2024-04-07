import os
import math
import pandas as pd

try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")


import logging
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.update_deep import update_deep_dictionary
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.custom.orcaflex_analysis_components import OrcaFlexAnalysis

ou = OrcaflexUtilities()
ofa = OrcaFlexAnalysis()


class orcaflex_post_process:

    def __init__(self):
        pass

    def post_process_router(self, cfg):

        cfg = self.get_cfg_with_master_data(cfg)
        if cfg['orcaflex']['postprocess']['summary']['flag'] or cfg[
                    'orcaflex']['postprocess']['RangeGraph']['flag'] or cfg[
                        'orcaflex']['postprocess']['time_series'][
                            'flag'] or cfg['orcaflex']['postprocess'][
                                'cummulative_histograms']['flag']:
            post_process_data_flag = True
        else:
            post_process_data_flag = False

        if cfg['orcaflex']['postprocess']['visualization']['flag']:
            post_process_visualization_flag = True
        else:
            post_process_visualization_flag = False

        if post_process_data_flag:
            cfg.update({cfg['basename']: {}})
            ofa.post_process(cfg)
            ofa.save_summary(cfg)
        elif post_process_visualization_flag:
            self.get_visualizations(cfg)
        else:
            logging.info("No postprocess option to run specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if 'summary_settings_master' in cfg:
            summary_settings_master = cfg['summary_settings_master'].copy()
            summary_settings = cfg['summary_settings']

            for group_index in range(0, len(summary_settings['groups'])):
                group = summary_settings['groups'][group_index].copy()
                group = update_deep_dictionary(summary_settings_master['groups'][0], group)
                summary_settings['groups'][group_index] = group.copy()

        return cfg

    def get_visualizations(self, cfg):
        ov = orcaflex_visualizations()
        ov.get_visualizations(cfg)

    def post_process_superseded(self, cfg):
        # Intialize output arrays
        RangeAllFiles = []
        SummaryDFAllFiles = []

        # Load Simulation file(s)
        sim_files = cfg.file_management['input_files']['*.sim']
        summary_cfg = cfg['summary_settings'].copy()
        for fileIndex in range(0, len(sim_files)):
            FileName = sim_files[fileIndex]
            FileDescription = None
            FileObjectName = None
            model = OrcFxAPI.Model(FileName)

            # Perform (All) Range Postprocessing for each simulation file
            if cfg['orcaflex']['postprocess']['RangeGraph']['flag']:
                RangeAllFiles.append(
                    self.get_range_graphs(model, cfg, FileObjectName))

            for summary_group_index in range(0, len(summary_cfg['groups'])):
                try:
                    SummaryDFAllFiles[summary_group_index]
                except:
                    SummaryDFAllFiles.append(pd.DataFrame())

                SummaryDFAllFiles[summary_group_index] = self.get_summary_group(
                    model, cfg, SummaryDFAllFiles[summary_group_index],
                    summary_group_index, FileDescription, FileObjectName,
                    FileName)
                # TO DO: Parametrize this.
            # decimals = pd.Series([1, 1], index=columns)
        return RangeAllFiles, SummaryDFAllFiles

    def get_range_graphs(self, model, cfg, FileObjectName):
        # Range Graphs for a simulation
        RangeFile = []

        for RangeGraphIndex in range(0, len(cfg['RangeGraph'])):
            RangeDF = pd.DataFrame()
            #Read Object
            try:
                objectName = cfg['RangeGraph'][RangeGraphIndex]['ObjectName']
                OrcFXAPIObject = model[objectName]
            except:
                OrcFXAPIObject = model[FileObjectName]

            # Arc Length Definition
            #Time Period Definition
            TimePeriod = exec("OrcFxAPI.{0}".format(
                cfg['RangeGraph'][RangeGraphIndex]['SimulationPeriod']))
            VariableName = cfg['RangeGraph'][RangeGraphIndex]['Variable']

            try:
                if len(cfg['RangeGraph'][RangeGraphIndex]['ArcLength']) > 1:
                    StartArcLength = cfg['RangeGraph'][RangeGraphIndex][
                        'ArcLength'][0]
                    EndArcLength = cfg['RangeGraph'][RangeGraphIndex][
                        'ArcLength'][1]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength, EndArcLength))
                else:
                    StartArcLength = cfg['RangeGraph'][RangeGraphIndex][
                        'ArcLength'][0]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength))
                    # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
            except:
                arclengthRange = None
                output = OrcFXAPIObject.RangeGraph(
                    VariableName, TimePeriod, arclengthRange=arclengthRange)

            # output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100))
            # Assign Arc Length
            AdditionalDataName = 'X'
            RangeDF[AdditionalDataName] = output.X

            for AdditionalDataIndex in range(
                    0,
                    len(cfg['RangeGraph'][RangeGraphIndex]['AdditionalData'])):
                AdditionalDataName = cfg['RangeGraph'][RangeGraphIndex][
                    'AdditionalData'][AdditionalDataIndex]
                if AdditionalDataName == "Max":
                    RangeDF[VariableName] = output.Max
                elif AdditionalDataName == "Min":
                    RangeDF[VariableName] = output.Min
                elif AdditionalDataName == "Mean":
                    RangeDF[VariableName] = output.Mean
                if VariableName == "API STD 2RD Method 1":
                    APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
                    RangeDF[VariableName] = APISTD2RDM1
            RangeFile.append(RangeDF)

        return RangeFile

    def get_summary_group(self, model, cfg, SummaryDF, summary_group_index,
                          FileDescription, FileObjectName, FileName):

        summary_group = cfg['summary_settings']['groups'][summary_group_index]

        if SummaryDF.empty:
            columns = []
            columns.append('FileName')
            columns.append('Description')
            for column_index in range(0, len(summary_group['Columns'])):
                columns.append(summary_group['Columns'][column_index]['Label'])

            SummaryDF = pd.DataFrame(columns=columns)

        SummaryFile = []
        SummaryFile.append(FileName)
        SummaryFile.append(FileDescription)
        for column_index in range(0, len(summary_group['Columns'])):
            RangeDF = pd.DataFrame()
            #Read Object

            try:
                objectName = summary_group['Columns'][column_index][
                    'ObjectName']
                OrcFXAPIObject = model[objectName]
            except:
                OrcFXAPIObject = model[FileObjectName]

            # Arc Length Definition
            #Time Period Definition
            TimePeriod = exec("OrcFxAPI.{0}".format(
                summary_group['Columns'][column_index]['SimulationPeriod']))
            VariableName = summary_group['Columns'][column_index]['Variable']

            try:
                if len(summary_group['Columns'][column_index]['ArcLength']) > 1:
                    StartArcLength = summary_group['Columns'][column_index][
                        'ArcLength'][0]
                    EndArcLength = summary_group['Columns'][column_index][
                        'ArcLength'][1]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength, EndArcLength))
                else:
                    StartArcLength = summary_group['Columns'][column_index][
                        'ArcLength'][0]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength))
                    # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
            except:
                arclengthRange = None
                output = OrcFXAPIObject.RangeGraph(
                    VariableName, TimePeriod, arclengthRange=arclengthRange)

            # output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100))
            # Assign Arc Length
            AdditionalDataName = 'X'
            RangeDF[AdditionalDataName] = output.X

            for AdditionalDataIndex in range(
                    0,
                    len(summary_group['Columns'][column_index]
                        ['AdditionalData'])):
                AdditionalDataName = summary_group['Columns'][column_index][
                    'AdditionalData'][AdditionalDataIndex]
                if AdditionalDataName == "Max":
                    RangeDF[VariableName] = output.Max
                    if VariableName == "API STD 2RD Method 1":
                        APISTD2RDM1 = [
                            math.sqrt(x) for x in RangeDF[VariableName]
                        ]
                        RangeDF[VariableName] = APISTD2RDM1
                    SummaryFile.append(max(RangeDF[VariableName]))
                elif AdditionalDataName == "Min":
                    RangeDF[VariableName] = output.Min
                    if VariableName == "API STD 2RD Method 1":
                        APISTD2RDM1 = [
                            math.sqrt(x) for x in RangeDF[VariableName]
                        ]
                        RangeDF[VariableName] = APISTD2RDM1
                    SummaryFile.append(min(RangeDF[VariableName]))

        SummaryDF.loc[len(SummaryDF)] = SummaryFile

        return SummaryDF

    def LinkedStatistics(self):
        # LinkedStatistics using OrcaFlex. Code currently not working.
        VariableName = 'Effective Tension', 'Bend Moment'
        arclengthRange = OrcFxAPI.oeArcLength(25.0)
        TimePeriod = exec("OrcFxAPI.{0}".format(
            cfg["PostProcess"]['Summary'][SummaryIndex]['SimulationPeriod']))
        stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod,
                                                arclengthRange)
        query = stats.Query('Effective Tension', 'Bend Moment')
        print(query.ValueAtMax)
        print(query.ValueAtMin)
        # Alternatively, use Range graphs for required quantities and obtain them using DFs. (easiest to customize)


class orcaflex_visualizations:

    def __init__(self):
        pass

    def get_visualizations(self, cfg):
        self.save_views_for_files(cfg)

    def is_file_valid(self, file_name):
        is_file_valid, file_name = is_file_valid_func(file_name)

        return is_file_valid, file_name

    def save_views_for_files(self, cfg):
        model = OrcFxAPI.Model()
        combined_model = None

        if cfg.file_management['files']['files_in_current_directory'][
                'flag']:
            orcaflex_extensions = ['yml', 'yaml', 'dat', 'sim', 'txt']

        else:
            orcaflex_extensions = cfg.file_management['input_files'].keys()

        for file_ext in orcaflex_extensions:
            raw_input_files_for_ext = cfg.file_management['input_files'][
                file_ext]

            for input_file_index in range(0, len(raw_input_files_for_ext)):
                input_file = raw_input_files_for_ext[input_file_index]

                model.LoadData(input_file)

                if cfg['visualization_settings']['combined']:
                    print(f"Combined model code in library does not exist")
                    # combined_model = self.combine_models(combined_model, model)

                model = self.set_general_visualization_settings(model, cfg)
                model.CalculateStatics()
                self.save_all_views(model, input_file, cfg)

            #TODO 
            # if cfg['visualization_settings']['combined']:
            #     combined_model.CalculateStatics()
                
    def set_general_visualization_settings(self, model, cfg):
        #TODO for TDP Colour change
        # line = model[cfg['visualization_settings']['tdp_line']]
        # line.ContactPenColour = 128 * 65536 + 128 * 256 + 128

        env = model['Environment']
        # env.SeabedPenStyle = "Clear"
        # env.SeabedProfilePenStyle = "Clear"
        env.SeaSurfacePenStyle = "Clear"
        model.general.NorthDirectionDefined = "No"

        #TODO for vessel settings
        # vessel = model["SevenArctic"]
        # x_value = vessel.InitialX
        # y_value = vessel.InitialY
        # heading = vessel.InitialHeading

        hide_items = cfg['visualization_settings']['hide_items']

        all_objects = []
        for obj in model.objects:
            Name = str(obj)
            all_objects.append(Name)
        for item in hide_items:
            if item in all_objects:
                model[item].Hidden = "Yes"

        #TODO crane settings
        # crane = model["250TeCrane"]
        # crane.OutsidePenStyle = "Dot"
        # crane.InsidePenStyle = "Clear"
        # crane.NumberOfLines = 2
        return model

    def combine_models(self, combined_model, model):
        if combined_model is None:
            combined_model = model
        else:
            for obj in model.objects:
                combined_model.createObject(obj)
                line = combined_model.CreateObject(obj.type)

        combined_model.SaveData("combined_model.dat")
        return combined_model

    def save_all_views(self, model, file_name, cfg):

        viewparams_cfg = cfg['visualization_settings']['viewparams']
        for view_label in list(viewparams_cfg.keys()):
            viewparams = self.assign_view_parameters(model, cfg, view_label)
            self.save_image(model, file_name, viewparams, view_label)

    def assign_view_parameters(self, model, cfg, view_label):

        viewparams_view_label_cfg = cfg['visualization_settings']['viewparams'][view_label]
        viewparams = model.defaultViewParameters

        if 'SeaSurfacePenStyle' in viewparams_view_label_cfg:
            env = model['Environment']
            env.SeaSurfacePenStyle = viewparams_view_label_cfg['SeaSurfacePenStyle']

        for key in viewparams_view_label_cfg:
            try:
                if key == 'ViewCentre':
                    ViewCentre = viewparams_view_label_cfg['ViewCentre']
                    for i in range(0, len(ViewCentre)):
                        viewparams.ViewCentre[i] = ViewCentre[i]
                elif key == 'RelativeToObject':
                    viewparams.RelativeToObject = model[
                        viewparams_view_label_cfg['RelativeToObject']]
                else:
                    setattr(viewparams, key, viewparams_view_label_cfg[key])
            except Exception as e:
                logging.error(str(e))

        return viewparams

    def save_image(self, model, file_name, viewparams, view_label):
        file_location = os.path.split(file_name)[0]
        file_name_img = os.path.basename(file_name).split(
            ".")[0] + "_" + view_label + ".jpg"
        file_name_with_path = os.path.join(file_location, file_name_img)
        logging.info(f"Saving ...  {file_name_img}  view")
        model.SaveModelView(file_name_with_path, viewparams)
