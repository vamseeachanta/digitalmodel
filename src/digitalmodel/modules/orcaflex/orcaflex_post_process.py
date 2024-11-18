# Standard library imports
import copy
import logging
import math

# Third party imports
import pandas as pd

try:
    # Third party imports
    import OrcFxAPI
except:
    logging.debug("OrcFxAPI not available")


# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.modules.orcaflex.opp import OrcaFlexAnalysis
from digitalmodel.modules.orcaflex.opp_visualization import OPPVisualization
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()
ofa = OrcaFlexAnalysis()

opp_visualization = OPPVisualization()

class orcaflex_post_process:

    def __init__(self):
        pass

    def post_process_router(self, cfg):

        orcaflex_license_flag = ou.is_orcaflex_available()
        assert (orcaflex_license_flag)
        if not orcaflex_license_flag:
            raise Exception("Orcaflex license not available.")

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
            opp_visualization.get_visualizations(cfg)
        else:
            logging.info("No postprocess option to run specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if 'summary_settings_master' in cfg:
            summary_settings_master = cfg['summary_settings_master'].copy()
            summary_settings = cfg['summary_settings']

            for group_index in range(0, len(summary_settings['groups'])):
                group = summary_settings['groups'][group_index].copy()

                if 'Columns' in summary_settings_master['groups'][0]:
                    for column_index in range(0, len(group['Columns'])):
                        column = group['Columns'][column_index].copy()
                        column = update_deep_dictionary(
                            summary_settings_master['groups'][group_index]['Columns'][0], column)
                        group['Columns'][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(summary_settings_master['groups'][group_index], group)
                summary_settings['groups'][group_index] = copy.deepcopy(group)

            cfg['summary_settings'] = copy.deepcopy(summary_settings)

        if 'time_series_settings_master' in cfg:
            time_series_settings_master = cfg['time_series_settings_master'].copy()
            time_series_settings = cfg['time_series_settings']

            for group_index in range(0, len(time_series_settings['groups'])):
                group = time_series_settings['groups'][group_index].copy()

                if 'Columns' in time_series_settings_master['groups'][0]:
                    for column_index in range(0, len(group['Columns'])):
                        column = group['Columns'][column_index].copy()
                        column = update_deep_dictionary(
                            time_series_settings_master['groups'][group_index]['Columns'][0], column)
                        group['Columns'][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(time_series_settings_master['groups'][group_index], group)
                time_series_settings['groups'][group_index] = copy.deepcopy(group)

            cfg['time_series_settings'] = copy.deepcopy(time_series_settings)

        return cfg


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

