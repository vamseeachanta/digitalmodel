import os
import OrcFxAPI
# import pandas as pd
# import sys
# import math
# import pickle


class orcaflex_post_process:

    def __init__(self):
        pass

    def post_process_router(self, cfg):
        if cfg['orcaflex']['postprocess']['visualization']:
            self.get_visualizations(cfg)

    def get_visualizations(self, cfg):
        ov = orcaflex_visualizations()
        ov.get_visualizations(cfg)


class orcaflex_visualizations:

    def __init__(self):
        pass

    def get_visualizations(self, cfg):
        self.get_files(cfg)
        self.save_views_for_files(cfg)

    def get_files(self, cfg):
        for file_index in range(0, len(cfg['Files']['data'])):
            file_cfg = cfg['Files']['data'][file_index]
            if not os.path.isfile(file_cfg['Name']):
                file_cfg['Name'] = os.path.join(os.getcwd(), file_cfg['Name'])

    def save_views_for_files(self, model, x):
        model = OrcFxAPI.Model()

        for x in wf.loadcasedat:
            print(x)
            model.LoadData(x)

            # for TDP Colour change
            line = model["10inGE_R1"]
            line.ContactPenColour = 128 * 65536 + 128 * 256 + 128

            env = model['Environment']
            # env.SeabedPenStyle = "Clear"
            # env.SeabedProfilePenStyle = "Clear"
            env.SeaSurfacePenStyle = "Clear"
            model.general.NorthDirectionDefined = "No"

            vessel = model["NO102"]
            x_value = vessel.InitialX
            y_value = vessel.InitialY
            heading = vessel.InitialHeading

            # items_to_hide = ["20m_FPSO_envelop","ML1","ML2","ML3","ML4","ML5","ML6","ML7","ML8","ML9","Turret","Turret_Ref","I_Tube_R1"
            #                  ,"STPLightShip","STP_buoy_main body","TurretShape","STP_buoy_origin","GE_Curve","b6 vls arch1","HOP-1", "HOP-2"
            #                  ,"VLS_Bellmouth","Yoke_Contrain","R1_PIHO_Con" ]
            items_to_hide = [
                "20m_FPSO_envelop", "LowestTransfer Position", "b6 vls arch1",
                "R1_PIHO_Con", "PullHead_R1", "250TeCrane"
            ]

            all_objects = []
            for obj in model.objects:
                y = str(obj)
                name_raw = y.split(":")[1]
                name_raw_2 = name_raw[:len(name_raw) - 2]
                all_objects.append(name_raw_2[2:])
            for item in items_to_hide:
                if item in all_objects:
                    model[item].Hidden = "Yes"
                else:
                    pass

            # crane = model["250TeCrane"]
            # crane.OutsidePenStyle = "Dot"
            # crane.InsidePenStyle = "Clear"
            # crane.NumberOfLines = 2

            model.CalculateStatics()
            self.plan_view(model, x)
            self.elevation_view(model, x)

    def plan_view(self, model, x):
        '''        Plan View      '''
        viewparams = model.defaultViewParameters
        viewparams.RelativeToObject = model['NO102']
        viewparams.ViewCentre = -150, 0, 0
        viewparams.ViewSize = 200
        # if heading == 0:
        #     viewparams.ViewCentre[0] = x_value - 50
        #     viewparams.ViewCentre[1] = 0
        # else:
        #     viewparams.ViewCentre[0] = x_value - 50
        #     viewparams.ViewCentre[1] = y_value * 0.5
        viewparams.DrawViewAxes = False
        viewparams.DrawGlobalAxes = False
        viewparams.DrawScaleBar = False
        viewparams.ViewAzimuth = 270
        viewparams.ViewElevation = 90
        viewparams.ViewGamma = 0
        viewparams.Height = 500
        viewparams.Width = 800
        viewparams.BackgroundColour = 255 * 65536 + 255 * 256 + 255

        file_name = wf.location_name + x.split(
            "/")[1][:-4] + "_image_PlanView.jpg"
        print(file_name)
        model.SaveModelView(file_name, viewparams)

    def elevation_view(self, model, x):
        '''        Elevation  View      '''
        env.SeaSurfacePenStyle = "Solid"
        viewparams = model.defaultViewParameters
        viewparams.RelativeToObject = model['NO102']
        viewparams.ViewCentre = -150, 0, -200
        viewparams.ViewSize = 550
        # if heading == 0:
        #     viewparams.ViewCentre[0] = x_value - 50
        #     viewparams.ViewCentre[1] = 0
        # else:
        #     viewparams.ViewCentre[0] = x_value - 50
        #     viewparams.ViewCentre[1] = y_value * 0.5
        # viewparams.ViewCentre[2] = -170
        viewparams.DrawViewAxes = False
        viewparams.DrawGlobalAxes = False
        viewparams.DrawScaleBar = False
        viewparams.ViewAzimuth = 270
        viewparams.ViewElevation = 0
        viewparams.ViewGamma = 0
        viewparams.Height = 500
        viewparams.Width = 800
        viewparams.BackgroundColour = 255 * 65536 + 255 * 256 + 255

        file_name = wf.location_name + x.split(
            "/")[1][:-4] + "_image_ElevationView.jpeg"
        print(file_name)
        model.SaveModelView(file_name, viewparams)


#
# '''
# To load and postprocess model files
# '''
# def postProcess(cfg):
#     # Intialize output arrays
#     RangeAllFiles = []
#     SummaryDFAllFiles = []
#
#     # Load Simulation file(s)
#     for fileIndex in range(0, len(cfg['Analysis']['input_files']['no_ext'])):
#         FileName = cfg['Analysis']['input_files']['no_ext'][fileIndex] + '.sim'
#         FileDescription = cfg['Files'][fileIndex]['Label']
#         FileObjectName = cfg['Files'][fileIndex]['ObjectName']
#         model = loadSimulation(FileName)
#
#         # Perform (All) Range Postprocessing for each simulation file
#         RangeAllFiles.append(postProcessRange(model, cfg, FileObjectName))
#
#         for SummaryIndex in range(0, len(cfg['Summary'])):
#             try:
#                 SummaryDFAllFiles[SummaryIndex]
#             except:
#                 SummaryDFAllFiles.append(pd.DataFrame())
#
#             SummaryDFAllFiles[SummaryIndex] = postProcessSummary(model, cfg, SummaryDFAllFiles[SummaryIndex], SummaryIndex, FileDescription, FileObjectName, FileName)
#             # TO DO: Parametrize this.
#         # decimals = pd.Series([1, 1], index=columns)
#     return RangeAllFiles, SummaryDFAllFiles
#
# # Load Simulation File
# def loadSimulation(FileName):
#     model = OrcFxAPI.Model(FileName)
#     return model
#
# def postProcessRange(model, cfg, FileObjectName):
#     # Range Graphs for a simulation
#     RangeFile = []
#
#     for RangeGraphIndex in range(0, len(cfg['RangeGraph'])):
#         RangeDF = pd.DataFrame()
#         #Read Object
#         try:
#             objectName = cfg['RangeGraph'][RangeGraphIndex]['ObjectName']
#             OrcFXAPIObject = model[objectName]
#         except:
#             OrcFXAPIObject = model[FileObjectName]
#
#         # Arc Length Definition
#         #Time Period Definition
#         TimePeriod = exec("OrcFxAPI.{0}" .format(cfg['RangeGraph'][RangeGraphIndex]['SimulationPeriod']))
#         VariableName = cfg['RangeGraph'][RangeGraphIndex]['Variable']
#
#         try:
#             if len(cfg['RangeGraph'][RangeGraphIndex]['ArcLength']) > 1:
#                 StartArcLength = cfg['RangeGraph'][RangeGraphIndex]['ArcLength'][0]
#                 EndArcLength = cfg['RangeGraph'][RangeGraphIndex]['ArcLength'][1]
#                 output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength, EndArcLength))
#             else:
#                 StartArcLength = cfg['RangeGraph'][RangeGraphIndex]['ArcLength'][0]
#                 output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength))
#                 # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
#         except:
#             arclengthRange = None
#             output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = arclengthRange)
#
#         # output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100))
#         # Assign Arc Length
#         AdditionalDataName = 'X'
#         RangeDF[AdditionalDataName] = output.X
#
#         for AdditionalDataIndex in range(0, len(cfg['RangeGraph'][RangeGraphIndex]['AdditionalData'])):
#             AdditionalDataName = cfg['RangeGraph'][RangeGraphIndex]['AdditionalData'][AdditionalDataIndex]
#             if AdditionalDataName == "Max":
#                 RangeDF[VariableName] = output.Max
#             elif AdditionalDataName == "Min":
#                 RangeDF[VariableName] = output.Min
#             elif AdditionalDataName == "Mean":
#                 RangeDF[VariableName] = output.Mean
#             if VariableName == "API STD 2RD Method 1":
#                 APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
#                 RangeDF[VariableName]= APISTD2RDM1
#         RangeFile.append(RangeDF)
#
#     return RangeFile
#
# def postProcessSummary(model, cfg, SummaryDF, SummaryIndex, FileDescription, FileObjectName, FileName):
#     if SummaryDF.empty:
#         columns = []
#         columns.append('FileName')
#         columns.append('Description')
#         for SummaryColumnIndex in range(0, len(cfg['Summary'][SummaryIndex]['Columns'])):
#             columns.append(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['Label'])
#
#
#         SummaryDF = pd.DataFrame(columns = columns)
#
#     SummaryFile = []
#     SummaryFile.append(FileName)
#     SummaryFile.append(FileDescription)
#     for SummaryColumnIndex in range(0, len(cfg['Summary'][SummaryIndex]['Columns'])):
#         RangeDF = pd.DataFrame()
#         #Read Object
#
#         try:
#             objectName = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ObjectName']
#             OrcFXAPIObject = model[objectName]
#         except:
#             OrcFXAPIObject = model[FileObjectName]
#
#         # Arc Length Definition
#         #Time Period Definition
#         TimePeriod = exec("OrcFxAPI.{0}" .format(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['SimulationPeriod']))
#         VariableName = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['Variable']
#
#         try:
#             if len(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength']) > 1:
#                 StartArcLength = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength'][0]
#                 EndArcLength = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength'][1]
#                 output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength, EndArcLength))
#             else:
#                 StartArcLength = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength'][0]
#                 output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength))
#                 # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
#         except:
#             arclengthRange = None
#             output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = arclengthRange)
#
#         # output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100))
#         # Assign Arc Length
#         AdditionalDataName = 'X'
#         RangeDF[AdditionalDataName] = output.X
#
#         for AdditionalDataIndex in range(0, len(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['AdditionalData'])):
#             AdditionalDataName = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['AdditionalData'][AdditionalDataIndex]
#             if AdditionalDataName == "Max":
#                 RangeDF[VariableName] = output.Max
#                 if VariableName == "API STD 2RD Method 1":
#                     APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
#                     RangeDF[VariableName]= APISTD2RDM1
#                 SummaryFile.append(max(RangeDF[VariableName]))
#             elif AdditionalDataName == "Min":
#                 RangeDF[VariableName] = output.Min
#                 if VariableName == "API STD 2RD Method 1":
#                     APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
#                     RangeDF[VariableName]= APISTD2RDM1
#                 SummaryFile.append(min(RangeDF[VariableName]))
#
#
#     SummaryDF.loc[len(SummaryDF)] = SummaryFile
#
#     return SummaryDF
#
# def LinkedStatistics():
# # LinkedStatistics using OrcaFlex. Code currently not working.
#     VariableName = 'Effective Tension', 'Bend Moment'
#     arclengthRange = OrcFxAPI.oeArcLength(25.0)
#     TimePeriod = exec("OrcFxAPI.{0}" .format(cfg["PostProcess"]['Summary'][SummaryIndex]['SimulationPeriod']))
#     stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod, arclengthRange)
#     query = stats.Query('Effective Tension', 'Bend Moment')
#     print(query.ValueAtMax)
#     print(query.ValueAtMin)
# # Alternatively, use Range graphs for required quantities and obtain them using DFs. (easiest to customize)
