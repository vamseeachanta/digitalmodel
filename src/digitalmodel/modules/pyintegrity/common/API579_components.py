import numpy as np
import pandas as pd
from pathlib import Path

from pyintegrity.custom.API579.plotContourf import plotContourf
from pyintegrity.custom.API579.customInputs import ExcelRead
from pyintegrity.common.data import AttributeDict
from pyintegrity.common.pipe_components import PipeComponents

from pyintegrity.common.DataFrame_To_Image import DataFrame_To_Image
from pyintegrity.custom.API579.plotCustom import plotCustom
from pyintegrity.custom.API579.saveData import saveDataYaml
from pyintegrity.common.DataFrame_To_xlsx import DataFrameArray_To_xlsx_openpyxl
from pyintegrity.custom.API579.customInputs import ExcelRead


class API579_components():

    def __init__(self, cfg):
        self.cfg = cfg

    def gml(self):
        import numpy as np
        import pandas as pd

        self.cfg['Result'] = {}
        self.cfg['Result']['Circumference'] = []

        self.read_gml_grids()

        for fileIndex in range(0, len(self.gml_wt_grids)):
            df = self.gml_wt_grids[fileIndex]
            self.prepare_contour_plot_gml(df, fileIndex)

            summary, GMLMAWPResults = self.API579GML(df, fileIndex)
            self.cfg['Result']['Circumference'].append(summary)

            GMLMAWPResultsDF = pd.DataFrame.from_dict(GMLMAWPResults)
            FileName = self.cfg['Analysis']['result_folder'] + self.cfg[
                'Analysis']['file_name'] + 'Summary_GML_' + str(fileIndex)
            print(FileName)
            Acceptable_FCA = np.interp(
                self.cfg['Design'][0]['InternalPressure']['Outer_Pipe'],
                GMLMAWPResultsDF['MAWP'].iloc[::-1].values,
                GMLMAWPResultsDF['FCA'].iloc[::-1].values)
            print("Acceptable GML FCA for design condition 1 is : {0}".format(
                Acceptable_FCA))

            self.plot_and_save_gml_results(Acceptable_FCA, GMLMAWPResultsDF,
                                           fileIndex)

        return self.cfg

    def plot_and_save_gml_results(self, Acceptable_FCA, GMLMAWPResultsDF,
                                  fileIndex):

        if self.cfg.default['Analysis']['GML']['data_source'] == 'xlsx':
            custom_data = self.cfg['ReadingSets'][fileIndex].copy()
        elif self.cfg.default['Analysis']['GML']['data_source'] == 'simulated':
            custom_data = self.cfg['gml_simulated_grid'][fileIndex].copy()
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_' + custom_data['Label'].replace(
                ' ', '_') + '_summary_GML_PLT'

        custom_plot_data = {
            "PltSupTitle":
                self.cfg['PlotSettings']['GML']['PltSupTitle'],
            "PltTitle":
                self.cfg['PlotSettings']['GML']['PltTitle'] + ', ' +
                custom_data['Label'],
            "PltXLabel":
                self.cfg['PlotSettings']['GML']['PltXLabel'],
            "PltYLabel":
                self.cfg['PlotSettings']['GML']['PltYLabel'],
            'Label': ['MAWP'],
            'PltColumns': ['FCA', 'MAWP'],
            "YLim":
                self.cfg['PlotSettings']['GML']['ylim'],
            'Axhline': [
                self.cfg['Design'][0]['InternalPressure']['Outer_Pipe']
            ],
            'TextFields': [{
                "x":
                    0.0,
                "y":
                    GMLMAWPResultsDF['MAWP'][0] + 100,
                "Text":
                    'MAWP at 0 inch FCA: {0:.0f} psi'.format(
                        GMLMAWPResultsDF['MAWP'][0])
            }, {
                "x":
                    Acceptable_FCA,
                "y":
                    self.cfg['Design'][0]['InternalPressure']['Outer_Pipe'] +
                    150,
                "Text":
                    'Allowable FCA: {0:.4f} inch'.format(Acceptable_FCA)
            }],
            'FileName':
                file_name + '.png'
        }

        plotCustom(GMLMAWPResultsDF, custom_plot_data)
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_' + custom_data['Label'].replace(
                ' ', '_') + '_summary_GML_TBL'
        saveDataYaml(self.cfg, file_name)
        GMLSummaryDF = pd.DataFrame.from_dict(
            self.cfg['Result']['Circumference'])
        GMLSummaryDF = GMLSummaryDF.round(3)
        cols = [
            'Description', "Len (inch)", "Min WT (inch)", "Avg. WT (inch)",
            'Corr. Rate (inch/year)', 'Rem. Life (yrs)', "Max WT (inch)"
        ]
        GMLSummaryDF = GMLSummaryDF[cols]
        DataFrame_To_Image(GMLSummaryDF, file_name + '.png')
        GMLSummaryDF.to_csv(file_name + '.csv')

    def prepare_contour_plot_gml(self, df, fileIndex):
        # TODO Move contour plot to vizualization library

        if self.cfg.default['Analysis']['GML']['data_source'] == 'xlsx':
            custom_data = self.cfg['ReadingSets'][fileIndex].copy()
        elif self.cfg.default['Analysis']['GML']['data_source'] == 'simulated':
            custom_data = self.cfg['gml_simulated_grid'][fileIndex].copy()
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_' + custom_data['Label'].replace(
                ' ', '_')

        # Generate Heat Map (X = Circumference, Y = Length)
        custom_plot_data = {
            "Index_Name":
                'Length',
            "Columns_Name":
                'Circumference',
            "FileName":
                file_name,
            "PltSupTitle":
                self.cfg['PlotSettings']['Data']['PltSupTitle'],
            "PltTitle":
                self.cfg['PlotSettings']['Data']['PltTitle'] + ' ' +
                custom_data['Label'] +
                ', Tnom: {0} inch, TminBurst: {1} inch'.format(
                    self.cfg.Outer_Pipe['Geometry']['Design_WT'],
                    self.cfg.Outer_Pipe['Geometry']['tmin']),
            "PltXLabel":
                self.cfg['PlotSettings']['Data']['PltXLabel'],
            "PltYLabel":
                self.cfg['PlotSettings']['Data']['PltYLabel'],
            "PltLims":
                custom_data['Contour'],
        # https://matplotlib.org/api/_as_gen/matplotlib.pyplot.imshow.html
            "PltOrigin":
                'upper',
            "PltShowContourLines":
                False,
            "DataFrameRowReversed":
                True
        }
        # plotHeatMap(df, customdata)
        plotContourf(df, custom_plot_data)
        # Generate Heat Map (X = Length, Y = Circumference)
        custom_plot_data = {
            "Index_Name":
                'Length',
            "Columns_Name":
                'Circumference',
            "FileName":
                file_name + '_Transpose',
            "PltSupTitle":
                self.cfg['PlotSettings']['Data']['PltSupTitle'],
            "PltTitle":
                self.cfg['PlotSettings']['Data']['PltTitle'] + ' ' +
                custom_data['Label'] +
                ', Tnom: {0} inch, TminBurst: {1} inch'.format(
                    self.cfg.Outer_Pipe['Geometry']['Design_WT'],
                    self.cfg.Outer_Pipe['Geometry']['tmin']),
            "PltXLabel":
                self.cfg['PlotSettings']['Data']['PltYLabel'],
            "PltYLabel":
                self.cfg['PlotSettings']['Data']['PltXLabel'],
            "PltLims":
                custom_data['Contour'],
        # https://matplotlib.org/api/_as_gen/matplotlib.pyplot.imshow.html
            "PltOrigin":
                'upper',
            "PltShowContourLines":
                False,
            "DataFrameRowReversed":
                True
        }
        # plotHeatMap(df, customdata)
        plotContourf(df.T, custom_plot_data)

    def lml(self):
        import numpy as np
        import pandas as pd

        LMLSummaryDFArray = []
        LMLFileNameArray = []
        for fileIndex in range(0, len(self.cfg['LML']['LTA'])):
            customdata = {
                "io": self.cfg['LML']['LTA'][fileIndex]['io'],
                "sheet_name": self.cfg['LML']['LTA'][fileIndex]['sheet_name'],
                "skiprows": self.cfg['LML']['LTA'][fileIndex]['skiprows'],
                "skipfooter": self.cfg['LML']['LTA'][fileIndex]['skipfooter'],
                "index_col": self.cfg['LML']['LTA'][fileIndex]['index_col']
            }
            df = ExcelRead(customdata)
            df = df * self.cfg['LML']['LTA'][fileIndex]['DataCorrectionFactor']
            LMLResults = self.API579LML(df, self.cfg, fileIndex)
            LMLSummaryDF = pd.DataFrame.from_dict(LMLResults)
            FileName = self.cfg['Analysis']['result_folder'] + self.cfg[
                'Analysis']['file_name'] + 'Summary_LML_' + str(fileIndex)
            LMLFileNameArray.append('LML_' + str(fileIndex) + '_')
            decimalArray = pd.Series(
                [2, 2, 0, 0, 0, 0, 3, 3, 3, 3, 3, 1, 1, 3, 3, 3],
                index=LMLSummaryDF.columns.values)
            LMLSummaryDF = LMLSummaryDF.round(decimalArray)
            LMLSummaryDFArray.append(LMLSummaryDF)
            DataFrame_To_Image(LMLSummaryDF,
                               FileName + '_TBL' + '.png',
                               col_width=1.0)
            # LMLSummaryDF.to_csv(FileName + '_TBL' +'.csv')
            # DataFrame_To_xlsx(LMLSummaryDF, customdata)
            no_of_design_conditions = len(self.cfg['Design'])
            for design_condition_index in range(0, no_of_design_conditions):
                print(FileName)
                Acceptable_FCA = np.interp(
                    self.cfg['Design'][design_condition_index]
                    ['InternalPressure']['Outer_Pipe'],
                    LMLSummaryDF['MAWPr, L2'].iloc[::-1].values,
                    LMLSummaryDF['FCA'].iloc[::-1].values)
                print("Acceptable LML FCA is : {0}".format(Acceptable_FCA))

                customdata = {
                    "PltSupTitle":
                        self.cfg['PlotSettings']['LML']['PltSupTitle'],
                    "PltTitle":
                        self.cfg['PlotSettings']['LML']['PltTitle'] + ', ' +
                        self.cfg['LML']['LTA'][fileIndex]['Label'],
                    "PltXLabel":
                        self.cfg['PlotSettings']['LML']['PltXLabel'],
                    "PltYLabel":
                        self.cfg['PlotSettings']['LML']['PltYLabel'],
                    'Label': [
                        'MAWP, No Flaw', 'MAWP Reduced, L1', 'MAWP Reduced, L2'
                    ],
                    'PltColumns': ['FCA', 'MAWP', 'MAWPr, L1', 'MAWPr, L2'],
                    "YLim": [10000, 20000],
                    'Axhline': [
                        self.cfg['Design'][design_condition_index]
                        ['InternalPressure']
                    ],
                # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
                    'TextFields': [{
                        "x": 0.05,
                        "y": 1200,
                        "Text": "MAWP Reduced"
                    }, {
                        "x":
                            0.15,
                        "y":
                            14000,
                        "Text":
                            "Flaw dimension (inch): s={0} and c={1}".format(
                                LMLSummaryDF.iloc[0]['s'],
                                LMLSummaryDF.iloc[0]['c'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            13500,
                        "Text":
                            "Nominal WT (inch) ={0}".format(
                                LMLSummaryDF.iloc[0]['tavg'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            13000,
                        "Text":
                            "Avg. Measured WT (inch) ={0}".format(
                                LMLSummaryDF.iloc[0]['tavg'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            12000,
                        "Text":
                            "Min. Measured WT (inch)={0}".format(
                                LMLSummaryDF.iloc[0]['tmm'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            11000,
                        "Text":
                            "FCA outsideFlaw = {0} times FCA flaw".format(
                                self.cfg['LML']['LTA'][fileIndex]
                                ['FCANonFlawRatio'])
                    }],
                    'FileName':
                        self.cfg['Analysis']['result_folder'] +
                        self.cfg['Analysis']['file_name'] + 'Summary_LML_' +
                        str(fileIndex) + '_PLT' + '.png'
                }
                plotCustom(LMLSummaryDF, customdata)

                #  For plot without the MAWP
                customdata = {
                    "PltSupTitle":
                        self.cfg['PlotSettings']['LML']['PltSupTitle'],
                    "PltTitle":
                        self.cfg['PlotSettings']['LML']['PltTitle'] + ', ' +
                        self.cfg['LML']['LTA'][fileIndex]['Label'],
                    "PltXLabel":
                        self.cfg['PlotSettings']['LML']['PltXLabel'],
                    "PltYLabel":
                        self.cfg['PlotSettings']['LML']['PltYLabel'],
                    'Label': ['MAWP Reduced, L1', 'MAWP Reduced, L2'],
                    'PltColumns': ['FCA', 'MAWPr, L1', 'MAWPr, L2'],
                    "YLim": [10000, 20000],
                    'Axhline': [
                        self.cfg['Design'][design_condition_index]
                        ['InternalPressure']
                    ],
                # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
                    'TextFields': [{
                        "x": 0.05,
                        "y": 1200,
                        "Text": "MAWP Reduced"
                    }, {
                        "x":
                            0.15,
                        "y":
                            14000,
                        "Text":
                            "Flaw dimension (inch): s={0} and c={1}".format(
                                LMLSummaryDF.iloc[0]['s'],
                                LMLSummaryDF.iloc[0]['c'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            13500,
                        "Text":
                            "Nominal WT (inch) ={0}".format(
                                LMLSummaryDF.iloc[0]['tavg'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            13000,
                        "Text":
                            "Avg. Measured WT (inch) ={0}".format(
                                LMLSummaryDF.iloc[0]['tavg'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            12000,
                        "Text":
                            "Min. Measured WT (inch)={0}".format(
                                LMLSummaryDF.iloc[0]['tmm'])
                    }, {
                        "x":
                            0.15,
                        "y":
                            11000,
                        "Text":
                            "FCA outsideFlaw = {0} times FCA flaw".format(
                                self.cfg['LML']['LTA'][fileIndex]
                                ['FCANonFlawRatio'])
                    }],
                    'FileName':
                        self.cfg['Analysis']['result_folder'] +
                        self.cfg['Analysis']['file_name'] + 'Summary_LML_' +
                        str(fileIndex) + '_PLT1' + '.png'
                }
                plotCustom(LMLSummaryDF, customdata)

            #  Send LML Data to Excel Sheet
            customdata = {
                "FileName":
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '.xlsx',
                "SheetNames":
                    LMLFileNameArray,
                "thin_border":
                    True
            }
            DataFrameArray_To_xlsx_openpyxl(LMLSummaryDFArray, customdata)

        return self.cfg

    def b31g(self):
        from results.API579.customInputs import ExcelRead
        from results.API579.plotCustom import plotCustom

        for fileIndex in range(0, len(self.cfg['B31G'])):
            customdata = {
                "io": self.cfg['B31G'][fileIndex]['io'],
                "sheet_name": self.cfg['B31G'][fileIndex]['sheet_name'],
                "skiprows": self.cfg['B31G'][fileIndex]['skiprows'],
                "skipfooter": self.cfg['B31G'][fileIndex]['skipfooter'],
                "index_col": self.cfg['B31G'][fileIndex]['index_col']
            }
            df = ExcelRead(customdata)
            df.replace(to_replace='No Limit', value=99, inplace=True)
            df.reset_index(level=0, inplace=True)
            df.columns = df.columns.map(str)

            customdata = {
                "PltSupTitle":
                    self.cfg['PlotSettings']['B31G']['PltSupTitle'],
                "PltTitle":
                    self.cfg['PlotSettings']['B31G']['PltTitle'] + ', ' +
                    self.cfg['B31G'][fileIndex]['Label'],
                "PltXLabel":
                    self.cfg['PlotSettings']['B31G']['PltXLabel'],
                "PltYLabel":
                    self.cfg['PlotSettings']['B31G']['PltYLabel'],
                'Label': ['Pipe WT:' + str(x) for x in df.columns]
                         [1:len(df.columns)],
                'PltColumns': [str(x) for x in df.columns],
                "YLim": [0, 20],
                'Axhline': [self.cfg['B31G'][fileIndex]['LengthLimit']],
            # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
                'TextFields': [{
                    "x": 0.05,
                    "y": 1200,
                    "Text": " "
                }, {
                    "x": 0.05,
                    "y": 1000,
                    "Text": " "
                }],
                'FileName':
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + 'B31G_' +
                    str(fileIndex) + '_PLT' + '.png'
            }

            plotCustom(df, customdata)

        return self.cfg

    def read_gml_grids(self):
        if self.cfg.default['Analysis']['GML']['data_source'] == 'xlsx':
            self.read_gml_grids_from_excel()
        elif self.cfg.default['Analysis']['GML']['data_source'] == 'simulated':
            self.read_gml_grids_from_simulations()

    def read_gml_grids_from_simulations(self):
        self.gml_wt_grids = []
        for fileIndex in range(0, len(self.cfg['gml_simulated_grid'])):
            custom_data = self.cfg['gml_simulated_grid'][fileIndex]
            df = self.simulate_wt_grid(custom_data)
            if len(df) > 0:
                self.gml_wt_grids.append(df)

    def simulate_wt_grid(self, data):
        import math

        import pandas as pd

        index = list(range(0, data['grid_length']))
        columns = list(
            range(0,
                  int(self.cfg.Outer_Pipe['Geometry']['Nominal_OD'] * math.pi)))
        df = pd.DataFrame(self.cfg.Outer_Pipe['Geometry']['Design_WT'] *
                          data['wt_to_nominal_outside_loss_area'],
                          index=index,
                          columns=columns)
        start_index = int(len(index) / 2 - data['circumferencial_length'] / 2)
        end_index = start_index + data['circumferencial_length'] - 1
        start_column = int(len(columns) / 2 - data['axial_length'] / 2)
        end_column = start_column + data['axial_length'] - 1

        for index in range(start_index, end_index + 1):
            for column in range(start_column, end_column + 1):
                df.set_value(
                    index, column,
                    self.cfg.Outer_Pipe['Geometry']['Design_WT'] *
                    data['wt_to_nominal_loss_area'])

        return df

    def read_gml_grids_from_excel(self):
        self.gml_wt_grids = []
        for fileIndex in range(0, len(self.cfg['ReadingSets'])):
            customdata = {
                "io": self.cfg['ReadingSets'][fileIndex]['io'],
                "sheet_name": self.cfg['ReadingSets'][fileIndex]['sheet_name'],
                "skiprows": self.cfg['ReadingSets'][fileIndex]['skiprows'],
                "skipfooter": self.cfg['ReadingSets'][fileIndex]['skipfooter'],
                "index_col": self.cfg['ReadingSets'][fileIndex]['index_col']
            }
            df = ExcelRead(customdata)
            df = df * self.cfg['ReadingSets'][fileIndex]['DataCorrectionFactor']
            if len(df) > 0:
                self.gml_wt_grids.append(df)

    def API579GML(self, df, fileIndex):
        import math

        cfg = self.cfg
        customdata = {
            "tmin":
                cfg.Outer_Pipe['Geometry']['tmin'],
            "NominalID":
                cfg.Outer_Pipe['Geometry']['Nominal_OD'] -
                2 * cfg.Outer_Pipe['Geometry']['Design_WT'],
            "RSFa":
                cfg['API579Parameters']['RSFa'],
            "NominalWT":
                cfg.Outer_Pipe['Geometry']['Design_WT'],
            "Age":
                cfg['API579Parameters']['Age'],
            "FCARateFloor":
                cfg['API579Parameters']['FCARateFloor'],
            "FutureCorrosionRate":
                self.cfg.default['Analysis']['GML']['FCARate']
        }

        # Perform AssementLengthEvaluation for all data
        data = self.GMLAssessmentLengthEvaluation(df.min(), customdata)
        # AssessmentLength determination
        if cfg['default']['Analysis']['GML']['Circumference']:
            RemainingLife_df = df.copy()
            FutureCorrosionRate_df = df.copy()
            HistoricalCorrosionRate_df = df.copy()
            Tmin_df = df.copy()
            Tmax_df = df.copy()
            Tmean_df = df.copy()
            WTAcceptabilityRatio_df = df.copy()
            AveragingLength_df = df.copy()
            COVWT_df = df.copy()
            MAWP_df = df.copy()

            if (cfg.default['settings']
                ['AssessmentLengthCeilingFactor_Circumference'] != None):
                LengthCeilLimit = cfg.default['settings']['AssessmentLengthCeilingFactor_Circumference'] * math.pi * \
                                  cfg['Geometry']['NominalOD']
                if data['AveragingLength'] > LengthCeilLimit:
                    data['AveragingLength'] = LengthCeilLimit

            AveragingIndexRange = self.get_nearest_vector_index(
                df.columns, data['AveragingLength'])
            if AveragingIndexRange < len(df.columns):
                data['AveragingLength'] = df.columns[AveragingIndexRange]
            else:
                data['AveragingLength'] = df.columns[-1]
            # Construct thickness profile array
            for MarchingLengthIndex in range(0, len(df)):
                for MarchingCircumIndex in range(0, len(df.columns)):
                    DataArray = []
                    for dataIndex in range(
                            MarchingCircumIndex,
                            MarchingCircumIndex + AveragingIndexRange):
                        if dataIndex >= len(df.columns):
                            ColumnIndex = dataIndex - len(df.columns)
                        else:
                            ColumnIndex = dataIndex
                        # Construct thickness profile array
                        DataArray.append(df.iloc[MarchingLengthIndex,
                                                 ColumnIndex])

                    # if MarchingLengthIndex == 9:
                    #     debuggingIndex = 1
                    data = self.GMLAssessmentLengthEvaluation(
                        DataArray, customdata)

                    if (cfg.default['settings']
                        ['AssessmentLengthCeilingFactor_Circumference'] !=
                            None):
                        LengthCeilLimit = cfg.default['settings']['AssessmentLengthCeilingFactor_Circumference'] * math.pi * \
                                          cfg.Outer_Pipe['Geometry']['Nominal_OD']
                        if data['AveragingLength'] > LengthCeilLimit:
                            data['AveragingLength'] = LengthCeilLimit

                    AveragingIndexRange2 = self.get_nearest_vector_index(
                        df.columns, data['AveragingLength'])
                    if AveragingIndexRange2 < len(df.columns):
                        data['AveragingLength'] = df.columns[
                            AveragingIndexRange2]
                    else:
                        data['AveragingLength'] = df.columns[-1]
                    # Perform averaging again
                    for dataIndex in range(
                            MarchingCircumIndex,
                            MarchingCircumIndex + AveragingIndexRange2 - 1):
                        if dataIndex >= len(df.columns):
                            ColumnIndex = dataIndex - len(df.columns)
                        else:
                            ColumnIndex = dataIndex
                        # Construct thickness profile array
                        DataArray.append(df.iloc[MarchingLengthIndex,
                                                 ColumnIndex])

                    data = self.GMLAssessmentLengthEvaluation(
                        DataArray, customdata)

                    if (cfg.default['settings']
                        ['AssessmentLengthCeilingFactor_Circumference'] !=
                            None):
                        LengthCeilLimit = cfg.default['settings']['AssessmentLengthCeilingFactor_Circumference'] * math.pi * \
                                          cfg.Outer_Pipe['Geometry']['Nominal_OD']
                        if data['AveragingLength'] > LengthCeilLimit:
                            data['AveragingLength'] = LengthCeilLimit
                    elif (data['AveragingLength'] >
                          math.pi * cfg.Outer_Pipe['Geometry']['Nominal_OD']):
                        data['AveragingLength'] = math.pi * cfg.Outer_Pipe[
                            'Geometry']['Nominal_OD']

                    if AveragingIndexRange2 < len(df.columns):
                        data['AveragingLength'] = df.columns[
                            AveragingIndexRange2]
                    else:
                        data['AveragingLength'] = df.columns[-1]

                    customdata.update(data)
                    data = self.GMLAcceptability(customdata)
                    customdata.update(data)

                    RemainingLife_df.iloc[
                        MarchingLengthIndex,
                        ColumnIndex] = customdata['remainingLife']
                    FutureCorrosionRate_df.iloc[
                        MarchingLengthIndex,
                        ColumnIndex] = customdata['futureCorrosionRate']
                    HistoricalCorrosionRate_df.iloc[
                        MarchingLengthIndex,
                        ColumnIndex] = customdata['HistoricalCorrosionRate']
                    Tmin_df.iloc[MarchingLengthIndex,
                                 ColumnIndex] = customdata['tmm']
                    Tmax_df.iloc[MarchingLengthIndex,
                                 ColumnIndex] = customdata['tmax']
                    Tmean_df.iloc[MarchingLengthIndex,
                                  ColumnIndex] = customdata['tam']
                    WTAcceptabilityRatio_df.iloc[
                        MarchingLengthIndex,
                        ColumnIndex] = customdata['WTAcceptabilityRatio']
                    AveragingLength_df.iloc[
                        MarchingLengthIndex,
                        ColumnIndex] = customdata['AveragingLength']

            PrefixFileName = cfg['Analysis']['result_folder'] + cfg['Analysis'][
                'file_name'] + '_Circumference_' + str(fileIndex) + '_'
            self.SaveResults(RemainingLife_df,
                             PrefixFileName + 'RemainingLife_df')
            self.SaveResults(FutureCorrosionRate_df,
                             PrefixFileName + 'FutureCorrosionRate_df')
            self.SaveResults(HistoricalCorrosionRate_df,
                             PrefixFileName + 'HistoricalCorrosionRate_df')
            self.SaveResults(Tmin_df, PrefixFileName + 'Tmin_df')
            self.SaveResults(Tmax_df, PrefixFileName + 'Tmax_df')
            self.SaveResults(Tmean_df, PrefixFileName + 'Tmean_df')
            self.SaveResults(WTAcceptabilityRatio_df,
                             PrefixFileName + 'WTAcceptabilityRatio_df')
            self.SaveResults(AveragingLength_df,
                             PrefixFileName + 'AveragingLength_df')

            if self.cfg.default['Analysis']['GML']['data_source'] == 'xlsx':
                custom_grid_data = self.cfg['ReadingSets'][fileIndex].copy()
            elif self.cfg.default['Analysis']['GML'][
                    'data_source'] == 'simulated':
                custom_grid_data = self.cfg['gml_simulated_grid'][
                    fileIndex].copy()

            file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
                'Analysis']['file_name'] + '_' + custom_grid_data[
                    'Label'].replace(' ', '_')

            summary = {
                "Description":
                    custom_grid_data['Label'],
                "Rem. Life (yrs)":
                    RemainingLife_df.min(axis=0, skipna=True).min(),
                "Corr. Rate (inch/year)":
                    FutureCorrosionRate_df.max(axis=0, skipna=True).max(),
            #  "Max Historical Corrosion Rate (inch/year)": HistoricalCorrosionRate_df.max(axis=0, skipna=True).max(),
                "Min WT (inch)":
                    Tmin_df.min(axis=0, skipna=True).min(),
                "Max WT (inch)":
                    Tmax_df.max(axis=0, skipna=True).max(),
                "Avg. WT (inch)":
                    Tmean_df.min(axis=0, skipna=True).min(),
            #  "Min WT Acceptability Ratio": WTAcceptabilityRatio_df.min(axis=0, skipna=True).min(),
                "Len (inch)":
                    AveragingLength_df.min(axis=0, skipna=True).min()
            }

            GMLMAWPResults = []
            for FCAIndex in range(
                    0, len(self.cfg.default['Analysis']['GML']['FCA'])):
                FCA = self.cfg.default['Analysis']['GML']['FCA'][FCAIndex]
                t = Tmean_df.min(axis=0, skipna=True).min() - FCA

                MAWP = self.getMAWP(t, cfg)
                GMLResult = {"FCA": FCA, "t": t, "MAWP": MAWP}
                GMLMAWPResults.append(GMLResult)

        return summary, GMLMAWPResults

    def GMLAssessmentLengthEvaluation(self, DataArray, data):
        import math

        import numpy as np

        tmm = min(DataArray)
        tam = np.nanmean(DataArray)
        tmax = max(DataArray)

        FCAml = tam - data['tmin']
        nominalWTml = data['tmin'] - FCAml
        IDml = (data['NominalID']) + 2 * FCAml
        Rt = (tmm - FCAml) / nominalWTml

        if (Rt < data['RSFa']):
            lengthParameter = 1.123 * math.sqrt((
                (1 - Rt) / (1 - Rt / data['RSFa']))**2 - 1)
        else:
            lengthParameter = 50.00

        AveragingLength = lengthParameter * math.sqrt(IDml * nominalWTml)

        AssessmentLengthResult = {
            "AveragingLength": AveragingLength,
            "tmm": tmm,
            "FCAml": FCAml,
            "tam": tam,
            "LengthParameter": lengthParameter,
            "tmax": tmax
        }
        return AssessmentLengthResult

    def GMLAcceptability(self, data):
        LHS = data['tmm'] - data['FCAml']
        RHS = max(0.5 * data['tmin'], max(0.2 * data['NominalWT'], 0.1))
        WTAcceptabilityRatio = LHS / RHS
        # >1 acceptable else unacceptable

        # Remaining Life Calculation
        HistoricalCorrosionRate = (data['NominalWT'] -
                                   data['tam']) / data['Age']
        if data['FutureCorrosionRate'] == 'Historical':
            if HistoricalCorrosionRate < data['FCARateFloor']:
                futureCorrosionRate = data['FCARateFloor']
            else:
                futureCorrosionRate = HistoricalCorrosionRate
        else:
            futureCorrosionRate = data['FutureCorrosionRate']

        remainingLife = (data['tam'] - data['tmin']) / futureCorrosionRate

        result = {
            "WTAcceptabilityRatio": WTAcceptabilityRatio,
            "remainingLife": remainingLife,
            "futureCorrosionRate": futureCorrosionRate,
            "HistoricalCorrosionRate": HistoricalCorrosionRate
        }

        return result

    def get_nearest_vector_index(self, array, value):
        vector_index = None
        for i in range(0, len(array)):
            if array[i] > value:
                vector_index = i
                break

        if vector_index != None:
            return vector_index
        else:
            return len(array)

    def SaveResults(self, df, FileName):
        df.to_csv(FileName + '.csv')

    def API579LML(self, df, cfg, fileIndex):
        LMLResults = self.LMLAcceptability(df, cfg, fileIndex)
        return LMLResults

    def LMLAcceptability(self, df, cfg, fileIndex):
        import math

        # dfLTA = df.iloc[[cfg['LML']['LTA'][fileIndex]['sIndex'][0]: cfg['LML']['LTA'][fileIndex]['sIndex'][1]], \
        #         [cfg['LML']['LTA'][fileIndex]['cIndex'][0]: cfg['LML']['LTA'][fileIndex]['cIndex'][1]]]
        sIndex0 = cfg['LML']['LTA'][fileIndex]['sIndex'][0]
        sIndex1 = cfg['LML']['LTA'][fileIndex]['sIndex'][1]
        cIndex0 = cfg['LML']['LTA'][fileIndex]['cIndex'][0]
        cIndex1 = cfg['LML']['LTA'][fileIndex]['cIndex'][1]
        dfLTA = df.iloc[sIndex0:sIndex1, cIndex0:cIndex1].copy()
        print(dfLTA)

        customdata = {
            "Index_Name":
                'Length',
            "Columns_Name":
                'Circumference',
            "FileName":
                self.cfg.Analysis['result_folder'] + '\\Plot\\Flaw_' +
                Path(cfg['LML']['LTA'][fileIndex]['io']).stem + '_' +
                cfg['LML']['LTA'][fileIndex]['Label'],
            "PltSupTitle":
                cfg['PlotSettings']['Data']['PltSupTitle'],
            "PltTitle":
                cfg['PlotSettings']['Data']['PltTitle'] + ' ' +
                cfg['LML']['LTA'][fileIndex]['Label'] +
                ', Tnom: {0} inch, Tmin: {1} inch'.format(
                    cfg.Outer_Pipe['Geometry']['Design_WT'],
                    cfg.Outer_Pipe['Geometry']['tmin']),
            "PltXLabel":
                cfg['PlotSettings']['Data']['PltXLabel'],
            "PltYLabel":
                cfg['PlotSettings']['Data']['PltYLabel'],
            "PltLims":
                cfg['LML']['LTA'][fileIndex]['Contour'],
        # https://matplotlib.org/api/_as_gen/matplotlib.pyplot.imshow.html
            "PltOrigin":
                'upper',
            "PltShowContourLines":
                False,
            "DataFrameRowReversed":
                True
        }
        # plotHeatMap(df, customdata)

        plotContourf(dfLTA, customdata)

        dfrd = df.copy()
        dfrd.iloc[sIndex0:sIndex1, cIndex0:cIndex1] = np.nan
        trd = dfrd.mean(axis=0, skipna=True).mean()

        LMLResults = []
        for FCAIndex in range(0, len(cfg['LML']['LTA'][fileIndex]['FCA'])):
            FCA = cfg['LML']['LTA'][fileIndex]['FCA'][FCAIndex]
            FCAOutsideFlaw = FCA * cfg['LML']['LTA'][fileIndex][
                'FCANonFlawRatio']
            tc = trd - FCAOutsideFlaw
            s = dfLTA.index.max() - dfLTA.index.min() + 1
            c = dfLTA.columns.max() - dfLTA.columns.min() + 1

            tmm = dfLTA.min(axis=0, skipna=True).min()
            Rt = (tmm - FCA) / tc
            cfg.Outer_Pipe['Geometry']['Nominal_ID'] = cfg.Outer_Pipe['Geometry']['Nominal_OD'] - 2 * \
                                                       cfg.Outer_Pipe['Geometry']['Design_WT']
            LongitudinalFlawLengthParameter = 1.285 * s / math.sqrt(
                cfg.Outer_Pipe['Geometry']['Nominal_ID'] * tc)

            if (Rt >= 0.2) and (tmm - FCA >= 0.05) and (
                    cfg['LML']['LTA'][fileIndex]['Lmsd'] >= 1.8 *
                    math.sqrt(cfg.Outer_Pipe['Geometry']['Nominal_ID'] * tc)):
                MAWP = self.getMAWP(tc, cfg)
            else:
                MAWP = None
                Level1Outcome = 'Fail'
                raise Exception("LML Level 1 and 2 are failed.")

            if MAWP != None:
                customData = {
                    "LongitudinalFlawLengthParameter":
                        LongitudinalFlawLengthParameter,
                    "Rt":
                        Rt,
                    "RSFa":
                        cfg['API579Parameters']['RSFa'],
                    "MAWP":
                        MAWP,
                    "tc":
                        tc,
                    "NominalID":
                        cfg.Outer_Pipe['Geometry']['Nominal_ID']
                }

                # Level 1 Evaluation
                data = self.LMLMAWPrEvaluation(customData, cfg, Level=1)
                MAWPrL1 = data['MAWPr']
                MtL1 = data['Mt']
                MAWPrEvaluationL1 = data['MAWPrEvaluation']
                RSFL1 = data['RSF']

                # Level 2 Evaluation
                if cfg['Geometry']['NominalID'] is None:
                    cfg['Geometry']['NominalID'] = cfg['Geometry'][
                        'NominalOD'] - 2 * cfg['Geometry']['DesignWT']
                (tavgiL2, sL2) = self.LMLLevel2Evaluation(dfLTA, customData)
                RSFL2 = []
                MAWPrL2 = []
                MtL2 = []
                MAWPrEvaluationL2 = []
                for L2EvalIndex in range(0, len(tavgiL2)):
                    AARatio = (tavgiL2[L2EvalIndex] - FCA) / tc
                    LongitudinalFlawLengthParameter = 1.285 * sL2[
                        L2EvalIndex] / math.sqrt(
                            cfg['Geometry']['NominalID'] * tc)
                    customData.update({
                        "AARatio":
                            AARatio,
                        "LongitudinalFlawLengthParameter":
                            LongitudinalFlawLengthParameter
                    })
                    data = self.LMLMAWPrEvaluation(customData, cfg, Level=2)
                    RSFL2.append(data['RSF'])
                    MAWPrL2.append(data['MAWPr'])
                    MtL2.append(data['Mt'])
                    MAWPrEvaluationL2.append(data['MAWPrEvaluation'])

            LMLResult = {
                "FCA": FCA,
                "tc": tc,
                "s": s,
                "c": c,
                "tmm": tmm,
                "MAWP": MAWP,
                "Rt": Rt,
                "Flaw Parameter": LongitudinalFlawLengthParameter,
                "Mt, L1": MtL1,
                "MAWPrEval, L1": MAWPrEvaluationL1,
                "RSF, L1": RSFL1,
                "MAWPr, L1": MAWPrL1,
                "MAWPr, L2": min(MAWPrL2),
                "RSF, L2": min(RSFL2),
                "Mt, L2": min(MtL2),
                "tavg": trd
            }
            LMLResults.append(LMLResult)

        return LMLResults

    def getMAWP(self, measured_thickness, cfg_temp):
        import copy

        cfg_temp = AttributeDict(copy.deepcopy(cfg_temp))
        cfg_temp['Outer_Pipe']['Geometry']['Design_WT'] = measured_thickness
        pc = PipeComponents(cfg_temp)
        pc.evaluate_pipe_capacity()
        design_code = cfg_temp['Design'][0]['Code'][0]['Outer_Pipe']
        max_pressure_based_on_thickness = \
            cfg_temp['Result']['Outer_Pipe']['internal_pressure'][design_code]['Design_WT_Max_Pressure'][
            'Zero Corrosion Allowance']
        return max_pressure_based_on_thickness

    def LMLMAWPrEvaluation(self, data, cfg, Level):
        import numpy as np

        if data['LongitudinalFlawLengthParameter'] <= 0.354:
            RtFloor = 0.2
        elif data['LongitudinalFlawLengthParameter'] < 20:
            RSFa = data['RSFa']
            # TODO: Bring Table into an Excel sheet format or array format
            Mt = np.interp(
                data['LongitudinalFlawLengthParameter'],
                cfg['API579Parameters']['FoliasFactor']['FlawParameter'],
                cfg['API579Parameters']['FoliasFactor']['Mt']['Cylindrical'])
            RtFloor = (RSFa - RSFa / Mt) / (1 - RSFa / Mt)
        elif data['LongitudinalFlawLengthParameter'] > 20:
            RtFloor = 0.9

        if data['Rt'] > RtFloor:
            MAWPrEvaluation = False
            MAWPr = data['MAWP']
            RSF = 1
        else:
            if Level == 1:
                RSF = data['Rt'] / (1 - (1 - data['Rt']) / Mt)
            elif Level == 2:
                RSF = data['AARatio'] / (1 - (1 - data['AARatio']) / Mt)
            if RSF >= RSFa:
                MAWPrEvaluation = False
                MAWPr = data['MAWP']
            else:
                MAWPrEvaluation = True
                MAWPr = data['MAWP'] * RSF / RSFa

        result = {
            "MAWPrEvaluation": MAWPrEvaluation,
            "Mt": Mt,
            "MAWPr": MAWPr,
            "RSF": RSF
        }

        return result

    def GMLMAWPrEvaluation(self, data, cfg):
        if data['Rt'] > RtFloor:
            MAWPrEvaluation = False
            MAWPr = data['MAWP']
            RSF = 1
        else:
            if Level == 1:
                RSF = data['Rt'] / (1 - (1 - data['Rt']) / Mt)
            elif Level == 2:
                RSF = data['AARatio'] / (1 - (1 - data['AARatio']) / Mt)
            if RSF >= RSFa:
                MAWPrEvaluation = False
                MAWPr = data['MAWP']
            else:
                MAWPrEvaluation = True
                MAWPr = data['MAWP'] * RSF / RSFa

        result = {
            "MAWPrEvaluation": MAWPrEvaluation,
            "Mt": Mt,
            "MAWPr": MAWPr,
            "RSF": RSF
        }

        return result

    def LMLLevel2Evaluation(self, dfLTA, data):
        tavgi = []
        s = []
        for i in range(0, len(dfLTA.columns)):
            LongitudinalCTP = dfLTA.iloc[:, i].values
            LongitudinalCTPAscending = sorted(LongitudinalCTP)
            for j in range(1, len(LongitudinalCTPAscending)):
                s.append((j + 1) * (dfLTA.index[1] - dfLTA.index[0]))
                tavgi.append(
                    sum(LongitudinalCTPAscending[0:j]) /
                    len(LongitudinalCTPAscending[0:j]))

        return (tavgi, s)