import os
import glob
import pandas as pd

from assetutilities.common.data import SaveData
from assetutilities.common.data import SaveData, Transform
from assetutilities.common.visualization_components import VisualizationComponents

from digitalmodel.custom.fea_model.VesselType_components import VesselType
from digitalmodel.custom.fea_model.Constraint_components import Constraint
from digitalmodel.custom.fea_model.shape_components import Shape
from digitalmodel.custom.fea_model.environment_components import Environment
from digitalmodel.custom.fea_model.general_components import General
from digitalmodel.custom.fea_model.environment_components import Environment
from digitalmodel.custom.fea_model.LineType_components import LineType
from digitalmodel.custom.fea_model.buoy_components import Buoy
from digitalmodel.custom.fea_model.line_components import Line
from digitalmodel.custom.fea_model.Vessel_components import Vessel
from digitalmodel.custom.fea_model.VariableData_components import VariableData
from digitalmodel.custom.fea_model.group_components import Group

class FEAComponents():

    def __init__(self, cfg):
        self.cfg = cfg
        self.custom_fea_files = []

    def get_raw_data(self):
        if self.cfg.default['data_source'] == 'db':
            from common.database import Database
            db_properties = self.cfg.db
            self.dbe = Database(db_properties)
            if self.cfg.default.__contains__('input_data'):
                cfg_input = self.cfg.default['input_data'].copy()
                self.dbe.get_input_data(cfg_input)
            else:
                print("No input data in configuration")
        else:
            print("No data source specified")

    def prepare_FEAModel(self):
        if self.cfg.FEAProgram['name'] == "OrcaFlex":
            self.fea_model = {}
            self.AssignGeneral()
            self.AssignEnvironment()

            for Asset in self.cfg.Assets:
                if 'fea_type' in Asset:
                    self.AssignAssetProperties(Asset)

    def AssignGeneral(self):
        general = General(cfg=self.cfg['General'])
        fea_data = general.getGeneralOrcaFlexSetUp()
        self.fea_model.update(fea_data)

    def AssignEnvironment(self):
        enviornment = Environment(cfg=self.cfg['Environment'])
        fea_data = enviornment.getOrcaFlexEnvironment()
        self.fea_model.update(fea_data)

    def AssignAssetProperties(self, Asset):
        if Asset['fea_type'] == 'buoy':
            self.AssignBuoyProperties(Asset)
        elif Asset['fea_type'] == 'lineType':
            self.AssignLineTypeProperties(Asset)
        elif Asset['fea_type'] == 'line':
            self.AssignLineProperties(Asset)
        elif Asset['fea_type'] == 'vesselType':
            self.AssignVesselTypeProperties(Asset)
        elif Asset['fea_type'] == 'vessel':
            self.AssignVesselProperties(Asset)
        elif Asset['fea_type'] == 'shape':
            self.AssignShapeProperties(Asset)
        elif Asset['fea_type'] == 'group':
            self.AssignGroupProperties(Asset)
        elif Asset['fea_type'] == 'constraint':
            self.AssignConstraintProperties(Asset)
        elif Asset['purpose'] == 'VariableData':
            self.AssignVariableData(Asset)

    def AssignBuoyProperties(self, Asset):
        buoy = self.GetBuoyProperties(Asset)
        if '6DBuoys' not in self.fea_model:
            self.fea_model['6DBuoys'] = []
        self.fea_model['6DBuoys'].append(buoy)

    def AssignLineProperties(self, Asset):
        line = self.GetLineProperties(Asset)
        if 'Lines' not in self.fea_model:
            self.fea_model['Lines'] = []
        self.fea_model['Lines'].append(line)

    def AssignLineTypeProperties(self, Asset):
        line_type = self.GetLineTypeProperties(Asset)
        if 'LineTypes' not in self.fea_model:
            self.fea_model['LineTypes'] = []
        self.fea_model['LineTypes'].append(line_type)

    def AssignVesselProperties(self, Asset):
        vessel = self.GetVesselProperties(Asset)
        if 'Vessels' not in self.fea_model:
            self.fea_model['Vessels'] = []
        self.fea_model['Vessels'].append(vessel)

    def AssignVesselTypeProperties(self, Asset):
        import os
        vessel_type = self.GetVesselTypeProperties(Asset)
        if vessel_type is not None and 'VesselTypes' not in self.fea_model:
            self.fea_model['VesselTypes'] = []
            self.fea_model['VesselTypes'].append(vessel_type)
        elif 'filename' in Asset['cfg']:
            if os.path.isfile(Asset['cfg']['filename']):
                self.custom_fea_files.append(Asset['cfg']['filename'])
            else:
                raise FileNotFoundError

    def AssignConstraintProperties(self, Asset):
        constraint = self.GetConstraintProperties(Asset)
        if 'Constraints' not in self.fea_model:
            self.fea_model['Constraints'] = []
        self.fea_model['Constraints'].append(constraint)

    def AssignVariableData(self, Asset):
        variable_data = self.GetVariableData(Asset)
        variable_data_class = Asset['fea_type']
        if 'VariableData' not in self.fea_model:
            self.fea_model['VariableData'] = {variable_data_class : [variable_data]}
        elif variable_data_class in self.fea_model['VariableData']:
            self.fea_model['VariableData'][variable_data_class].append(variable_data)

    def AssignShapeProperties(self, Asset):
        shape = self.GetShapeProperties(Asset)
        if 'Shapes' not in self.fea_model:
            self.fea_model['Shapes'] = []
        self.fea_model['Shapes'].append(shape)

    def AssignGroupProperties(self, Asset):
        group = self.GetGroupProperties(Asset)
        self.fea_model['Groups'] = group

    def GetBuoyProperties(self, Asset):
        cfg = Asset['cfg']
        buoy = Buoy(cfg)
        fea_data = buoy.get_orcaflex_properties()

        return fea_data

    def GetLineTypeProperties(self, Asset):
        cfg = Asset
        line_type = LineType(cfg)
        fea_data = line_type.get_orcaflex_properties()

        return fea_data

    def GetLineProperties(self, Asset):
        cfg = Asset
        line = Line(cfg)
        fea_data = line.get_orcaflex_properties()

        return fea_data

    def GetVesselTypeProperties(self, Asset):
        cfg = Asset
        line_type = VesselType(cfg)
        fea_data = line_type.get_orcaflex_properties()

        return fea_data

    def GetVesselProperties(self, Asset):
        cfg = Asset
        line = Vessel(cfg)
        fea_data = line.get_orcaflex_properties()

        return fea_data

    def GetConstraintProperties(self, Asset):
        cfg = Asset
        constraint = Constraint(cfg)
        fea_data = constraint.get_orcaflex_properties()

        return fea_data

    def GetVariableData(self, Asset):
        cfg = Asset
        variable_data = VariableData(cfg)
        fea_data = variable_data.get_orcaflex_properties()

        return fea_data

    def GetShapeProperties(self, Asset):
        cfg = Asset
        line = Shape(cfg)
        fea_data = line.get_orcaflex_properties()

        return fea_data

    def GetGroupProperties(self, Asset):
        cfg = Asset
        line = Group(cfg)
        fea_data = line.get_orcaflex_properties()

        return fea_data

    def save_output(self):
        save_data = SaveData()
        self.cfg['Analysis']['fe_filename'] = self.get_fe_filename()
        fea_file_data_list = self.custom_fea_files + [self.fea_model]
        save_data.WriteOrcaFlexYMLFile(fea_file_data_list, self.cfg['Analysis']['fe_filename'])

    def prepare_visualizations(self):
        vc = VisualizationComponents(self.cfg)
        vc.prepare_visualizations(self)

    def load_variations(self):
        if self.cfg['Environment'].__contains__('LoadVariation') and self.cfg['Environment']['LoadVariation']['flag']:
            environment = Environment(self.cfg.Environment)
            save_data = SaveData()
            trans = Transform()

            LoadVariation_cfg = {'flag': True}
            self.loadvariations = pd.read_csv(self.cfg['Environment']['LoadVariation']['file_name'])
            fe_filename_array = []
            self.overwrite_fe_files()
            for row_index in range(0, len(self.loadvariations)):
                load_variation = self.loadvariations.iloc[row_index].to_dict()
                load_variation = trans.convert_numpy_types_to_native_python_types(cfg={
                    'datatype': dict,
                    'data': load_variation
                })
                fea_data = environment.update_with_load_variation(load_variation)
                self.fea_model.update(fea_data)
                LoadVariation_cfg.update({'index': str(row_index).rjust(len(str(len(self.loadvariations))), '0')})
                fe_filename = self.get_fe_filename(LoadVariation_cfg)
                fe_filename_array.append(fe_filename)
                fea_file_data_list = self.custom_fea_files + [self.fea_model]
                save_data.WriteOrcaFlexYMLFile(fea_file_data_list, fe_filename)

            self.save_fe_batch_file(fe_filename_array)

    def get_fe_filename(self, LoadVariation_cfg={'flag': False, 'index': '000'}):
        if not LoadVariation_cfg['flag']:
            if self.cfg['Analysis'].__contains__('drop_basename') and self.cfg['Analysis']['drop_basename']:
                fe_filename = self.cfg['Analysis']['fe_folder'] + 'basename.yml'
            else:
                fe_filename = self.cfg['Analysis']['fe_folder'] + self.cfg['Analysis']['file_name'] + '.yml'
        else:
            if self.cfg['Analysis'].__contains__('drop_basename') and self.cfg['Analysis']['drop_basename']:
                fe_filename = self.cfg['Analysis']['fe_folder'] + self.cfg['Analysis'][
                    'fe_file_prefix'] + LoadVariation_cfg['index'] + '.yml'
            else:
                fe_filename = self.cfg['Analysis']['fe_folder'] + self.cfg['Analysis'][
                    'file_name'] + '_' + LoadVariation_cfg['index'] + '.yml'

        return fe_filename

    def overwrite_fe_files(self):
        yaml_files = glob.glob(self.cfg['Analysis']['fe_folder'] + self.cfg['Analysis']['fe_file_prefix'] + '*.yml')
        for file in yaml_files:
            os.remove(file)

    def save_fe_batch_file(self, fe_filename_array):
        df = self.loadvariations.copy()
        df['fe_filename'] = fe_filename_array
        if self.cfg['Analysis'].__contains__('drop_basename') and self.cfg['Analysis']['drop_basename']:
            filename = self.cfg['Analysis']['fe_folder'] + self.cfg['Analysis']['fe_file_prefix'] + 'batch.csv'
        else:
            filename = self.cfg['Analysis']['fe_folder'] + self.cfg['Analysis']['file_name'] + 'batch.csv'

        df.to_csv(filename, index=False)
