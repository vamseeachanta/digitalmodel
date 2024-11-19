import os
import math

from assetutilities.common.data import SaveData

from digitalmodel.common.orcaflex_model_utilities import OrcaflexModelUtilities

save_data = SaveData()
omu = OrcaflexModelUtilities()


class OrcaflexLineTypes:

    def __init__(self):
        pass

    def get_umbilical_lines(self, cfg):
        for line_type in cfg['inputs']:
            umbilical_properties = self.get_umbilical_properties(line_type, cfg)

        return cfg

    def get_umbilical_properties(self, line_type, cfg):
        umbilical_properties = self.assign_key_umbilical_properties(line_type)
        umbilical_properties_dict = {'LineTypes': {'New': line_type['name'], line_type['name']: umbilical_properties}}
        self.save_model(umbilical_properties_dict, line_type, cfg)

    def assign_key_umbilical_properties(self, line_type):
        umbilical_template = omu.get_umbilical_LineType_template()
        umbilical_properties = umbilical_template["LineTypes"][umbilical_template["LineTypes"]["New"]].copy()
        ID = line_type['ID']
        buoyancy_force = line_type['mass']['air'] - line_type['mass']['water']
        OD = round(((buoyancy_force*4)/(math.pi*line_type['seawater_density']) + ID**2)**0.5, 4)
        MassPerUnitLength = round(line_type['mass']['air']/1000, 5)

        umbilical_properties['OD'] = OD
        umbilical_properties['ID'] = ID
        umbilical_properties['MassPerUnitLength'] = MassPerUnitLength
        umbilical_properties['EI'][0] = round(line_type['stiffness']['bending']/1000, 3)
        umbilical_properties['EA'] = round(line_type['stiffness']['axial']/1000, 3)
        umbilical_properties['GJ'] = round(line_type['stiffness']['torsion']/1000, 3)
        umbilical_properties['NormalDragLiftDiameter'] = round(line_type['OD'], 3)
        umbilical_properties['AxialDragLiftDiameter'] = round(line_type['OD'], 3)

        return umbilical_properties

    def save_model(self, yaml_data, line_type, cfg):

        output_dir = line_type['output_dir']
        if output_dir is None or not os.path.isdir(output_dir):
            output_dir = cfg.Analysis['analysis_root_folder']

        filename = line_type['output_filename']

        save_data.saveDataYaml(yaml_data,
                               output_dir + "\\" + filename,
                               default_flow_style=False)
