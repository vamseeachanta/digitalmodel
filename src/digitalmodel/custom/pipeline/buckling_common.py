# Standard library imports

# Third party imports
from assetutilities.common.visualization.visualization_templates import (
    VisualizationTemplates,
)
from scipy import interpolate

viz_templates = VisualizationTemplates()
class CommonBucklingCaculations:
    
    def __init__(self):
        pass

    def run(self, cfg):
        friction_force = self.get_friction_force(cfg)
        cfg['pipeline']['friction_force'] = friction_force
        lateral_buckling_df = self.get_lateral_buckling(cfg)
        
        self.save_results(cfg, lateral_buckling_df)

    def get_friction_force(self, cfg):
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        
        mass_in_water = system_properties['mass']['water']['with_internal_fluid']
        friction_cfg = cfg['pipeline']['soil']
        
        axial_friction_force = mass_in_water * friction_cfg['axial_breakout_friction_coeff']
        lateral_friction_force = mass_in_water * friction_cfg['lateral_breakout_friction_coeff']
        
        friction = {'axial': axial_friction_force, 'lateral': lateral_friction_force}

        return friction
    
    def get_mesh(self, length):
        no_of_elements = 100
        element_array = list(range(0,no_of_elements+1))
        length_factor_array = [item/no_of_elements for item in element_array]
        length_array = [item*length for item in length_factor_array]
        
        mesh = {'element': element_array, 'length_factor': length_factor_array, 'length': length_array}

        return mesh

    def assign_mesh(self, df, length):
        mesh = self.get_mesh(length)
        df['element'] = mesh['element']
        df['length_factor'] = mesh['length_factor']
        length_array = mesh['length']
        df['length'] = mesh['length']
        return mesh, df
        
    def get_differential_temp(self, cfg, mesh):
        temperature_cfg = cfg['pipeline']['crossection'][0]['temperature']
        
        x_values = temperature_cfg['length_factor']
        y_values = temperature_cfg['temperature']
        
        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = mesh['length_factor']
        differential_temperature_array = [round(float(f(item)),3) for item in length_factor_array]

        return differential_temperature_array

