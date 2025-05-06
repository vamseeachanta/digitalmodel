# Standard library imports

# Third party imports
from assetutilities.common.visualization.visualization_templates_matplotlib import (
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

    def get_internal_pressure(self, cfg, df):
        internal_fluid_cfg = cfg['pipeline']['crossection'][0]['internal_fluid']
        pressure_cfg = internal_fluid_cfg['pressure']
        reference_pressure = pressure_cfg['pressure']

        water_depth_array = list(df['water_depth'])
        density = internal_fluid_cfg['density']
        fluid_column = [(pressure_cfg['top_side_elevation'] + item)*12 for item in water_depth_array]
        pressure_internal = [reference_pressure + density * item for item in fluid_column]

        df['pressure_internal'] = pressure_internal

        return df

    def get_external_pressure(self, cfg, df):

        water_depth_array = list(df['water_depth'])
        density = cfg['pipeline']['crossection'][0]['external_fluid']['density']

        fluid_column = [item*12 for item in water_depth_array]
        pressure_external = [density * item for item in fluid_column]

        df['pressure_external'] = pressure_external

        return df


    def assign_mesh(self, df, length):
        mesh = self.get_mesh(length)
        df['element'] = mesh['element']
        df['length_factor'] = mesh['length_factor']
        df['length'] = mesh['length']

        return df

    def get_differential_temp(self, cfg, df):
        temperature_cfg = cfg['pipeline']['crossection'][0]['temperature']

        x_values = temperature_cfg['length_factor']
        y_values = temperature_cfg['temperature']

        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = list(df['length_factor'])
        differential_temperature_array = [round(float(f(item)),3) for item in length_factor_array]
        df['differential_temperature'] = differential_temperature_array

        return df

    def get_circumferential_stress(self, cfg, df):

        pipe_properties = cfg['pipeline']['pipe_properties']
        A = pipe_properties[0]['section']['A']
        Ai = pipe_properties[0]['section']['Ai']

        pressure_internal = list(df['pressure_internal'])
        pressure_external = list(df['pressure_external'])
        zipped_items = list(zip(pressure_internal, pressure_external))
        circumferential_stress_array = [2* (item[0]-item[1]) * Ai / A for item in zipped_items]
        df['circumferential_stress'] = circumferential_stress_array

        return df

    def get_longitudinal_stress_fully_restrained(self, cfg, df):
        pipe_properties = cfg['pipeline']['pipe_properties']

        E = pipe_properties[0]['material']['E']
        ThermalExpansionCoefficient = pipe_properties[0]['material']['ThermalExpansionCoefficient']
        Poissonsratio = pipe_properties[0]['material']['Poissonsratio']

        differential_temperature = df['differential_temperature']
        circumferential_stress_array = df['circumferential_stress']

        zipped_array = list(zip(circumferential_stress_array, differential_temperature))
        longitudinal_stress_fully_restrained = [item[0]*Poissonsratio - E*ThermalExpansionCoefficient * item[1] for item in zipped_array]
        df['longitudinal_stress_mid_zone'] = longitudinal_stress_fully_restrained
        df['longitudinal_stress_fully_restrained'] = longitudinal_stress_fully_restrained

        return df

    def get_longitudinal_force_fully_restrained(self, cfg, df):
        pipe_properties = cfg['pipeline']['pipe_properties']
        A = pipe_properties[0]['section']['A']

        longitudinal_stress_fully_restrained = df['longitudinal_stress_fully_restrained']
        longitudinal_force_fully_restrained = [item * A for item in longitudinal_stress_fully_restrained]
        df['longitudinal_force_fully_restrained'] = longitudinal_force_fully_restrained

        return df

    def get_water_depth(self, cfg, df):
        water_depth_cfg = cfg['pipeline']['water_depth']

        x_values = water_depth_cfg['length_factor']
        y_values = water_depth_cfg['depth']

        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = df['length_factor']
        water_depth_array = [round(float(f(item)),3) for item in length_factor_array]
        df['water_depth'] = water_depth_array

        return df

