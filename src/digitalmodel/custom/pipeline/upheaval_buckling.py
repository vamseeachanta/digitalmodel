# Standard library imports
import os

# Third party imports
import pandas as pd
from assetutilities.common.visualization.visualization_templates import (
    VisualizationTemplates,
)
from assetutilities.engine import engine as au_engine
from scipy import interpolate

# Reader imports
from digitalmodel.custom.pipeline.buckling_common import CommonBucklingCaculations

viz_templates = VisualizationTemplates()
cbc = CommonBucklingCaculations()

class UpheavalBuckling:
    
    def __init__(self):
        pass

    def run(self, cfg):

        friction_force = self.get_friction_force(cfg)
        cfg['pipeline']['friction_force'] = friction_force
        
        upheaval_buckling_df = self.get_upheaval_buckling(cfg)
        
        self.save_results(cfg, upheaval_buckling_df)

    def get_friction_force(self, cfg):
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        
        mass_in_water = system_properties['mass']['water']['with_internal_fluid']
        friction_cfg = cfg['pipeline']['soil']
        
        friction_force = mass_in_water * friction_cfg['friction_coefficient']
        
        friction = {'axial': friction_force, 'lateral': friction_force, 'vertical': friction_force}

        return friction
    
    def get_upheaval_buckling(self, cfg):

        uphaeval_buckling_df = pd.DataFrame()

        length = cfg['pipeline']['length'] * 12 /0.3048

        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        friction_force = cfg['pipeline']['friction_force']
        tension_cfg  = cfg['pipeline']['tension']

        mesh, uphaeval_buckling_df = cbc.assign_mesh(uphaeval_buckling_df, length)
        length_array = mesh['length']

        differential_temperature = cbc.get_differential_temp(cfg, mesh)
        uphaeval_buckling_df['differential_temperature'] = differential_temperature

        get_compression_at_fully_restrained_zone = self.get_compression_at_fully_restrained_zone(cfg, uphaeval_buckling_df)        
        
        water_depth_array = self.get_water_depth(cfg, mesh)
        uphaeval_buckling_df['water_depth'] = water_depth_array

        pressure_array = self.get_pressure(cfg, mesh)
        uphaeval_buckling_df['pressure_array'] = pressure_array

        return uphaeval_buckling_df

    def get_compression_at_fully_restrained_zone(self, cfg, uphaeval_buckling_df):
        
        avg_diff_temp_for_pipeline_section = self.get_avg_differential_temperature_for_pipeline_section(uphaeval_buckling_df)
        
        circumferential_stress = 
        
        


    def get_avg_differential_temperature_for_pipeline_section(self, df):
        pipeline_section_percent = 10
        length_array = list(df['length'])
        section_length = length_array[-1] * pipeline_section_percent/100
        df_section = df[df['length'] <= section_length]
        differential_temperature_for_pipeline_section = df_section['differential_temperature'].mean()

        return differential_temperature_for_pipeline_section

    def get_pressure(self, cfg, mesh):
        pressure_cfg = cfg['pipeline']['crossection'][0]['pressure']

        x_values = pressure_cfg['length_factor']
        y_values = pressure_cfg['pressure']

        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = mesh['length_factor']
        pressure_array = [round(float(f(item)),3) for item in length_factor_array]

        return pressure_array

    def get_water_depth(self, cfg, mesh):
        water_depth_cfg = cfg['pipeline']['crossection'][0]['water_depth']

        x_values = water_depth_cfg['length_factor']
        y_values = water_depth_cfg['water_depth']

        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = mesh['length_factor']
        water_depth_array = [round(float(f(item)),3) for item in length_factor_array]

        return water_depth_array

    def save_results(self, cfg, lateral_buckling_df):
        file_name = cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling.csv'
        file_name = os.path.join(cfg['Analysis']['result_folder'], file_name)
        lateral_buckling_df.to_csv(file_name, index=False)

        csv_groups = [{'file_name': file_name, 'label': ''}]
        self.save_temperature_plot(cfg, csv_groups.copy())
        self.save_buckling_force_plot_imp(cfg, csv_groups.copy())
        self.save_buckling_force_plot_met(cfg, csv_groups.copy())
        self.save_buckling_stress_plot_imp(cfg, csv_groups.copy())
        self.save_buckling_stress_plot_met(cfg, csv_groups.copy())
        self.save_buckling_strain_plot(cfg, csv_groups.copy())

    def save_temperature_plot(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())

        plot_yml['data']['groups'] = csv_groups
        columns= { 'x': ['length'], 'y': ['differential_temperature'] }
        plot_yml['master_settings']['groups']['columns'] = columns

        transform = [{ 'column': 'length', 'scale': 0.0254, 'shift': 0 }]
        plot_yml['master_settings']['groups']['transform'] = transform

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_temperature', 
                    'title': 'Temperature along length',
                    'xlabel': 'Length (m)',
                    'ylabel': 'Differential Temperature (deg C)',
}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_force_plot_imp(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())

        plot_yml['data']['groups'] = csv_groups

        columns= { 'x': ['length'], 'y': ['longitudinal_force_hot_end', 'longitudinal_force_cold_end', 'fully_restrained_axial_force', 'effective_axial_force', 'min_critical_buckling_load'] }
        plot_yml['master_settings']['groups']['columns'] = columns

        transform = [{ 'column': 'length', 'scale': 0.0254, 'shift': 0 },
                     { 'column': 'longitudinal_force_hot_end', 'scale': 0.001, 'shift': 0 },
                     { 'column': 'longitudinal_force_cold_end', 'scale': 0.001, 'shift': 0 },
                     { 'column': 'fully_restrained_axial_force', 'scale': 0.001, 'shift': 0 },
                     { 'column': 'effective_axial_force', 'scale': 0.001, 'shift': 0 },
                        { 'column': 'min_critical_buckling_load', 'scale': 0.001, 'shift': 0 }
                     ]
        plot_yml['master_settings']['groups']['transform'] = transform

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling_force_imp', 
                    'title': 'Force along length',
                    'xlabel': 'Length (m)',
                    'ylabel': 'Force (kips)'}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_force_plot_met(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())

        plot_yml['data']['groups'] = csv_groups

        columns= { 'x': ['length'], 'y': ['longitudinal_force_hot_end', 'longitudinal_force_cold_end', 'fully_restrained_axial_force', 'effective_axial_force', 'min_critical_buckling_load'] }
        plot_yml['master_settings']['groups']['columns'] = columns

        transform = [{ 'column': 'length', 'scale': 0.0254, 'shift': 0 },
                     { 'column': 'longitudinal_force_hot_end', 'scale': 0.004449816, 'shift': 0 },
                     { 'column': 'longitudinal_force_cold_end', 'scale': 0.004449816, 'shift': 0 },
                     { 'column': 'fully_restrained_axial_force', 'scale': 0.004449816, 'shift': 0 },
                     { 'column': 'effective_axial_force', 'scale': 0.004449816, 'shift': 0 },
                        { 'column': 'min_critical_buckling_load', 'scale': 0.004449816, 'shift': 0 }
                     ]
        plot_yml['master_settings']['groups']['transform'] = transform

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling_force_met', 
                    'title': 'Force along length',
                    'xlabel': 'Length (m)',
                    'ylabel': 'Force (kN)'}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_stress_plot_imp(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())

        plot_yml['data']['groups'] = csv_groups

        columns= { 'x': ['length'], 'y': ['longitudinal_stress_hot_end', 'longitudinal_stress_cold_end', 'circumferential_stress', 'thermal_stress', 'longitudinal_stress_mid_zone'] }
        plot_yml['master_settings']['groups']['columns'] = columns

        length_in_to_m = 0.0254
        stress_psi_to_ksi = 0.001
        transform = [{ 'column': 'length', 'scale': length_in_to_m, 'shift': 0 },
                     { 'column': 'longitudinal_stress_hot_end', 'scale': stress_psi_to_ksi, 'shift': 0 },
                     { 'column': 'longitudinal_stress_cold_end', 'scale': stress_psi_to_ksi, 'shift': 0 },
                     { 'column': 'circumferential_stress', 'scale': stress_psi_to_ksi, 'shift': 0 },
                     { 'column': 'thermal_stress', 'scale': stress_psi_to_ksi, 'shift': 0 },
                     { 'column': 'longitudinal_stress_mid_zone', 'scale': stress_psi_to_ksi, 'shift': 0 }
                     ]
        plot_yml['master_settings']['groups']['transform'] = transform

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling_stress_imp', 
                    'title': 'Stress along length',
                    'xlabel': 'Length (m)',
                    'ylabel': 'Stress (ksi)'}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_stress_plot_met(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())

        plot_yml['data']['groups'] = csv_groups

        columns= { 'x': ['length'], 'y': ['longitudinal_stress_hot_end', 'longitudinal_stress_cold_end', 'circumferential_stress', 'thermal_stress', 'longitudinal_stress_mid_zone'] }
        plot_yml['master_settings']['groups']['columns'] = columns

        length_in_to_m = 0.0254
        stress_psi_to_mpa = 0.00689476
        transform = [{ 'column': 'length', 'scale': length_in_to_m, 'shift': 0 },
                     { 'column': 'longitudinal_stress_hot_end', 'scale': stress_psi_to_mpa, 'shift': 0 },
                     { 'column': 'longitudinal_stress_cold_end', 'scale': stress_psi_to_mpa, 'shift': 0 },
                     { 'column': 'circumferential_stress', 'scale': stress_psi_to_mpa, 'shift': 0 },
                     { 'column': 'thermal_stress', 'scale': stress_psi_to_mpa, 'shift': 0},
                     { 'column': 'longitudinal_stress_mid_zone', 'scale': stress_psi_to_mpa, 'shift': 0 }
                     ]
        plot_yml['master_settings']['groups']['transform'] = transform

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling_stress_met', 
                    'title': 'Stress along length',
                    'xlabel': 'Length (m)',
                    'ylabel': 'Stress (MPa)'}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def save_buckling_strain_plot(self, cfg, csv_groups):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())

        plot_yml['data']['groups'] = csv_groups

        columns= { 'x': ['length'], 'y': ['longitudinal_strain_hot_end', 'longitudinal_strain_cold_end'] }
        plot_yml['master_settings']['groups']['columns'] = columns

        transform = [{ 'column': 'length', 'scale': 0.0254, 'shift': 0 },
                     { 'column': 'longitudinal_strain_hot_end', 'scale': 1, 'shift': 0 },
                     { 'column': 'longitudinal_strain_cold_end', 'scale': 1, 'shift': 0 }
                     ]
        plot_yml['master_settings']['groups']['transform'] = transform

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling_strain', 
                    'title': 'Strain along length',
                    'xlabel': 'Length (m)',
                    'ylabel': 'Strain'}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)
