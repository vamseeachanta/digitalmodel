# Standard library imports
import math
import os

# Third party imports
import pandas as pd
from assetutilities.common.visualization.visualization_templates_matplotlib import (
    VisualizationTemplates,
)
from assetutilities.engine import engine as au_engine

# Reader imports
from digitalmodel.modules.pipeline.buckling_common import CommonBucklingCaculations

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

        df = pd.DataFrame()

        length = cfg['pipeline']['length'] * 12 /0.3048

        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        friction_force = cfg['pipeline']['friction_force']
        tension_cfg  = cfg['pipeline']['tension']

        df = cbc.assign_mesh(df, length)
        df = cbc.get_water_depth(cfg, df)
        df = cbc.get_differential_temp(cfg, df)
        df = cbc.get_internal_pressure(cfg, df)
        df = cbc.get_external_pressure(cfg, df)

        df = cbc.get_circumferential_stress(cfg, df)
        df = cbc.get_longitudinal_stress_fully_restrained(cfg, df)
        df = cbc.get_longitudinal_force_fully_restrained(cfg, df)
        force_in_pipeline_section = self.get_force_in_pipe_section(cfg, df)

        uplift_resistance_model = self.get_uplift_resistance_model(cfg, df, force_in_pipeline_section)
        trench_and_rock_burial_model = self.get_trench_and_rock_burial_model(cfg, df)
        soil_pipe_friction_model = self.get_soil_pipe_friction_model(cfg)
        level_1_result = self.get_level_1_analysis(cfg, df, uplift_resistance_model, trench_and_rock_burial_model, soil_pipe_friction_model, force_in_pipeline_section)

        return df

    def get_level_1_analysis(self, cfg, df, uplift_resistance_model, trench_and_rock_burial_model, soil_pipe_friction_model, force_in_pipeline_section):
        buckle_region_length_factor = 0.01
        l_b = df['length'].iloc[-1] * buckle_region_length_factor
        
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        friction_force = cfg['pipeline']['friction_force']
        tension_cfg  = cfg['pipeline']['tension']
        qb = trench_and_rock_burial_model['burial_rock_weight']['pipe_seabed']
        E = pipe_properties[0]['material']['E']
        I = pipe_properties[0]['section']['I']

        N = 20.19 * E * I / (l_b**2)
        
        n_1 = (N/(E*I))**0.5
        # TODO HOLD eigen value solution not implemented.
        NL = 4.493
        
        
        
        
        # Solve wave equation with boundary conditions
        # Solve maximum allowable thermal differential temperature allowed.

        m_1 = qb / (E*I)
        level_1_analysis = {'N': N, 'n_1': n_1, 'm_1': m_1, 'NL' : NL}
        
        self.get_buckle_result(cfg, df, level_1_analysis, uplift_resistance_model, trench_and_rock_burial_model, soil_pipe_friction_model, force_in_pipeline_section)
        
        return level_1_analysis
    
    def get_buckle_result(self, cfg, df, level_1_analysis, uplift_resistance_model, trench_and_rock_burial_model, soil_pipe_friction_model, force_in_pipeline_section):
        
        NL = level_1_analysis['NL']

        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        friction_force = cfg['pipeline']['friction_force']
        tension_cfg  = cfg['pipeline']['tension']
        qb = trench_and_rock_burial_model['burial_rock_weight']['pipe_seabed']
        E = pipe_properties[0]['material']['E']
        I = pipe_properties[0]['section']['I']

        
        L_end = 100
        T_max = 200
        array_length = 101
        Lb_array = [item/(array_length-1)*L_end for item in list(range(0,array_length))]
        
        buckle_df = pd.DataFrame()
        buckle_df['Lb'] = Lb_array
        for Lb in Lb_array:
            if Lb> 0:
                n1 = 20.19**0.5/Lb
                N = NL**2 * (E*I) / Lb**2
                C_x = 1 - math.cos(n1) 
                B = 10
                F_b = N + B

    
    def get_soil_pipe_friction_model(self, cfg):
        soil_cfg = cfg['pipeline']['soil']
        u_m  = soil_cfg['displacement_at_full_friction_mobilization']
        
        friction_angle_of_burial_material = soil_cfg['trench']['friction_angle_of_burial_material']
        friction_coefficient_burial = math.radians(friction_angle_of_burial_material)

        friction_df = pd.DataFrame()
        u = [ item**u_m/100 for item in list(range(0, 101, 1))]
        friction_df['u'] = u
        friction_coefficient = [-1*abs(item)/item *friction_coefficient_burial* (1 - math.e**(-25*abs(item)/u_m)) if item>0 else 0 for item in u]
        friction_df['friction_coefficient'] = friction_coefficient
        
        soil_pipe_friction_model = {'u_m': u_m, 'friction_df': friction_df}
        
        return soil_pipe_friction_model



    def get_uplift_resistance_model(self, cfg, df, force_in_pipeline_section):
        soil_cfg = cfg['pipeline']['soil']
        H = soil_cfg['trench']['burial_depth_to_pipe_center_line']
        pipe_properties = cfg['pipeline']['pipe_properties']
        OD = pipe_properties[0]['coating'][-1]['OD']

        term_1 = (0.02 + 0.08* H/OD)*OD
        term_2 = 0.1*OD
        vertical_displacement_at_max_uplift_resistance = min(term_1, term_2)

        uplift_resistance_model = {'vertical_displacement_at_max_uplift_resistance': vertical_displacement_at_max_uplift_resistance}

        return uplift_resistance_model

    def get_trench_and_rock_burial_model(self, cfg, df):
        soil_cfg = cfg['pipeline']['soil']
        pipe_properties = cfg['pipeline']['pipe_properties']
        mass_in_water_with_internal_fluid = cfg['pipeline']['system_properties']['mass']['water']['with_internal_fluid']
        friction_angle_of_burial_material = soil_cfg['trench']['friction_angle_of_burial_material']
        trench_angle = soil_cfg['trench']['trench_angle']
        friction_coefficient_burial = math.tan(math.radians(friction_angle_of_burial_material))
        H = soil_cfg['trench']['burial_depth_to_pipe_center_line']
        Ht = soil_cfg['trench']['trench_depth_below_seabed']
        OD = pipe_properties[0]['coating'][-1]['OD']
        zeta_trench_factor = OD/(2*math.cos(math.radians(trench_angle)))
        density_of_burial_material = soil_cfg['trench']['density_of_burial_material']

        burial_rock_weight = 0
        term_1 = density_of_burial_material * H *OD
        term_2 = math.pi/8 * density_of_burial_material *OD**2
        term_3 = density_of_burial_material * H**2 * math.tan(friction_coefficient_burial)
        term_4 = density_of_burial_material * H**2 * math.tan(friction_coefficient_burial)* math.cos(friction_coefficient_burial)
        if H > OD/2 and H <= (Ht -zeta_trench_factor) and Ht > 0:
            burial_rock_weight = term_1 - term_2 + term_3
        elif H > (Ht - zeta_trench_factor) and H > OD/2:
            burial_rock_weight = term_1 - term_2 + term_4

        weight_pipe_seabed = burial_rock_weight + mass_in_water_with_internal_fluid
        trench_and_rock_burial_model = {'burial_rock_weight': {'burial_pipe': burial_rock_weight, 'pipe_seabed':weight_pipe_seabed }}

        return trench_and_rock_burial_model

    def get_force_in_pipe_section(self, cfg, df):
        pipeline_section_percent = 10
        length_array = list(df['length'])
        section_length = length_array[-1] * pipeline_section_percent/100
        df_section = df[df['length'] <= section_length]
        force_in_pipeline_section = df_section['longitudinal_force_fully_restrained'].mean()

        return force_in_pipeline_section


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
        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())

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
        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())

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
        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())

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
        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())

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
        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())

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
        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())

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
