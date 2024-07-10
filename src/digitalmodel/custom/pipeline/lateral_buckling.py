# Standard library imports
import logging
import os

# Third party imports
import pandas as pd
from assetutilities.common.visualization.visualization_templates import (
    VisualizationTemplates,
)
from assetutilities.engine import engine as au_engine
from scipy import interpolate

viz_templates = VisualizationTemplates()
class LateralBuckling:
    
    def __init__(self):
        pass

    def run(self, cfg):
        friction_force = self.get_friction_force(cfg)
        cfg['pipeline']['friction_force'] = friction_force
        lateral_buckling_df = self.get_lateral_buckling(cfg)
        
        self.save_results(cfg, lateral_buckling_df)

    def save_results(self, cfg, lateral_buckling_df):
        file_name = cfg['Analysis']['file_name_for_overwrite'] + '_lateral_buckling.csv'
        file_name = os.path.join(cfg['Analysis']['result_folder'], file_name)
        lateral_buckling_df.to_csv(file_name, index=False)
        
        self.save_temperature_plot(cfg, lateral_buckling_df)
        
    def save_temperature_plot(self, cfg, lateral_buckling_df):
        plot_yml = viz_templates.get_xy_plot(cfg['Analysis'].copy())
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def get_friction_force(self, cfg):
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        
        mass_in_water = system_properties['mass']['water']['with_internal_fluid']
        friction_cfg = cfg['pipeline']['soil']
        
        axial_friction_force = mass_in_water * friction_cfg['axial_breakout_friction_coeff']
        lateral_friction_force = mass_in_water * friction_cfg['lateral_breakout_friction_coeff']
        
        friction = {'axial': axial_friction_force, 'lateral': lateral_friction_force}

        return friction
    
    def get_lateral_buckling(self, cfg):
        
        lateral_buckling_df = pd.DataFrame()
        length = cfg['pipeline']['length'] * 12 /0.3048

        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        friction = self.get_friction_force(cfg)
        tension_cfg  = cfg['pipeline']['tension']

        mesh = self.get_mesh(length)
        lateral_buckling_df['element'] = mesh['element']
        lateral_buckling_df['length_factor'] = mesh['length_factor']
        length_array = mesh['length']
        lateral_buckling_df['length'] = mesh['length']
        
        differential_temperature = self.get_differential_temp(cfg, mesh)

        lateral_buckling_df['differential_temperature'] = differential_temperature

        pressure = pipe_properties[0]['internal_fluid']['pressure']
        pressure = 1307.6 # For benchmarking
        A_system = system_properties['A']
        A = pipe_properties[0]['section']['A']
        Ai = pipe_properties[0]['section']['Ai']

        logitudinal_force_term1 = tension_cfg['start'] - tension_cfg['lay_tension'] + pressure * Ai
        logitudinal_force = [logitudinal_force_term1 - friction['axial']*(item-length_array[0]) for item in length_array]
        longitudinal_stress_hot_end = [item/A_system for item in logitudinal_force]
        lateral_buckling_df['longitudinal_stress_hot_end'] = longitudinal_stress_hot_end

        logitudinal_force_term1 = tension_cfg['end'] - tension_cfg['lay_tension'] + pressure * Ai
        logitudinal_force = [logitudinal_force_term1 - friction['axial']*(length_array[-1]-item) for item in length_array]
        longitudinal_stress_cold_end = [item/A for item in logitudinal_force]
        lateral_buckling_df['longitudinal_stress_cold_end'] = longitudinal_stress_cold_end

        circumferential_stress = [2* pressure * Ai / A] * len(length_array)
        lateral_buckling_df['circumferential_stress'] = circumferential_stress

        E = pipe_properties[0]['material']['E']
        ThermalExpansionCoefficient = pipe_properties[0]['material']['ThermalExpansionCoefficient']
        Poissonsratio = pipe_properties[0]['material']['Poissonsratio']

        zipped_array = list(zip(longitudinal_stress_hot_end, circumferential_stress, differential_temperature))
        longitudinal_strain_hot_end =  [(item[0] - item[1]*Poissonsratio)/E + ThermalExpansionCoefficient * item[2] for item in zipped_array]
        lateral_buckling_df['longitudinal_strain_hot_end'] = longitudinal_strain_hot_end
        
        zipped_array = list(zip(longitudinal_stress_cold_end, circumferential_stress, differential_temperature))
        longitudinal_strain_cold_end = [(item[0] - item[1]*Poissonsratio)/E + ThermalExpansionCoefficient * item[2] for item in zipped_array]
        lateral_buckling_df['longitudinal_strain_cold_end'] = longitudinal_strain_cold_end

        zipped_array = list(zip(circumferential_stress, differential_temperature))
        longitudinal_stress_mid_zone = [item[0]*Poissonsratio - E*ThermalExpansionCoefficient * item[1] for item in zipped_array]
        lateral_buckling_df['longitudinal_stress_mid_zone'] = longitudinal_stress_mid_zone

        #Anchor Length
        for idx in range(0, len(length_array)):
            if longitudinal_stress_hot_end[idx] < longitudinal_stress_mid_zone[idx]:
                anchor_length_start = length_array[idx]
            else:
                anchor_length_start = length_array[-1]*2

            if longitudinal_stress_cold_end[idx] < longitudinal_stress_mid_zone[idx]:
                anchor_length_end = length_array[idx]
            else:
                anchor_length_end = length_array[0] -1

        anchor_length = {'start': anchor_length_start, 'end': anchor_length_end} 


        reference_length = (friction['axial'] * length_array[-1] + tension_cfg['start'] - tension_cfg['end'])/(2*friction['axial'])
        #TODO check certain conditions

        length_start = reference_length
        length_end = length_start

        #Buckling Check
        fully_restrained_axial_force_term1 = tension_cfg['lay_tension'] - pressure * Ai *(1-2*Poissonsratio) 
        fully_restrained_axial_force = [fully_restrained_axial_force_term1 - A *E*ThermalExpansionCoefficient* item for item in differential_temperature]
        lateral_buckling_df['fully_restrained_axial_force'] = fully_restrained_axial_force

        effective_axial_force_array = []
        for idx in range(0, len(length_array)):
            if length_array[idx] < length_start:
                effective_axial_force = -friction['axial']*length_array[idx] + tension_cfg['start']
            elif length_array[idx]< length_end:
                effective_axial_force = fully_restrained_axial_force[idx]
            else:
                effective_axial_force = -friction['axial']*(length_array[-1] - length_array[idx]) + tension_cfg['end']
            effective_axial_force_array.append(effective_axial_force)
        lateral_buckling_df['effective_axial_force'] = effective_axial_force_array

        I = pipe_properties[0]['section']['I']
        critical_buckling_load = 0.65* 2.26* (E*A)**0.25 * (E*I)**0.25 * friction['lateral']**0.5
        
        route_curve_radius = cfg['pipeline']['route']['curve_radius'] /0.0254
        critical_buckling_load_for_route = friction['lateral'] * route_curve_radius
        min_critical_buckling_load = -min(critical_buckling_load, critical_buckling_load_for_route)

        logging.info(f"S Effective at L = L/2, {effective_axial_force_array[int(len(length_array)/2)]}")
        logging.info(f"S Restrained at L = 0, {fully_restrained_axial_force[0]/1000}")
        logging.info(f"S Critical at L = 0, {min_critical_buckling_load}")

        return lateral_buckling_df


    def get_mesh(self, length):
        no_of_elements = 100
        element_array = list(range(0,no_of_elements+1))
        length_factor_array = [item/no_of_elements for item in element_array]
        length_array = [item*length for item in length_factor_array]
        
        mesh = {'element': element_array, 'length_factor': length_factor_array, 'length': length_array}

        return mesh
        
    def get_differential_temp(self, cfg, mesh):
        temperature_cfg = cfg['pipeline']['crossection'][0]['temperature']
        
        x_values = temperature_cfg['length_factor']
        y_values = temperature_cfg['temperature']
        
        f = interpolate.interp1d(x_values, y_values)
        length_factor_array = mesh['length_factor']
        differential_temperature_array = [round(float(f(item)),3) for item in length_factor_array]

        return differential_temperature_array
                