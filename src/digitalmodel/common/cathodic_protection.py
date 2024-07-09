# Standard library imports
import math
import os

# Third party imports
import numpy as np
import pandas as pd
from scipy.interpolate import griddata

# Reader imports
from digitalmodel.common.cp_DNV_RP_F103_2010 import DNV_RP_F103

dnv_rp_f103 = DNV_RP_F103()
class CathodicProtection():

    
    def __init__(self):
        pass

    def is_float(value):
        try:
            float(value)
            return True
        except ValueError:
            return False
        
    def router(self, cfg):
        if cfg['inputs']['calculation_type'] == 'ABS_gn_ships_2018':
            self.ABS_gn_ships_2018(cfg)
        elif cfg['inputs']['calculation_type'] in ['DNV_RP_F103_2010', 'DNV_RP_F103_2019']:
            dnv_rp_f103.router(cfg)
        else:
            raise (Exception(f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"))

        return cfg


    def ABS_gn_ships_2018(self, cfg):
        """
        This method is used to calculate the cathodic protection for ABS gn ships 2018
        """
        
        structure_area = self.get_structure_area(cfg)
        breakdown_factor = self.assess_coating_breakdown(cfg)
        current_density = self.get_design_current_density(cfg)

        current_demand = self.get_current_demand(cfg, structure_area, current_density)

        anode_capacity = self.get_anode_current_capacity(cfg)
        
        anodes_required = self.get_anodes_required(cfg,breakdown_factor,current_demand,anode_capacity)
        
        anode_initial_check = self.get_anode_initial_check(cfg,anode_capacity,breakdown_factor,current_demand, anodes_required)
        
        anode_final_check = self.anode_final_check(cfg,anode_capacity,breakdown_factor,anode_initial_check, anodes_required,current_demand)

        seawater_resistivity = self.get_seawater_resistivity(cfg)
        
        outputs_dict = {'structure_area': structure_area, 'breakdown_factor': breakdown_factor,
                          'current_density': current_density, 'current_demand': current_demand,
                          'anode_capacity': anode_capacity, 'anodes_required': anodes_required,
                          'anode_initial_check': anode_initial_check, 'anode_final_check': anode_final_check,
                          'seawater_resistivity':seawater_resistivity}

        cfg['outputs'] = outputs_dict
        self.save_csv_output(cfg)

        return cfg

    def save_csv_output(self, cfg):
        outputs_dict = cfg['outputs']

        anode_count_initial_kg = outputs_dict['anodes_required']['count']['initial']
        anode_count_mean_kg = outputs_dict['anodes_required']['count']['mean']
        anode_count_final_kg = outputs_dict['anodes_required']['count']['final']
        
        anode_mass_initial_kg = outputs_dict['anodes_required']['mass']['initial']
        anode_mass_mean_kg = outputs_dict['anodes_required']['mass']['mean']
        anode_mass_final_kg = outputs_dict['anodes_required']['mass']['final']

        output_summary = {'anode_count_initial_kg': anode_count_initial_kg, 'anode_count_mean_kg': anode_count_mean_kg,
                'anode_count_final_kg': anode_count_final_kg, 'anode_mass_initial_kg': anode_mass_initial_kg,
                'anode_mass_mean_kg': anode_mass_mean_kg, 'anode_mass_final_kg': anode_mass_final_kg}
        df = pd.DataFrame(output_summary, index=[0])

        result_folder = cfg['Analysis']['result_folder']
        filename = cfg['Analysis']['file_name'] + '.csv'
        filename_with_path = os.path.join(result_folder, filename)

        df.to_csv(filename_with_path, index=False)

        cfg['outputs']['summary'] = output_summary

        return cfg

    def assess_coating_breakdown(self, cfg):
        """

        """
        design_life = cfg['inputs']['design_life']
        environment_cfg = cfg['inputs']['environment']
        coating_breakdown_cfg = cfg['inputs']['structure']['area']['coating_breakdown']

        #TODO - implement yearly breakdown calculations using lookup from inputs
        # for idx in range(0, math.ceil(10.3)):
        #     year = idx +1
        #     year_breakdown = 3
        breakdown_percentage = coating_breakdown_cfg['percentage']
        breakdown_factor = [1 + percentage_val/100 for percentage_val in breakdown_percentage]

        cummulative_breakdown_factor = []
        value = 1
        for idx in range(0, len(breakdown_factor)):
            value = value*breakdown_factor[idx]
            cummulative_breakdown_factor.append(value)

        fcm = np.mean(cummulative_breakdown_factor)
        fcm = round(float(fcm), 4)
        fcf = cummulative_breakdown_factor[-1]
        fcf = round(float(fcf), 4)
        resistivity = environment_cfg['seawater_resistivity']['input']
        
        breakdown_factor = {'resistivity' :resistivity ,'fcm': round(fcm,3), 'fcf': round(fcf,3)}
        return breakdown_factor

    def get_structure_area(self, cfg):
        """

        """
        structure_cfg = cfg['inputs']['structure']
        coated = structure_cfg['area']['value']*structure_cfg['area']['coat_percentage']/100
        uncoated = structure_cfg['area']['value']*(1-structure_cfg['area']['coat_percentage']/100)
        structure_area = {'coated': coated, 'uncoated': uncoated}

        return structure_area
        
    def get_design_current_density(self, cfg):
        """
        
        """
        current_density_cfg = cfg['inputs']['current_density']
        initial_coated_value = current_density_cfg['initial']['coated']
        initial_uncoated_value = current_density_cfg['initial']['uncoated']

        final_value = self.assess_coating_breakdown(cfg)['fcf']

        deterioration_final_current_density = initial_coated_value * final_value
        disbondment_final_current_density = initial_uncoated_value * (final_value - 1)

        deterioration_mean = np.mean([initial_coated_value, deterioration_final_current_density])
        deterioration_mean = round(float(deterioration_mean), 4)
        disbondment_mean = np.mean([initial_coated_value, disbondment_final_current_density])
        disbondment_mean = round(float(disbondment_mean), 4)

        design_current_density = { 'initial': {'coated': initial_coated_value,'uncoated': initial_uncoated_value},
                 'mean': {  'deterioration': round(deterioration_mean, 3),
                            'disbondment': round(disbondment_mean, 3) },
                 'final': { 'deterioration': round(deterioration_final_current_density, 3),
                            'disbondment': round(disbondment_final_current_density, 3)}
                                 }
          
        return design_current_density

    def get_current_demand(self, cfg, structure_area, current_density):
        """
        This method is used to calculate the current demand
        """
        
        coating_breakdown_type = cfg['inputs']['structure']['area']['coating_breakdown']['type']

           
        coated_area = structure_area['coated']
        uncoated_area = structure_area['uncoated']
        
        initial_demand = (coated_area * current_density['initial']['coated'] + uncoated_area * current_density['initial']['uncoated'])
        mean_demand = (coated_area * current_density['mean'][coating_breakdown_type] + uncoated_area * current_density['mean'][coating_breakdown_type])
        final_demand = (coated_area * current_density['final'][coating_breakdown_type] + uncoated_area * current_density['final'][coating_breakdown_type])

        initial_demand = initial_demand/1000
        mean_demand = mean_demand/1000
        final_demand = final_demand/1000
        
        current_demand = {'initial': initial_demand,'mean': round(mean_demand,3),'final': round(final_demand,3)}

        return current_demand

    
    def get_anode_current_capacity(self, cfg):
        """
        This method is used to calculate the anode current capacity
        """
        anode_cfg = cfg['inputs']['anode_capacity']
        anode_current_capacity = anode_cfg['anode_current_capacity']
        anode_utilisation_factor = anode_cfg['anode_utilisation_factor']
        
        anode_length = anode_cfg['physical_properties']['mean_length']
        anode_width = anode_cfg['physical_properties']['width']
        anode_height = anode_cfg['physical_properties']['height']
        anode_weight = anode_cfg['physical_properties']['gross_weight']
        
        volume = anode_cfg['physical_properties']['mean_length'] * anode_cfg['physical_properties']['width'] * anode_cfg['physical_properties']['height']

        anode_capacity = {'anode_length': anode_length, 'anode_width':anode_width, 'anode_height':anode_height,
                          'anode_gross_weight': anode_weight, 'anode_volume': round(volume,5),'utilisation_factor':anode_utilisation_factor}
        return anode_capacity
    
        
    def get_anodes_required(self,cfg, breakdown_factor, current_demand, anode_capacity):
        """
        This method is used to calculate weight and no of anodes required
        """
        design_life = cfg['inputs']['design_life']
        anode_current = cfg['inputs']['anode_capacity']['anode_current_capacity']
        anode_utilisation = cfg['inputs']['anode_capacity']['anode_utilisation_factor']
        
        anode_net_weight = cfg['inputs']['anode_capacity']['physical_properties']['net_weight']
        ratio_of_net_weight = cfg['inputs']['anode_capacity']['physical_properties']['ratio_of_gross_to_net_weight']

        anode_mass_initial = current_demand['initial'] / anode_current * 24 * 365 * design_life / anode_utilisation
        anode_mass_mean = current_demand['mean'] / anode_current * 24 * 365 * design_life / anode_utilisation
        anode_mass_final = current_demand['final'] / anode_current * 24 * 365 * design_life / anode_utilisation
        
        anode_mass_initial_count = anode_mass_initial / anode_net_weight
        anode_mass_mean_count = anode_mass_mean / anode_net_weight
        anode_mass_final_count = anode_mass_final / anode_net_weight
        
        anode_mass = {'initial': round(anode_mass_initial_count, 3), 'mean': round(anode_mass_mean_count, 3), 'final': round(anode_mass_final_count , 3)}   
        anode_count = {'initial': math.ceil(anode_mass_initial_count), 'mean': math.ceil(anode_mass_mean_count), 'final': math.ceil(anode_mass_final_count  )}
        
        anodes_required = {'mass': anode_mass, 'count': anode_count}
        
        return anodes_required

    def get_anode_initial_check(self, cfg, anode_capacity, breakdown_factor, current_demand,anodes_required):

        """
        This method is used to check the initial anode
        """
        anode_cfg = cfg['inputs']['anode_shape']
        Delta_E = abs( anode_cfg['steel_iron_calomel_voltage'] - anode_cfg['anode_voltage'] )
        anode_length_width = anode_capacity['anode_length'] / anode_capacity['anode_width']
        resistivity = breakdown_factor['resistivity']
        anode_exposed_area = 2 * (anode_capacity['anode_length'] * anode_capacity['anode_width'] + anode_capacity['anode_width']
                              * anode_capacity['anode_height'] + anode_capacity['anode_length'] * anode_capacity['anode_height'])

        if anode_length_width > 4:
            anode_length = anode_capacity['anode_length']
            anode_width = anode_capacity['anode_width']

            mean_dimension = np.mean([anode_length, anode_width])
            mean_dimension = round(float(mean_dimension), 5)

            anode_resistance = resistivity / (2 * mean_dimension)
        else:
            anode_resistance = 0.315 * resistivity / math.sqrt(anode_exposed_area)

        invidual_anode_current = Delta_E / anode_resistance
        
        total_anode_current = invidual_anode_current * anodes_required['mass']['mean']
        
        if total_anode_current > current_demand['initial'] :
            initial_total_current_output_check = "Yes"
        else:
            initial_total_current_output_check = "No"
        
        if initial_total_current_output_check == "Yes":
            updated_anode_count = " - "
        else:
            updated_anode_count = current_demand['initial'] / invidual_anode_current
        if initial_total_current_output_check == "Yes":
            updated_anode_weight = " - "
        else:
            updated_anode_weight = updated_anode_count * anode_capacity['anode_gross_weight'] / 1000

        anode_initial_check = {'delta_E':round(Delta_E,3), 'anode_resistance':round(anode_resistance,3), 'individual_anode_current': round(invidual_anode_current,3),
                               'total_anode_current': round(total_anode_current,3), 'initial_total_anode_output_check':initial_total_current_output_check,
                               'updated_anode_count':round(updated_anode_count,3),'updated_anode-weight':round(updated_anode_weight,1)

                              }
        return anode_initial_check


    
    def anode_final_check(self, cfg, anode_capacity,breakdown_factor,anode_initial_check,anodes_required,current_demand):
        
        """
        This method is used to check the final anode
        """
        depleted_anode_mass = anode_capacity['anode_gross_weight'] * (1- anode_capacity['utilisation_factor'])
        depleted_anode_length = anode_capacity['anode_length']* (1- 0.1 * anode_capacity['utilisation_factor'])

        anode_length_width = depleted_anode_length / anode_capacity['anode_width']
        anode_exposed_area = 2*(anode_length_width* anode_capacity['anode_width']+anode_capacity['anode_width']
                                *anode_capacity['anode_height']+anode_length_width *anode_capacity['anode_height'])
        if anode_length_width > 4:
            depleted_anode = depleted_anode_length
            anode_width = anode_capacity['anode_width']

            mean_dimension = np.mean([depleted_anode,anode_width])
            mean_dimension = float(mean_dimension)

            anode_resistance = breakdown_factor['resistivity'] / (2 * mean_dimension)
        else:
            anode_resistance = 0.315* breakdown_factor['resistivity']/ math.sqrt(anode_exposed_area)
        
        individual_anode_output = anode_initial_check['delta_E'] / anode_resistance
        total_anode_current_output = individual_anode_output * anodes_required['mass']['mean']
        
        if total_anode_current_output >current_demand['final']:
            initial_total_current_output_check = "Yes"
        else:
            initial_total_current_output_check = "No"
        if initial_total_current_output_check == "Yes" :
            updated_anode_count = " - "
        else:
            updated_anode_count = current_demand['final'] / individual_anode_output

        if anode_initial_check['initial_total_anode_output_check'] == "Yes":
            updated_anode_weight = " - "
        else:
            updated_anode_weight = updated_anode_count * anode_capacity['anode_gross_weight'] / 1000
        
        anode_final_check = {'updated_anode_count':round(updated_anode_count,3),'updated_anode_weight':round(updated_anode_weight,3),
                             'initial_total_current_output_check':(initial_total_current_output_check),
                             'total_current_output':round(total_anode_current_output,3),'individual_anode_output':round(individual_anode_output,3),
                             'anode_resistance':round(anode_resistance,3)
                             }
        return anode_final_check

    
    def get_seawater_resistivity(self, cfg):
        """
        This method is used to calculate seawater resistivity
        """
        seawater_cfg = cfg['inputs']['environment']
        csv_file = seawater_cfg['seawater_resistivity']['csv_filename']
        
        temperature = seawater_cfg['temperature']
        salinity = seawater_cfg['salinity']
        input_value = seawater_cfg['seawater_resistivity']['input']
        
        if CathodicProtection.is_float(input_value):
            resistivity = float(input_value)
        else:
            df = pd.read_csv(csv_file)
        
            def calculate_resistivity(temperature, salinity):
                input_check = df[(df['Temperature (deg C)'] == temperature) & (df['Salinity (%)'] == salinity)]
            
                if input_check.empty:  
                    points = df[['Temperature (deg C)', 'Salinity (%)']].values
                    values = df['Conductance 1/(ohm.cm)'].values
                    conductance = griddata(points, values, (temperature, salinity), method='linear')
        
                # If interpolation result is NaN use nearest neighbor
                    if np.isnan(conductance):
                        conductance = griddata(points, values, (temperature, salinity), method='nearest')
                else:
                    conductance = input_check['Conductance 1/(ohm.cm)'].values[0] 
        
                resistivity = 1 / conductance
    
                return round(resistivity,5)
        
            resistivity = calculate_resistivity(temperature, salinity)
            return resistivity
        
        return resistivity        
        return resistivity