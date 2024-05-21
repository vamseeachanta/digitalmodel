import math
import numpy as np
from scipy.interpolate import interp1d

class CathodicProtection():

    def __init__(self):
        pass

    def router(self, cfg):
        if cfg['inputs']['calculation_type'] == 'ABS_gn_ships_2018':
            self.ABS_gn_ships_2018(cfg)
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
        
        my_value = cfg.inputs['design_data']['seawater_max_temperature'] * cfg.inputs['design_data']['design_life']

    def assess_coating_breakdown(self, cfg):
        """

        """
        design_life = cfg['inputs']['design_life']
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
        fcf = cummulative_breakdown_factor[-1]

        breakdown_factor = {'fcm': fcm, 'fcf': fcf}
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
        disbonding_final_current_density = initial_uncoated_value * (final_value - 1)

        deterioration_mean = np.mean([initial_coated_value, deterioration_final_current_density])
        disbondment_mean = np.mean([initial_coated_value, disbonding_final_current_density])

        design_current_density = {'initial': {'coated': initial_coated_value, 'uncoated': initial_uncoated_value},
            'mean': {'deterioration': {'coated': initial_coated_value, 'uncoated': initial_uncoated_value}, 
                     'disbondment': {'coated': initial_coated_value, 'uncoated': initial_uncoated_value},},
            'final': {'deterioration': {'coated': initial_coated_value, 'uncoated': initial_uncoated_value},
                      'disbondment': {'coated': initial_coated_value, 'uncoated': initial_uncoated_value},}  
            }
        
        return design_current_density

    def get_current_demand(self, cfg, structure_area, current_density):
        """
        This method is used to calculate the current demand
        """
        
        coating_breakdown_type = cfg['inputs']['structure']['area']['coating_breakdown']['type']

           
        coated_area = structure_area['coated']
        uncoated_area = structure_area['uncoated']
        
            
        initial_demand = coated_area * current_density['initial']['coated'] + uncoated_area * current_density['initial']['uncoated']
        mean_demand = coated_area * current_density['mean'][coating_breakdown_type]['coated'] + uncoated_area * current_density['mean'][coating_breakdown_type]['uncoated']
        final_demand = coated_area * current_density['final'][coating_breakdown_type]['coated'] + uncoated_area * current_density['final'][coating_breakdown_type]['uncoated']

        initial_demand = initial_demand/1000
        mean_demand = mean_demand/1000
        final_demand = final_demand/1000
        
        current_demand = {'initial': initial_demand,'mean': mean_demand,'final': final_demand}

        return current_demand

    
    def get_anode_current_capacity(self, cfg):
        """
        This method is used to calculate the anode current capacity
        """
        anode_cfg = cfg['inputs']['anode_capacity']
        anode_current_capacity = anode_cfg['anode_current_capacity']
        anode_utilisation_factor = anode_cfg['anode_utilisation_factor']
        
        
        volume = anode_cfg['physical_properties']['mean_length'] * anode_cfg['physical_properties']['width'] * anode_cfg['physical_properties']['height']

        anode_capacity = {'anode_volume': volume}
        return anode_capacity
    
        
    def get_anodes_required(self,cfg, breakdown_factor, current_demand, anode_capacity):
        """
        This method is used to calculate weight and no of anodes required
        """
        design_life = cfg['inputs']['design_life']
        anode_current = cfg['inputs']['anode_capacity']['anode_current_capacity']
        anode_utilisation = cfg['inputs']['anode_capacity']['anode_utilisation_factor']
        anode_net_weight = cfg['inputs']['anode_capacity']['physical_properties']['net_weight']

        anode_mass_mean = current_demand['mean'] / anode_current * 24 * 365 * design_life / anode_utilisation/anode_net_weight
        anode_mass_initial = current_demand['initial'] / anode_current * 24 * 365 * design_life / anode_utilisation/anode_net_weight
        anode_mass_final = current_demand['final'] / anode_current * 24 * 365 * design_life / anode_utilisation/anode_net_weight
        anode_mass = {'mean': round(anode_mass_mean, 3) , 'initial': round(anode_mass_initial, 3), 'final': round(anode_mass_final, 3)}
        
        anode_count_mean = math.ceil(anode_mass_mean / anode_capacity['anode_volume'])
        anode_count_initial = math.ceil(anode_mass_initial / anode_capacity['anode_volume'])
        anode_count_final = math.ceil(anode_mass_final / anode_capacity['anode_volume'])
        anode_count = {'mean': anode_count_mean, 'initial': anode_count_initial, 'final': anode_count_final}
        
        anodes_required = {'mass': anode_mass, 'count': anode_count}
        
        return anodes_required



    def anode_initial_check(self, cfg):
        """
        This method is used to check the initial anode
        """
        pass


    def anode_final_check(self, cfg):
        
        """
        This method is used to check the final anode
        """
        pass

    def get_seawater_resistivity(self, cfg):
        """
        This method is used to calculate seawater resistivity
        """
        pass
    