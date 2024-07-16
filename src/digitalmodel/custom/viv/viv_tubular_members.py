# Standard library imports
import math

g_inch_per_second_squared = 386.4

class VIVTubularMembers:
    def __init__(self):
        pass
    
    def analyze(self, cfg):

        natural_frequencies = self.get_natural_frequencies(cfg)
        vs_frequencies = self.get_vs_frequencies(cfg)
        
    def get_natural_frequencies(self, cfg):
        
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        
        E = pipe_properties[0]['material']['E']
        I = pipe_properties[0]['section']['I']

        mass_without_internal_fluid = system_properties['mass']['air']['without_internal_fluid']
        mass_with_internal_fluid = system_properties['mass']['air']['with_internal_fluid']
        
        span_length = cfg['pipeline']['span_length'] *12

        simply_supported_ends_empty = []
        for mode_number in list(range(1, 4)):
            frequency = (1 / (2 * math.pi)) * (mode_number*math.pi/span_length)**2 * ((E * I) / (mass_without_internal_fluid/g_inch_per_second_squared)) ** 0.5
            simply_supported_ends_empty.append(frequency)

        simply_supported_ends_flooded = []
        for mode_number in list(range(1, 4)):
            frequency = (1 / (2 * math.pi)) * (mode_number*math.pi/span_length)**2 * ((E * I) / (mass_with_internal_fluid/g_inch_per_second_squared)) ** 0.5
            simply_supported_ends_flooded.append(frequency)

        eigen_values_clamped = cfg['modes']['eigen_values']['clamped']
        clamped_clamped_empty = []
        for mode_number in list(range(1, 4)):
            frequency = (1 / (2 * math.pi)) * (eigen_values_clamped[mode_number-1]/span_length)**2 * ((E * I) / (mass_without_internal_fluid/g_inch_per_second_squared)) ** 0.5
            clamped_clamped_empty.append(frequency)

        clamped_clamped_flooded = []
        for mode_number in list(range(1, 4)):
            frequency = (1 / (2 * math.pi)) * (eigen_values_clamped[mode_number-1]/span_length)**2 * ((E * I) / (mass_with_internal_fluid/g_inch_per_second_squared)) ** 0.5
            clamped_clamped_flooded.append(frequency)

        free_free_empty = []
        for mode_number in list(range(1, 4)):
            frequency = (1 / (2 * math.pi)) * (22.373/span_length**2) * ((E * I) / (mass_without_internal_fluid/g_inch_per_second_squared)) ** 0.5
            free_free_empty.append(frequency)

        free_free_flooded = []
        for mode_number in list(range(1, 4)):
            frequency = (1 / (2 * math.pi)) * (22.373/span_length**2) * ((E * I) / (mass_with_internal_fluid/g_inch_per_second_squared)) ** 0.5
            free_free_flooded.append(frequency)

        natural_frequencies = {'simply_simply': {'empty': simply_supported_ends_empty, 'flooded': simply_supported_ends_flooded}, 
                          'clamped_clamped': {'empty': clamped_clamped_empty, 'flooded': clamped_clamped_flooded},
                          'free_free': {'empty': free_free_empty, 'flooded': free_free_flooded}}
        
        return natural_frequencies

    def get_vs_frequencies(self, cfg):
        st = cfg['viv']['st']
        external_fluid_cfg = cfg['pipeline']['crossection'][0]['external_fluid']
        density = external_fluid_cfg['density']
        kinematic_viscosity = external_fluid_cfg['kinematic_viscosity'] * (1/0.0254)**2
        OD = cfg['pipeline']['crossection'][0]['Nominal_OD']
        
        current_cfg = cfg['environment']['current']
        for current_item in current_cfg:
            current_label = current_item['label']
            for depth_item in current_item['data']:
                current_depth = depth_item['depth']
                current_speed = depth_item['speed'] /0.0254
                Re = (current_speed * OD) / kinematic_viscosity
                shredding_frequency = st * current_speed/ OD
                KC = current_speed /shredding_frequency / OD

                viv_item = {'depth': current_depth, 'speed': current_speed, 'Re': Re, 'shredding_frequency': shredding_frequency, 'KC': KC}
                
                

