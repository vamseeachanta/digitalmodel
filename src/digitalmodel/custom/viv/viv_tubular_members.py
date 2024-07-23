# Standard library imports
import math
import os

# Third party imports
import pandas as pd

g_inch_per_second_squared = 386.4

class VIVTubularMembers:
    def __init__(self):
        pass
    
    def analyze(self, cfg):

        natural_frequencies = self.get_natural_frequencies(cfg)
        vs_frequencies = self.get_vs_frequencies(cfg)
        safety_factors = self.get_safety_factors(cfg, natural_frequencies, vs_frequencies)
        
        cfg['viv']['natural_frequencies'] = natural_frequencies.to_json()
        cfg['viv']['vs_frequencies'] = vs_frequencies.to_json()
        cfg['viv']['safety_factors'] = safety_factors.to_json()

        self.save_plots(cfg, safety_factors)

        return cfg

    def save_plots(self, cfg, natural_frequencies, vs_frequencies, safety_factors):
        self.save_natural_frequencies_plot(cfg, natural_frequencies)

    def save_natural_frequencies_plot(self, cfg, natural_frequencies):
        plot_yml = viz_templates.get_xy_plot_line_csv(cfg['Analysis'].copy())


    def get_natural_frequencies(self, cfg):

        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        
        E = pipe_properties[0]['material']['E']
        I = pipe_properties[0]['section']['I']

        mass_without_internal_fluid = system_properties['mass']['air']['without_internal_fluid']
        mass_with_internal_fluid = system_properties['mass']['air']['with_internal_fluid']
        
        span_length_array = cfg['pipeline']['span_length']
        span_length_array = [item*12 for item in span_length_array]

        natural_frequencies_df = pd.DataFrame(columns = ['span_length', 'boundary_condition', 'internal_fluid', 'mode 1', 'mode 2', 'mode 3'])

        for span_length in span_length_array:
            simply_supported_ends_empty = []
            for mode_number in list(range(1, 4)):
                frequency = (1 / (2 * math.pi)) * (mode_number*math.pi/span_length)**2 * ((E * I) / (mass_without_internal_fluid/g_inch_per_second_squared)) ** 0.5
                simply_supported_ends_empty.append(frequency)
            natural_frequencies_df.loc[len(natural_frequencies_df)] = [span_length/12, 'simply_supported', 'empty', simply_supported_ends_empty[0], simply_supported_ends_empty[1], simply_supported_ends_empty[2]]

            simply_supported_ends_flooded = []
            for mode_number in list(range(1, 4)):
                frequency = (1 / (2 * math.pi)) * (mode_number*math.pi/span_length)**2 * ((E * I) / (mass_with_internal_fluid/g_inch_per_second_squared)) ** 0.5
                simply_supported_ends_flooded.append(frequency)
            natural_frequencies_df.loc[len(natural_frequencies_df)] = [span_length/12, 'simply_supported', 'flooded', simply_supported_ends_flooded[0], simply_supported_ends_flooded[1], simply_supported_ends_flooded[2]]

            eigen_values_clamped = cfg['modes']['eigen_values']['clamped']
            clamped_clamped_empty = []
            for mode_number in list(range(1, 4)):
                frequency = (1 / (2 * math.pi)) * (eigen_values_clamped[mode_number-1]/span_length)**2 * ((E * I) / (mass_without_internal_fluid/g_inch_per_second_squared)) ** 0.5
                clamped_clamped_empty.append(frequency)
            natural_frequencies_df.loc[len(natural_frequencies_df)] = [span_length/12, 'clamped_clamped', 'empty', clamped_clamped_empty[0], clamped_clamped_empty[1], clamped_clamped_empty[2]]

            clamped_clamped_flooded = []
            for mode_number in list(range(1, 4)):
                frequency = (1 / (2 * math.pi)) * (eigen_values_clamped[mode_number-1]/span_length)**2 * ((E * I) / (mass_with_internal_fluid/g_inch_per_second_squared)) ** 0.5
                clamped_clamped_flooded.append(frequency)
            natural_frequencies_df.loc[len(natural_frequencies_df)] = [span_length/12, 'clamped_clamped', 'flooded', clamped_clamped_flooded[0], clamped_clamped_flooded[1], clamped_clamped_flooded[2]]

            free_free_empty = []
            for mode_number in list(range(1, 4)):
                frequency = (1 / (2 * math.pi)) * (22.373/span_length**2) * ((E * I) / (mass_without_internal_fluid/g_inch_per_second_squared)) ** 0.5
                free_free_empty.append(frequency)
            natural_frequencies_df.loc[len(natural_frequencies_df)] = [span_length/12, 'free_free', 'empty', free_free_empty[0], free_free_empty[1], free_free_empty[2]]

            free_free_flooded = []
            for mode_number in list(range(1, 4)):
                frequency = (1 / (2 * math.pi)) * (22.373/span_length**2) * ((E * I) / (mass_with_internal_fluid/g_inch_per_second_squared)) ** 0.5
                free_free_flooded.append(frequency)
            natural_frequencies_df.loc[len(natural_frequencies_df)] = [span_length/12, 'free_free', 'flooded', free_free_flooded[0], free_free_flooded[1], free_free_flooded[2]]

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_natural_frequencies.csv')
        natural_frequencies_df.to_csv(filename)

        return natural_frequencies_df

    def get_vs_frequencies(self, cfg):
        st = cfg['viv']['st']
        external_fluid_cfg = cfg['pipeline']['crossection'][0]['external_fluid']
        kinematic_viscosity = external_fluid_cfg['kinematic_viscosity'] * (1/0.0254)**2
        OD = cfg['pipeline']['crossection'][0]['Nominal_OD']
        current_cfg = cfg['environment']['current']

        viv_frequencies_df = pd.DataFrame(columns = ['current_label', 'depth', 'current_speed', 'Re', 'shredding_frequency', 'KC'])
        for current_item in current_cfg:
            current_label = current_item['label']
            for depth_item in current_item['data']:
                current_depth = depth_item['depth']
                current_speed = depth_item['speed'] /0.0254
                Re = (current_speed * OD) / kinematic_viscosity
                shredding_frequency = st * current_speed/ OD
                KC = current_speed /shredding_frequency / OD

                viv_frequencies_df.loc[len(viv_frequencies_df)] = [current_label, current_depth, current_speed, Re, shredding_frequency, KC]

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_vs_frequencies.csv')
        viv_frequencies_df.to_csv(filename)

        return viv_frequencies_df
    
    def get_safety_factors(self, cfg, natural_frequencies, vs_frequencies):
        safety_factor_df = pd.DataFrame(columns = ['span_length', 'current_label', 'depth', 'current_speed',  'Re', 'shredding_frequency', 'KC', 'boundary_condition', 'internal_fluid', 'mode 1', 'safety_factor'])
        
        for vs_index, vs_row in vs_frequencies.iterrows():
            current_label = vs_row['current_label']
            depth = vs_row['depth']
            current_speed = vs_row['current_speed']
            Re = vs_row['Re']
            shredding_frequency = vs_row['shredding_frequency']
            KC = vs_row['KC']

            for nf_index, nf_row in natural_frequencies.iterrows():
                span_length = nf_row['span_length']
                boundary_condition = nf_row['boundary_condition']
                internal_fluid = nf_row['internal_fluid']
                mode_1 = nf_row['mode 1']
                safety_factor = mode_1 / shredding_frequency

                safety_factor_df.loc[len(safety_factor_df)] = [span_length, current_label, depth, current_speed, Re, shredding_frequency, KC, boundary_condition, internal_fluid, mode_1, safety_factor]

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_safety_factors.csv')
        safety_factor_df.to_csv(filename)
