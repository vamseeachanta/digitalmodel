# Standard library imports
import math
import os

# Third party imports
import pandas as pd
from assetutilities.common.visualization.visualization_templates import (
    VisualizationTemplates,
)
from assetutilities.engine import engine as au_engine

viz_templates = VisualizationTemplates()
g_inch_per_second_squared = 386.4

class VIVTubularMembers:
    def __init__(self):
        pass
    
    def analyze(self, cfg):

        natural_frequencies_df = self.get_natural_frequencies(cfg)
        vs_frequencies_df = self.get_vs_frequencies(cfg)
        safety_factors_df = self.get_safety_factors(cfg, natural_frequencies_df, vs_frequencies_df)
        

        self.save_results(cfg, natural_frequencies_df, vs_frequencies_df, safety_factors_df)

        return cfg

    def save_results(self, cfg, natural_frequencies_df, vs_frequencies_df, safety_factors_df):

        cfg['viv']['natural_frequencies'] = natural_frequencies_df.to_json()
        cfg['viv']['vs_frequencies'] = vs_frequencies_df.to_json()
        cfg['viv']['safety_factors'] = safety_factors_df.to_json()

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_natural_frequencies.csv')
        natural_frequencies_df.to_csv(filename, index=False)

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_vs_frequencies.csv')
        vs_frequencies_df.to_csv(filename, index=False)

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_safety_factors.csv')
        safety_factors_df.to_csv(filename, index=False)

        self.save_plots(cfg)


    def save_plots(self, cfg):
        self.get_natural_frequencies_plot(cfg)
        self.get_vs_frequencies_plot(cfg)
        self.get_safety_factor_plot(cfg)

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
            if cfg['modes']['analysis']['simply_supported'] == True:
                self.get_simply_supported_modes(E, I, mass_without_internal_fluid, mass_with_internal_fluid, natural_frequencies_df, span_length)

            if cfg['modes']['analysis']['clamped_clamped'] == True:
                self.get_clamped_modes(cfg, E, I, mass_without_internal_fluid, mass_with_internal_fluid, natural_frequencies_df, span_length)

            if cfg['modes']['analysis']['free_free'] == True:
                self.get_free_free_modes(E, I, mass_without_internal_fluid, mass_with_internal_fluid, natural_frequencies_df, span_length)

        return natural_frequencies_df

    def get_free_free_modes(self, E, I, mass_without_internal_fluid, mass_with_internal_fluid, natural_frequencies_df, span_length):
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

    def get_clamped_modes(self, cfg, E, I, mass_without_internal_fluid, mass_with_internal_fluid, natural_frequencies_df, span_length):
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

    def get_simply_supported_modes(self, E, I, mass_without_internal_fluid, mass_with_internal_fluid, natural_frequencies_df, span_length):
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

    def get_vs_frequencies(self, cfg):
        st = cfg['viv']['st']
        external_fluid_cfg = cfg['pipeline']['crossection'][0]['external_fluid']
        kinematic_viscosity = external_fluid_cfg['kinematic_viscosity'] * (1/0.0254)**2
        OD = cfg['pipeline']['crossection'][0]['Nominal_OD']
        current_cfg = cfg['environment']['current']

        viv_frequencies_df = pd.DataFrame(columns = ['current_label', 'depth', 'current_speed', 'Re', 'shredding_frequency_inline', 'KC', 'shredding_frequency_crossflow'])
        for current_item in current_cfg:
            current_label = current_item['label']
            for depth_item in current_item['data']:
                current_depth = depth_item['depth']
                current_speed = depth_item['speed']
                current_speed_in_inch_per_s = current_speed /0.0254
                Re = (current_speed * OD) / kinematic_viscosity
                shredding_frequency_inline = st * current_speed/ OD
                KC = current_speed_in_inch_per_s /shredding_frequency_inline / OD
                shredding_frequency_crossflow = shredding_frequency_inline/2

                viv_frequencies_df.loc[len(viv_frequencies_df)] = [current_label, current_depth, current_speed, Re, shredding_frequency_inline, KC, shredding_frequency_crossflow]

        return viv_frequencies_df
    
    def get_safety_factors(self, cfg, natural_frequencies, vs_frequencies):
        safety_factor_df = pd.DataFrame(columns = ['span_length', 'current_label', 'depth', 'current_speed',  'Re', 'shredding_frequency_inline', 'KC', 'boundary_condition', 'internal_fluid', 'mode 1', 'safety_factor_inline', 'safety_factor_crossflow'])   
        
        for vs_index, vs_row in vs_frequencies.iterrows():
            current_label = vs_row['current_label']
            depth = vs_row['depth']
            current_speed = vs_row['current_speed']
            Re = vs_row['Re']
            shredding_frequency_inline = vs_row['shredding_frequency_inline']
            shredding_frequency_crossflow = vs_row['shredding_frequency_crossflow']
            KC = vs_row['KC']

            for nf_index, nf_row in natural_frequencies.iterrows():
                span_length = nf_row['span_length']
                boundary_condition = nf_row['boundary_condition']
                internal_fluid = nf_row['internal_fluid']
                mode_1 = nf_row['mode 1']
                safety_factor_inline = mode_1 / shredding_frequency_inline
                safety_factor_crossflow = mode_1 / shredding_frequency_crossflow
                
                safety_factor_df.loc[len(safety_factor_df)] = [span_length, current_label, depth, current_speed, Re, shredding_frequency_inline, KC, boundary_condition, internal_fluid, mode_1, safety_factor_inline, safety_factor_crossflow] 

        
        return safety_factor_df

    def get_natural_frequencies_plot(self, cfg):
        plot_yml = viz_templates.get_xy_line_input(cfg['Analysis'].copy())

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_natural_frequencies.csv')
        df = pd.read_csv(filename)

        boundary_condition_array = df['boundary_condition'].unique()
        internal_fluid_array = df['internal_fluid'].unique()
        modes_array = ['mode 1']
        
        groups = []
        for boundary_condition in boundary_condition_array:
            for internal_fluid in internal_fluid_array:
                for modes in modes_array:
                    group_item = {}
                    df_filtered = df[(df['boundary_condition'] == boundary_condition) & (df['internal_fluid'] == internal_fluid)]
                    x_values = list(df_filtered['span_length'].values)
                    y_values = list(df_filtered[modes].values)
                    group_item['x'] = [x_values]
                    group_item['y'] = [y_values]
                    group_item['label'] = boundary_condition + ', ' + internal_fluid + ', ' + modes
                    groups.append(group_item)

        plot_yml['data']['groups'] = groups

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_natural_frequencies_' + modes, 
                    'title': 'Natural Frequency of Mode 1',
                    'xlabel': 'Span Length (ft)',
                    'ylabel': 'Natural Frequency (Hz)',
}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def get_vs_frequencies_plot(self, cfg):
        plot_yml = viz_templates.get_xy_scatter_input(cfg['Analysis'].copy())

        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_vs_frequencies.csv')
        df = pd.read_csv(filename)

        current_label_array = df['current_label'].unique()
        groups = []
        for current_label in current_label_array:
            group_item = {}
            df_filtered = df[(df['current_label'] == current_label)]
            x_values = list(df_filtered['current_speed'].values)
            y_values = list(df_filtered['shredding_frequency_inline'].values)
            group_item['x'] = [x_values]
            group_item['y'] = [y_values]
            group_item['label'] = current_label + ', Inline'
            groups.append(group_item)

            group_item = {}
            x_values = list(df_filtered['current_speed'].values)
            y_values = list(df_filtered['shredding_frequency_crossflow'].values)
            group_item['x'] = [x_values]
            group_item['y'] = [y_values]
            group_item['label'] = current_label + ', Crossflow'
            groups.append(group_item)

        plot_yml['data']['groups'] = groups

        settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + '_vs_frequencies',
                    'title': 'VIV Frequency',
                    'xlabel': 'Current Speed (m/s)',
                    'ylabel': 'VIV Frequency (Hz)',
}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def get_safety_factor_plot(self, cfg):
        filename = os.path.join(cfg['Analysis']['result_folder'], cfg['Analysis']['file_name'] + '_safety_factors.csv')
        df = pd.read_csv(filename)

        current_label_array = df['current_label'].unique()
        internal_fluid_array = df['internal_fluid'].unique()
        boundary_condition_array = df['boundary_condition'].unique()
        depth_array = df['depth'].unique()
        depth_array = [0, 100]

        for depth in depth_array:
            for current_label in current_label_array:
                plot_yml = viz_templates.get_xy_line_input(cfg['Analysis'].copy())
                groups = []
                for internal_fluid in internal_fluid_array:
                    for boundary_condition in boundary_condition_array:
                        df_filtered = df[(df['current_label'] == current_label) & (df['internal_fluid'] == internal_fluid) & (df['boundary_condition'] == boundary_condition) & (df['depth'] == depth)]
                        group_item = {}
                        x_values = list(df_filtered['span_length'].values)
                        y_values = list(df_filtered['safety_factor_inline'].values)
                        group_item['x'] = [x_values]
                        group_item['y'] = [y_values]
                        group_item['label'] = current_label + ", " + boundary_condition + ", " + internal_fluid + ', Inline'
                        groups.append(group_item.copy())

                        group_item = {}
                        x_values = list(df_filtered['span_length'].values)
                        y_values = list(df_filtered['safety_factor_crossflow'].values)
                        group_item['x'] = [x_values]
                        group_item['y'] = [y_values]
                        group_item['label'] = current_label + ", " + boundary_condition + ", " + internal_fluid + ', Crossflow'
                        groups.append(group_item.copy())

                plot_yml['data']['groups'] = groups

                settings = {'file_name':  cfg['Analysis']['file_name_for_overwrite'] + f'_safety_factors_curr_{current_label}_dp{str(depth)}',
                            'title': f'Safety Factor from VIV Frequency, Depth: {depth} %',
                            'xlabel': 'Span Length (ft)',
                            'ylabel': 'Safety Factor',
        }
                plot_yml['settings'].update(settings)
                au_engine(inputfile=None, cfg=plot_yml, config_flag=False)
