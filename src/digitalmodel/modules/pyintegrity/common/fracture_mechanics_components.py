import logging

from pyintegrity.common.data import AttributeDict, ReadData
from pyintegrity.common.fad import FAD
from pyintegrity.common.BS7910_critical_flaw_limits import BS7910_2013
from pyintegrity.custom.PipeSizing import PipeSizing
from pyintegrity.common.visualizations import Visualization
from pyintegrity.common.yml_utilities import WorkingWithYAML

ww_yaml = WorkingWithYAML()


class FractureMechanicsComponents():

    def __init__(self, cfg):
        self.cfg = cfg
        self.init_add_pipe_properties_to_cfg()

    def evaluate_FAD(self):
        print("Fatigue Assessment Diagram (FAD) analysis ....")
        fatigue_assessment_diagram = FAD(self.cfg)
        self.fad = fatigue_assessment_diagram.get_BS7910_2013_FAD()
        print("Fatigue Assessment Diagram (FAD) analysis .... COMPLETED")

    def critical_flaw_limits(self):
        self.bs7910_2013_critical_flaw = BS7910_2013(self.cfg, self.fad)
        self.bs7910_2013_critical_flaw.get_critical_allowable_flaw()

    def get_flaw_growth_for_fatigue_loading(self):
        self.bs7910_2013_flaw_growth = BS7910_2013(self.cfg, self.fad)
        self.get_histogram_loading()
        # TODO
        # Add external_surface
        # Add both objects to 2 DFs
        # Save them in all 4 single objects i.e. critical & initial in single .xlsx
        bending_factor = self.cfg['loading']['histograms']['from_xlsx'][0][
            'bending_factor']
        self.bs7910_2013_flaw_growth.get_approximate_flaw_growth_due_to_histograms(
            self.histograms_df, bending_factor)
        self.bs7910_2013_flaw_growth.get_flaw_growth_due_to_histograms(
            self.histograms_df, bending_factor)

    def get_initial_allowable_flaw_for_life(self):
        self.bs7910_2013_flaw_growth.get_initial_allowable_flaw_for_life(
            self.histograms_df)

    def get_histogram_loading(self):
        read_data = ReadData()
        cfg_temp = AttributeDict()
        cfg_temp['files'] = self.cfg.loading['histograms']
        cfg_temp = self.update_cfg_with_library_file(cfg_temp=cfg_temp)
        self.histograms_df = read_data.from_xlsx(cfg_temp)

    def update_cfg_with_library_file(self, cfg_temp):
        for i in range(0, len(cfg_temp['files']['from_xlsx'])):
            cfg_temp['files']['from_xlsx'][i]['io']
            ww_yaml_cfg = {'filename': cfg_temp['files']['from_xlsx'][i]['io']}
            cfg_temp['files']['from_xlsx'][i][
                'io'] = ww_yaml.get_library_filename(ww_yaml_cfg)

        return cfg_temp

    def save_result_tables(self):
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_fad.csv'
        # Fatigue Assessment Diagram
        self.fad['option_1'].to_csv(file_name)

        # Unstable Fracture Limits
        for location_item in self.cfg.default['settings']['location_array']:
            file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
                'Analysis']['file_name'] + '_unstable_fracture_limits.csv'
            self.bs7910_2013_critical_flaw.result[location_item][
                'fracture_limit'].to_csv(file_name)

        # Approximate minimum flaw solutions for varying starting flaw
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_approximate_min_initial_flaw.csv'
        self.bs7910_2013_flaw_growth.approximate_flaw_growth_df.to_csv(
            file_name)

        # Time Marching minimum flaw solution
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_flaw_growth.csv'
        self.bs7910_2013_flaw_growth.flaw_growth_df.to_csv(file_name)

        # Time Marching minimum flaw solution
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name'] + '_time_marching_min_initial_flaw.csv'
        self.bs7910_2013_flaw_growth.flaw_growth_df.to_csv(file_name)

    def create_visualizations(self):
        self.create_viz_fatigue_assessment_diagrams()
        self.create_viz_unstable_fracture_limits()
        self.create_viz_minimum_allowable_flaws()
        self.create_viz_flaw_growth_rates()

    def create_viz_fatigue_assessment_diagrams(self):
        viz_data = Visualization()
        plt_settings = self.cfg['plot_settings'][0]
        plt_settings.update({
            'file_name':
                self.cfg['Analysis']['result_folder'] +
                self.cfg['Analysis']['file_name'] + '_' +
                plt_settings['file_name_extension'] + '.png'
        })
        viz_data.from_df_columns(self.fad['option_1'], plt_settings)
        viz_data.add_title_and_axis_labels(plt_settings)
        viz_data.add_x_y_lim_formats()
        viz_data.add_text_fields()
        viz_data.save_and_close()

    def create_viz_unstable_fracture_limits(self):
        viz_data = Visualization()
        plt_settings = self.cfg['plot_settings'][1]
        plt_settings.update({
            'file_name':
                self.cfg['Analysis']['result_folder'] +
                self.cfg['Analysis']['file_name'] + '_' +
                plt_settings['file_name_extension'] + '.png'
        })
        for location_item in self.cfg.default['settings']['location_array']:
            for orientation_item in self.cfg.default['settings'][
                    'orientation_array']:
                plt_settings['label'] = [
                    location_item + ' flaw, ' + orientation_item
                ]
                temp_df = self.bs7910_2013_critical_flaw.result[location_item][
                    'fracture_limit']
                viz_data.from_df_columns(
                    temp_df[temp_df.flaw_orientation == orientation_item],
                    plt_settings)
        viz_data.add_title_and_axis_labels(plt_settings=plt_settings)
        viz_data.add_legend()
        viz_data.add_x_y_lim_formats()
        viz_data.save_and_close()

    def create_viz_minimum_allowable_flaws(self):
        self.create_viz_min_init_flaw_by_component()
        self.create_viz_min_init_flaw_by_service_life()
        self.create_viz_min_init_flaw_by_flaw_location()
        self.create_viz_min_init_flaw_by_flaw_orientation()

    def create_viz_flaw_growth_rates(self):
        self.create_viz_flaw_growth_by_component()
        self.create_viz_flaw_growth_by_service_life()
        self.create_viz_flaw_growth_by_flaw_location()
        self.create_viz_flaw_growth_by_flaw_orientation()

    def create_viz_min_init_flaw_by_component(self):
        viz = Visualization()
        min_init_df = self.bs7910_2013_flaw_growth.initial_allowable_flaw_df
        for cfg_component_index in range(
                0,
                len(self.cfg.loading['histograms']['from_xlsx'][0]
                    ['component_index'])):
            component_index = self.cfg.loading['histograms']['from_xlsx'][0][
                'component_index'][cfg_component_index]
            component_name = self.histograms_df.columns[component_index]
            component_label = self.cfg.loading['histograms']['from_xlsx'][0][
                'component_label'][cfg_component_index]
            plt_settings = self.cfg['plot_settings'][2].copy()
            plt_settings['title'] = plt_settings['title'].format(
                component_label)
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_name_extension'] + '_component_' +
                    str(component_index) + '.png'
            })

            for location_item in self.cfg.default['settings']['location_array']:
                for service_life in self.cfg.default['settings'][
                        'service_life']['values']:
                    for orientation_item in self.cfg.default['settings'][
                            'orientation_array']:
                        df_temp = min_init_df[
                            (min_init_df.component == component_name) &
                            (min_init_df.flaw_location == location_item) &
                            (min_init_df.service_life == service_life) &
                            (min_init_df.flaw_orientation == orientation_item)]
                        if len(df_temp) > 0:
                            df_temp = self.make_initial_flaw_monotonous(
                                df_temp.copy())
                            plt_settings['label'] = [
                                location_item.replace('_', ' ') + ', ' +
                                orientation_item + ' orientation' + ', ' +
                                str(service_life) + ' years'
                            ]
                            viz.from_df_columns(df_temp, plt_settings)

            if hasattr(viz, 'plt_settings'):
                viz.add_title_and_axis_labels(plt_settings=plt_settings)
                viz.add_legend()
                viz.add_x_y_lim_formats()
                viz.save_and_close()

    def create_viz_min_init_flaw_by_service_life(self):
        viz = Visualization()
        min_init_df = self.bs7910_2013_flaw_growth.initial_allowable_flaw_df

        for service_life in self.cfg.default['settings']['service_life'][
                'values']:
            plt_settings = self.cfg['plot_settings'][2].copy()
            plt_settings['title'] = plt_settings['title'].format(
                str(service_life) + ' years')
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_name_extension'] + '_' +
                    str(service_life) + '_years' + '.png'
            })
            for cfg_component_index in range(
                    0,
                    len(self.cfg.loading['histograms']['from_xlsx'][0]
                        ['component_index'])):
                component_index = self.cfg.loading['histograms']['from_xlsx'][
                    0]['component_index'][cfg_component_index]
                component_name = self.histograms_df.columns[component_index]
                component_label = self.cfg.loading['histograms']['from_xlsx'][
                    0]['component_label'][cfg_component_index]

                for location_item in self.cfg.default['settings'][
                        'location_array']:
                    for orientation_item in self.cfg.default['settings'][
                            'orientation_array']:
                        df_temp = min_init_df[
                            (min_init_df.component == component_name) &
                            (min_init_df.flaw_location == location_item) &
                            (min_init_df.service_life == service_life) &
                            (min_init_df.flaw_orientation == orientation_item)]
                        if len(df_temp) > 0:
                            df_temp = self.make_initial_flaw_monotonous(
                                df_temp.copy())
                            plt_settings['label'] = [
                                component_label + ', ' +
                                location_item.replace('_', ' ') + ', ' +
                                orientation_item + ' orientation'
                            ]
                            viz.from_df_columns(df_temp, plt_settings)

            if hasattr(viz, 'plt_settings'):
                viz.add_title_and_axis_labels(plt_settings=plt_settings)
                viz.add_legend()
                viz.add_x_y_lim_formats()
                viz.save_and_close()

    def create_viz_min_init_flaw_by_flaw_location(self):
        viz = Visualization()
        min_init_df = self.bs7910_2013_flaw_growth.initial_allowable_flaw_df
        for location_item in self.cfg.default['settings']['location_array']:
            plt_settings = self.cfg['plot_settings'][2].copy()
            plt_settings['title'] = plt_settings['title'].format(
                location_item.replace('_', ' ') + ' flaw')
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_name_extension'] + '_' + location_item +
                    '.png'
            })
            for cfg_component_index in range(
                    0,
                    len(self.cfg.loading['histograms']['from_xlsx'][0]
                        ['component_index'])):
                component_index = self.cfg.loading['histograms']['from_xlsx'][
                    0]['component_index'][cfg_component_index]
                component_name = self.histograms_df.columns[component_index]
                component_label = self.cfg.loading['histograms']['from_xlsx'][
                    0]['component_label'][cfg_component_index]

                for service_life in self.cfg.default['settings'][
                        'service_life']['values']:
                    for orientation_item in self.cfg.default['settings'][
                            'orientation_array']:
                        df_temp = min_init_df[
                            (min_init_df.component == component_name) &
                            (min_init_df.flaw_location == location_item) &
                            (min_init_df.service_life == service_life) &
                            (min_init_df.flaw_orientation == orientation_item)]
                        if len(df_temp) > 0:
                            df_temp = self.make_initial_flaw_monotonous(
                                df_temp.copy())
                            plt_settings['label'] = [
                                component_label + ', ' + orientation_item +
                                ' orientation' + ', ' + str(service_life) +
                                ' years'
                            ]
                            viz.from_df_columns(df_temp, plt_settings)

            if hasattr(viz, 'plt_settings'):
                viz.add_title_and_axis_labels(plt_settings)
                viz.add_legend()
                viz.add_x_y_lim_formats()
                viz.save_and_close()

    def create_viz_min_init_flaw_by_flaw_orientation(self):
        viz = Visualization()
        min_init_df = self.bs7910_2013_flaw_growth.initial_allowable_flaw_df

        for orientation_item in self.cfg.default['settings'][
                'orientation_array']:
            plt_settings = self.cfg['plot_settings'][2].copy()
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_name_extension'] + '_' +
                    orientation_item + '.png'
            })
            plt_settings['title'] = plt_settings['title'].format(
                orientation_item + ' Orientation')

            for cfg_component_index in range(
                    0,
                    len(self.cfg.loading['histograms']['from_xlsx'][0]
                        ['component_index'])):
                component_index = self.cfg.loading['histograms']['from_xlsx'][
                    0]['component_index'][cfg_component_index]
                component_name = self.histograms_df.columns[component_index]
                component_label = self.cfg.loading['histograms']['from_xlsx'][
                    0]['component_label'][cfg_component_index]

                for service_life in self.cfg.default['settings'][
                        'service_life']['values']:
                    for location_item in self.cfg.default['settings'][
                            'location_array']:
                        df_temp = min_init_df[
                            (min_init_df.component == component_name) &
                            (min_init_df.flaw_location == location_item) &
                            (min_init_df.service_life == service_life) &
                            (min_init_df.flaw_orientation == orientation_item)]
                        if len(df_temp) > 0:
                            df_temp = self.make_initial_flaw_monotonous(
                                df_temp.copy())
                            plt_settings['label'] = [
                                component_label + ', ' +
                                location_item.replace('_', ' ') + ', ' +
                                str(service_life) + ' years'
                            ]
                            viz.from_df_columns(df_temp, plt_settings)

            if hasattr(viz, 'plt_settings'):
                viz.add_title_and_axis_labels(plt_settings)
                viz.add_legend()
                viz.add_x_y_lim_formats()
                viz.save_and_close()

    def create_viz_flaw_growth_by_component(self):
        viz = Visualization()
        growth_df = self.bs7910_2013_flaw_growth.flaw_growth_df
        for cfg_component_index in range(
                0,
                len(self.cfg.loading['histograms']['from_xlsx'][0]
                    ['component_index'])):
            component_index = self.cfg.loading['histograms']['from_xlsx'][0][
                'component_index'][cfg_component_index]
            component_name = self.histograms_df.columns[component_index]
            component_label = self.cfg.loading['histograms']['from_xlsx'][0][
                'component_label'][cfg_component_index]

            for location_item in self.cfg.default['settings']['location_array']:
                for orientation_item in self.cfg.default['settings'][
                        'orientation_array']:
                    plt_settings = self.cfg['plot_settings'][3].copy()
                    plt_settings['title'] = plt_settings['title'].format(
                        component_label, location_item.replace('_', ' '),
                        orientation_item + ' orientation')
                    plt_settings.update({
                        'file_name':
                            self.cfg['Analysis']['result_folder'] +
                            self.cfg['Analysis']['file_name'] + '_' +
                            plt_settings['file_name_extension'] +
                            '_component_' + str(component_index) + '_' +
                            location_item + '_' + orientation_item + '.png'
                    })

                    df_temp = growth_df[
                        (growth_df.component == component_name) &
                        (growth_df.flaw_location == location_item) &
                        (growth_df.flaw_orientation == orientation_item)]
                    c_array = df_temp.final_flaw_length.unique()
                    for flaw_index in range(0, len(c_array)):
                        if (flaw_index % 2 == 0):
                            plt_settings['label'] = [
                                'flaw, start len = {} mm'.format(
                                    c_array[flaw_index])
                            ]
                            viz.from_df_columns(
                                df_temp[df_temp.final_flaw_length ==
                                        c_array[flaw_index]], plt_settings)

                    viz.add_title_and_axis_labels(plt_settings)
                    viz.add_legend()
                    viz.add_x_y_lim_formats()
                    viz.save_and_close()

    def create_viz_flaw_growth_by_service_life(self):
        pass

    def create_viz_flaw_growth_by_flaw_location(self):
        pass

    def create_viz_flaw_growth_by_flaw_orientation(self):
        pass

    def init_add_pipe_properties_to_cfg(self):

        pipe_sizing = PipeSizing(self.cfg)
        property_dictionary = pipe_sizing.get_pipe_system_properties()
        self.cfg.pipe_properties = property_dictionary['section_properties'][
            'pipe']

    def make_initial_flaw_monotonous(self, df, variable='final_flaw_length'):
        df.sort_values(by=[variable], ascending=True, inplace=True)
        df.reset_index(inplace=True, drop=True)
        for row_index in range(0, len(df) - 1):
            if df.final_flaw_depth[row_index +
                                   1] > df.final_flaw_depth[row_index]:
                df.ix[row_index + 1,
                      'final_flaw_depth'] = df.final_flaw_depth[row_index]
        return df
