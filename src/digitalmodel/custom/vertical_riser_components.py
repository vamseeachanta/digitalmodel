import logging
import math

from assetutilities.common.data import ReadData

from digitalmodel.custom.PipeSizing import PipeSizing
from digitalmodel.common.typical_riser_stack_up_calculations import \
    TypicalRiserStackUpCalculations
from digitalmodel.common.orcaflex_model_components import OrcaflexModelComponents
from digitalmodel.common.shear7_model_components import Shear7ModelComponents
from digitalmodel.common.visualizations import Visualization


class VerticalRiser():

    def __init__(self, cfg):
        self.cfg = cfg
        self.set_up_pipe_properties()

    def set_up_pipe_properties(self):
        self.pipe_properties = []

    def prepare_riser_model(self):
        self.read_riser_input_data()
        self.assign_key_properties_to_self()
        self.prepare_flexible_joint_data()
        self.prepare_tensioner_data()
        self.prepare_geotechnical_data()

        self.delete_unwanted_joint_rows()
        self.calculate_auxiliary_line_properties()
        self.calculate_joint_properties()
        self.evaluate_stack_up_properties()
        print("Successful riser data preparation.")

    def read_riser_input_data(self):
        read_data = ReadData()
        custom_cfg = {'files': self.cfg['stack_up']}
        for riser_data_index in range(0, len(custom_cfg['files']['from_xlsx'])):
            attribute_name = custom_cfg['files']['from_xlsx'][riser_data_index][
                'label']
            setattr(self, attribute_name,
                    read_data.from_xlsx(custom_cfg, riser_data_index))

    def assign_key_properties_to_self(self):
        self.host_id = self.cfg['default']['analysis']['host']['id']
        self.host_name = self.hosts[self.hosts['host_id'] ==
                                    self.host_id]['host_name'].values[0]
        self.host_construction_type = self.hosts[
            self.hosts['host_id'] ==
            self.host_id]['construction_type'].values[0]
        self.well_id = self.cfg['default']['analysis']['well']['id']
        self.riser_shape = self.cfg['default']['analysis']['riser']['shape']
        if self.cfg['default']['analysis']['riser']['type']['drilling']:
            self.riser_type = 'drilling'
        elif self.cfg['default']['analysis']['riser']['type']['intervention']:
            self.riser_type = 'intervention'
        elif self.cfg['default']['analysis']['riser']['type']['top_tensioned']:
            self.riser_type = 'top_tensioned'
        if (self.stack_up_table[self.stack_up_table['Component'] ==
                                'Support Spring']['No. Of. Joints'].values[0] ==
                1):
            self.support_spring = True
        else:
            self.support_spring = False

        self.initial_down_stroke = self.cfg['stack_up']['stroke_setting'][
            'initial_down_stroke']
        self.model_stroke_in_and_out_limits = self.cfg['stack_up'][
            'stroke_setting']['model_stroke_in_and_out_limits']

        self.fea_type = 'Fatigue'
        self.water_depth = \
        self.stack_up_table[self.stack_up_table['Component'] == 'MSL']['Component Top Elevation above Mudline'].values[
            0] * 0.3048
        self.loading_index = 0

        if self.fea_type == 'Fatigue':
            self.geotechnical_application_type = 'cyclic'
        elif self.fea_type == 'Extreme':
            self.geotechnical_application_type = 'static'
        elif self.fea_type == 'Axial':
            self.geotechnical_application_type = 'axial'
        self.py_model_name = 'p-y_' + self.geotechnical_application_type

        if self.riser_shape == 'catenary':
            if self.cfg['default']['Analysis']['SLWR']:
                self.riser_name = 'SLWR'
            if self.cfg['default']['Analysis']['SCR']:
                self.riser_name = 'SCR'
        elif self.riser_shape == 'vertical':
            self.riser_name = 'vertical'

    def calculate_joint_properties(self):
        for row_index in range(0, len(self.riser_joints)):
            self.get_fea_properties(row_index)

    def delete_unwanted_joint_rows(self):
        self.riser_joints.columns = self.riser_joints.iloc[0]
        self.riser_joints.drop([0, 1], inplace=True)
        self.riser_joints = self.riser_joints[
            (self.riser_joints['host_id'] == self.host_id) |
            (self.riser_joints['well_id'] == self.well_id)]
        self.riser_joints.reset_index(inplace=True)

    def get_fea_properties(self, row_index, weight_method=None):

        pipe_data = \
            {
                     'Outer_Pipe':
                         {'Geometry': {'Nominal_OD': self.riser_joints.iloc[row_index]['Pipe OD'], 'Nominal_ID': None, 'Design_WT': self.riser_joints.iloc[row_index]['Pipe WT'], 'Corrosion_Allowance': 0.0},
                          'Material': {'Material' : 'Steel', 'Material_Grade': 'API 5L X80', 'Insulation': None, 'Buoyancy': None}},
                     'Inner_Pipe': None,
                     'Material': self.cfg['Material']
            }
        pipe_sizing = PipeSizing(pipe_data)
        property_dictionary = pipe_sizing.get_pipe_system_properties()

        property_dictionary['section_properties'][
            'pipe'] = self.update_property_dictionary_to_si_units(
                property_dictionary['section_properties']['pipe'])

        cd_array = [
            self.riser_joints.iloc[row_index]['Lateral Drag Coefficient, ' +
                                              self.fea_type + ', Cd'], '~',
            self.riser_joints.iloc[row_index]['Axial Drag Coefficient, ' +
                                              self.fea_type + ', Cd']
        ]
        ca_array = [
            self.riser_joints.iloc[row_index]['Lateral Added mass, Ca'], '~',
            self.riser_joints.iloc[row_index]['Axial Added mass, Ca']
        ]
        pipe_id = (self.riser_joints.iloc[row_index]['Pipe OD'] -
                   2 * self.riser_joints.iloc[row_index]['Pipe WT']) * 0.0254

        if self.riser_joints.iloc[row_index]['Weight Method'] == 'measured':
            logging.info("Calculating based on measured properties")
            dry_weight_per_unit_length = self.riser_joints.iloc[row_index][
                'Dry wt.'] * 0.4536 * 1000 / (
                    self.riser_joints.iloc[row_index]['Length'] * 0.3048)
            wet_weight_per_unit_length = self.riser_joints.iloc[row_index][
                'Wet wt.'] * 0.4536 * 1000 / (
                    self.riser_joints.iloc[row_index]['Length'] * 0.3048)
            buoyancy_diameter = math.sqrt(
                (dry_weight_per_unit_length - wet_weight_per_unit_length) * 4 /
                math.pi / self.cfg['Material']['SeaWater']['Rho'] + pipe_id**2)

            property_dictionary['section_properties']['pipe'].update(
                {'MassPerUnitLength': dry_weight_per_unit_length})
        elif self.riser_joints.iloc[row_index]['Weight Method'] == 'calculated':
            logging.info("Calculating properties based on wall thickness")
            buoyancy_diameter = self.riser_joints.iloc[row_index][
                'Pipe OD'] * 0.0254
            dry_weight_per_unit_length = property_dictionary[
                'section_properties']['pipe'][
                    'MassPerUnitLength'] * 0.4536 / 0.0254
            wet_weight_per_unit_length =  dry_weight_per_unit_length - \
                                          math.pi/4*self.cfg['Material']['SeaWater']['Rho']*buoyancy_diameter**2/1000*9.81
            property_dictionary['section_properties']['pipe'].update({
                'OD': self.riser_joints.iloc[row_index]['Pipe OD'],
                'MassPerUnitLength': dry_weight_per_unit_length,
                'StressOD': self.riser_joints.iloc[row_index]['Pipe OD'],
            })

        else:
            logging.info(
                "Properties can not evaluated for below jt values row as *Weight Method* not available"
            )
            logging.info(self.riser_joints.iloc[row_index])

        if math.isnan(self.riser_joints.iloc[row_index]['Drag Diameter']):
            drag_diameter = buoyancy_diameter
        else:
            drag_diameter = self.riser_joints.iloc[row_index][
                'Drag Diameter'] * 0.0254

        effective_ID_with_axiliary_lines = math.sqrt(
            4 / math.pi *
            (property_dictionary['section_properties']['pipe']['Ai'] +
             self.auxiliary_lines_properties['Ai']))
        property_dictionary['section_properties']['pipe'].update({
            'Name': self.riser_joints.iloc[row_index]['Component'],
            'OD': buoyancy_diameter,
            'ID': pipe_id,
            'Category': 'General',
            'Cd': cd_array,
            'Ca': ca_array,
            'StressOD': self.riser_joints.iloc[row_index]['Pipe OD'] * 0.0254,
            'StressID': pipe_id,
            'drag_diameter': drag_diameter,
            'buoyancy_diameter': buoyancy_diameter,
            'wet_weight_per_unit_length': wet_weight_per_unit_length,
            'internal_fluid_ID': pipe_id
        })

        for line_item_index in range(0, len(self.stack_up_table)):
            if self.stack_up_table.iloc[line_item_index][
                    'Aux Lines Present'] == 1:
                property_dictionary['section_properties']['pipe'].update(
                    {'internal_fluid_ID': effective_ID_with_axiliary_lines})

        # TODO Convert this into DF so things can be simple look_up
        self.pipe_properties.append(property_dictionary)

    def update_property_dictionary_to_si_units(self, quantities_imperial):
        # TODO implement KGs lamda concept for unit conversion
        quantities_si = quantities_imperial
        quantities_si['A'] = quantities_imperial['A'] * 0.0254**2
        quantities_si['Ai'] = quantities_imperial['Ai'] * 0.0254**2
        quantities_si['Ao'] = quantities_imperial['Ao'] * 0.0254**2
        quantities_si['I'] = quantities_imperial['I'] * 0.0254**4
        quantities_si['Io'] = quantities_imperial['Io'] * 0.0254**4
        quantities_si['Ii'] = quantities_imperial['Ii'] * 0.0254**4
        quantities_si['J'] = quantities_imperial['J'] * 0.0254**4
        quantities_si['Jo'] = quantities_imperial['Jo'] * 0.0254**4
        quantities_si['Ji'] = quantities_imperial['Ji'] * 0.0254**4
        quantities_si['EI'] = quantities_imperial['EI'] * 6.894745 * (0.0254**4)
        quantities_si['EA'] = quantities_imperial['EA'] * 6.894745 * (0.0254**2)
        quantities_si['GJ'] = quantities_imperial['GJ'] * 6.894745 * (0.0254**4)
        quantities_si['E'] = quantities_imperial['E']**6.894745
        quantities_si['SMYS'] = quantities_imperial['SMYS'] * 6.894745
        quantities_si['SMUS'] = quantities_imperial['SMUS'] * 6.894745
        quantities_si['SMUS'] = quantities_imperial['SMUS'] * 6.894745

        return quantities_si

    def prepare_flexible_joint_data(self):
        import logging
        self.project_flexible_joints = self.flexible_joints[
            (self.flexible_joints['host_id'] == self.host_id) &
            (self.flexible_joints['fea_type'] == self.fea_type)]

        logging.info(self.project_flexible_joints)

    def prepare_tensioner_data(self):
        import logging
        self.project_tensioners = self.tensioners[(
            self.tensioners['host_id'] == self.host_id)]

        logging.info(self.project_tensioners)

    def prepare_geotechnical_data(self):
        import logging
        self.project_geotechnical = self.geotechnical[
            (self.geotechnical['well_id'] == self.well_id) &
            (self.geotechnical['application_type'] ==
             self.geotechnical_application_type)]

        self.project_geotechnical = self.project_geotechnical.sort_values(
            by=['depth_below_seabed', 'y']).copy()
        logging.info(self.project_geotechnical)

    def prepare_report_tables(self):
        pass

    def evaluate_stack_up_properties(self):
        stack_up_calculations = TypicalRiserStackUpCalculations(self)
        stack_up_calculations.prepare_stack_up_properties_df()
        self.stack_up_properties_df = stack_up_calculations.stack_up_properties_df

    def calculate_auxiliary_line_properties(self):
        self.auxiliary_lines_properties = {'Ai': 0}
        auxiliary_lines = self.riser_joints[self.riser_joints['Joint Type'] ==
                                            'auxiliary_line']
        auxiliary_lines.reset_index(inplace=True)
        auxiliary_lines_internal_area = 0
        for joint_item_index in range(0, len(auxiliary_lines)):
            for properties_index in range(0, len(self.pipe_properties)):
                if auxiliary_lines.loc[
                        joint_item_index,
                        'Component'] == self.pipe_properties[properties_index][
                            'section_properties']['pipe']['Name']:
                    auxiliary_lines_internal_area = auxiliary_lines_internal_area + self.pipe_properties[
                        0]['section_properties']['pipe']['Ai']

        self.auxiliary_lines_properties['Ai'] = auxiliary_lines_internal_area

    def prepare_fea_model(self):
        print("Building FEA model ... ")
        self.fea_model_prep = OrcaflexModelComponents()
        self.fea_model_prep.orcaflex_model(self)

        print("Building FEA model... COMPLETE")

    def prepare_plots_and_save_data(self):
        self.prepare_plots()
        self.save_data()

    def build_fea_model(self, shear7_flag=False):
        self.fea_model_prep.build_model(shear7_flag)

    def prepare_shear7_model(self):
        print("Building Shear7 model ... ")
        self.shear7_model_comp = Shear7ModelComponents()
        self.shear7_model_comp.prep_shear7_model(self)
        self.shear7_model_comp.save_model()

        print("Building Shear7 model... COMPLETE")

    def save_data(self):
        file_name = ({
            'file_name':
                self.cfg['Analysis']['result_folder'] +
                self.cfg['Analysis']['file_name'] + '_' +
                '{0}'.format('stack_up_properties') + '.csv'
        })
        self.fea_model_prep.stack_up_properties_df.to_csv(
            file_name['file_name'])

    def prepare_plots(self):

        for plot_settings_index in range(0, len(self.cfg['plot_settings'])):
            plt_settings = self.cfg['plot_settings'][plot_settings_index].copy()
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '_' +
                    '{0}'.format(plt_settings['file_name']) + '.png'
            })
            viz = Visualization(plt_settings)
            viz.from_df_columns(self.fea_model_prep.stack_up_properties_df)
            viz.add_title_and_axis_labels()
            viz.plt.grid()
            viz.save_and_close()

    def alter_properties_for_Shear7_software(self):
        minimum_fea_segment_length = self.stack_up_table['FEA Segment Length'][
            1:len(self.stack_up_table)].min()
        self.stack_up_table['FEA Segment Length'] = minimum_fea_segment_length
