class TypicalRiserStackUpCalculations():

    def __init__(self, riser_model):
        self.riser_model = riser_model

    def prepare_stack_up_properties_df(self):
        import numpy as np
        import pandas as pd
        columns = ['component', 'component_bottom_elevation_from_end_A', 'stack_length',
                   'component_bottom_elevation_above_mudline', 'component_top_elevation_above_mudline',
                   'StressOD', 'StressID', 'drag_diameter', 'buoyancy_diameter', 'internal_fluid_ID',
                   'A', 'I', 'J', 'EI', 'EA', 'GJ', 'stack_in_service_weight',
                   'wet_weight_per_unit_length', 'dry_weight_per_unit_length', 'internal_fluid_weight_per_unit_length',
                   'internal_fluid_weight_in_seawater_per_unit_length',
                   'component_bottom_effective_tension', 'component_top_effective_tension', 'true_tension']
        self.stack_up_properties_df = pd.DataFrame(0, index=np.arange(len(self.riser_model.stack_up_table)), \
                                                   columns=columns)

        self.populate_elevation_from_endA_in_df()
        self.populate_shear7_x_by_L_in_df()
        self.populate_elevation_relative_to_mudline_in_df()
        self.populate_elevation_relative_to_MSL_in_df()
        self.evaluate_inservice_weights_and_tensions()
        self.riser_model.cfg['Analysis']['result'] = {
            'component_bottom_elevation_from_end_A': self.stack_up_properties_df['component_bottom_elevation_from_end_A'].tolist(),
            'component_bottom_effective_tension': self.stack_up_properties_df['component_bottom_effective_tension'].tolist()}


    def populate_elevation_from_endA_in_df(self):
        current_component_bottom_elevation_from_end_A = 0
        current_stack_item_length = 0
        for line_item_index in range(0, len(self.riser_model.stack_up_table)):
            if (isinstance(self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'], int)) and \
                    (self.riser_model.stack_up_table.iloc[line_item_index]['Component'] not in ['Upper FJ', 'Lower FJ',
                                                                                                'Tensioners',
                                                                                                'Support Spring']):
                current_stack_item_length = self.riser_model.stack_up_table.iloc[line_item_index][
                                                'Component Length'] * 0.3048 * \
                                            self.riser_model.stack_up_table.iloc[line_item_index][
                                                'No. Of. Joints']
                current_component_bottom_elevation_from_end_A = current_component_bottom_elevation_from_end_A + \
                                                                current_stack_item_length

            self.stack_up_properties_df.loc[line_item_index, 'component'] = \
                self.riser_model.stack_up_table.iloc[line_item_index]['Component']
            self.stack_up_properties_df.loc[line_item_index,
                                            'component_bottom_elevation_from_end_A'] = current_component_bottom_elevation_from_end_A
            self.stack_up_properties_df.loc[line_item_index, 'stack_length'] = current_stack_item_length
            current_stack_item_length = 0

    def populate_shear7_x_by_L_in_df(self):
        self.riser_model.total_riser_length = self.stack_up_properties_df['component_bottom_elevation_from_end_A'].max()
        self.stack_up_properties_df['shear7_top_x_by_L'] = (self.stack_up_properties_df[
            'component_bottom_elevation_from_end_A'] -  self.stack_up_properties_df['stack_length']) / self.riser_model.total_riser_length
        self.stack_up_properties_df['shear7_bottom_x_by_L'] = (self.stack_up_properties_df[
            'component_bottom_elevation_from_end_A']) / self.riser_model.total_riser_length

    def populate_elevation_relative_to_mudline_in_df(self):
        reference_elevation_index = \
        self.riser_model.stack_up_table[self.riser_model.stack_up_table['Component'] == 'Wellhead'].index[0]
        self.stack_up_properties_df.loc[reference_elevation_index, 'component_top_elevation_above_mudline'] = \
            self.riser_model.stack_up_table.iloc[reference_elevation_index]['Component Top Elevation above Mudline']*0.3048
        # March down the riser from reference
        for stack_item_index in range(reference_elevation_index + 1, len(self.riser_model.stack_up_table)):
            self.stack_up_properties_df.loc[stack_item_index, 'component_top_elevation_above_mudline'] = \
                self.stack_up_properties_df.iloc[stack_item_index - 1]['component_top_elevation_above_mudline'] \
                    - self.stack_up_properties_df.iloc[stack_item_index-1]['stack_length']
        # March up the riser from reference
        for stack_item_index in range(reference_elevation_index - 1, -1, -1):
            self.stack_up_properties_df.loc[stack_item_index, 'component_top_elevation_above_mudline'] = \
                self.stack_up_properties_df.iloc[stack_item_index + 1]['component_top_elevation_above_mudline'] \
              + self.stack_up_properties_df.iloc[stack_item_index]['stack_length']

        for stack_item_index in range(0, len(self.riser_model.stack_up_table)):
            if self.stack_up_properties_df.loc[stack_item_index, 'component'] == 'Mudline':
                self.stack_up_properties_df.loc[stack_item_index, 'component_top_elevation_above_mudline'] = 0
            if self.stack_up_properties_df.loc[stack_item_index, 'component'] == 'MSL':
                self.stack_up_properties_df.loc[stack_item_index, 'component_top_elevation_above_mudline'] = self.riser_model.water_depth

        for stack_item_index in range(0, len(self.riser_model.stack_up_table)):
            self.stack_up_properties_df.loc[stack_item_index, 'component_bottom_elevation_above_mudline'] = \
                self.stack_up_properties_df.iloc[stack_item_index]['component_top_elevation_above_mudline'] - \
                self.stack_up_properties_df.iloc[stack_item_index]['stack_length']

    def populate_elevation_relative_to_MSL_in_df(self):
        for stack_item_index in range(0, len(self.riser_model.stack_up_table)):
            self.stack_up_properties_df.loc[stack_item_index, 'elevation_above_MSL'] = \
                self.stack_up_properties_df.iloc[stack_item_index]['component_bottom_elevation_above_mudline'] - \
                self.riser_model.water_depth

    def evaluate_inservice_weights_and_tensions(self):
        self.assign_key_properties_to_df()
        self.evaluate_stack_in_service_weights()
        self.evaluate_effective_tension()
        self.evaluate_stretch_in_riser()


    def assign_key_properties_to_df(self):
        import math
        for stack_item_index in range(0, len(self.riser_model.stack_up_table)):
            for joint_index in range(0, len(self.riser_model.riser_joints)):
                if self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['Name'] == \
                        self.riser_model.stack_up_table.iloc[stack_item_index]['Component']:
                    self.stack_up_properties_df.loc[stack_item_index, 'dry_weight_per_unit_length'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe'][
                            'MassPerUnitLength'] / 1000 * 9.81
                    self.stack_up_properties_df.loc[stack_item_index, 'wet_weight_per_unit_length'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe'][
                            'wet_weight_per_unit_length'] / 1000 * 9.81
                    self.stack_up_properties_df.loc[stack_item_index, 'StressOD'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['StressOD']
                    self.stack_up_properties_df.loc[stack_item_index, 'StressID'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['StressID']
                    self.stack_up_properties_df.loc[stack_item_index, 'drag_diameter'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['drag_diameter']
                    self.stack_up_properties_df.loc[stack_item_index, 'buoyancy_diameter'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['buoyancy_diameter']
                    self.stack_up_properties_df.loc[stack_item_index, 'internal_fluid_ID'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['internal_fluid_ID']
                    self.stack_up_properties_df.loc[stack_item_index, 'A'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['A']
                    self.stack_up_properties_df.loc[stack_item_index, 'I'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['I']
                    self.stack_up_properties_df.loc[stack_item_index, 'J'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['J']
                    self.stack_up_properties_df.loc[stack_item_index, 'EI'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['EI']
                    self.stack_up_properties_df.loc[stack_item_index, 'EA'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['EA']
                    self.stack_up_properties_df.loc[stack_item_index, 'GJ'] = \
                        self.riser_model.pipe_properties[joint_index]['section_properties']['pipe']['GJ']
                    self.stack_up_properties_df.loc[stack_item_index, 'internal_fluid_weight_per_unit_length'] = \
                        math.pi/4*(self.stack_up_properties_df.loc[stack_item_index, 'internal_fluid_ID'])**2* \
                        self.riser_model.cfg['Material']['Fluid']['Rho'] / 1000*9.81
                    self.stack_up_properties_df.loc[stack_item_index, 'internal_fluid_weight_in_seawater_per_unit_length'] = \
                        math.pi/4*(self.stack_up_properties_df.loc[stack_item_index, 'internal_fluid_ID'])**2* \
                        (self.riser_model.cfg['Material']['Fluid']['Rho']-self.riser_model.cfg['Material']['SeaWater']['Rho']) / 1000*9.81


    def evaluate_stack_in_service_weights(self):
        for stack_item_index in range(0, len(self.riser_model.stack_up_table)):
            if (self.stack_up_properties_df.iloc[stack_item_index][
                'component_bottom_elevation_above_mudline'] >= self.riser_model.water_depth):
                self.stack_up_properties_df.loc[stack_item_index, 'stack_in_service_weight'] = \
                    (self.stack_up_properties_df.iloc[stack_item_index]['dry_weight_per_unit_length'] + \
                     self.stack_up_properties_df.iloc[stack_item_index]['internal_fluid_weight_per_unit_length']) * \
                    self.stack_up_properties_df.iloc[stack_item_index]['stack_length']
            elif (self.stack_up_properties_df.iloc[stack_item_index][
                      'component_top_elevation_above_mudline'] < self.riser_model.water_depth):
                self.stack_up_properties_df.loc[stack_item_index, 'stack_in_service_weight'] = \
                    (self.stack_up_properties_df.iloc[stack_item_index]['wet_weight_per_unit_length'] + \
                     self.stack_up_properties_df.iloc[stack_item_index]['internal_fluid_weight_in_seawater_per_unit_length']) * \
                    self.stack_up_properties_df.iloc[stack_item_index]['stack_length']
            else:
                self.stack_up_properties_df.loc[stack_item_index, 'stack_in_service_weight'] = \
                    (self.stack_up_properties_df.iloc[stack_item_index]['wet_weight_per_unit_length'] + \
                     self.stack_up_properties_df.iloc[stack_item_index]['internal_fluid_weight_in_seawater_per_unit_length']) * \
                    (self.riser_model.water_depth - self.stack_up_properties_df.iloc[stack_item_index][
                        'component_bottom_elevation_above_mudline']) + \
                    (self.stack_up_properties_df.iloc[stack_item_index]['dry_weight_per_unit_length'] + \
                     self.stack_up_properties_df.iloc[stack_item_index]['internal_fluid_weight_per_unit_length']) * \
                    (self.stack_up_properties_df.iloc[stack_item_index][
                         'component_top_elevation_above_mudline'] - self.riser_model.water_depth)

    def evaluate_effective_tension(self):
        import logging
        overpull_component = self.riser_model.cfg['stack_up']['overpull'][self.riser_model.riser_type][
            'bottom_of_component']
        overpull_effective_tension = self.riser_model.cfg['stack_up']['overpull'][self.riser_model.riser_type][
                                         'value'] * 0.4536 * 9.81
        overpull_component_index = \
        self.riser_model.stack_up_table[self.riser_model.stack_up_table['Component'] == overpull_component].index[0]
        self.stack_up_properties_df.loc[
            overpull_component_index, 'component_bottom_effective_tension'] = overpull_effective_tension
        self.stack_up_properties_df.loc[overpull_component_index,
                                        'component_top_effective_tension'] = overpull_effective_tension + \
                            self.stack_up_properties_df.iloc[overpull_component_index]['stack_in_service_weight']
        # March down the riser from reference
        for stack_item_index in range(overpull_component_index + 1, len(self.riser_model.stack_up_table)):
            self.stack_up_properties_df.loc[stack_item_index, 'component_bottom_effective_tension'] = \
                self.stack_up_properties_df.iloc[stack_item_index - 1]['component_bottom_effective_tension'] \
                - self.stack_up_properties_df.iloc[stack_item_index]['stack_in_service_weight']
            self.stack_up_properties_df.loc[stack_item_index, 'component_top_effective_tension'] = \
                self.stack_up_properties_df.iloc[stack_item_index - 1]['component_bottom_effective_tension']
        # March up the riser from reference
        for stack_item_index in range(overpull_component_index - 1, 0, -1):
            if self.riser_model.stack_up_table.iloc[stack_item_index]['Component'] == 'Tensioners':
                self.stack_up_properties_df.loc[stack_item_index, 'component_bottom_effective_tension'] = 0
                self.riser_model.total_tensioners_tension = float(
                    self.stack_up_properties_df.iloc[stack_item_index + 1]['component_bottom_effective_tension'])
                support_spring_component_end_index = stack_item_index - 1
            else:
                self.stack_up_properties_df.loc[stack_item_index, 'component_bottom_effective_tension'] = \
                    self.stack_up_properties_df.iloc[stack_item_index + 1]['component_bottom_effective_tension'] \
                    + self.stack_up_properties_df.iloc[stack_item_index + 1]['stack_in_service_weight']

            self.stack_up_properties_df.loc[stack_item_index, 'component_top_effective_tension'] = \
                self.stack_up_properties_df.iloc[stack_item_index]['component_bottom_effective_tension'] + \
                self.stack_up_properties_df.iloc[stack_item_index][
                    'stack_in_service_weight']

        if self.riser_model.support_spring:
            self.riser_model.total_support_tension = float(
                self.stack_up_properties_df.iloc[1]['component_top_effective_tension'])
        else:
            self.riser_model.total_support_tension = 0
            tension_adjustment = self.stack_up_properties_df.iloc[1]['component_top_effective_tension']
            # Adjust tension above tensioners
            for stack_item_index in range(support_spring_component_end_index, 0, -1):
                self.stack_up_properties_df.loc[stack_item_index, 'component_bottom_effective_tension'] = \
                    self.stack_up_properties_df.iloc[stack_item_index]['component_bottom_effective_tension'] \
                    - tension_adjustment
                self.stack_up_properties_df.loc[stack_item_index, 'component_top_effective_tension'] = \
                    self.stack_up_properties_df.iloc[stack_item_index]['component_top_effective_tension'] \
                    - tension_adjustment

        logging.info(self.stack_up_properties_df.to_string())

    def evaluate_stretch_in_riser(self):
        for stack_item_index in range(0, len(self.stack_up_properties_df)):
            stack_length = EA = self.stack_up_properties_df.iloc[stack_item_index]['stack_length']
            strain = 0
            axial_stretch=0
            if stack_length > 0:
                F = (self.stack_up_properties_df.iloc[stack_item_index]['component_bottom_effective_tension'] +
                     self.stack_up_properties_df.iloc[stack_item_index]['component_top_effective_tension']) / 2
                EA = self.stack_up_properties_df.iloc[stack_item_index]['EA']
                strain = F/EA
                axial_stretch = strain*stack_length

            self.stack_up_properties_df.loc[stack_item_index, 'strain'] = strain
            self.stack_up_properties_df.loc[stack_item_index, 'axial_stretch'] = axial_stretch