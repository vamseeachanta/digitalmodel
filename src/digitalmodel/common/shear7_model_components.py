import logging
from assetutilities.common.data import SaveData


class Shear7ModelComponents():

    def __init__(self):
        self.shear7_model = []

    def prep_shear7_model(self, riser_model):

        self.riser_model = riser_model
        self.title()
        self.riser_joints_to_shear7_zones()

        self.add_block_1()
        self.add_block_2()
        self.add_block_3()
        self.add_block_4()
        self.add_block_5()
        self.add_block_6()

    def title(self):
        import json
        depth_below_msl =  [-self.riser_model.stack_up_properties_df[self.riser_model.stack_up_properties_df['component'] == 'MSL']['elevation_above_MSL'].values[0],
            -self.riser_model.stack_up_properties_df[self.riser_model.stack_up_properties_df['component'] == 'Mudline']['elevation_above_MSL'].values[0] ]
        shear7_x_by_L =  [self.riser_model.stack_up_properties_df[self.riser_model.stack_up_properties_df['component'] == 'MSL']['shear7_bottom_x_by_L'].values[0],
            self.riser_model.stack_up_properties_df[self.riser_model.stack_up_properties_df['component'] == 'Mudline']['shear7_bottom_x_by_L'].values[0] ]
        model_state_information = {'depth_below_msl': depth_below_msl, 'shear7_x_by_L': shear7_x_by_L,
            'water_depth': self.riser_model.water_depth
            }

        self.shear7_model.append(json.dumps(model_state_information))
        self.shear7_model.append(
            "Shear7 v4.6c {0}, {1} riser model".format(self.riser_model.cfg['Analysis']['file_name'],
                                                   self.riser_model.riser_type))

    def add_block_1(self):
        self.shear7_model.append("*** BLOCK 1. UNIT SYSTEM ***")
        self.shear7_model.append("0             flag for unit")

    def riser_joints_to_shear7_zones(self):
        self.all_zone_properties = []
        self.number_of_zones = 0
        for riser_joint_index in range(0, len(self.riser_model.stack_up_properties_df)):
            if self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['stack_length'] != 0:
                zone_properties = []
                self.number_of_zones = self.number_of_zones + 1
                zone_properties.append("{:.5f}       {:.5f}       zone 1   start and end point in x/L, {}" .format(
                    self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['shear7_top_x_by_L'],
                    self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['shear7_bottom_x_by_L'],
                    self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['component'])
                )
                zone_properties.append(
                    "{:.5f}       {:.5f}       {:.5f}       Hydro dia, outer dia and inner strength dia" .format(
                        self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['drag_diameter'],
                        self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['StressOD'],
                        self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['StressID']
                    )
                )
                zone_properties.append(
                    "{:.2E}	{:.2f}	{:.2f}			inertia(m^4), mass in air+fluids (kg/m), sub wt. (N/m)".format(
                        self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['I'],
                        (self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['dry_weight_per_unit_length'] +
                        self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['internal_fluid_weight_per_unit_length'])*1000/9.81,
                        self.riser_model.stack_up_properties_df.iloc[riser_joint_index]['wet_weight_per_unit_length']*1000
                    )
                )
                zone_properties.append("{:.2E}	1.0				Youngs Modulus, SN Curve Number" .format(
                    self.riser_model.cfg['Material']['Steel']['E'] * 0.4536 * 9.81 / (0.0254) ** 2
                )
                                       )
                zone_properties.append(
                    "0.40	0.18	1.00	1		Bandwidth St code Cl reduction factor   ZONE CL TYPE")
                zone_properties.append("1.00	0.20	0.18	0.20		Ca, DampCoeff1, DampCoef2, DampCoef3")

                self.all_zone_properties = self.all_zone_properties + zone_properties

    def add_block_2(self):
        self.shear7_model.append("***BLOCK 2. structural and hydrodynamic data***")
        self.shear7_model.append("1		flag for structural model")
        self.shear7_model.append("{:.2f}		model length" .format(self.riser_model.total_riser_length))
        self.shear7_model.append("{0}		number of elements")
        self.shear7_model.append("1025		volume density of the fluid (kg/m**3)")
        self.shear7_model.append("1.308E-06		kinematic viscosity of the fluid (m**2/s)")
        self.shear7_model.append("0.005		structural damping coefficient:")
        self.shear7_model.append("0		effective tension at origin (N)")
        self.shear7_model.append("{0}		no. of zones to define sectional property" .format(self.number_of_zones))
        self.shear7_model = self.shear7_model + self.all_zone_properties

    def add_block_3(self):
        self.shear7_model.append("*** BLOCK 3. CURRENT DATA ***")
        self.shear7_model.append("{0}  1.0   1      no. of profile data pts, probability, profile ID")
        self.shear7_model.append("{x_by_L}     {current_speed} Replace values ")
        self.shear7_model.append("{:.4f}     0.0000" .format(
            self.riser_model.stack_up_properties_df[self.riser_model.stack_up_properties_df['component'] == 'Mudline']['shear7_bottom_x_by_L'].values[0]))
        self.shear7_model.append("1.0000     0.0000")

    def add_block_4(self):
        self.shear7_model.append("*** BLOCK 4. s-n and scf data ***")
        self.shear7_model.append("1                                   No of SN Curves Defined - DNV '84 B Seawater CP")
        self.shear7_model.append("1           1                       SN Curve ID No, No. of S-N curve segments")
        self.shear7_model.append("0                                   cut-off stress range (N/m**2), endurance limit")
        self.shear7_model.append("1.000E+06  6.3154E+11               stress range (N/m**2) cycles to failure")
        self.shear7_model.append("1.000E+09  6.3154E+02               stress range (N/m**2) cycles to failure")
        self.shear7_model.append("1.3                                 Global stress concentration factor")
        self.shear7_model.append("0                                  No. of local stress concentration positions")

    def add_block_5(self):
        self.shear7_model.append("*** BLOCK 5. computation/output option ***")
        self.shear7_model.append("2                                   Calculation option")
        self.shear7_model.append(
            "0           1           0.01        Response location definition (start, stop, increment, in x/L)")
        self.shear7_model.append("0                                   Gravitational acceleration (m/s**2)")
        self.shear7_model.append("0.05        0.3                     power cutoff, primary zone amplitude limit ")
        self.shear7_model.append(
            "1                                   power value exponent (0 - equal probabilities; 1 - power ratio) ")
        self.shear7_model.append(
            "0                                   flag for importing nodal effective tension and mass, (0=no; 1=yes)")
        self.shear7_model.append("0                                   flag for writing the *.anm file (0=no, 1=yes)")
        self.shear7_model.append("1                                   flag for generating *.scr file, (0=no; 1=yes)")
        self.shear7_model.append("1                                   flag for generating *.dmg file, (0=no; 1=yes)")
        self.shear7_model.append("0                                   flag for generating *.fat file, (0=no; 1=yes)")
        self.shear7_model.append(
            "0                                   *.out file selection: 0=*.out, 1=*.out1, 2=*.out1+*.out2")

    def add_block_6(self):
        self.shear7_model.append("*** BLOCK 6. supplemental data ***")

    def save_model(self):
        save_data = SaveData()
        file_name = self.riser_model.cfg['Analysis']['fe_folder'] + self.riser_model.cfg['Analysis']['file_name'] + '_shear7'
        save_data.save_ascii_file_from_array(self.shear7_model, file_name, '.dat')
        print("Successfully write file: {0}{1}" .format(file_name, '.dat'))
