import logging
import math
import numpy as np
import pandas as pd
from multiprocessing import Pool

from pyintegrity.common.data import AttributeDict
from pyintegrity.common.data import ReadData
from pyintegrity.common.data import AttributeDict
from pyintegrity.common.math_solvers import Scipy_Interpolation
from pyintegrity.common.parallel_process_components import \
    ParallelProcessComponents
from pyintegrity.common.yml_utilities import WorkingWithYAML

ww_yaml = WorkingWithYAML()


class BS7910_2013():

    def __init__(self, cfg, fad):
        self.cfg = cfg
        self.flaw = AttributeDict(self.cfg.flaw)
        self.fad = fad
        self.init_assign_key_properties()
        self.init_read_stress_intensity_solution_tables()

    def init_read_stress_intensity_solution_tables(self):
        read_data = ReadData()
        cfg_temp = {
            'files': self.cfg.default['stress_intensity_factor_solution_tables']
        }
        cfg_temp = self.update_cfg_with_library_file(cfg_temp=cfg_temp)
        self.stress_intensity_solution_tables = read_data.from_xlsx(cfg_temp)

    def update_cfg_with_library_file(self, cfg_temp):
        for i in range(0, len(cfg_temp['files']['from_xlsx'])):
            cfg_temp['files']['from_xlsx'][i]['io']
            ww_yaml_cfg = {'filename': cfg_temp['files']['from_xlsx'][i]['io']}
            cfg_temp['files']['from_xlsx'][i][
                'io'] = ww_yaml.get_library_filename(ww_yaml_cfg)

        return cfg_temp

    def get_critical_allowable_flaw(self):
        logging.info("Input Flaw ...")
        self.get_flaw_fad_properties()
        self.get_K_r_allowable()
        self.result = AttributeDict()
        for location_item in self.cfg.default['settings']['location_array']:
            self.flaw.location = location_item
            print(
                "Performing Unstable Fracture Limit Analysis for flaw location: {} ..."
                .format(location_item))
            self.iterate_flaw_length_for_unstable_limits()
            logging.info(self.critical_allowable_flaw_df)
            self.result.update({
                location_item: {
                    'fracture_limit': self.critical_allowable_flaw_df
                }
            })
            print(
                "Unstable Fracture Limit Analysis for flaw location: {} ... COMPLETE \n"
                .format(location_item))

    def get_minimum_allowable_initial_flaw(self):
        pass

    def get_K_r_allowable(self):
        try:
            self.K_r_allowable = np.interp(self.L_r,
                                           self.fad['option_1']['L_r'],
                                           self.fad['option_1']['K_r'])
        except:
            print(
                "Could not evaluate self.K_r_allowable. Check results using breakpoint in function 'get_K_r_allowable' "
            )

    def iterate_flaw_length_for_unstable_limits(self):

        self.critical_allowable_flaw_df = pd.DataFrame(
            columns=self.cfg.default['settings']['fad_properties_columns'])
        a_start = self.cfg.default['settings']['a_array']['start']
        a_end = self.cfg.default['settings']['a_array']['end']
        a_step = self.cfg.default['settings']['a_array']['step']
        a_nsteps = int((a_end - a_start) / a_step + 1)
        for flaw_orientation in self.cfg.default['settings'][
                'orientation_array']:
            self.flaw.orientation = flaw_orientation

            for c in self.cfg.default['settings']['c_array']:
                self.iterate_flaw_length_for_unstable_limits_for_single_c_value(
                    a_nsteps, a_start, a_step, c)

    def iterate_flaw_length_for_unstable_limits_for_single_c_value(
            self, a_nsteps, a_start, a_step, c):
        self.flaw.dimensions['c'] = c
        for flaw_theta in self.cfg.default['settings']['theta_array']:
            self.flaw.theta = flaw_theta
            self.fad_properties_df = pd.DataFrame(
                columns=self.cfg.default['settings']['fad_properties_columns'])
            for a_index in range(0, a_nsteps):
                a = a_start + a_index * a_step
                self.flaw.dimensions['a'] = a
                self.get_flaw_fad_properties()
                self.get_K_r_allowable()
                self.fad_properties_df.loc[len(self.fad_properties_df)] = [
                    self.flaw.geometry, self.flaw.orientation,
                    self.flaw.location, self.flaw.theta, a, c, self.K_r,
                    self.L_r, self.K_r_allowable
                ]
            for df_index in range(0, len(self.fad_properties_df) - 1):
                if self.fad_properties_df.K_r[
                        df_index] > self.fad_properties_df.K_r_allowable[
                            df_index]:
                    if df_index > 0:
                        a_end = self.fad_properties_df.flaw_depth[df_index - 1]
                        a_nsteps = int((a_end - a_start) / a_step + 1)
                    break

            if self.cfg.default['settings']['save_detailed_results']:
                file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
                    'Analysis']['file_name'] + '_' + 'len_{0}.csv'
                self.fad_properties_df.to_csv(file_name.format(c))

            self.critical_allowable_flaw_df.loc[len(
                self.critical_allowable_flaw_df)] = self.fad_properties_df.iloc[
                    df_index - 1]

    def get_flaw_fad_properties(self):

        # TODO implement crack face pressure for pressurized components
        logging.info("Flaw analysis ...")
        self.a = self.flaw.dimensions['a']
        self.c = self.flaw.dimensions['c']
        self.get_reference_annexes()
        self.get_stress_intensity_factor_solution_by_annex(
            self.stress_intensity_annex)
        self.get_stress_intensity_factor_at_discontinuities()
        self.get_stress_intensity_factor_solution(stress_intensity_key='deep')

        K_mat = self.get_critical_stress_intensity_factor()
        self.get_reference_stress_by_annex(self.reference_stress_annex)
        self.L_r = self.reference_stress / (
            self.material_grade_properties['SMYS'])
        self.get_plasticity_correction()
        self.K_r = self.K_I / K_mat + self.plasticity_correction

        logging.info("Flaw properties  ... COMPLETED")

    def get_reference_annexes(self):
        if self.flaw.geometry == 'thin_pipe':
            if self.flaw.orientation == 'axial':
                if self.flaw.location == 'internal_surface':
                    stress_intensity_annex = 'Annex_M_7_2_2'
                    reference_stress_annex = 'Annex_P_9_2'
                elif self.flaw.location == 'external_surface':
                    stress_intensity_annex = 'Annex_M_7_2_4'
                    reference_stress_annex = 'Annex_P_9_4'
                elif self.flaw.location == 'embedded':
                    stress_intensity_annex = 'Annex_M_7_3_6'
                    reference_stress_annex = 'Annex_P_10_6'
                else:
                    print(
                        f"No calculation found for flaw geometry: {self.flaw.geometry} and flaw oreintation: {self.flaw.orientation}"
                    )
                    exit()
            elif self.flaw.orientation == 'circumferential':
                if self.flaw.location == 'internal_surface':
                    stress_intensity_annex = 'Annex_M_7_3_2'
                    reference_stress_annex = 'Annex_P_10_2'
                elif self.flaw.location == 'external_surface':
                    stress_intensity_annex = 'Annex_M_7_3_4'
                    reference_stress_annex = 'Annex_P_10_4'
                elif self.flaw.location == 'embedded':
                    stress_intensity_annex = 'Annex_M_7_3_6'
                    reference_stress_annex = 'Annex_P_10_6'
                else:
                    print(
                        f"No calculation found for flaw geometry: {self.flaw.geometry} and flaw orientation: {self.flaw.orientation}"
                    )
                    exit()

        self.stress_intensity_annex = stress_intensity_annex
        self.reference_stress_annex = reference_stress_annex

    def get_stress_intensity_factor_solution(self, stress_intensity_key):
        M = self.M
        fw = self.fw
        if ((self.flaw.location in ['external_surface', 'internal_surface']) and
                self.flaw.orientation in ['axial']) or (
                    (self.flaw.location in ['internal_surface']) and
                    self.flaw.orientation in ['circumferential']):
            M_m = self.stress_intensity[stress_intensity_key]['M_m']
            M_km = self.stress_intensity[stress_intensity_key]['M_km']
            M_b = self.stress_intensity[stress_intensity_key]['M_b']
            M_kb = self.stress_intensity[stress_intensity_key]['M_kb']
        elif (self.flaw.location in [
                'embedded'
        ]) or (self.flaw.location in ['external_surface'] and
               self.flaw.orientation in ['circumferential']):
            M_m = self.M_m
            M_b = self.M_b
            M_km = 1.0
            M_kb = self.cfg.default['settings']['scf_bending']
        else:
            print("No stress intensity factor solution found in program")

        # Calculated based on BS7910, Part 6.4 and Annex D
        P_m = self.cfg.loading['primary_membrane_stress'][
            'value'] * self.cfg.loading['primary_membrane_stress'][
                'membrane_factor'] * 0.00689476
        self.P_m = P_m
        P_b = self.cfg.loading['primary_membrane_stress'][
            'value'] * self.cfg.loading['primary_membrane_stress'][
                'bending_factor'] * 0.00689476
        self.P_b = P_b

        Q_m = self.cfg.loading['secondary_membrane_stress']['stress_to_yield'][self.flaw.orientation] * \
              self.material_grade_properties['SMYS'] * 0.00689476
        Q_b = Q_m * self.cfg.loading['secondary_membrane_stress'][
            'bending_factor']

        k_tm = self.k_tm
        k_tb = self.k_tb
        k_m = self.k_m

        self.Y_sigma_primary = M * fw * (k_tm * M_km * M_m * P_m +
                                         k_tb * M_kb * M_b * (P_b +
                                                              (k_m - 1) * P_m))
        self.Y_sigma_secondary = M_m * Q_m + M_b * Q_b

        self.Y_delta_sigma_primary = M * fw * (
            k_tm * M_km * M_m * self.delta_P_m + k_tb * M_kb * M_b *
            (self.delta_P_b + (k_m - 1) * self.delta_P_m))

        if self.Y_sigma_secondary > 0:
            Y_sigma = self.Y_sigma_primary + self.Y_sigma_secondary
        else:
            Y_sigma = self.Y_sigma_primary

        self.K_I = Y_sigma * math.sqrt(math.pi * self.flaw.dimensions['a'])
        self.delta_K_I = self.Y_delta_sigma_primary * math.sqrt(
            math.pi * self.flaw.dimensions['a'])

    def get_finite_width_correction_factor(self):
        if self.stress_intensity_annex in [
                'Annex_M_7_2_2', 'Annex_M_7_2_4', 'Annex_M_7_3_2'
        ]:
            fw = 1
        elif self.stress_intensity_annex in ['Annex_M_7_3_4', 'Annex_M_4_1']:
            sec_of_value = math.sqrt(math.pi * self.c / self.W *
                                     math.sqrt(2 * self.a / self.B))
            try:
                fw = math.sqrt(1 / (math.cos(sec_of_value)))
            except:
                fw = 1
                print("secant of value error: {}".format(sec_of_value))
        elif self.stress_intensity_annex in ['Annex_M_7_3_6', 'Annex_M_4_3']:
            sec_of_value = math.sqrt(math.pi * self.c / self.W *
                                     math.sqrt(2 * self.a / self.B_dash))
            try:
                fw = math.sqrt(1 / (math.cos(sec_of_value)))
            except:
                fw = 1
                print("secant of value error: {}".format(sec_of_value))
        else:
            print("No reference found for fw calculation")

        self.fw = fw

    def get_stress_intensity_factor_solution_by_annex(self, reference_section):
        reference_function = getattr(
            self, 'get_stress_intensity_factor_solution_' + reference_section)
        reference_function()

    def get_stress_intensity_factor_solution_Annex_M_4_1(self):

        self.assign_key_flaw_dimensions_for_calculations()
        self.perform_flaw_dimension_acceptance_check()

        self.get_f_theta_value(self.theta)
        self.get_phi_value()

        self.M = 1
        self.get_finite_width_correction_factor()

        if (self.a / (2 * self.c)) <= 0.5:
            M_1 = 1.13 - 0.09 * self.a / self.c
            M_2 = 0.89 / (0.2 + self.a / self.c) - 0.54
            M_3 = 0.5 - (
                1 / (0.65 + self.a / self.c)) + 14 * (1 - self.a / self.c)**24
            g = 1 + (0.1 + 0.35 *
                     (self.a / self.B)**2) * (1 - math.sin(self.theta))**2
        else:
            M_1 = math.sqrt(self.c / self.a) * (1 + 0.04 * self.c / self.a)
            M_2 = 0.2 * (self.c / self.a)**4
            M_3 = -0.11 * (self.c / self.a)**4
            g = 1 + (0.1 + 0.35 * (self.c / self.a) *
                     (self.a / self.B)**2) * (1 - math.sin(self.theta))**2

        self.M_m = (M_1 + M_2 * (self.a / self.B)**2 + M_3 *
                    (self.a / self.B)**4) * g * self.f_theta / self.phi

        if (self.a / (2 * self.c)) <= 0.5:
            G_1 = -1.22 - 0.12 * self.a / self.c
            G_2 = 0.55 - 1.05 * (self.a / self.c)**0.75 + 0.47 * (self.a /
                                                                  self.c)**1.5
            q = 0.2 + self.a / self.c + 0.6 * (self.a / self.B)
            H_1 = 1 - 0.34 * (self.a / self.B) - 0.11 * (self.a / self.c) * (
                self.a / self.B)
        else:
            G_1 = -2.11 + 0.75 * self.c / self.a
            G_2 = 0.55 - 0.72 * (self.a / self.c)**0.75 + 0.14 * (self.a /
                                                                  self.c)**1.5
            q = 0.2 + self.c / self.a + 0.6 * (self.a / self.B)
            H_1 = 1 - (0.04 + 0.41 * (self.c / self.a) * (self.a / self.B)
                      ) + (0.55 - 1.93 * (self.c / self.a)**0.75 + 1.38 *
                           (self.c / self.a)**1.5) * (self.a / self.B)**2

        H_2 = 1 + G_1 * (self.a / self.B) + G_2 * (self.a / self.B)**2
        H = H_1 + (H_2 - H_1) * (math.sin(self.theta))**q
        self.M_b = H * self.M_m

    def get_stress_intensity_factor_solution_Annex_M_4_3(self):

        self.assign_key_flaw_dimensions_for_calculations()
        self.perform_flaw_dimension_acceptance_check()

        self.get_f_theta_value(self.theta)
        self.get_phi_value()

        self.M = 1
        self.get_finite_width_correction_factor()

        if (self.a / (2 * self.c)) <= 0.5:
            M_1 = 1
        else:
            M_1 = (self.c / self.a)**0.5
        M_2 = 0.05 / (0.11 + (self.a / self.c)**1.5)
        M_3 = 0.29 / (0.23 + (self.a / self.c)**1.5)
        g = 1 - (((2 * self.a / self.B_dash)**4) *
                 (2.6 - (4 * self.a / self.B_dash)**0.5) /
                 (1 + 4 * self.a / self.c)) * math.fabs(math.cos(self.theta))
        self.M_m = (M_1 + M_2 * (2 * self.a / self.B_dash)**2 + M_3 *
                    (2 * self.a / self.B_dash)**4) * g * self.f_theta / self.phi

        if self.p / self.B <= 0.1841:
            lamda_1 = 1.044
            lamda_2 = -2.44
            lamda_3 = 0
            lamda_4 = -3.166
        else:
            if self.a / self.B <= 0.125:
                lamda_1 = 0.94
                lamda_2 = -1.875
                lamda_3 = -0.1146
                lamda_4 = -1.844
            else:
                lamda_1 = 1.06
                lamda_2 = -2.2
                lamda_3 = -0.6666
                lamda_4 = -0.6666

        M_b_numerator = lamda_1 + lamda_2 * (self.p / self.B) + lamda_3 * (
            self.a / self.B) + lamda_4 * (self.p * self.a / (self.B**2))
        self.M_b = M_b_numerator / self.phi

    def get_stress_intensity_factor_solution_Annex_M_7_2_2(self):

        self.assign_key_flaw_dimensions_for_calculations()
        self.perform_flaw_dimension_acceptance_check()

        self.M = 1
        self.get_finite_width_correction_factor()
        stress_intensity_solution_table = 'Table_M_2'

        if (not self.cfg.default['settings'].__contains__(
                'stress_intensity_factor_solution')) or (
                    self.cfg.default['settings']
                    ['stress_intensity_factor_solution'] == 'constant_values'):
            self.stress_intensity = {
                'deep': {
                    'M_m': 0.932,
                    'M_b': 0.698,
                    'M_km': 1.0,
                    'M_kb': self.cfg.default['settings']['scf_bending']
                },
                'free_surface': {
                    'M_m': 0.676,
                    'M_b': 0.632,
                    'M_km': 1.0,
                    'M_kb': self.cfg.default['settings']['scf_bending']
                }
            }
        else:
            self.stress_intensity = self.get_stress_intensity_solution_by_table_data(
                stress_intensity_solution_table)

    def get_stress_intensity_factor_solution_Annex_M_7_2_4(self):

        self.assign_key_flaw_dimensions_for_calculations()
        self.perform_flaw_dimension_acceptance_check()

        self.M = 1
        self.get_finite_width_correction_factor()
        # TODO Convert Table M.4 into a xlsx
        self.stress_intensity = {
            'deep': {
                'M_m': 0.953,
                'M_b': 0.716,
                'M_km': 1.0,
                'M_kb': self.cfg.default['settings']['scf_bending']
            },
            'free_surface': {
                'M_m': 0.685,
                'M_b': 0.641,
                'M_km': 1.0,
                'M_kb': self.cfg.default['settings']['scf_bending']
            }
        }

    def get_stress_intensity_factor_solution_Annex_M_7_3_2(self):

        self.assign_key_flaw_dimensions_for_calculations()
        self.perform_flaw_dimension_acceptance_check()

        self.M = 1
        self.fw = 1
        # TODO Add contribution to global bending moment on cylinder at later date

        # TODO Convert Table M.7 into a xlsx
        self.stress_intensity = {
            'deep': {
                'M_m': 0.999,
                'M_b': 0.731,
                'M_km': 1.0,
                'M_kb': self.cfg.default['settings']['scf_bending']
            },
            'free_surface': {
                'M_m': 0.731,
                'M_b': 0.628,
                'M_km': 1.0,
                'M_kb': self.cfg.default['settings']['scf_bending']
            }
        }

    def get_stress_intensity_factor_solution_Annex_M_7_3_4(self):
        self.get_stress_intensity_factor_solution_Annex_M_4_1()

    def get_stress_intensity_factor_solution_Annex_M_7_3_6(self):
        self.get_stress_intensity_factor_solution_Annex_M_4_3()

    def get_stress_intensity_solution_by_table_data(
            self, stress_intensity_solution_table):
        a_over_B = self.a / self.B
        a_over_c = self.a / self.c
        if stress_intensity_solution_table in [
                'Table_M_2', 'Table_M_4', 'Table_M_7'
        ]:
            B_over_ri = self.B / self.ri
            df = self.stress_intensity_solution_tables[
                stress_intensity_solution_table]
            theta = 90
            theta_location = 'Depth'
            M_m_deep, M_b_deep = self.dataframe_manual_interpolation_using_filters(
                B_over_ri, a_over_B, a_over_c, df, theta, theta_location)
            theta = 0
            theta_location = 'Surface'
            M_m_free_surface, M_b_free_surface = self.dataframe_manual_interpolation_using_filters(
                B_over_ri, a_over_B, a_over_c, df, theta, theta_location)

        self.stress_intensity = {
            'deep': {
                'M_m': M_m_deep,
                'M_b': M_b_deep,
                'M_km': 1.0,
                'M_kb': self.cfg.default['settings']['scf_bending']
            },
            'free_surface': {
                'M_m': M_m_free_surface,
                'M_b': M_b_free_surface,
                'M_km': 1.0,
                'M_kb': self.cfg.default['settings']['scf_bending']
            }
        }

        return self.stress_intensity

    def dataframe_manual_interpolation_using_scipy(self, B_over_ri, a_over_B,
                                                   a_over_c, df, theta,
                                                   theta_location):
        # Approximate method. Still with high computational cost. Not used
        scipy_interp = Scipy_Interpolation()
        a_over_B_list = df[df['theta'] == theta].a_over_B.to_list()
        a_over_c_list = df[df['theta'] == theta].a_over_c.to_list()
        B_over_ri_list = df[df['theta'] == theta].B_over_ri.to_list()

        X = [[
            a_over_B_list[index], a_over_c_list[index], B_over_ri_list[index]
        ] for index in range(0, len(a_over_B_list))]
        Mm_list = df[df['theta'] == theta].Mm
        Mb_list = df[df['theta'] == theta].Mb
        n_neighbors = 8
        M_m = scipy_interp.solve_y_for_X(X, Mm_list, n_neighbors,
                                         [a_over_B, a_over_c, B_over_ri])
        M_b = scipy_interp.solve_y_for_X(X, Mb_list, n_neighbors,
                                         [a_over_B, a_over_c, B_over_ri])
        return M_m, M_b

    def dataframe_manual_interpolation_using_filters(self, B_over_ri, a_over_B,
                                                     a_over_c, df, theta,
                                                     theta_location):
        import statistics

        import numpy as np

        a_over_B_look_up = df.a_over_B.sort_values().unique()
        a_over_c_look_up = df.a_over_c.sort_values().unique()
        B_over_ri_look_up = df.B_over_ri.sort_values().unique()

        if a_over_B < a_over_B_look_up[0]:
            a_over_B_low = a_over_B_look_up[0]
        else:
            a_over_B_low = a_over_B_look_up[np.where(
                a_over_B_look_up <= a_over_B)][-1]
        if a_over_B > a_over_B_look_up[-1]:
            a_over_B_high = a_over_B_look_up[-1]
        else:
            a_over_B_high = a_over_B_look_up[np.where(
                a_over_B_look_up >= a_over_B)][0]

        if a_over_c < a_over_c_look_up[0]:
            a_over_c_low = a_over_c_look_up[0]
        else:
            a_over_c_low = a_over_c_look_up[np.where(
                a_over_c_look_up <= a_over_c)][-1]
        if a_over_c > a_over_c_look_up[-1]:
            a_over_c_high = a_over_c_look_up[-1]
        else:
            a_over_c_high = a_over_c_look_up[np.where(
                a_over_c_look_up >= a_over_c)][0]

        if B_over_ri < B_over_ri_look_up[0]:
            B_over_ri_low = B_over_ri_look_up[0]
        else:
            B_over_ri_low = B_over_ri_look_up[np.where(
                B_over_ri_look_up <= B_over_ri)][-1]
        if B_over_ri > B_over_ri_look_up[-1]:
            B_over_ri_high = B_over_ri_look_up[-1]
        else:
            B_over_ri_high = B_over_ri_look_up[np.where(
                B_over_ri_look_up >= B_over_ri)][0]

        df_temp_low = df[(df['a_over_B'] == a_over_B_low) &
                         (df['a_over_c'] == a_over_c_low) &
                         (df['B_over_ri'] == B_over_ri_low) &
                         (df['theta'] == theta)].copy()
        df_temp_low.reset_index(inplace=True, drop=True)
        df_temp_high = df[(df['a_over_B'] == a_over_B_high) &
                          (df['a_over_c'] == a_over_c_high) &
                          (df['B_over_ri'] == B_over_ri_high) &
                          (df['theta'] == theta)].copy()
        df_temp = df_temp_low.copy()
        row = [a_over_B, a_over_c, B_over_ri, theta, theta_location, None, None]
        df_temp.loc[len(df_temp)] = row
        df_temp = df_temp.append(df_temp_high, ignore_index=True)
        # TODO replace below manual interpolation to scipy package in future

        if a_over_B_low == a_over_B_high:
            a_over_B_factor = 0
        else:
            a_over_B_factor = (
                df_temp['a_over_B'].iloc[1] - df_temp['a_over_B'].iloc[0]) / (
                    df_temp['a_over_B'].iloc[2] - df_temp['a_over_B'].iloc[0])

        if a_over_c_low == a_over_c_high:
            a_over_c_factor = 0
        else:
            a_over_c_factor = (
                df_temp['a_over_c'].iloc[1] - df_temp['a_over_c'].iloc[0]) / (
                    df_temp['a_over_c'].iloc[2] - df_temp['a_over_c'].iloc[0])

        if B_over_ri_low == B_over_ri_high:
            B_over_ri_factor = 0
        else:
            B_over_ri_factor = (
                df_temp['B_over_ri'].iloc[1] - df_temp['B_over_ri'].iloc[0]) / (
                    df_temp['B_over_ri'].iloc[2] - df_temp['B_over_ri'].iloc[0])

        average_factor = statistics.mean(
            [a_over_B_factor, a_over_c_factor, B_over_ri_factor])

        M_m = df_temp['Mm'].iloc[0] + (df_temp['Mm'].iloc[2] -
                                       df_temp['Mm'].iloc[0]) * average_factor
        M_b = df_temp['Mb'].iloc[0] + (df_temp['Mb'].iloc[2] -
                                       df_temp['Mb'].iloc[0]) * average_factor

        return M_m, M_b

    def perform_flaw_dimension_acceptance_check(self):
        a_over_B_flag = False
        a_over_c_flag = False
        B_over_ri_flag = False
        c_x_2_over_w_flag = False
        a_over_B_dash_flag = False

        if self.stress_intensity_annex in ['Annex_M_7_2_2', 'Annex_M_7_2_4']:
            a_over_B_flag = True
            a_over_B_range = [0, 0.8]
            a_over_c_flag = True
            a_over_c_range = [0.05, 1.0]
            B_over_ri_flag = True
            B_over_ri_range = [0.1, 0.25]
            c_x_2_over_w_flag = True
            c_x_2_over_w_range = [None, 0.15]

        if self.stress_intensity_annex in ['Annex_M_7_3_2']:
            a_over_B_flag = True
            a_over_B_range = [0, 0.8]
            a_over_c_flag = True
            a_over_c_range = [0.1, 1.0]
            B_over_ri_flag = True
            B_over_ri_range = [0.1, 0.20]

        if self.stress_intensity_annex in ['Annex_M_4_3']:
            a_over_c_flag = True
            a_over_c_range = [0.0, 2.0]
            c_x_2_over_w_flag = True
            c_x_2_over_w_range = [None, 0.5]
            a_over_B_dash_flag = True

        a_over_B = self.a / self.B
        B_over_ri = self.B / self.ri
        a_over_c = self.a / self.c
        c_x_2_over_w = 2 * self.c / self.W
        if self.flaw.location == 'embedded':
            a_over_B_dash = self.a / self.B_dash

        if a_over_B_flag:
            if (a_over_B >= a_over_B_range[0]) and (a_over_B <=
                                                    a_over_B_range[1]):
                logging.info("a/B check acceptable")
            else:
                logging.info(
                    "flaw a/B, {} beyond code limits. Can not proceed further.".
                    format(a_over_B))

        if a_over_c_flag:
            if (a_over_c >= a_over_c_range[0]) and (a_over_c <=
                                                    a_over_c_range[0]):
                logging.info("a/c check acceptable")
            else:
                logging.info(
                    "flaw a/c, {} beyond code limits. Can not proceed further.".
                    format(a_over_c))

        if B_over_ri_flag:
            if B_over_ri >= B_over_ri_range[0] and B_over_ri <= B_over_ri_range[
                    1]:
                logging.info("B/ri check acceptable")
            else:
                logging.info(
                    "flaw B/ri, {} beyond code limits. Can not proceed further."
                    .format(B_over_ri))
        if c_x_2_over_w_flag:
            if (c_x_2_over_w <= c_x_2_over_w_range[1]):
                logging.info("2c/W check acceptable")
            else:
                logging.info(
                    "flaw 2c/W, {} beyond code limits. Can not proceed further."
                    .format(c_x_2_over_w))

        if a_over_B_dash_flag:
            if self.stress_intensity_annex in ['Annex_M_4_3']:
                if a_over_B_dash <= (0.625 * (a_over_c) + 0.6):
                    logging.info("2c/B_dash check acceptable")
                else:
                    logging.info(
                        "flaw 2c/B_dash, {} beyond code limits. Can not proceed further."
                        .format(a_over_B_dash))

    def get_reference_stress_by_annex(self, reference_section):
        reference_function = getattr(
            self, 'get_reference_stress_' + reference_section)
        reference_function()

    def get_reference_stress_Annex_P_9_2(self):
        reference_stress_equation = 'Equation_P_18'
        self.get_reference_stress_by_equation(reference_stress_equation)

    def get_reference_stress_Annex_P_9_4(self):
        reference_stress_equation = 'Equation_P_18'
        self.get_reference_stress_by_equation(reference_stress_equation)

    def get_reference_stress_Annex_P_10_2(self):
        reference_stress_equation = 'Equation_P_23'
        self.get_reference_stress_by_equation(reference_stress_equation)

    def get_reference_stress_Annex_P_10_4(self):
        reference_stress_equation = 'Equation_P_23'
        self.get_reference_stress_by_equation(reference_stress_equation)

    def get_reference_stress_Annex_P_10_6(self):
        reference_stress_equation = 'Equation_P_11'
        self.get_reference_stress_by_equation(reference_stress_equation)

    def get_reference_stress_by_equation(self, reference_equation):
        reference_function = getattr(
            self, 'get_reference_stress_' + reference_equation)
        self.reference_stress = reference_function()

    def get_reference_stress_Equation_P_23(self):

        if (math.pi * self.radius) >= (self.c + self.B):
            alpha_double_dash = (self.a / self.B) / (1 + self.B / self.c)
        else:
            alpha_double_dash = (self.a / self.B) * (self.c /
                                                     (math.pi * self.radius))

        sigma_reference_term1 = self.P_m * (
            math.pi * (1 - self.a / self.B) + 2 *
            (self.a / self.B) * math.sin(self.c / self.radius)) / (
                (1 - self.a / self.B) * (math.pi - (self.c / self.radius) *
                                         (self.a / self.B)))
        sigma_reference_term2 = 2 * self.P_b / (3 * (1 - alpha_double_dash)**2)
        sigma_reference = sigma_reference_term1 + sigma_reference_term2

        return sigma_reference

    def get_reference_stress_Equation_P_18(self):

        if self.W >= 2 * (self.c + self.B):
            alpha_double_dash = (self.a / self.B) / (1 + self.B / self.c)
        else:
            alpha_double_dash = (self.a / self.B) * (2 * self.c / self.W)

        M_T = math.sqrt(1 + 1.6 * (self.c**2) / self.radius / self.B)
        M_s = (1 - self.a / self.B / M_T) / (1 - self.a / self.B)

        sigma_ref_term1 = 1.2 * M_s * self.P_m
        sigma_ref_term2 = 2 * self.P_b / (3 * (1 - alpha_double_dash)**2)
        sigma_ref = sigma_ref_term1 + sigma_ref_term2

        return sigma_ref

    def get_reference_stress_Equation_P_11(self):

        if self.W >= 2 * (self.c + self.B):
            self.alpha_double_dash = (2 * self.a / self.B) / (1 +
                                                              self.B / self.c)
        else:
            self.alpha_double_dash = (2 * self.a / self.B) / (2 * self.c /
                                                              self.W)

        sigma_reference_denominator = 3 * (
            (1 - self.alpha_double_dash)**2 +
            4 * self.p * self.alpha_double_dash / self.B)
        sigma_reference_numerator_term1 = self.P_b + 3 * self.P_m * self.alpha_double_dash
        sigma_reference_numerator_term2 = math.sqrt(
            sigma_reference_numerator_term1**2 +
            3 * self.P_m**2 * sigma_reference_denominator)

        sigma_reference = (
            sigma_reference_numerator_term1 +
            sigma_reference_numerator_term2) / sigma_reference_denominator

        return sigma_reference

    def get_stress_intensity_factor_at_discontinuities(self):
        self.k_tm = 1
        self.k_tb = 1
        self.k_m = 1

    def init_assign_key_properties(self):
        self.material_grade = self.cfg['Outer_Pipe']['Material'][
            'Material_Grade']
        self.material = self.cfg['Outer_Pipe']['Material']['Material']
        self.material_properties = self.cfg['Material'][self.material].copy()
        self.material_properties['E'] = self.cfg['Material'][
            self.material]['E'] * 0.00689476
        self.material_grade_properties = self.cfg['Material'][
            self.material]['Grades'][self.material_grade].copy()
        self.material_grade_properties['SMYS'] = self.cfg['Material'][
            self.material]['Grades'][self.material_grade]['SMYS'] * 0.00689476
        self.material_grade_properties['SMUS'] = self.cfg['Material'][
            self.material]['Grades'][self.material_grade]['SMUS'] * 0.00689476
        self.pipe_properties = None
        self.delta_P_m = 0
        self.delta_P_b = 0

    def get_critical_stress_intensity_factor(self):
        import math
        #     TODO implement J-Integral method using section 7.1.4.6
        ratio_of_yield_to_tensile = self.material_grade_properties[
            'SMYS'] / self.material_grade_properties['SMUS']
        if ratio_of_yield_to_tensile < 0.98 and ratio_of_yield_to_tensile > 0.3:
            m = 1.517 * (ratio_of_yield_to_tensile**-0.3188)
        else:
            m = 1.5
        delta_mat = self.cfg.default['settings']['ctod']

        K_mat = math.sqrt(m * self.material_grade_properties['SMYS'] *
                          delta_mat * self.material_properties['E'] /
                          (1 - self.material_properties['Poissionsratio']**2))

        return K_mat

    def get_plasticity_correction(self):
        if (self.Y_sigma_secondary < 0) or (self.Y_sigma_primary <= 0):
            rho = 0
            V = 1
        else:
            validation_ratio = self.Y_sigma_secondary / (self.Y_sigma_primary /
                                                         self.L_r)
            if validation_ratio < 5.2:
                rho_1 = 0.1 * (validation_ratio**0.714) - 0.007 * (
                    validation_ratio**2) + 0.00003 * (validation_ratio**5)
            else:
                rho_1 = 0.25

            if rho_1 < 0.25 and rho_1 > 0:
                if self.L_r <= 0.8:
                    rho = rho_1
                elif self.L_r < 1.05:
                    rho = 4 * rho_1 * (1.05 - self.L_r)
                else:
                    rho = 0
            else:
                rho = 0

            if self.L_r < 1.05:
                V_choice_1 = 1 + 0.2 * self.L_r + 0.02 * validation_ratio * (
                    1 + 2 * self.L_r)
                V_choice_2 = 3.1 - 2 * self.L_r
                V = min(V_choice_1, V_choice_2)
            else:
                V = 1

        self.plasticity_correction = rho
        self.V = V

    def get_phi_value(self):
        if (self.a / 2 / self.c >= 0) and (self.a / 2 / self.c <= 0.5):
            self.phi = math.sqrt(1 + 1.464 * (self.a / self.c)**1.65)
        else:
            self.phi = math.sqrt(1 + 1.464 * (self.c / self.a)**1.65)

    def get_f_theta_value(self, theta):
        if (self.a / 2 / self.c >= 0) and (self.a / 2 / self.c <= 0.5):
            self.f_theta = ((self.a / self.c)**2 * (math.cos(theta))**2 +
                            (math.sin(theta))**2)**0.25
        else:
            self.f_theta = ((self.c / self.a)**2 * (math.sin(theta))**2 +
                            (math.cos(theta))**2)**0.25

    def assign_key_flaw_dimensions_for_calculations(self):
        self.ri = self.cfg.pipe_properties['geometry']['Nominal_ID'] / 2 * 25.4
        self.B = self.cfg.Outer_Pipe['Geometry']['Design_WT'] * 25.4
        self.theta = math.radians(self.flaw.theta)

        self.W = 2 * self.ri
        if self.flaw.location == 'external_surface':
            self.radius = self.cfg.pipe_properties['geometry'][
                'Nominal_OD'] / 2 * 25.4
        elif self.flaw.location == 'internal_surface':
            self.radius = self.ri
        elif self.flaw.location == 'embedded':
            self.p = self.flaw.dimensions['p']
            self.B_dash = 2 * self.a + 2 * self.p

    # TODO combine approximate and actual flaw growth functions (get_approximate_flaw_growth_due_to_histograms, get_flaw_growth_due_to_histograms)
    def get_approximate_flaw_growth_due_to_histograms(self, histograms,
                                                      bending_factor):
        self.cfg.default['settings'][
            'stress_intensity_factor_solution'] = 'constant_values'

        if self.cfg.default['settings']['multi_process']:
            pp = ParallelProcessComponents(cfg=None)
            number_of_workers = pp.get_number_of_workers(50)

        self.approximate_flaw_growth_df = pd.DataFrame(
            columns=self.cfg.default['settings']
            ['flaw_growth_properties_columns'])
        f_o_s = self.cfg.default['settings']['factor_of_safety']
        service_life = max(
            self.cfg.default['settings']['service_life']['values'])
        for component_index in self.cfg.loading['histograms']['from_xlsx'][0][
                'component_index']:
            component_name = histograms.columns[component_index]
            print("Performing approximate initial flaw analysis for {0} ...".
                  format(component_name))

            if self.cfg.default['settings']['multi_process']:
                pool = Pool(number_of_workers)
                print("Performing Multi-process python using Pool workers {}".
                      format(number_of_workers))
                function_args = ((bending_factor, component_index, f_o_s,
                                  histograms, location_item, service_life,
                                  flaw_orientation)
                                 for location_item in
                                 self.cfg.default['settings']['location_array']
                                 for flaw_orientation in self.cfg.
                                 default['settings']['orientation_array'])
                flaw_growth_for_single_flaw_location_df_array = pool.starmap(
                    self.
                    get_approximate_flaw_growth_solution_for_single_flaw_location,
                    function_args)
                pool.close()
                for df in flaw_growth_for_single_flaw_location_df_array:
                    if not (len(df) == 0):
                        self.approximate_flaw_growth_df = pd.concat(
                            [self.approximate_flaw_growth_df, df], sort=True)
            else:
                for location_item in self.cfg.default['settings'][
                        'location_array']:
                    for flaw_orientation in self.cfg.default['settings'][
                            'orientation_array']:
                        allowable_initial_flaw_for_single_flaw_location_df = self.get_approximate_flaw_growth_solution_for_single_flaw_location(
                            bending_factor, component_index, f_o_s, histograms,
                            location_item, service_life, flaw_orientation)
                        if not (len(
                                allowable_initial_flaw_for_single_flaw_location_df
                        ) == 0):
                            self.approximate_flaw_growth_df = pd.concat([
                                self.approximate_flaw_growth_df,
                                allowable_initial_flaw_for_single_flaw_location_df
                            ],
                                                                        sort=True
                                                                       )

            print("...... for {0} ... COMPLETE \n".format(component_name))

    def get_flaw_growth_due_to_histograms(self, histograms, bending_factor):

        self.cfg.default['settings'][
            'stress_intensity_factor_solution'] = 'table'
        if self.cfg.default['settings']['multi_process']:
            pp = ParallelProcessComponents(cfg=None)
            number_of_workers = pp.get_number_of_workers(50)

        self.flaw_growth_df = pd.DataFrame(columns=self.cfg.default['settings']
                                           ['flaw_growth_properties_columns'])
        f_o_s = self.cfg.default['settings']['factor_of_safety']
        for component_index in self.cfg.loading['histograms']['from_xlsx'][0][
                'component_index']:
            component_name = histograms.columns[component_index]
            print("Performing time marching initial flaw analysis for {0} ...".
                  format(component_name))

            if self.cfg.default['settings']['multi_process']:
                pool = Pool(number_of_workers)
                print("Performing Multi-process python using Pool workers {}".
                      format(number_of_workers))
                function_args = ((bending_factor, component_index, f_o_s,
                                  histograms, location_item, flaw_orientation)
                                 for location_item in
                                 self.cfg.default['settings']['location_array']
                                 for flaw_orientation in self.cfg.
                                 default['settings']['orientation_array'])
                flaw_growth_for_single_flaw_location_df_array = pool.starmap(
                    self.get_flaw_growth_for_single_flaw_location,
                    function_args)
                pool.close()
                for df in flaw_growth_for_single_flaw_location_df_array:
                    if not (len(df) == 0):
                        self.flaw_growth_df = pd.concat(
                            [self.flaw_growth_df, df], sort=True)
            else:
                for location_item in self.cfg.default['settings'][
                        'location_array']:
                    for flaw_orientation in self.cfg.default['settings'][
                            'orientation_array']:
                        allowable_initial_flaw_for_single_flaw_location_df = self.get_flaw_growth_for_single_flaw_location(
                            bending_factor, component_index, f_o_s, histograms,
                            location_item, flaw_orientation)
                        if not (len(
                                allowable_initial_flaw_for_single_flaw_location_df
                        ) == 0):
                            self.flaw_growth_df = pd.concat([
                                self.flaw_growth_df,
                                allowable_initial_flaw_for_single_flaw_location_df
                            ],
                                                            sort=True)

            print("...... for {0} ... COMPLETE \n".format(component_name))

    def get_flaw_growth_for_single_flaw_location(self, bending_factor,
                                                 component_index, f_o_s,
                                                 histograms, location_item,
                                                 flaw_orientation):
        self.flaw_growth_for_single_flaw_location_df = pd.DataFrame(
            columns=self.cfg.default['settings']
            ['flaw_growth_properties_columns'])
        self.flaw.location = location_item
        self.flaw.orientation = flaw_orientation
        component_name = histograms.columns[component_index]
        print(
            "            ..... for component flaw location: {1}, flaw orientation: {2} ..."
            .format(component_name, location_item, flaw_orientation))
        for c_index in range(0, len(self.cfg.default['settings']['c_array'])):
            self.flaw.dimensions['c'] = self.cfg.default['settings']['c_array'][
                c_index]
            df_temp = self.approximate_flaw_growth_df[
                (self.approximate_flaw_growth_df.component == component_name) &
                (self.approximate_flaw_growth_df.flaw_location == location_item)
                & (self.approximate_flaw_growth_df.flaw_orientation
                   == flaw_orientation) &
                (self.approximate_flaw_growth_df.initial_flaw_length
                 == self.flaw.dimensions['c'])].copy()
            try:
                if (len(df_temp) > 0) and (df_temp.initial_flaw_depth.iloc[0]
                                           is not None):
                    self.flaw.dimensions['a'] = round(
                        df_temp.initial_flaw_depth.iloc[0] * 0.75, 1)
                else:
                    self.flaw.dimensions['a'] = 0.8
            except:
                self.flaw.dimensions['a'] = 0.8
            service_life = 0
            solution_end_life = max(
                self.cfg.default['settings']['service_life']['values'])
            solution_step = self.cfg.default['settings']['service_life'][
                'solution_step']
            life_nsteps = int((solution_end_life * 4) / solution_step)
            self.get_flaw_fad_properties()
            self.get_K_r_allowable()
            self.flaw_growth_for_single_flaw_location_df.loc[len(
                self.flaw_growth_for_single_flaw_location_df)] = [
                    histograms.columns[component_index], service_life,
                    self.flaw.geometry, self.flaw.orientation,
                    self.flaw.location, self.flaw.theta,
                    self.flaw.dimensions['a'], self.flaw.dimensions['c'],
                    self.K_r, self.L_r, self.K_r_allowable
                ]

            for life_index in range(0, life_nsteps):
                if service_life >= 2 * solution_end_life:
                    solution_step = 4 * self.cfg.default['settings'][
                        'service_life']['solution_step']
                elif service_life >= solution_end_life:
                    solution_step = 2 * self.cfg.default['settings'][
                        'service_life']['solution_step']
                else:
                    solution_step = self.cfg.default['settings'][
                        'service_life']['solution_step']

                service_life = service_life + solution_step
                for histogram_row_index in range(0, len(histograms) - 1):
                    number_of_cycles = (histograms.iloc[histogram_row_index][
                        histograms.columns[component_index]] /
                                        2) * solution_step * f_o_s
                    if number_of_cycles > 0:
                        self.delta_P_m = 0
                        self.delta_P_b = (
                            histograms.iloc[histogram_row_index]['bins'] +
                            histograms.iloc[histogram_row_index + 1]['bins']
                        ) / 2 / 1000 * bending_factor
                        self.get_flaw_growth_from_one_cycle()

                        delta_a_per_bin = self.delta_a_per_cycle_stress * number_of_cycles
                        self.flaw.dimensions[
                            'a'] = self.flaw.dimensions['a'] + delta_a_per_bin
                        self.flaw.dimensions['c'] = self.flaw.dimensions['c']

                self.get_flaw_fad_properties()
                self.get_K_r_allowable()

                if self.K_r >= self.K_r_allowable:
                    break
                else:
                    self.flaw_growth_for_single_flaw_location_df.loc[len(
                        self.flaw_growth_for_single_flaw_location_df)] = [
                            histograms.columns[component_index], service_life,
                            self.flaw.geometry, self.flaw.orientation,
                            self.flaw.location, self.flaw.theta,
                            self.flaw.dimensions['a'],
                            self.flaw.dimensions['c'], self.K_r, self.L_r,
                            self.K_r_allowable
                        ]

        print(
            "            ..... for component flaw location: {1}, flaw orientation: {2} ... COMPLETE"
            .format(component_name, location_item, flaw_orientation))
        return self.flaw_growth_for_single_flaw_location_df

    def get_flaw_growth_from_one_cycle(self):
        self.get_flaw_fad_properties()
        if self.delta_K_I <= self.cfg.paris_curve['transfer_delta_K']:
            self.delta_a_per_cycle_stress = self.cfg.paris_curve['A_1'] * (
                self.delta_K_I)**self.cfg.paris_curve['m_1']
        else:
            self.delta_a_per_cycle_stress = self.cfg.paris_curve['A_2'] * (
                self.delta_K_I)**self.cfg.paris_curve['m_2']

        logging.info(
            "crack size_growth_ per cycle for stress range {0}  MPa is : {1} mm"
            .format(self.delta_P_b, self.delta_a_per_cycle_stress))

    def get_initial_allowable_flaw_for_life(self, histograms):
        self.initial_allowable_flaw_df = pd.DataFrame(
            columns=self.cfg.default['settings']
            ['flaw_growth_properties_columns'])

        for cfg_component_index in range(
                0,
                len(self.cfg.loading['histograms']['from_xlsx'][0]
                    ['component_index'])):
            component_index = self.cfg.loading['histograms']['from_xlsx'][0][
                'component_index'][cfg_component_index]
            component_name = histograms.columns[component_index]
            for location_item in self.cfg.default['settings']['location_array']:
                for orientation_item in self.cfg.default['settings'][
                        'orientation_array']:
                    c_array = self.flaw_growth_df.final_flaw_length.unique()
                    for flaw_length in c_array:
                        df_temp = self.flaw_growth_df[
                            (self.flaw_growth_df.component == component_name) &
                            (self.flaw_growth_df.flaw_location == location_item)
                            & (self.flaw_growth_df.flaw_orientation
                               == orientation_item) &
                            (self.flaw_growth_df.final_flaw_length
                             == flaw_length)]
                        for service_life in self.cfg.default['settings'][
                                'service_life']['values']:
                            if df_temp.service_life.max() >= service_life:
                                df_temp_service_life = df_temp_service_life = df_temp[
                                    df_temp.service_life <= (
                                        df_temp.service_life.max() -
                                        service_life)].tail(1).copy()
                                df_temp_service_life.service_life = service_life
                                self.initial_allowable_flaw_df = pd.concat(
                                    [
                                        self.initial_allowable_flaw_df,
                                        df_temp_service_life
                                    ],
                                    sort=True)

    def get_approximate_flaw_growth_solution_for_single_flaw_location(
            self, bending_factor, component_index, f_o_s, histograms,
            location_item, service_life, flaw_orientation):
        self.approximate_allowable_initial_flaw_analysis_single_flaw_location_df = pd.DataFrame(
            columns=self.cfg.default['settings']
            ['approximate_initial_flaw_properties_columns'])

        component_name = histograms.columns[component_index]
        self.flaw.location = location_item
        self.flaw.orientation = flaw_orientation
        print(
            "            ..... for component flaw location: {1}, flaw orientation: {2} ..."
            .format(component_name, location_item, flaw_orientation))
        a_start = self.cfg.default['settings']['a_array']['start']
        a_end = self.cfg.default['settings']['a_array']['end']
        a_step = self.cfg.default['settings']['a_array']['step']
        a_nsteps = int((a_end - a_start) / a_step + 1)
        for c in self.cfg.default['settings']['c_array']:
            self.approximate_flaw_growth_for_single_flaw_location_and_length_df = pd.DataFrame(
                columns=self.cfg.default['settings']
                ['approximate_initial_flaw_properties_columns'])
            self.flaw.dimensions['c'] = c
            for a_index in range(0, a_nsteps):
                a = a_start + a_index * a_step
                self.flaw.dimensions['a'] = a
                for histogram_row_index in range(0, len(histograms) - 1):
                    number_of_cycles = (histograms.iloc[histogram_row_index][
                        histograms.columns[component_index]] /
                                        2) * service_life * f_o_s
                    if number_of_cycles > 0:
                        self.delta_P_m = 0
                        self.delta_P_b = (
                            histograms.iloc[histogram_row_index]['bins'] +
                            histograms.iloc[histogram_row_index + 1]['bins']
                        ) / 2 / 1000 * bending_factor
                        self.get_flaw_growth_from_one_cycle()

                        delta_a_per_bin = self.delta_a_per_cycle_stress * number_of_cycles
                        self.flaw.dimensions[
                            'a'] = self.flaw.dimensions['a'] + delta_a_per_bin
                        self.flaw.dimensions['c'] = self.flaw.dimensions['c']

                self.get_K_r_allowable()
                self.approximate_flaw_growth_for_single_flaw_location_and_length_df.loc[
                    len(self.
                        approximate_flaw_growth_for_single_flaw_location_and_length_df
                       )] = [
                           histograms.columns[component_index], service_life,
                           self.flaw.geometry, self.flaw.orientation,
                           self.flaw.location, self.flaw.theta, a, c,
                           self.flaw.dimensions['a'], self.flaw.dimensions['c'],
                           self.K_r, self.L_r, self.K_r_allowable
                       ]

            for df_index in range(
                    0,
                    len(self.
                        approximate_flaw_growth_for_single_flaw_location_and_length_df
                       ) - 1):
                if self.approximate_flaw_growth_for_single_flaw_location_and_length_df.K_r[
                        df_index] > self.approximate_flaw_growth_for_single_flaw_location_and_length_df.K_r_allowable[
                            df_index]:
                    a_end = self.approximate_flaw_growth_for_single_flaw_location_and_length_df.initial_flaw_depth[
                        df_index]
                    # a_nsteps = int((a_end - a_start) / a_step + 1)
                    break

            if self.cfg.default['settings']['save_detailed_results']:
                self.approximate_flaw_growth_for_single_flaw_location_and_length_df.to_csv(
                    'results/len_{}.csv'.format(c))

            try:
                self.approximate_allowable_initial_flaw_analysis_single_flaw_location_df.loc[
                    len(self.approximate_allowable_initial_flaw_analysis_single_flaw_location_df)] = \
                self.approximate_flaw_growth_for_single_flaw_location_and_length_df.iloc[df_index-1]
            except:
                print(
                    "IndexError: single positional indexer is out-of-bounds for df_index: {}"
                    .format(df_index))

        print(
            "            ..... for component flaw location: {1}, flaw orientation: {2} ... COMPLETE"
            .format(component_name, location_item, flaw_orientation))
        return self.approximate_allowable_initial_flaw_analysis_single_flaw_location_df
