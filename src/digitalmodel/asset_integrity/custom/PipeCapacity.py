"""Custom pipe capacity methods for asset integrity assessments.

Collapse pressure per API TR 5C3 (performance properties of pipe used as casing
or tubing); burst per Barlow / thick-wall.
"""

import logging
import math
from copy import deepcopy
from ..common.validation import finite_number, solve_bracket

from digitalmodel.units import Q_


class PipeCapacity():

    def __init__(self, cfg):
        self.cfg = cfg

    def evaluate_pipe_wall(self):
        if self.cfg['Outer_Pipe'] != None:
            self.evaluate_load_conditions(pipe_flag='Outer_Pipe')
        if self.cfg['Inner_Pipe'] != None:
            self.evaluate_load_conditions(pipe_flag='Inner_Pipe')

    def evaluate_load_conditions(self, pipe_flag):
        designs = self.cfg.get('Design')
        if not designs:
            raise ValueError('Design requires a nonempty requested check set')
        for design in designs:
            if not design.get('Code'):
                raise ValueError('each Design requires a nonempty code check set')
            if design.get('Load Condition', {}).get(pipe_flag) not in (
                    'internal_pressure', 'external_pressure', 'collapse_propagation'):
                raise ValueError('unsupported or missing load condition for requested pipe')
            for code in design['Code']:
                if not isinstance(code.get(pipe_flag), str) or not code[pipe_flag].strip():
                    raise ValueError('each requested check requires a specification code')
        for load_condition_index in range(0, len(self.cfg['Design'])):
            for code_index in range(
                    0, len(self.cfg['Design'][load_condition_index]['Code'])):
                self.design_condition_index = load_condition_index
                specification_code = self.cfg['Design'][load_condition_index][
                    'Code'][code_index][pipe_flag]
                logging.info("Finished processing results for code: {0}".format(
                    specification_code))
                load_condition = self.cfg['Design'][load_condition_index][
                    'Load Condition'][pipe_flag]
                if load_condition == 'internal_pressure':
                    self.internal_pressure(pipe_flag, specification_code,
                                           load_condition_index)
                if load_condition == 'external_pressure':
                    self.external_pressure(pipe_flag, specification_code,
                                           load_condition_index)
                if load_condition == 'collapse_propagation':
                    self.collapse_propagation(pipe_flag, specification_code,
                                              load_condition_index)

    def internal_pressure(self, pipe_flag, specification_code,
                          load_condition_index):
        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT']
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]

        minimum_thickness, pressure = self.internal_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness)
        self.update_pressure_results(minimum_thickness,
                                     pipe_flag,
                                     pressure,
                                     specification_code,
                                     thickness,
                                     load_condition,
                                     custom_tag='Zero Corrosion Allowance')

        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT'] - self.cfg[
            pipe_flag]['Geometry']['Corrosion_Allowance']
        minimum_thickness, pressure = self.internal_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness)

        self.update_pressure_results(
            minimum_thickness +
            self.cfg[pipe_flag]['Geometry']['Corrosion_Allowance'],
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag='With Corrosion Allowance')

    def collapse_propagation(self, pipe_flag, specification_code,
                             load_condition_index):
        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT']
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]

        minimum_thickness, pressure = self.collapse_propagation_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness)
        self.update_pressure_results(minimum_thickness,
                                     pipe_flag,
                                     pressure,
                                     specification_code,
                                     thickness,
                                     load_condition,
                                     custom_tag='Zero Corrosion Allowance')

        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT'] - self.cfg[
            pipe_flag]['Geometry']['Corrosion_Allowance']
        minimum_thickness, pressure = self.collapse_propagation_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness)

        self.update_pressure_results(
            minimum_thickness +
            self.cfg[pipe_flag]['Geometry']['Corrosion_Allowance'],
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag='With Corrosion Allowance')

    def external_pressure(self, pipe_flag, specification_code,
                          load_condition_index):
        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT']
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]

        minimum_thickness, pressure = self.external_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness)
        self.update_pressure_results(minimum_thickness,
                                     pipe_flag,
                                     pressure,
                                     specification_code,
                                     thickness,
                                     load_condition,
                                     custom_tag='Zero Corrosion Allowance')

        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT'] - self.cfg[
            pipe_flag]['Geometry']['Corrosion_Allowance']
        minimum_thickness, pressure = self.external_pressure_specification_code_based_evaluation(
            load_condition_index, pipe_flag, specification_code, thickness)

        self.update_pressure_results(
            minimum_thickness +
            self.cfg[pipe_flag]['Geometry']['Corrosion_Allowance'],
            pipe_flag,
            pressure,
            specification_code,
            thickness,
            load_condition,
            custom_tag='With Corrosion Allowance')

    def update_pressure_results(self, minimum_thickness, pipe_flag, pressure,
                                specification_code, thickness, load_condition,
                                custom_tag):

        for name, value in (('minimum wall', minimum_thickness), ('capacity', pressure),
                            ('evaluated wall', thickness)):
            finite_number(value, name, positive=True)
        result = self.cfg.setdefault('Result', {}).setdefault(pipe_flag, {}).setdefault(
            load_condition, {}).setdefault(specification_code, {})
        records = result.setdefault('design_conditions', [])
        records.append({
            'design_condition_index': getattr(self, 'design_condition_index', len(records)),
            'tag': custom_tag, 'thickness': thickness,
            'minimum_thickness': minimum_thickness, 'capacity': pressure,
            'status': 'EVALUATED',
            'criterion': 'governing required wall is maximum; governing capacity is minimum across every design condition',
        })
        # Keep the legacy lookup keys as conservative envelopes, using raw values.
        selected = [row for row in records if row['tag'] == custom_tag]
        for key, value in (
                ('thickness', min(row['thickness'] for row in selected)),
                ('minimum_thickness', max(row['minimum_thickness'] for row in selected)),
                ('Design_WT_Max_Pressure', min(row['capacity'] for row in selected))):
            result.setdefault(key, {})[custom_tag] = value

    def _configuration_at_wall(self, pipe_flag, thickness):
        finite_number(thickness, 'evaluated wall', positive=True)
        cfg = deepcopy(self.cfg)
        geometry = cfg[pipe_flag]['Geometry']
        finite_number(geometry['Nominal_OD'], 'outside diameter', positive=True)
        if 2 * thickness >= geometry['Nominal_OD']:
            raise ValueError('evaluated wall must be less than the outside radius')
        geometry['Design_WT'] = thickness
        geometry['Nominal_ID'] = geometry['Nominal_OD'] - 2 * thickness
        return cfg

    def internal_pressure_specification_code_based_evaluation(
            self, load_condition_index, pipe_flag, specification_code,
            thickness):
        if 'ASME B31' in specification_code:
            pressure = self.evaluate_burst_pressure_modified_burlow_equation(
                pipe_flag, specification_code, load_condition_index, thickness)
            minimum_thickness = self.evaluate_burst_minimum_thickness_modified_burlow_equation(
                pipe_flag, specification_code, load_condition_index)

        if 'API STD 2RD-2013' in specification_code:
            pressure = self.evaluate_burst_pressure_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index, thickness)
            minimum_thickness = self.evaluate_burst_minimum_thickness_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index)

        if 'API RP 1111-2009' in specification_code:
            pressure = self.evaluate_burst_pressure_API_RP_1111(
                pipe_flag, specification_code, load_condition_index, thickness)
            minimum_thickness = self.evaluate_burst_minimum_thickness_API_RP_1111(
                pipe_flag, specification_code, load_condition_index)

        if 'API RP 16Q-2017' in specification_code:
            api_rp_16q = API_RP_16Q(self._configuration_at_wall(pipe_flag, thickness))
            pressure = api_rp_16q.get_burst_pressure(pipe_flag,
                                                     load_condition_index)
            minimum_thickness = api_rp_16q.get_burst_minimum_thickness(
                pipe_flag, load_condition_index)

        if '30 CFR Part 250' in specification_code:
            cfr_30_part_250 = CFR_30_Part_250(self._configuration_at_wall(pipe_flag, thickness))
            pressure = cfr_30_part_250.get_burst_pressure(
                pipe_flag, load_condition_index)
            minimum_thickness = cfr_30_part_250.get_burst_minimum_thickness(
                pipe_flag, load_condition_index)

        return minimum_thickness, pressure

    def evaluate_burst_pressure_API_STD_2RD(self, pipe_flag, specification_code,
                                            load_condition_index, thickness):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        pressure = self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                *self.cfg['DesignFactors'][specification_code][load_condition]['k']['API 5L'] \
                *(self.cfg[pipe_flag]['Material']['SMYS'] + self.cfg[pipe_flag]['Material']['SMUS']) \
                *math.log(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                          /(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] - 2*thickness))
        return pressure

    def evaluate_burst_pressure_API_RP_1111(self, pipe_flag, specification_code,
                                            load_condition_index, thickness):
        d_over_t = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] / thickness
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]

        if d_over_t <= self.cfg['DesignFactors'][specification_code][
                'D_over_T_Trasition_Ratio']:
            pressure = self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                *0.45*(self.cfg[pipe_flag]['Material']['SMYS'] + self.cfg[pipe_flag]['Material']['SMUS']) \
                *math.log(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                          /(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] - 2*thickness))
        else:
            pressure = self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                *0.90*(self.cfg[pipe_flag]['Material']['SMYS'] + self.cfg[pipe_flag]['Material']['SMUS']) \
                *(thickness /(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] - thickness))

        return pressure

    def evaluate_burst_minimum_thickness_API_RP_1111(self, pipe_flag,
                                                     specification_code,
                                                     load_condition_index):
        d_over_t = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] / thickness
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]

        if d_over_t <= self.cfg['DesignFactors'][specification_code][
                'D_over_T_Trasition_Ratio']:
            burst_by_factored_strength = (self.cfg['Design'][load_condition_index]['InternalPressure'][pipe_flag] \
                                           - self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]) \
                        /0.45/self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                        /(self.cfg[pipe_flag]['Material']['SMYS'] + self.cfg[pipe_flag]['Material']['SMUS'])

            minimum_thickness = 0.5*(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                     - self.cfg[pipe_flag]['Geometry']['Nominal_OD']/math.exp(burst_by_factored_strength))
        else:
            burst_by_factored_strength = (self.cfg['Design'][load_condition_index]['InternalPressure'][pipe_flag] \
                                           - self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]) \
                        /0.90/self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                        /(self.cfg[pipe_flag]['Material']['SMYS'] + self.cfg[pipe_flag]['Material']['SMUS'])

            minimum_thickness = burst_by_factored_strength*self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                     /(1 + burst_by_factored_strength)

        return minimum_thickness

    def evaluate_burst_minimum_thickness_API_STD_2RD(self, pipe_flag,
                                                     specification_code,
                                                     load_condition_index):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        burst_by_factored_strength = (self.cfg['Design'][load_condition_index]['InternalPressure'][pipe_flag] \
                                       - self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]) \
                    /self.cfg['DesignFactors'][specification_code][load_condition]['k']['API 5L'] \
                    /self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                    /(self.cfg[pipe_flag]['Material']['SMYS'] + self.cfg[pipe_flag]['Material']['SMUS'])

        minimum_thickness = 0.5*(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                 - self.cfg[pipe_flag]['Geometry']['Nominal_OD']/math.exp(burst_by_factored_strength))

        return minimum_thickness

    def evaluate_burst_pressure_modified_burlow_equation(
            self, pipe_flag, specification_code, load_condition_index,
            thickness):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        stress_hoop_allowable = self.cfg[pipe_flag]['Material']['SMYS'] \
                      *self.cfg['DesignFactors'][specification_code][load_condition] \
                      *self.cfg[pipe_flag]['Material']['WeldFactor']['Seamless'] \
                      *self.cfg['Design'][load_condition_index]['Material']['temperature_derating'][pipe_flag][specification_code]
        d_over_t = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] / thickness

        if d_over_t >= self.cfg['DesignFactors'][specification_code][
                'D_over_T_Trasition_Ratio']:
            pressure = 2*thickness*stress_hoop_allowable/self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                       + self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]
        else:
            pressure = 2*thickness*stress_hoop_allowable/(self.cfg[pipe_flag]['Geometry']['Nominal_OD'] - thickness) \
                       + self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]

        return pressure

    def evaluate_burst_minimum_thickness_modified_burlow_equation(
            self, pipe_flag, specification_code, load_condition_index):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        stress_hoop = self.cfg[pipe_flag]['Material']['SMYS']\
                      *self.cfg['DesignFactors'][specification_code][load_condition]\
                      *self.cfg[pipe_flag]['Material']['WeldFactor']['Seamless'] \
                      *self.cfg['Design'][load_condition_index]['Material']['temperature_derating'][pipe_flag][specification_code]

        d_over_t = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] / self.cfg[
            pipe_flag]['Geometry']['Design_WT']
        # TODO Need itreation to ensure new d_over_t still satisfies the criteria
        if d_over_t >= self.cfg['DesignFactors'][specification_code][
                'D_over_T_Trasition_Ratio']:
            minimum_thickness = (self.cfg['Design'][load_condition_index]['InternalPressure'][pipe_flag]
                                 - self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag])\
                                * self.cfg[pipe_flag]['Geometry']['Nominal_OD']/(2*stress_hoop)
        else:
            minimum_thickness = (self.cfg['Design'][load_condition_index]['InternalPressure'][pipe_flag] \
                                 - self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]) \
                                *self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                                /(2*stress_hoop + (self.cfg['Design'][load_condition_index]['InternalPressure'][pipe_flag] \
                                           - self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]))

        return minimum_thickness

    def external_pressure_specification_code_based_evaluation(
            self, load_condition_index, pipe_flag, specification_code,
            thickness):
        if 'API STD 2RD-2013' in specification_code:
            pressure = self.evaluate_collapse_pressure_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index, thickness)
            minimum_thickness = self.evaluate_collapse_minimum_thickness_API_STD_2RD(
                pipe_flag, specification_code, load_condition_index)

        if 'API TR 5C3-2018' in specification_code:
            api_tr_5c3 = API_TR_5C3(self._configuration_at_wall(pipe_flag, thickness), pipe_flag, load_condition_index)
            pressure = api_tr_5c3.get_collapse_pressure()
            minimum_thickness = api_tr_5c3.get_collapse_minimum_thickness()

        return minimum_thickness, pressure

    def evaluate_collapse_pressure_API_STD_2RD(self, pipe_flag,
                                               specification_code,
                                               load_condition_index, thickness):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]

        d_over_t = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] / thickness
        t_over_d = 1 / d_over_t
        material = self.cfg[pipe_flag]['Material']['Material']
        E = self.cfg['Material'][material]['E']
        Poissionsratio = self.cfg['Material'][material]['Poissionsratio']

        Py = 2 * self.cfg[pipe_flag]['Material']['SMYS'] * t_over_d
        Pel = 2 * E * (t_over_d**3) / (1 - Poissionsratio**2)
        Pc = Py * Pel / (math.sqrt(Py**2 + Pel**2))
        Pe = Pc * self.cfg['DesignFactors'][specification_code][load_condition][
            'Fd']

        return Pe

    def evaluate_collapse_minimum_thickness_API_STD_2RD(self, pipe_flag,
                                                        specification_code,
                                                        load_condition_index):
        d_over_t = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] / self.cfg[
            pipe_flag]['Geometry']['Design_WT']
        t_over_d = 1 / d_over_t
        material = self.cfg[pipe_flag]['Material']['Material']
        E = self.cfg['Material'][material]['E']
        Poissionsratio = self.cfg['Material'][material]['Poissionsratio']
        # TODO Calculation to be completed.
        return 100

    def collapse_propagation_specification_code_based_evaluation(
            self, load_condition_index, pipe_flag, specification_code,
            thickness):
        if 'API STD 2RD-2013' in specification_code or 'API RP 1111-2009' in specification_code:
            pressure = self.evaluate_collapse_propagation_pressure_API_RP_1111(
                pipe_flag, specification_code, load_condition_index, thickness)
            minimum_thickness = self.evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
                pipe_flag, specification_code, load_condition_index)

        return minimum_thickness, pressure

    def evaluate_collapse_propagation_pressure_API_RP_1111(
            self, pipe_flag, specification_code, load_condition_index,
            thickness):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        Pp = 24*self.cfg[pipe_flag]['Material']['SMYS']\
                              *((thickness/self.cfg[pipe_flag]['Geometry']['Nominal_OD'])**2.4)

        pressure = Pp * self.cfg['DesignFactors'][specification_code][
            load_condition]['Fp']

        return pressure

    def evaluate_collapse_propagation_minimum_thickness_API_RP_1111(
            self, pipe_flag, specification_code, load_condition_index):
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        if self.cfg['Design'][load_condition_index]['ExternalPressure'][
                pipe_flag] == None:
            Po = self.cfg['Design'][load_condition_index]['ExternalPressure'][
                pipe_flag]
        else:
            Po = self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]['fluid_density']\
                *self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]['fluid_column']*12

        Pp = Po / self.cfg['DesignFactors'][specification_code][load_condition][
            'Fp']
        minimum_thickness = ((Pp/24/self.cfg[pipe_flag]['Material']['SMYS'])**(1/2.4)) \
                        *self.cfg[pipe_flag]['Geometry']['Nominal_OD']

        return minimum_thickness


class InternalPressureMethods():
    pass


class OtherMethodsTobeIncorporated():
    # TODO LogitudinalStress, EquivalentStress
    def ASMEB314InternalPressure(data):
        DesignPressure = 2 * data['S'] * data['t'] / data['D'] * data[
            'F'] * data['WeldFactor'] * data['T']
        Stress_Hoop = data['S'] * data['F'] * data['WeldFactor'] * data['T']

        if data['D'] / data['t'] >= 30:
            ThicknessMinimum_Pressure = (
                data['Pi'] - data['Po']) * data['D'] / (2 * Stress_Hoop)
        else:
            ThicknessMinimum_Pressure = (data['Pi'] -
                                         data['Po']) * data['D'] / (
                                             2 * Stress_Hoop +
                                             (data['Pi'] - data['Po']))

        data.update({
            "MaximumDesignPressure": DesignPressure,
            "MinimumWallThickness_Pressure": ThicknessMinimum_Pressure
        })
        return data

    def ASMEB314LogitudinalStress(data):
        if data['Condition'] == "Restrained":
            Stress_Elongation = -data['E'] * data['Alpha'] * (data['T1'] -
                                                              data['T2'])
            Stress_Longitudinal = data['S'] * data['F'] * data[
                'WeldFactor'] * data['T']
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Hoop = (Stress_Longitudinal - Stress_Elongation -
                           Stress_Moment -
                           Stress_Axial) / data['Poissionsratio']

            DbytRatio = data['D'] / data['t']
            if DbytRatio >= 30:
                ThicknessMinimum_Longitudinal = (
                    data['Pi'] - data['Po']) * data['D'] / (2 * Stress_Hoop)
            else:
                ThicknessMinimum_Longitudinal = (data['Pi'] -
                                                 data['Po']) * data['D'] / (
                                                     2 * Stress_Hoop +
                                                     (data['Pi'] - data['Po']))

            data.update({"MinimumWallThickness_Longitudinal": ThicknessMinimum_Longitudinal, \
            "Stress_Elongation": Stress_Elongation, "Stress_Logitudinal": Stress_Longitudinal,
            "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
            "Stress_Moment": Stress_Moment})

            return data

    def ASMEB314EquivalentStress(data):
        if data['Condition'] == "Restrained":
            Stress_Elongation = -data['E'] * data['Alpha'] * (data['T1'] -
                                                              data['T2'])
            Stress_Equivalent = data['S'] * data['F'] * data[
                'WeldFactor'] * data['T']
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Torsion = 0
            Stress_Hoop = ((-1*math.sqrt(Stress_Equivalent**2/2 - Stress_Torsion)/(data['Poissionsratio']-1)) \
                            - Stress_Elongation - Stress_Moment)/(1+data['Poissionsratio'])

            DbytRatio = data['D'] / data['t']
            if DbytRatio >= 30:
                ThicknessMinimum_Equivalent = (
                    data['Pi'] - data['Po']) * data['D'] / (2 * Stress_Hoop)
            else:
                ThicknessMinimum_Equivalent = (data['Pi'] -
                                               data['Po']) * data['D'] / (
                                                   2 * Stress_Hoop +
                                                   (data['Pi'] - data['Po']))

            data.update({"MinimumWallThickness_Equivalent": ThicknessMinimum_Equivalent, \
            "Stress_Elongation": Stress_Elongation,
            "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
            "Stress_Moment": Stress_Moment, "Stress_Equivalent" : Stress_Equivalent})

            return data

    def ASMEB318InternalPressure(data):
        DesignPressure = 2 * data['S'] * data['t'] / data['D'] * data[
            'F'] * data['WeldFactor'] * data['T']

        DbytRatio = data['D'] / data['t']
        if DbytRatio >= 30:
            ThicknessMinimum_Pressure = (
                data['Pi'] - data['Po']) * data['D'] / (
                    2 * data['S'] * data['F'] * data['WeldFactor'] * data['T'])
        else:
            ThicknessMinimum_Pressure = (
                data['Pi'] - data['Po']) * data['D'] / (
                    2 * data['S'] * data['F'] * data['WeldFactor'] * data['T'] +
                    (data['Pi'] - data['Po']))

        data.update({
            "MaximumDesignPressure": DesignPressure,
            "MinimumWallThickness": ThicknessMinimum_Pressure
        })

        return data

    def ASMEB318LogitudinalStress(data):
        if data['Condition'] == "Restrained":
            Stress_Elongation = -data['E'] * data['Alpha'] * (data['T1'] -
                                                              data['T2'])
            Stress_Longitudinal = data['S'] * data['F'] * data[
                'WeldFactor'] * data['T']
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Hoop = (Stress_Longitudinal - Stress_Elongation -
                           Stress_Moment) / data['Poissionsratio']

            DbytRatio = data['D'] / data['t']
            if DbytRatio >= 30:
                ThicknessMinimum_Longitudinal = (
                    data['Pi'] - data['Po']) * data['D'] / (2 * Stress_Hoop)
            else:
                ThicknessMinimum_Longitudinal = (data['Pi'] -
                                                 data['Po']) * data['D'] / (
                                                     2 * Stress_Hoop +
                                                     (data['Pi'] - data['Po']))

            data.update({"MinimumWallThickness_Longitudinal": ThicknessMinimum_Longitudinal, \
            "Stress_Elongation": Stress_Elongation, "Stress_Logitudinal": Stress_Longitudinal,
            "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
            "Stress_Moment": Stress_Moment})

            return data

    def ASMEB318EquivalentStress(data):
        if data['Condition'] == "Restrained":
            Stress_Elongation = -data['E'] * data['Alpha'] * (data['T1'] -
                                                              data['T2'])
            Stress_Equivalent = data['S'] * data['F'] * data[
                'WeldFactor'] * data['T']
            Stress_Moment = 0
            Stress_Axial = 0
            Stress_Torsion = 0
            Stress_Hoop = ((-1*math.sqrt(Stress_Equivalent**2/2 - Stress_Torsion)/(data['Poissionsratio']-1)) \
                            - Stress_Elongation - Stress_Moment)/(1+data['Poissionsratio'])

            DbytRatio = data['D'] / data['t']
            if DbytRatio >= 30:
                ThicknessMinimum_Equivalent = (
                    data['Pi'] - data['Po']) * data['D'] / (2 * Stress_Hoop)
            else:
                ThicknessMinimum_Equivalent = (data['Pi'] -
                                               data['Po']) * data['D'] / (
                                                   2 * Stress_Hoop +
                                                   (data['Pi'] - data['Po']))

            data.update({"MinimumWallThickness_Equivalent": ThicknessMinimum_Equivalent, \
            "Stress_Elongation": Stress_Elongation,
            "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
            "Stress_Moment": Stress_Moment, "Stress_Equivalent" : Stress_Equivalent})

            return data


class API_RP_2RD():

    def __init__(self, cfg):
        self.cfg = cfg


class API_RP_16Q():

    def __init__(self, cfg):
        import datetime
        self.cfg = cfg
        self.edition = 'second'
        self.release_date = datetime.date(2017, 4, 1)

    def get_burst_minimum_thickness(self, pipe_flag, load_condition_index):
        vm = VonMises_Pipe(self.cfg, pipe_flag, load_condition_index)
        minimum_thickness = vm.get_burst_minimum_thickness()

        return minimum_thickness

    def get_burst_pressure(self, pipe_flag, load_condition_index):
        vm = VonMises_Pipe(self.cfg, pipe_flag, load_condition_index)
        pressure = vm.get_burst_pressure()

        return pressure


class API_TR_5C3():

    def __init__(self, cfg, pipe_flag, load_condition_index):
        import datetime
        self.cfg = cfg
        self.edition = 'second'
        self.release_date = datetime.date(2018, 6, 1)
        self.init_calculate_required_properties(pipe_flag, load_condition_index)

    def init_calculate_required_properties(self, pipe_flag,
                                           load_condition_index):
        self.pipe_flag = pipe_flag
        self.D_o = self.cfg[pipe_flag]['Geometry']['Nominal_OD']
        self.D_i = self.cfg[pipe_flag]['Geometry']['Nominal_ID']
        self.t = self.cfg[pipe_flag]['Geometry']['Design_WT']
        self.loads = self.cfg['Design'][load_condition_index]
        self.P_o = self.loads['ExternalPressure'][pipe_flag]
        self.P_i = self.loads['InternalPressure'][pipe_flag]

    def get_collapse_pressure(self, t=None):
        if t == None:
            t = self.t

        d_over_t = self.D_o / t
        Pe = 46.95E+6 / (d_over_t * (d_over_t - 1)**2)

        return Pe

    def get_collapse_minimum_thickness(self, external_pressure=None):
        demand = self.loads['ExternalPressure'][self.pipe_flag] if external_pressure is None else external_pressure
        finite_number(demand, 'external pressure demand', positive=True)
        finite_number(self.D_o, 'outside diameter', positive=True)
        finite_number(self.t, 'wall', positive=True)
        if 2 * self.t >= self.D_o:
            raise ValueError('collapse wall must be less than the outside radius')
        ratio = self.D_o / self.t
        ratios = [ratio / 2 + (ratio * 30 - ratio / 2) / 100 * step
                  for step in range(1, 100)]
        pressures = [self.get_collapse_pressure(self.D_o / value) for value in ratios]
        solved = solve_bracket(demand, pressures[::-1], ratios[::-1],
                               lambda value: self.get_collapse_pressure(self.D_o / value))
        return self.D_o / solved


class VonMises_Pipe():

    def __init__(self, cfg, pipe_flag, load_condition_index):
        self.cfg = cfg
        self.init_calculate_required_properties(pipe_flag, load_condition_index)
        self.vm_stress = self.get_stress()

    def init_calculate_required_properties(self, pipe_flag,
                                           load_condition_index):
        self.pipe_flag = pipe_flag
        self.D_o = self.cfg[pipe_flag]['Geometry']['Nominal_OD']
        self.D_i = self.cfg[pipe_flag]['Geometry']['Nominal_ID']
        self.t = self.cfg[pipe_flag]['Geometry']['Design_WT']
        self.loads = self.cfg['Design'][load_condition_index]
        self.P_o = self.loads['ExternalPressure'][pipe_flag]
        self.P_i = self.loads['InternalPressure'][pipe_flag]

        self.section_properties = self.cfg.equivalent_pipe[
            'section_properties']['pipe']

        self.M = self.loads['BendingMoment'] * 8.850746 * 1000
        if self.loads['AxialForce'] is None:
            self.T_eff = Q_(self.loads['EffectiveTension'], 'kN').to('lbf').magnitude
            self.T = self.T_eff + self.P_i * self.D_i \
                     - self.P_o * self.D_o
        elif self.loads['EffectiveTension'] is None:
            self.T = Q_(self.loads['AxialForce'], 'kN').to('lbf').magnitude
            self.T_eff = self.loads['AxialForce'] - self.P_i * self.D_i \
                         + self.P_o * self.D_o
        else:
            raise (
                "Set Axial force or Effective tension to None to proceed with calculation"
            )

    def get_stress(self):
        import copy

        from ..common.data import AttributeDict
        from .PipeSizing import PipeSizing
        cfg_temp = AttributeDict(copy.deepcopy(self.cfg))
        cfg_temp[self.pipe_flag]['Geometry']['Design_WT'] = self.t
        cfg_temp[self.pipe_flag]['Geometry']['Nominal_ID'] = cfg_temp[
            'Outer_Pipe']['Geometry']['Nominal_OD'] - 2 * cfg_temp[
                'Outer_Pipe']['Geometry']['Design_WT']
        self.D_i = cfg_temp[self.pipe_flag]['Geometry']['Nominal_ID']
        Pipe = PipeSizing(cfg_temp)
        Pipe.evaluate_pipe_system_properties()
        section_properties = cfg_temp[self.pipe_flag]['section_properties'][
            'pipe']

        import math
        sigma_radial = -1 * (self.P_o * self.D_o +
                             self.P_i * self.D_i) / (self.D_o + self.D_i)
        sigma_hoop = (self.P_i - self.P_o) * self.D_o / (2 * self.t) - self.P_i
        sigma_axial_addition = self.T / section_properties['A'] + \
                               (self.M / 2 / section_properties['I']) * (self.D_o - self.t)
        sigma_axial_subtraction = self.T / section_properties['A'] - \
                                  (self.M / 2 / section_properties['I']) * (self.D_o - self.t)
        sigma_under_root_addition = (sigma_radial - sigma_hoop)**2 + (
            sigma_hoop - sigma_axial_addition)**2 + (sigma_axial_addition -
                                                     sigma_radial)**2
        sigma_under_root_subtraction = (sigma_radial - sigma_hoop)**2 + (
            sigma_hoop - sigma_axial_subtraction)**2 + (
                sigma_axial_subtraction - sigma_radial)**2

        sigma_under_root = max(sigma_under_root_addition,
                               sigma_under_root_subtraction)
        sigma = math.sqrt(sigma_under_root / 2)

        self.D_i = self.cfg[self.pipe_flag]['Geometry']['Nominal_ID']

        return sigma

    def _stress_at(self, *, wall=None, pressure=None):
        old_wall, old_pressure = self.t, self.P_i
        try:
            if wall is not None:
                self.t = wall
            if pressure is not None:
                self.P_i = pressure
            return self.get_stress()
        finally:
            self.t, self.P_i = old_wall, old_pressure

    def get_burst_minimum_thickness(self, internal_pressure=None):
        demand = self.P_i if internal_pressure is None else internal_pressure
        finite_number(demand, 'internal pressure demand')
        finite_number(self.t, 'wall', positive=True)
        allowable = self.loads['allowable_stress_to_yield_ratio'] * self.section_properties['SMYS']
        finite_number(allowable, 'allowable stress', positive=True)
        walls = [self.t / 8 + (self.t * 2 - self.t / 8) / 30 * step
                 for step in range(1, 30)]
        stresses = [self._stress_at(wall=wall, pressure=demand) for wall in walls]
        return solve_bracket(allowable, stresses[::-1], walls[::-1],
                             lambda wall: self._stress_at(wall=wall, pressure=demand))

    def get_burst_pressure(self, t=None):
        wall = self.t if t is None else t
        finite_number(wall, 'evaluated wall', positive=True)
        finite_number(self.P_i, 'internal pressure demand')
        allowable = self.loads['allowable_stress_to_yield_ratio'] * self.section_properties['SMYS']
        finite_number(allowable, 'allowable stress', positive=True)
        if self.P_i > 200:
            steps, lower, upper = 30, self.P_i / 2, self.P_i * 2
        else:
            steps, lower, upper = 100, 0, 25000
        pressures = [lower + (upper - lower) / steps * (step - 1)
                     for step in range(1, steps)]
        stresses = [self._stress_at(wall=wall, pressure=pressure) for pressure in pressures]
        return solve_bracket(allowable, stresses, pressures,
                             lambda pressure: self._stress_at(wall=wall, pressure=pressure))


class CFR_30_Part_250():

    def __init__(self, cfg):
        import datetime
        self.cfg = cfg
        self.specification_code = '30 CFR Part 250'

    def get_burst_minimum_thickness(self, pipe_flag, load_condition_index):
        specification_code = self.specification_code
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        internal_pressure = self.cfg['Design'][load_condition_index][
            'InternalPressure'][pipe_flag]
        external_pressure = self.cfg['Design'][load_condition_index][
            'ExternalPressure'][pipe_flag]
        stress_hoop_allowable = self.cfg[pipe_flag]['Material']['SMYS'] \
                      *self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                      *self.cfg[pipe_flag]['Material']['WeldFactor']['Seamless'] \
                      *self.cfg['Design'][load_condition_index]['Material']['temperature_derating'][pipe_flag][specification_code]

        minimum_thickness = (internal_pressure - external_pressure) * self.cfg[
            pipe_flag]['Geometry']['Nominal_OD'] / 2 / stress_hoop_allowable

        return minimum_thickness

    def get_burst_pressure(self, pipe_flag, load_condition_index):
        specification_code = self.specification_code
        load_condition = self.cfg['Design'][load_condition_index][
            'Load Condition'][pipe_flag]
        thickness = self.cfg[pipe_flag]['Geometry']['Design_WT']
        stress_hoop_allowable = self.cfg[pipe_flag]['Material']['SMYS'] \
                      *self.cfg['DesignFactors'][specification_code][load_condition]['Fd'] \
                      *self.cfg[pipe_flag]['Material']['WeldFactor']['Seamless'] \
                      *self.cfg['Design'][load_condition_index]['Material']['temperature_derating'][pipe_flag][specification_code]

        pressure = 2*thickness*stress_hoop_allowable/self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                   + self.cfg['Design'][load_condition_index]['ExternalPressure'][pipe_flag]

        return pressure
