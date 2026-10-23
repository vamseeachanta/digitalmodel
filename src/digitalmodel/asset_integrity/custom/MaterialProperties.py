"""Custom material property definitions for asset integrity."""

import logging

from digitalmodel.units import Q_
from ..common.update_deep import update_deep_dictionary
from ..common.validation import finite_number


class MaterialProperties():

    def __init__(self, cfg):
        self.cfg = cfg

    def evaluate_material_properties(self):
        if self.cfg['Outer_Pipe'] != None:
            self.get_material_properties(pipe_flag='Outer_Pipe')
            self.set_up_temperature_derating(pipe_flag='Outer_Pipe')
            self.evaluate_mass_and_weight_properties(pipe_flag='Outer_Pipe')
        if self.cfg['Inner_Pipe'] != None:
            self.get_material_properties(pipe_flag='Inner_Pipe')
            self.set_up_temperature_derating(pipe_flag='Inner_Pipe')
            self.evaluate_mass_and_weight_properties(pipe_flag='Inner_Pipe')

    def set_up_temperature_derating(self, pipe_flag):
        for load_condition_index in range(0, len(self.cfg['Design'])):
            for code_index in range(
                    0, len(self.cfg['Design'][load_condition_index]['Code'])):
                specification_code = self.cfg['Design'][load_condition_index][
                    'Code'][code_index][pipe_flag]
                design = self.cfg['Design'][load_condition_index]
                supplied = design.get('Material', {}).get('temperature_derating', {}).get(
                    pipe_flag, {}).get(specification_code)
                basis = design.get('Material', {}).get('temperature_derating_basis', {}).get(
                    pipe_flag, {}).get(specification_code)
                result = self.temperatureDerating({
                    'Code': specification_code,
                    'temperature': design.get('Temperature', {}).get('Operating', {}).get(pipe_flag),
                    'temperature_derating': supplied,
                    'temperature_derating_basis': basis,
                })
                update_deep_dictionary(
                    self.cfg['Design'][load_condition_index], {
                        'Material': {
                            'temperature_derating': {
                                pipe_flag: {
                                    specification_code:
                                        result['temperature_derating']
                                }
                            }
                        }
                    })

                logging.info(
                    "Material: Temperature derate check performed for {0}, per code {1}"
                    .format(pipe_flag, specification_code))

    def temperatureDerating(self, data):
        # Code-specific temperature domains are not qualified in this implementation.
        # Capacity requires an explicitly supplied factor on the current design basis.
        factor = data.get('temperature_derating')
        finite_number(factor, 'qualified temperature derating factor', positive=True, maximum=1)
        finite_number(data.get('temperature'), 'qualified temperature', minimum=-float('inf'))
        basis = data.get('temperature_derating_basis')
        if not isinstance(basis, str) or not basis.strip():
            raise ValueError('qualified temperature factor requires an explicit temperature and basis')
        return dict(data, temperature_derating=factor)

    def get_material_properties(self, pipe_flag):
        pass
        # material = self.cfg[pipe_flag]['Material']['Material']
        # material_grade = self.cfg[pipe_flag]['Material_Grade']
        # self.cfg[pipe_flag]['Material'].update(self.cfg['Material'][material]['Grades'][material_grade])
        # self.cfg[pipe_flag]['Material'].update(self.cfg['Material'][material])

    def evaluate_mass_and_weight_properties(self, pipe_flag):
        material = self.cfg[pipe_flag]['Material']['Material']
        material_density = self.cfg['Material'][material]['Rho']
        for load_condition_index in range(0, len(self.cfg['Design'])):
            mass = {}
            mass['pipe'] = self.cfg[pipe_flag]['section_properties']['pipe'][
                'A'] * material_density * 12
            mass['Coupling'] = mass['pipe'] * self.cfg[pipe_flag][
                'Manufacturing']['Coupling Mass Ratio']
            mass['internal_fluid']= (self.cfg[pipe_flag]['section_properties']['pipe']['Ai'] \
                          * self.cfg['Design'][load_condition_index]['InternalFluid'][pipe_flag])*12
            mass['dry'] = mass['pipe'] + mass['Coupling'] + mass[
                'internal_fluid']
            mass['buoyancy'] = (self.cfg[pipe_flag]['section_properties']['pipe']['Ao'] \
            * self.cfg['Design'][load_condition_index]['ExternalFluid'][pipe_flag])*12
            mass['wet'] = mass['dry'] - mass['buoyancy']

            update_deep_dictionary(self.cfg['Design'][load_condition_index],
                                   {'mass': mass})
