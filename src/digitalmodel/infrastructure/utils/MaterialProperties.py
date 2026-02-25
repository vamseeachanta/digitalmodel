import logging

from common.update_deep import update_deep_dictionary


class MaterialProperties():
    def __init__(self, cfg):
        self.cfg = cfg

    def evaluate_material_properties(self):
        if self.cfg['Outer_Pipe'] != None:
            self.get_material_properties(pipe_flag = 'Outer_Pipe')
            self.set_up_temperature_derating(pipe_flag = 'Outer_Pipe')
            self.evaluate_mass_and_weight_properties(pipe_flag = 'Outer_Pipe')
        if self.cfg['Inner_Pipe'] != None:
            self.get_material_properties(pipe_flag = 'Inner_Pipe')
            self.set_up_temperature_derating(pipe_flag = 'Inner_Pipe')
            self.evaluate_mass_and_weight_properties(pipe_flag = 'Inner_Pipe')


    def set_up_temperature_derating(self, pipe_flag):
        for load_condition_index in range(0, len(self.cfg['Design'])):
            for code_index in range(0, len(self.cfg['Design'][load_condition_index]['Code'])):
                specification_code = self.cfg['Design'][load_condition_index]['Code'][code_index][pipe_flag]
                result = self.temperatureDerating({"Code": specification_code})
                update_deep_dictionary(self.cfg['Design'][load_condition_index],
                {'Material':{'temperature_derating': {pipe_flag: {specification_code: result['temperature_derating']}}}})

                logging.info("Material: Temperature derate check performed for {0}, per code {1}".format(pipe_flag,
                                                                                                         specification_code))

    def temperatureDerating(self, data):
        if data['Code'] == "Other":
            if data['temperature'] > 50 and data['temperature'] <= 100:
                data['S'] = data['S'] - (25 - 0) / (100 - 50) * (data['temperature'] - 50) * 145.038
                data['U'] = data['U'] - (25 - 0) / (100 - 50) * (data['temperature'] - 50) * 145.038
            elif data['temperature'] > 100:
                data['S'] = data['S'] - (25 + (68 - 25) / (200 - 100) * (data['temperature'] - 100)) * 145.038
                data['U'] = data['U'] - (25 + (68 - 25) / (200 - 100) * (data['temperature'] - 100)) * 145.038

            return data
        if "ASME" in data['Code']:
            data['temperature_derating'] = 1
            return data

        if data['Code'] == "ASME B31.8":
            data['temperature_derating'] = 1
            return data

        if 'API STD 2RD-2013' in data['Code']:
            data['temperature_derating'] = 1
            return data

        if 'API RP 1111-2009' in data['Code']:
            data['temperature_derating'] = 1
            return data

        if 'API RP 16Q' in data['Code']:
            data['temperature_derating'] = 1
            return data

        if 'API TR 5C3' in data['Code']:
            data['temperature_derating'] = 1
            return data

        if '30 CFR Part 250' in data['Code']:
            data['temperature_derating'] = 1
            return data


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
            mass['pipe'] = self.cfg[pipe_flag]['section_properties']['pipe']['A']*material_density*12
            mass['Coupling'] = mass['pipe']*self.cfg[pipe_flag]['Manufacturing']['Coupling Mass Ratio']
            mass['internal_fluid']= (self.cfg[pipe_flag]['section_properties']['pipe']['Ai'] \
                          * self.cfg['Design'][load_condition_index]['InternalFluid'][pipe_flag])*12
            mass['dry'] =  mass['pipe'] + mass['Coupling'] + mass['internal_fluid']
            mass['buoyancy'] = (self.cfg[pipe_flag]['section_properties']['pipe']['Ao'] \
            * self.cfg['Design'][load_condition_index]['ExternalFluid'][pipe_flag])*12
            mass['wet'] = mass['dry'] - mass['buoyancy']

            update_deep_dictionary(self.cfg['Design'][load_condition_index], {'mass': mass})