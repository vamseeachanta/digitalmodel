import logging

from digitalmodel.units import Q_
from common.update_deep import update_deep_dictionary


class MaterialProperties():
    """Handles material property evaluation including temperature derating and mass calculations.

    Evaluates material properties for inner and outer pipes, performs
    temperature derating based on specification codes, and calculates
    mass and weight properties.

    Attributes:
        cfg: Configuration dictionary containing pipe and material data.
    """

    def __init__(self, cfg):
        """Initialize MaterialProperties with configuration.

        Args:
            cfg: Configuration dictionary with 'Outer_Pipe', 'Inner_Pipe',
                'Design', and 'Material' keys.
        """
        self.cfg = cfg

    def evaluate_material_properties(self):
        """Evaluate material properties for both inner and outer pipes.

        Processes material properties, temperature derating, and mass/weight
        calculations for each pipe that is defined in the configuration.
        """
        if self.cfg['Outer_Pipe'] != None:
            self.get_material_properties(pipe_flag = 'Outer_Pipe')
            self.set_up_temperature_derating(pipe_flag = 'Outer_Pipe')
            self.evaluate_mass_and_weight_properties(pipe_flag = 'Outer_Pipe')
        if self.cfg['Inner_Pipe'] != None:
            self.get_material_properties(pipe_flag = 'Inner_Pipe')
            self.set_up_temperature_derating(pipe_flag = 'Inner_Pipe')
            self.evaluate_mass_and_weight_properties(pipe_flag = 'Inner_Pipe')


    def set_up_temperature_derating(self, pipe_flag):
        """Set up temperature derating factors for all design load conditions.

        Args:
            pipe_flag: String identifier for the pipe ('Outer_Pipe' or 'Inner_Pipe').
        """
        for load_condition_index in range(0, len(self.cfg['Design'])):
            for code_index in range(0, len(self.cfg['Design'][load_condition_index]['Code'])):
                specification_code = self.cfg['Design'][load_condition_index]['Code'][code_index][pipe_flag]
                result = self.temperatureDerating({"Code": specification_code})
                update_deep_dictionary(self.cfg['Design'][load_condition_index],
                {'Material':{'temperature_derating': {pipe_flag: {specification_code: result['temperature_derating']}}}})

                logging.info("Material: Temperature derate check performed for {0}, per code {1}".format(pipe_flag,
                                                                                                         specification_code))

    def temperatureDerating(self, data):
        """Calculate temperature derating factor based on specification code.

        Applies code-specific temperature derating rules. For 'Other' codes,
        applies a temperature-dependent strength reduction. For ASME, API,
        and CFR codes, returns a derating factor of 1.

        Args:
            data: Dictionary with 'Code' key and optionally 'temperature',
                'S' (yield strength), and 'U' (ultimate strength) keys.

        Returns:
            dict: Updated data dictionary with 'temperature_derating' factor.
        """
        if data['Code'] == "Other":
            if data['temperature'] > 50 and data['temperature'] <= 100:
                data['S'] = data['S'] - (25 - 0) / (100 - 50) * (data['temperature'] - 50) * Q_(1, 'MPa').to('psi').magnitude
                data['U'] = data['U'] - (25 - 0) / (100 - 50) * (data['temperature'] - 50) * Q_(1, 'MPa').to('psi').magnitude
            elif data['temperature'] > 100:
                data['S'] = data['S'] - (25 + (68 - 25) / (200 - 100) * (data['temperature'] - 100)) * Q_(1, 'MPa').to('psi').magnitude
                data['U'] = data['U'] - (25 + (68 - 25) / (200 - 100) * (data['temperature'] - 100)) * Q_(1, 'MPa').to('psi').magnitude

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
        """Retrieve material properties for the specified pipe.

        Args:
            pipe_flag: String identifier for the pipe ('Outer_Pipe' or 'Inner_Pipe').

        Note:
            Currently a placeholder with implementation commented out.
        """
        pass
        # material = self.cfg[pipe_flag]['Material']['Material']
        # material_grade = self.cfg[pipe_flag]['Material_Grade']
        # self.cfg[pipe_flag]['Material'].update(self.cfg['Material'][material]['Grades'][material_grade])
        # self.cfg[pipe_flag]['Material'].update(self.cfg['Material'][material])

    def evaluate_mass_and_weight_properties(self, pipe_flag):
        """Calculate mass and weight properties for the specified pipe.

        Computes pipe mass, coupling mass, internal fluid mass, dry mass,
        buoyancy, and wet mass for each design load condition.

        Args:
            pipe_flag: String identifier for the pipe ('Outer_Pipe' or 'Inner_Pipe').
        """
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