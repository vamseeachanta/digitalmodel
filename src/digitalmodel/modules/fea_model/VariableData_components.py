class VariableData():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        variable_data = {}

        general = self.get_general()
        variable_data.update(general)

        relationData = self.get_relationData()
        variable_data.update(relationData)

        return variable_data

    def get_general(self):
        general = {
            "Name": "AxialStiffnessProfile"
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return general

    def get_relationData(self):

        data = []
        relationData = {"IndependentValue, DependentValue": data}

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_relationData_'
        property_group = relationData
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return relationData

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
