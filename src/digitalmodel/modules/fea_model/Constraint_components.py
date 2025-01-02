class Constraint():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        constraint = {}

        general = self.get_general()
        constraint.update(general)

        DoFs = self.get_DOFs()
        constraint.update(DoFs)

        stiffness = self.get_Stiffness()
        constraint.update(stiffness)

        CharacteristicProperties = self.get_CharacteristicProperties()
        constraint.update(CharacteristicProperties)

        drawing = self.get_drawing()
        constraint.update(drawing)

        return constraint

    def get_general(self):
        general = {
            "Name": "Rotation",
            "Connection": "SALM",
            "InitialPosition": [0, 0, 1.75],
            "InitialAttitude": [0, 0, 0],
            "ConstraintType": "Calculated DOFs",
            "CalculatedModelType": "Cartesian",
            "OutFrameReleaseStage": "~"
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return general

    def get_DOFs(self):

        DoF_data = []
        DoFs = {"DOFFree, DOFInitialValue": DoF_data}

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_DoFs_'
        property_group = DoFs
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return DoFs

    def get_Stiffness(self):
        connectionStiffness = {
            "TranslationalStiffness": 0,
            "RotationalStiffness": 0,
            "TranslationalDamping": 0,
            "RotationalDamping": 0
        }

        return connectionStiffness

    def get_CharacteristicProperties(self):
        CharacteristicProperties = {"CharacteristicLength": "~", "CharacteristicForce": "~"}

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_CharacteristicProperties_'
        property_group = CharacteristicProperties
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return CharacteristicProperties

    def get_drawing(self):
        drawing = {"Hidden": "Yes", "HideAfterOutFrameRelease": "No"}

        return drawing

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
