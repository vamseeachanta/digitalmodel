class LineType():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        line_type = {}

        general = self.get_general()
        line_type.update(general)

        geometryMass = self.get_geometryMass()
        line_type.update(geometryMass)

        limits = self.get_limits()
        line_type.update(limits)

        structure = self.get_structure()
        line_type.update(structure)

        contact = self.get_contact()
        line_type.update(contact)

        addedProperties = self.get_addedProperties()
        line_type.update(addedProperties)

        dragAndLift = self.get_dragAndLift()
        line_type.update(dragAndLift)

        stress = self.get_stress()
        line_type.update(stress)

        friction = self.get_friction()
        line_type.update(friction)

        damping = self.get_damping()
        line_type.update(damping)

        drawing = self.get_drawing()
        line_type.update(drawing)

        return line_type

    def get_general(self):
        general = {
            "Name": "Chain",
            "Category": "General",
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return general

    def get_geometryMass(self):
        geometryMass = {
            "OD": 0.171,
            "ID": 0,
            "CG": [0, 0],
            "BulkModulus": "Infinity",
            "MassPerUnitLength": 0.1795975,
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_geometryMass_'
        property_group = geometryMass
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return geometryMass

    def get_limits(self):
        limits = {
            "CompressionIsLimited": "Yes",
            "AllowableTension": "~",
            "MinRadius": ["~", "~"],
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_limits_'
        property_group = limits
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return limits

    def get_structure(self):
        structure = {
            "EI": [0, "~"],
            "EA": 770.735E3,
            "PoissonRatio": 0,
            "GJ": 10,
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_structure_'
        property_group = structure
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return structure

    def get_contact(self):

        contact = {
            "ContactDiameter": 0.31825,
            "ClashStiffness": 0,
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_contact_'
        property_group = contact
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return contact

    def get_addedProperties(self):
        addedProperties = {
            "Ca": [1, "~", 0.5],
            "Cm": ["~", "~", "~"],
            "Cs": 0,
            "Ce": 0,
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_addedProperties_'
        property_group = addedProperties
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return addedProperties

    def get_dragAndLift(self):
        import math
        dragAndLift = {
            "Cd": [2.4, "~", 1.15],
            "Cl": 0,
            "NormalDragLiftDiameter": self.cfg['cfg']['OD'],
            "AxialDragLiftDiameter": self.cfg['cfg']['OD'],
        }

        MarineGrowthThickness = self.cfg['cfg'].get('MarineGrowthThickness', 0)

        if self.cfg['cfg'].__contains__('Material') and self.cfg['cfg']['Material'] == 'steel':
            HydrodynamicOD = (((self.cfg['cfg']['MassPerUnitLength'] * 0.13) * 4 / math.pi / 1025)**0.5 +
                              2 * MarineGrowthThickness).__round__(3)
            dragAndLift['NormalDragLiftDiameter'] = HydrodynamicOD
            dragAndLift['AxialDragLiftDiameter'] = HydrodynamicOD

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_dragAndLift_'
        property_group = dragAndLift
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return dragAndLift

    def get_stress(self):
        stress = {
            "StressOD": "~",
            "StressID": "~",
            "AllowableStress": "~",
            "TensileStressLoadingFactor": 1,
            "BendingStressLoadingFactor": 1,
            "ShearStressLoadingFactor": 1,
            "TorsionalStressLoadingFactor": 1,
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_stress_'
        property_group = stress
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return stress

    def get_friction(self):

        friction = {
            "SeabedNormalFrictionCoefficient": 0.5,
            "SeabedAxialFrictionCoefficient": "~",
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_friction_'
        property_group = friction
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return friction

    def get_damping(self):

        damping = {
            "RayleighDampingCoefficients": "(no damping)",
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_damping_'
        property_group = damping
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return damping

    def get_drawing(self):

        drawing = {"Pen": [1, "Solid", "Lime"]}

        return drawing

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
