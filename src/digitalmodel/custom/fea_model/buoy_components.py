class Buoy():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        buoy = {}

        general = self.get_general()
        buoy.update(general)

        inertia = self.get_inertia()
        buoy.update(inertia)

        stacked_cylinder_data = self.get_stacked_cylinder()

        buoy.update(stacked_cylinder_data)

        GlobalAppliedLoads = [{"Origin": [0, 0, 0]}, {"Force": [0, 0, 0]}, {"Moment": [0, 0, 0]}]
        buoy.update({"GlobalAppliedLoads": GlobalAppliedLoads})

        LocalAppliedLoads = [{"Origin": [0, 0, 0]}, {"Force": [0, 0, 0]}, {"Moment": [0, 0, 0]}]
        buoy.update({"LocalAppliedLoads": LocalAppliedLoads})

        contact = {"SeabedFrictionCoefficient": 0, "TotalContactArea": "~"}
        buoy.update(contact)

        drawing = self.get_drawing()
        buoy.update(drawing)

        shaded_drawing = {"ShadedDrawingCullingMode": "Anticlockwise"}
        buoy.update(shaded_drawing)

        return buoy

    def get_general(self):
        general = {
            'Name': 'SALM',
            'BuoyType': 'Spar Buoy',
            'Connection': 'Free',
            'DegreesOfFreedomInStatics': 'All',
            'DampingRelativeTo': 'Earth',
            'DisturbanceVessel': "(none)",
            'WaveCalculationMethod': 'Specified by Environment',
            'InitialPosition': [0, 0, 0.7],
            'InitialAttitude': [0, 0, 0],
        }

        update_cfg = self.cfg.copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return general

    def get_inertia(self):
        inertia = {
            'Mass': 284,
            'MomentsOfInertia': [3346.5, 3346.5, 5112],
            'CentreOfMass': [0, 0, -1.68],
        }

        update_cfg = self.cfg.copy()
        calculate_function_prefix = 'get_inertia_'
        property_group = inertia
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return inertia

    def get_stacked_cylinder(self):
        stacked_cylinder = {
            'StackBaseCentre': [0, 0, -14.115],
            'BulkModulus': 'Infinity',
            'MunkMomentCoefficient': 0,
            'NormalDragAreaCalculatedFromGeometry': 'Yes',
            'StackedBuoyAddedMassAndDampingMethod': 'Values for each cylinder',
            'Cylinders': "None"
        }

        update_cfg = self.cfg['StackedBuoy'].copy()
        calculate_function_prefix = 'get_StackedBuoy_'
        property_group = stacked_cylinder
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        cylinder_data = []
        for cylinder_cfg in update_cfg['Cylinders']:
            cylinder_data.append(self.get_cylinder(cylinder_cfg))
        stacked_cylinder.update({'Cylinders': cylinder_data})

        return stacked_cylinder

    def get_cylinder(self, cylinder_cfg):
        cylinder = {
            "CylinderOuterDiameter": 4.6,
            "CylinderInnerDiameter": 0,
            "CylinderLength": 16.5,
            "CylinderAxialDragArea": 0,
            "DragForceCoefficient": [1.14, 0.85],
            "DragAreaMoment": [15, 0],
            "DragMomentCoefficient": [0.7, 0],
            "CylinderSlamForceDataEntry": 0,
            "CylinderSlamForceDataExit": 0,
            "AddedMassForceCoefficient": [1.06, 0.9],
            "InertiaForceCoefficient": ["~", "~"],
            "AddedMomentOfInertia": [220.8091774, 0],
            "UnitDampingForce": [160, 0],
            "UnitDampingMoment": [2500, 0],
        }

        MarineGrowthThickness = cylinder_cfg.get('MarineGrowthThickness', 0)
        cylinder_cfg['CylinderOuterDiameter'] = (cylinder_cfg['CylinderOuterDiameter'] +
                                                 2 * MarineGrowthThickness).__round__(3)
        update_cfg = cylinder_cfg.copy()
        calculate_function_prefix = 'get_cylinder_'
        property_group = cylinder
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return cylinder

    def get_cylinder_DragAreaMoment(self, cfg):
        l = cfg['CylinderLength']
        b = cfg['CylinderOuterDiameter']
        Ixx = b * (l**3) / 12
        Iyy = 0  # Assume symmetric
        return [Ixx.__round__(3), Iyy.__round__(3)]

    def get_inertia_MomentsOfInertia(self, cfg):
        Mass = [cylinder_item['Mass'] for cylinder_item in cfg['StackedBuoy']['Cylinders']]
        CylinderMassDiameter = [
            cylinder_item['CylinderMassDiameter'] for cylinder_item in cfg['StackedBuoy']['Cylinders']
        ]
        CylinderLength = [cylinder_item['CylinderLength'] for cylinder_item in cfg['StackedBuoy']['Cylinders']]

        Inertia_Z = sum(
            [mass * (cylinderMassDiameter**2) for mass, cylinderMassDiameter in zip(Mass, CylinderMassDiameter)])
        CentreOfMassEquivalent = self.get_inertia_CentreOfMass(cfg)
        Inertia_X = Inertia_Z / 2 + sum(Mass) * (abs(CentreOfMassEquivalent[2]) - sum(CylinderLength) / 2)**2
        Inertia_Y = Inertia_X
        return [float(Inertia_X).__round__(3), float(Inertia_Y).__round__(3), float(Inertia_Z).__round__(3)]

    def get_inertia_Mass(self, cfg):
        Mass = [cylinder_item['Mass'] for cylinder_item in cfg['StackedBuoy']['Cylinders']]
        return sum(Mass).__round__(3)

    def get_inertia_CentreOfMass(self, cfg):
        import numpy as np
        Mass = [cylinder_item['Mass'] for cylinder_item in cfg['StackedBuoy']['Cylinders']]
        CentreOfMass = [cylinder_item['CentreOfMass'] for cylinder_item in cfg['StackedBuoy']['Cylinders']]

        if len(CentreOfMass) > 1:
            CentreOfMassEquivalent_Array = [([centreOfMass_item * mass
                                              for centreOfMass_item in centreOfMass])
                                            for mass, centreOfMass in zip(Mass, CentreOfMass)]
            CentreOfMassEquivalent = CentreOfMassEquivalent_Array[0]
            for item in range(1, len(CentreOfMassEquivalent_Array)):
                CentreOfMassEquivalent = np.add(CentreOfMassEquivalent, CentreOfMassEquivalent_Array[item])
            CentreOfMassEquivalent = [item / sum(Mass) for item in CentreOfMassEquivalent]
        else:
            CentreOfMassEquivalent = CentreOfMass[0]

        CentreOfMassEquivalent = [float(item).__round__(3) for item in CentreOfMassEquivalent]
        return CentreOfMassEquivalent

    def get_drawing(self):
        drawing = {"Pen": [1, "Solid", "$FF80C0"]}

        update_cfg = self.cfg.copy()
        calculate_function_prefix = 'get_drawing_'
        property_group = drawing
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return drawing


    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
