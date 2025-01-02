class Line():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        line = {}

        general = self.get_general()
        line.update(general)

        connections = self.get_connections()
        line.update(connections)

        connectionStiffness = self.get_connectionStiffness()
        line.update(connectionStiffness)

        connectionFeeding = self.get_connectionFeeding()
        line.update(connectionFeeding)


        sections = self.get_sections()
        line.update(sections)

        contents = self.get_contents()
        line.update(contents)

        statics = self.get_statics()
        line.update(statics)

        results = self.get_results()
        line.update(results)


        drawing = self.get_drawing()
        line.update(drawing)

        return line

    def get_general(self):
        general = {
            "Name": "Chain2",
            "IncludeTorsion": "No",
            "TopEnd": "End A",
            "Representation": "Finite element",
            "PyModel": "(none)",
            "PreBendSpecifiedBy": "Bend angle",
            "DragFormulation": "Standard",
            "StaticsVIV": "None",
            "DynamicsVIV": "None",
            "WaveCalculationMethod": "Specified by Environment"
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return general

    def get_connections(self):

        connectionsData = []
        connections = {
            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzm, ConnectionDec, ConnectionGamma, ReleaseStage, ConnectionzRelativeTo":
                connectionsData
        }

        update_cfg = self.cfg['cfg']['connectionData'].copy()
        calculate_function_prefix = 'get_connections_'
        property_group = connections
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return connections

    def get_connectionStiffness(self):
        connectionStiffness = {'ConnectionStiffnessX, ConnectionStiffnessY': [[0, "~"], [0, "~"]]}

        return connectionStiffness

    def get_connectionFeeding(self):
        connectionFeeding = {'ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth': [["~", 0, 0.001], ["~", 0, 0.001]]}

        return connectionFeeding

    def get_sections(self):
        sections = {'LineType, Length, TargetSegmentLength': [["Chain", 11.8, 1]]}

        update_cfg = self.cfg['cfg']['sections'].copy()
        calculate_function_prefix = 'get_sections_'
        property_group = sections
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return sections

    def get_contents(self):
        contents = {
            "ContentsMethod": "Uniform",
            "IncludeAxialContentsInertia": "Yes",
            "ContentsDensity": 1,
            "ContentsPressureRefZ": "~",
            "ContentsPressure": 0,
            "ContentsFlowRate": 0
        }

        return contents

    def get_statics(self):
        statics = {
            "IncludedInStatics": "Yes",
            "StaticsStep1": "Catenary",
            "StaticsStep2": "None",
            "IncludeSeabedFrictionInStatics": "Yes",
            "LayAzimuth": 330,
            "AsLaidTension": 0
        }

        update_cfg = self.cfg['cfg']['statics'].copy()
        calculate_function_prefix = 'get_statics_'
        property_group = statics
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return statics

    def get_results(self):
        results = {
            "LogAcceleration": "No"
        }

        if 'results' in self.cfg['cfg']:
            update_cfg = self.cfg['cfg']['results'].copy()
        else:
            update_cfg = {}
        calculate_function_prefix = 'get_results_'
        property_group = results
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return results

    def get_drawing(self):
        drawing = {
            "SegmentPenMode": "Use Segment Pen",
            "DrawShadedNodesAsSpheres": "Yes",
            "SegmentPen": [1, "Solid", "Lime"],
            "ContactPen": [5, "Solid", "White"]
        }

        return drawing

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
