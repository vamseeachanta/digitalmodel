import logging
import math
import yaml
from collections import OrderedDict

from assetutilities.common.data import SaveData

class OrcaflexModelComponents():

    def __init__(self):
        pass

    def orcaflex_model(self, riser_model):
        self.model1 = OrderedDict()
        self.model2 = OrderedDict()
        self.riser_model = riser_model
        self.fea_type = riser_model.fea_type
        self.loading_index = riser_model.loading_index
        self.stack_up_properties_df = riser_model.stack_up_properties_df

        cfg_general = {
            "StageDuration":
                self.riser_model.cfg['FEASettings']['Stage'][self.fea_type]['Duration'],
            "TargetLogSampleInterval":
                self.riser_model.cfg['FEASettings']['Stage'][self.fea_type]['TargetLogSampleInterval'],
            "ImplicitConstantTimeStep":
                self.riser_model.cfg['FEASettings']['Stage'][self.fea_type]['ImplicitConstantTimeStep']
        }
        self.model1["General"] = self.GetGeneral(cfg_general)

        cfg_environment = {
            "Density": self.riser_model.cfg['Material']['SeaWater']['Rho'] / 1000,
            "WaterDepth": self.riser_model.water_depth,
            "SeabedNormalStiffness": self.riser_model.cfg['FEASettings']['Seabed']['Stiffness'][self.fea_type]
        }

        if self.riser_model.riser_shape == 'catenary':
            cfg_environment.update({"SeabedNormalStiffness": self.riser_model.cfg['FEASettings']['Seabed']['Stiffness'][self.fea_type] / \
                                                        ((self.riser_model.cfg['MainPipe'][ \
                               'InsulationSection']['OD'] + 2 * self.riser_model.cfg['geometry']['Strakes']['BaseThickness']) * 0.0254)})
        self.model1["Environment"] = self.GetEnvironment(cfg_environment)

        self.model2["RayleighDampingCoefficients"] = []
        self.model2["RayleighDampingCoefficients"].append(self.RayleighDampingCoefficients())
        self.riser_joints_to_line_types()

        self.customize_fj_types()
        self.model2['FlexJointTypes'] = self.fj_types

        self.customize_py_models()
        self.model2['P-yModels'] = self.py_models

        self.model2["Vessels"] = []
        self.model2["Vessels"].append(self.vessels())

        self.model2["Lines"] = []
        self.model2["Lines"].append(self.lines())

        self.links()
        self.model2["Links"] = self.links

        # customData = []
        # model1["VariableData"] = self.VariableData(customData)

    def riser_joints_to_line_types(self):
        self.model2["LineTypes"] = []
        for riser_joint_index in range(0, len(self.riser_model.riser_joints)):
            if self.riser_model.riser_joints.loc[riser_joint_index, 'Joint Type'] != 'auxiliary_line':
                customData = {
                    "Name":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['Name'],
                    "Category":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['Category'],
                    "OD":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['OD'],
                    "ID":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']
                        ['internal_fluid_ID'],
                    "MassPerUnitLength":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']
                        ['MassPerUnitLength'] / 1000,
                    "EI":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['EI'],
                    "EA":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['EA'],
                    "PoissonRatio":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']
                        ['PoissonRatio'],
                    "GJ":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['GJ'],
                    "Cd":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['Cd'],
                    "Ca":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['Ca'],
                    "NormalDragLiftDiameter":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']
                        ['drag_diameter'],
                    "AxialDragLiftDiameter":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']
                        ['drag_diameter'],
                    "StressOD":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['StressOD'],
                    "StressID":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['StressID'],
                    "SeabedNormalFrictionCoefficient":
                        self.riser_model.cfg['FEASettings']['Seabed']['FrictionCoefficient']['Normal'],
                    "SeabedAxialFrictionCoefficient":
                        self.riser_model.cfg['FEASettings']['Seabed']['FrictionCoefficient']['Axial'],
                    "CorrosionThickness":
                        0,
                    "SMYS":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['SMYS'],
                    "E":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['E'],
                    "SMUS":
                        self.riser_model.pipe_properties[riser_joint_index]['section_properties']['pipe']['SMUS'],
                    "RayleighDampingCoefficients":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "APISTD2RDFd1":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['CodeChecks']
                        ['APISTD2RDFd1'],
                    "APISTD2RDFd2":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['CodeChecks']
                        ['APISTD2RDFd2'],
                    "APISTD2RDDelta":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['CodeChecks']
                        ['APISTD2RDDelta'],
                    "APISTD2RDAlphaFab":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['CodeChecks']
                        ['APISTD2RDAlphaFab'],
                    "APISTD2RDk":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['CodeChecks']
                        ['APISTD2RDk'],
                    "Pen": [2, "Solid", "Yellow"]
                }

                self.model2["LineTypes"].append(self.lineTypes(customData))

    def GetGeneral(self, cfg=None):
        model = {}
        unit_system = {"UnitsSystem": "SI"}
        model.update(unit_system)

        statics = {
            "StaticsMethod": "Whole System statics",
            "BuoysIncludedInStatics": "Individually Specified",
            "StaticsMaxIterations": 800
        }
        model.update(statics)

        dynamics = {
            "DynamicsSolutionMethod": "Implicit time domain",
            "ImplicitUseVariableTimeStep": "No",
            "ImplicitConstantTimeStep": 0.25,
            "LogPrecision": "Single",
            "TargetLogSampleInterval": 0.1
        }
        model.update(dynamics)

        stage_duration = {"StageDuration": [10, 100]}
        model.update(stage_duration)

        if cfg is not None:
            model.update({"StageDuration": cfg['StageDuration']})
            model.update({"TargetLogSampleInterval": cfg['TargetLogSampleInterval']})
            model.update({"ImplicitConstantTimeStep": cfg['ImplicitConstantTimeStep']})

        return {"General": model}

    def GetEnvironment(self, cfg=None):

        model = {}
        model.update(self.seaAndSeabed(cfg))
        model.update(self.wave())
        model.update(self.current())
        model.update(self.wind())

        return {"Environment": model}

    def seaAndSeabed(self, cfg):
        model = {}
        # Sea
        model.update({"WaterSurfaceZ": 0})
        model.update({"KinematicViscosity": 1.35E-6})
        model.update({"SeaTemperature": 10})
        model.update({"ReynoldsNumberCalculation": "Flow Direction"})
        # Sea Density
        model.update({"HorizontalWaterDensityFactor": "~"})
        model.update({"VerticalDensityVariation": "Constant"})
        model.update({"Density": 1.025})
        # Seabed
        model.update({"SeabedType": "Flat"})
        model.update({"SeabedOrigin": [0, 0]})
        model.update({"WaterDepth": 30})
        model.update({"SeabedSlopeDirection": 0})
        model.update({"SeabedSlope": 0})
        model.update({"SeabedModel": "Linear"})
        model.update({"SeabedNormalStiffness": 100})
        model.update({"SeabedShearStiffness": "~"})

        if cfg is not None:
            model.update({"Density": cfg["Density"]})
            model.update({"WaterDepth": cfg['WaterDepth']})
            model.update({"SeabedNormalStiffness": cfg["SeabedNormalStiffness"]})

        return model

    def wave(self):
        model = {}
        if hasattr(self, 'riser_model'):
            if self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "Dean Stream":
                model.update({"SimulationTimeOrigin": 0})
                model.update({
                    "WaveTrains": [{
                        "Name": "Wave1",
                        "WaveType": "Dean Stream",
                        "WaveDirection": 180,
                        "WaveHeight": 7,
                        "WavePeriod": 8,
                        "WaveOrigin": [0, 0],
                        "WaveTimeOrigin": 0,
                        "WaveStreamFunctionOrder": 5
                    }]
                })
            elif self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "JONSWAP":
                model.update(self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave'])
            elif self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "Ochi-Hubble":
                model.update(self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave'])

        return model

    def current(self):
        model = {}
        # Current
        if hasattr(self, 'riser_model'):
            if self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]["MultipleCurrentDataCanBeDefined"]:
                model.update({"MultipleCurrentDataCanBeDefined": "Yes"})
                model.update({
                    "Currents": self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Currents']
                })
                model.update({
                    "ActiveCurrent":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['ActiveCurrent']
                })
            else:
                model.update({"MultipleCurrentDataCanBeDefined": "No"})
                model.update({"CurrentRamp": "No"})
                model.update({"HorizontalCurrentFactor": "~"})
                model.update({"CurrentMethod": "Interpolated"})
                model.update({
                    "RefCurrentSpeed":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Current']
                        ['RefCurrentSpeed']
                })
                model.update({
                    "RefCurrentDirection":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Current']
                        ['RefCurrentDirection']
                })
                model.update({
                    "CurrentDepth, CurrentFactor, CurrentRotation":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Current']
                        ['CurrentDepth, CurrentFactor, CurrentRotation']
                })

        return model

    def wind(self):
        model = {}
        # Wind
        model.update({"IncludeVesselWindLoads": "Yes"})
        model.update({"IncludeLineWindLoads": "Yes"})
        model.update({"IncludeBuoyWingWindLoads": "Yes"})
        model.update({"VerticalWindVariationFactor": "~"})
        model.update({"AirDensity": 0.00128})
        model.update({"WindType": "Constant"})
        model.update({"WindSpeed": 0})
        model.update({"WindDirection": 0})

        return model

    def vesselTypes(self, data):
        pass

    def supportTypes(self, data):
        pass

    def codeChecks(self, data):
        model = {}

        model.update({"APIRP2RDCorrosionThickness": data['CorrosionThickness']})
        model.update({"APIRP2RDSMYS": data['SMYS']})
        model.update({"APISTD2RDE": data['E']})
        model.update({"APISTD2RDS": data['SMYS']})
        model.update({"APISTD2RDU": data['SMUS']})
        model.update({"APISTD2RDDelta": data['APISTD2RDDelta']})
        model.update({"APISTD2RDFd1": data['APISTD2RDFd1']})
        model.update({"APISTD2RDFd2": data['APISTD2RDFd2']})
        model.update({"APISTD2RDAlphaFab": data['APISTD2RDAlphaFab']})
        model.update({"APISTD2RDk": data['APISTD2RDk']})

        return model

    def RayleighDampingCoefficients(self):
        # TODO self.customize_loading_data
        if self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Period1'] == "WavePeriod1":
            if self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "Dean Stream":
                customData = {
                    "Name":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "Mode":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                    "DampingRatio":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                    "Period1":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave']
                        ['WaveTrains'][0]['WaveTz'],
                    "ApplyToGeometricStiffness":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
                }
            elif self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "JONSWAP":
                customData = {
                    "Name":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "Mode":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                    "DampingRatio":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                    "Period1":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave']
                        ['WaveTrains'][0]['WaveTp'],
                    "ApplyToGeometricStiffness":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
                }
            elif self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "Ochi-Hubble":
                customData = {
                    "Name":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "Mode":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                    "DampingRatio":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                    "Period1":
                        1 / self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave']
                        ['WaveTrains'][0]['Wavefm1'],
                    "ApplyToGeometricStiffness":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
                }
        else:
            customData = {
                "Name":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                "Mode":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                "DampingRatio":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                "Period1":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Period1'],
                "ApplyToGeometricStiffness":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
            }

        model = {}

        model.update({"Name": customData['Name']})
        model.update({"Mode": customData['Mode']})
        model.update({"DampingRatio": customData['DampingRatio']})
        model.update({"Period1": customData['Period1']})
        model.update({"ApplyToGeometricStiffness": customData['ApplyToGeometricStiffness']})

        return model

    def lineTypes(self, data):
        model = {}
        if data['Category'] == "General":
            model.update({"Name": data["Name"]})
            model.update({"Category": "General"})
            model.update({"OD": data["OD"]})
            model.update({"ID": data["ID"]})
            model.update({"CG": [0, 0]})
            model.update({"BulkModulus": "Infinity"})
            model.update({"MassPerUnitLength": data['MassPerUnitLength']})
            model.update({"CompressionIsLimited": "No"})
            model.update({"AllowableTension": "~"})
            model.update({"MinRadius": ["~", "~"]})
            model.update({"EI": [data['EI'], "~"]})
            model.update({"EA": data['EA']})
            model.update({"PoissonRatio": data['PoissonRatio']})
            model.update({"GJ": data['GJ']})
            model.update({"Cd": data['Cd']})
            model.update({"Cl": 0})
            model.update({"NormalDragLiftDiameter": data['NormalDragLiftDiameter']})
            model.update({"AxialDragLiftDiameter": data['AxialDragLiftDiameter']})
            model.update({"Ca": data['Ca']})
            model.update({"Cm": ["~", "~", "~"]})
            model.update({"ContactDiameter": "~"})
            model.update({"ClashStiffness": 0})
            model.update({"StressOD": data['StressOD']})
            model.update({"StressID": data['StressID']})
            model.update({"AllowableStress": "~"})
            model.update({"TensileStressLoadingFactor": 1})
            model.update({"BendingStressLoadingFactor": 1})
            model.update({"ShearStressLoadingFactor": 1})
            model.update({"TorsionalStressLoadingFactor": 1})
            model.update({"SeabedNormalFrictionCoefficient": data['SeabedNormalFrictionCoefficient']})
            model.update({"SeabedAxialFrictionCoefficient": data['SeabedAxialFrictionCoefficient']})
            model.update({"RayleighDampingCoefficients": data['RayleighDampingCoefficients']})
            model.update({"CorrosionThickness": data['CorrosionThickness']})
            model.update({"SMYS": data['SMYS']})
            model.update(self.codeChecks(data))
            model.update({"Pen": data['Pen']})
            # model.update({codeChecks(data)})
        elif data['Category'] == "Homogeneous Pipe":
            model.update({"Name": data['Name']})
            model.update({"Category": data['Category']})
            model.update({"OD": data['OD']})
            model.update({"ID": data['ID']})
            model.update({"MaterialDensity": data['MaterialDensity']})
            model.update({"E": data['E']})
            model.update({"PoissonRatio": data['PoissonRatio']})
            model.update({"Cdn": data['Cdn']})
            model.update({"Cdz": data['Cdz']})
            model.update({"Cl": data['Cl']})
            model.update({"Can": data['Can']})
            model.update({"Caz": data['Caz']})
            model.update({"SeabedNormalFrictionCoefficient": data['SeabedNormalFrictionCoefficient']})
            model.update({"SeabedAxialFrictionCoefficient": data['SeabedAxialFrictionCoefficient']})
            model.update({"ClashStiffness": 0})
            model.update({"AllowableStress": data['AllowableStress']})
            model.update({"RayleighDampingCoefficients": data['RayleighDampingCoefficients']})
            model.update({"CoatingThickness": 0})
            model.update({"LiningThickness": 0})
            model.update(self.codeChecks(data))
            model.update({"Pen": data['Pen']})
        return model

    def lines(self):

        model = {}
        model.update({"Name": self.riser_model.riser_name})

        model.update({"IncludeTorsion": "No"})
        model.update({"TopEnd": "End A"})
        model.update({"DragFormulation": "Standard"})
        model.update({"StaticsVIV": "None"})
        model.update({"DynamicsVIV": "None"})
        model.update({"WaveCalculationMethod": "Specified by Environment"})

        if self.riser_model.riser_shape == 'catenary':
            model.update(self.line_items_catenary(self.fea_type, self.loading_index, self.riser_model.cfg, model))
            model.update({"PyModel": "(none)"})
        if self.riser_model.riser_shape == 'vertical':
            model.update({
                "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzm, ConnectionDec, ConnectionGamma, ReleaseStage, ConnectionzRelativeTo":
                    self.get_connection_definitions()
            })
            model.update({"ConnectionStiffnessX, ConnectionStiffnessY": self.get_connection_stiffnesses()})
            model.update({"LineType, Length, TargetSegmentLength": self.get_line_items_vertical()})

            self.get_fj_line_definitions()
            model.update({
                "AttachmentType, Attachmentx, Attachmenty, Attachmentz, AttachmentzRel, AttachmentName":
                    self.fj_line_definitions
            })
            model.update({"PyModel": self.riser_model.py_model_name})

        model.update({"ContentsMethod": "Uniform"})
        model.update({"IncludeAxialContentsInertia": "Yes"})
        model.update({"ContentsDensity": self.riser_model.cfg['Material']['Fluid']['Rho'] / 1000})
        model.update({"ContentsPressureRefZ": "~"})
        model.update({"ContentsPressure": self.riser_model.cfg['FEASettings']['DesignPressure']['Surface'] * 6.894745})
        model.update({"ContentsFlowRate": 0})
        model.update({"IncludedInStatics": "Yes"})
        model.update({"StaticsStep1": "Catenary"})

        if self.riser_model.riser_shape == 'catenary':
            model.update({"IncludeSeabedFrictionInStatics": "Yes"})
            model.update(
                {"LayAzimuth": self.riser_model.cfg['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel'] + 180})
            model.update({"AsLaidTension": 0})
        if self.riser_model.riser_shape == 'vertical':
            model.update({"StaticsStep2": "Full Statics"})
            model.update({"NodePen": [1, 'Solid', '$8080FF']})
            model.update({"DrawShadedNodesAsSpheres": "Yes"})

        return model

    def customize_fj_types(self):
        self.fj_types = []
        for line_item_index in range(0, len(self.riser_model.stack_up_table)):
            if self.riser_model.stack_up_table.iloc[line_item_index]['Component'] in ['Upper FJ', 'Lower FJ']:
                if (isinstance(self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'], int)) and \
                    (self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'] !=0):
                    component = self.riser_model.stack_up_table.iloc[line_item_index]['Component']
                    stiffness = self.riser_model.project_flexible_joints[
                        self.riser_model.project_flexible_joints['Component'] ==
                        component]['Rotational Stiffness'].values[0] * 0.3048 * 0.4536 * 9.81
                    self.fj_types.append({
                        "Name": component,
                        "BendStiffness": [stiffness, "~"],
                        "Pen": [7, 'Solid', 'Red']
                    })

    def customize_py_models(self):
        self.py_models = []
        py_curves = []
        for depth_below_seabed_item in self.riser_model.project_geotechnical.depth_below_seabed.unique():
            depth_geotechnical_data = self.riser_model.project_geotechnical[
                self.riser_model.project_geotechnical['depth_below_seabed'] == depth_below_seabed_item]
            py_curve = {"DepthBelowSeabedFrom": depth_below_seabed_item, "ModelType": 'P-y Table'}
            deflection_resistance = []
            for depth_geotechnical_data_index in range(0, len(depth_geotechnical_data)):
                deflection_resistance.append([
                    depth_geotechnical_data.iloc[depth_geotechnical_data_index]['y'],
                    depth_geotechnical_data.iloc[depth_geotechnical_data_index]['p']
                ])

            py_curve.update({"Deflection, Resistance": deflection_resistance})
            py_curves.append(py_curve)

        py_model = {'Name': self.riser_model.py_model_name, 'P-y Curves': py_curves}
        self.py_models.append(py_model)

    def links(self):
        self.links = []
        self.add_tensioners()
        self.add_support_spring()

    def add_support_spring(self):
        self.tensioner_vessel_locations(type='support_spring')
        tensioner_index = 0
        self.spring_properties(type='support_spring')
        link = {"Name": 'support_spring', "LinkType": 'Spring/Damper'}
        connection_array = []
        connection_array.append([
            self.riser_model.host_name + '_vessel', self.tensioner_vessel_hang_off_locations[tensioner_index][0],
            self.tensioner_vessel_hang_off_locations[tensioner_index][1],
            self.tensioner_vessel_hang_off_locations[tensioner_index][2] + 40
        ])
        connection_array.append([self.riser_model.riser_name, 0, 0, 0, 'End A'])
        link.update({"Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo": connection_array})
        link.update({"ReleaseStage": "~"})
        link.update({"LinearSpring": "No"})
        link.update({"SpringLength, SpringTension": self.tensioner_spring_properties})
        link.update({"LinearDamper": "No"})
        link.update({"DamperVelocity, DamperTension": [[0, 0]]})
        link.update({"Pen": [1, 'Solid', 'Red']})
        link.update({"ShadedDrawingDiameter": 0.25})
        self.links.append(link)

    def add_tensioners(self):
        self.tensioner_vessel_locations(type='tensioner')
        tensioner_bottom_distance_from_endA = float(self.stack_up_properties_df.iloc[
            self.riser_model.stack_up_table[self.riser_model.stack_up_table['Component'] == \
                                            'Tensioners'].index[0]]['component_bottom_elevation_from_end_A'])
        tensioner_bottom_elevation_above_MSL = float(self.stack_up_properties_df.iloc[
            self.riser_model.stack_up_table[self.riser_model.stack_up_table['Component'] == \
                                            'Tensioners'].index[0]]['component_bottom_elevation_from_end_A'])
        for tensioner_index in range(0, self.riser_model.project_tensioners['No.of Tensioners'].iloc[0]):
            link = {"Name": 'Tensioner' + str(tensioner_index), "LinkType": 'Spring/Damper'}
            connection_array = []
            connection_array.append([
                self.riser_model.host_name + '_vessel', self.tensioner_vessel_hang_off_locations[tensioner_index][0],
                self.tensioner_vessel_hang_off_locations[tensioner_index][1],
                self.tensioner_vessel_hang_off_locations[tensioner_index][2]
            ])
            connection_array.append([self.riser_model.riser_name, 0, 0, tensioner_bottom_distance_from_endA, 'End A'])
            # TODO utilize water depth and calculate the global coordinate of tensioner bottom. Calculate spring length and evaluate forces.
            # self.riser_model.water_depth
            link.update({"Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo": connection_array})
            link.update({"ReleaseStage": "~"})
            link.update({"LinearSpring": "No"})
            self.spring_properties(type='tensioner')
            link.update({"SpringLength, SpringTension": self.tensioner_spring_properties})
            link.update({"LinearDamper": "No"})
            link.update({"DamperVelocity, DamperTension": [[0, 0]]})
            link.update({"Pen": [1, 'Solid', 'Red']})
            link.update({"ShadedDrawingDiameter": 0.25})
            self.links.append(link)

    def tensioner_vessel_locations(self, type='tensioner'):
        self.tensioner_vessel_hang_off_locations = []

        moonpool_length = self.riser_model.project_tensioners['Row length'].iloc[0] * 0.3048
        moonpool_breadth = self.riser_model.project_tensioners['Separation between rows'].iloc[0] * 0.3048
        vertical_length_of_tensioner = self.riser_model.project_tensioners['vertical_length_of_tensioner'].iloc[
            0] * 0.3048
        self.tensioner_bottom_elevation_above_MSL = float(self.stack_up_properties_df.iloc[
            self.riser_model.stack_up_table[self.riser_model.stack_up_table['Component'] == \
                                            'Tensioners'].index[0]]['elevation_above_MSL'])
        tensioner_top_connection_elevation_above_msl = self.tensioner_bottom_elevation_above_MSL + vertical_length_of_tensioner

        self.riser_model.no_of_tensioners = self.riser_model.project_tensioners['No.of Tensioners'].iloc[0]
        if type == 'tensioner':
            if self.riser_model.no_of_tensioners == 6:
                self.tensioner_vessel_hang_off_locations.append(
                    [moonpool_length / 2, moonpool_breadth / 2, tensioner_top_connection_elevation_above_msl])
                self.tensioner_vessel_hang_off_locations.append(
                    [moonpool_length / 2, 0, tensioner_top_connection_elevation_above_msl])
                self.tensioner_vessel_hang_off_locations.append(
                    [moonpool_length / 2, -moonpool_breadth / 2, tensioner_top_connection_elevation_above_msl])
                self.tensioner_vessel_hang_off_locations.append(
                    [-moonpool_length / 2, moonpool_breadth / 2, tensioner_top_connection_elevation_above_msl])
                self.tensioner_vessel_hang_off_locations.append(
                    [-moonpool_length / 2, 0, tensioner_top_connection_elevation_above_msl])
                self.tensioner_vessel_hang_off_locations.append(
                    [-moonpool_length / 2, -moonpool_breadth / 2, tensioner_top_connection_elevation_above_msl])

        elif type == 'support_spring':
            self.tensioner_vessel_hang_off_locations.append(
                [0, 0, float(self.stack_up_properties_df.elevation_above_MSL.max()) + 30])

    def spring_properties(self, type='tensioner'):
        self.tensioner_spring_properties = []

        if type == 'tensioner':
            tensioner_stroke_range = self.riser_model.project_tensioners['stroke_range'].iloc[0] * 0.3048
            tensioner_initial_stroke_from_collapsed_length = tensioner_stroke_range * self.riser_model.initial_down_stroke
            tensioner_initial_stroke_from_extended_length = tensioner_stroke_range - tensioner_initial_stroke_from_collapsed_length

            distance_between_tensioner_ends = math.sqrt(self.tensioner_vessel_hang_off_locations[0][0]**2 +
                                                        self.tensioner_vessel_hang_off_locations[0][1]**2 +
                                                        (self.tensioner_vessel_hang_off_locations[0][2] -
                                                         self.tensioner_bottom_elevation_above_MSL)**2)

            total_stretch_of_riser = float(self.stack_up_properties_df['axial_stretch'].sum())
            self.tensioner_spring_initial_length = distance_between_tensioner_ends - total_stretch_of_riser
            self.tensioner_spring_collapsed_length = self.tensioner_spring_initial_length - tensioner_initial_stroke_from_collapsed_length
            self.tensioner_spring_extended_length = self.tensioner_spring_initial_length + tensioner_initial_stroke_from_extended_length

            tensioner_system_stiffness = self.riser_model.project_tensioners['Tensioner system stiffness'].iloc[0]
            idividual_tensioner_stiffness = tensioner_system_stiffness / self.riser_model.no_of_tensioners
            initial_spring_force = float(self.riser_model.total_tensioners_tension) / self.riser_model.no_of_tensioners
            fully_collapsed_spring_force = initial_spring_force - idividual_tensioner_stiffness * (
                tensioner_initial_stroke_from_collapsed_length)
            fully_extended_spring_force = initial_spring_force + idividual_tensioner_stiffness * (
                tensioner_initial_stroke_from_extended_length)

            if self.riser_model.model_stroke_in_and_out_limits:
                self.tensioner_spring_properties.append([0.001, 0.01])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_collapsed_length * 0.97, fully_collapsed_spring_force * 0.001])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_collapsed_length * 0.98, fully_collapsed_spring_force / 5])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_collapsed_length * 0.99, fully_collapsed_spring_force / 2])

            self.tensioner_spring_properties.append(
                [self.tensioner_spring_collapsed_length, fully_collapsed_spring_force])
            self.tensioner_spring_properties.append([self.tensioner_spring_initial_length, initial_spring_force])
            self.tensioner_spring_properties.append(
                [self.tensioner_spring_extended_length, fully_extended_spring_force])

            if self.riser_model.model_stroke_in_and_out_limits:
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_extended_length * 1.01, fully_extended_spring_force * 2])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_extended_length * 1.02, fully_extended_spring_force * 4])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_extended_length * 1.03, fully_extended_spring_force * 20])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_extended_length * 1.04, fully_extended_spring_force * 100])
                self.tensioner_spring_properties.append(
                    [self.tensioner_spring_extended_length * 1.05, fully_extended_spring_force * 1000])

        elif type == 'support_spring':
            self.tensioner_spring_properties.append([0, self.riser_model.total_support_tension])
            self.tensioner_spring_properties.append([50, self.riser_model.total_support_tension])

    def get_connection_definitions(self):
        connection_definitions = []
        connection_definitions.append(['Free', 0, 0, 25, 0, 180, 180, "~"])
        connection_definitions.append(
            ['Fixed', 0, 0,
             float(self.stack_up_properties_df['elevation_above_MSL'].min()), 0, 180, 180, "~"])
        return connection_definitions

    def get_connection_stiffnesses(self):
        connection_stiffnesses = []
        connection_stiffnesses.append([])
        connection_stiffnesses.append([0, "~"])
        return connection_stiffnesses

    def get_fj_line_definitions(self):
        self.fj_line_definitions = []
        for line_item_index in range(0, len(self.riser_model.stack_up_table)):
            if self.riser_model.stack_up_table.iloc[line_item_index]['Component'] in ['Upper FJ', 'Lower FJ']:
                if (isinstance(self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'], int)) and \
                    (self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'] !=0):
                    self.fj_line_definitions.append([
                        self.riser_model.stack_up_table.iloc[line_item_index]['Component'], 0, 0,
                        float(self.stack_up_properties_df.iloc[line_item_index]
                              ['component_bottom_elevation_from_end_A']), 'End A',
                        self.riser_model.stack_up_table.iloc[line_item_index]['Component']
                    ])

    def get_line_items_vertical(self):
        self.line_items_vertical = []
        for line_item_index in range(0, len(self.riser_model.stack_up_table)):
            if self.riser_model.stack_up_table.iloc[line_item_index]['Component'] not in [
                    'Upper FJ', 'Lower FJ', 'Tensioners', 'Support Spring'
            ]:
                if (isinstance(self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'], int)) and \
                    (self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'] !=0):
                    self.line_items_vertical.append([self.riser_model.stack_up_table.iloc[line_item_index]['Component'],
                                                     self.riser_model.stack_up_table.iloc[line_item_index]['Component Length']*0.3048* \
                                                     self.riser_model.stack_up_table.iloc[line_item_index]['No. Of. Joints'],
                                                     float(self.riser_model.stack_up_table.iloc[line_item_index]['FEA Segment Length'])])

        return self.line_items_vertical

    def VariableData(self, data):
        model = {}
        for i in range(0, len(data)):
            if data[i]["Type"] == "BendingConnectionStiffness":
                model["BendingConnectionStiffness"] = []
                model["BendingConnectionStiffness"].append(self.BendingConnectionStiffness(data[i]))
            elif data[i]["Type"] == "LineTypeOuterDiameter":
                model["LineTypeOuterDiameter"] = []
                model["LineTypeOuterDiameter"].append(self.LineTypeOuterDiameter(data[i]))

        return model

    def BendingConnectionStiffness(self, data):
        model = {}

        model.update({"Name": data['Name']})
        model.update({"IndependentValue, DependentValue": data["FlexJointStiffness"]})

        return model

    def LineTypeOuterDiameter(self, data):
        model = {}

        model.update({"Name": data['Name']})
        model.update({
            "IndependentValue, DependentValue": [[0, data['ThickEndOD']], [data['ProfileLength'], data['ThinEndOD']]]
        })

        return model

    def vessels(self):

        customData = {
            "Draught":
                self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Vessel']['Draft'],
            "InitialPosition":
                self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Vessel']
                ['InitialPosition'],
            "Orientation":
                self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Vessel']['Orientation']
        }

        model = {}

        model.update({"Name": self.riser_model.host_name + '_vessel'})
        model.update({"VesselType": self.riser_model.host_name})
        model.update({"Draught": customData['Draught']})
        model.update({"Length": "~"})
        model.update({"InitialPosition": customData['InitialPosition']})
        model.update({"Orientation": customData['Orientation']})
        model.update({"IncludedInStatics": "None"})
        model.update({"PrimaryMotion": "None"})
        model.update({"SuperimposedMotion": "Displacement RAOs + Harmonic Motion"})
        model.update({"IncludeAppliedLoads": "No"})
        model.update({"IncludeWaveLoad1stOrder": "No"})
        model.update({"IncludeWaveDriftLoad2ndOrder": "No"})
        model.update({"IncludeWaveDriftDamping": "No"})
        model.update({"IncludeSumFrequencyLoad": "No"})
        model.update({"IncludeAddedMassAndDamping": "No"})
        model.update({"IncludeManoeuvringLoad": "No"})
        model.update({"IncludeOtherDamping": "No"})
        model.update({"IncludeCurrentLoad": "No"})
        model.update({"IncludeWindLoad": "No"})

        return model

    def winches(self, data):
        pass

    def groups(self, data):
        pass

    def WriteOrcaflexModel(self, Files):
        with open(self.riser_model.cfg['Analysis']['fe_filename'], 'w') as f:
            for file in Files:
                if isinstance(file, str):
                    with open(file, "r") as infile:
                        f.write(infile.read())
                else:
                    yaml.dump(file, f)
        print('Successfully write file: "{0}"'.format(self.riser_model.cfg['Analysis']['fe_filename']))

    def build_model(self, shear7_flag):
        save_data = SaveData()
        cfg = self.riser_model.cfg
        FEAType = self.riser_model.fea_type

        if self.riser_model.riser_shape == 'catenary':
            if cfg['default']['Analysis']['SLWR']:
                for LoadingIndex in range(0, len(cfg['EnvironmentLoad'][FEAType])):
                    cfg['Analysis']['fe_filename'] = cfg['Analysis']['fe_folder'] + cfg['Analysis'][
                        'file_name'] + '_SLWR_FE_' + cfg['EnvironmentLoad'][FEAType][LoadingIndex]['Wave'][
                            'WaveTrains'][0]['Name'] + '.yml'
                    if cfg['default']['Analysis'][FEAType] == True:
                        FEAmodel1, FEAmodel2 = orcaflexModel(cfg, FEAType, LoadingIndex)
                        if FEAType == 'Extreme':
                            self.WriteOrcaflexModel(
                                [FEAmodel1, 'src/digitalmodel/tests/test_data/catenary_riser/VesselTypes_Extreme.yml', FEAmodel2], cfg)
                        elif FEAType == 'Fatigue':
                            self.WriteOrcaflexModel(
                                [FEAmodel1, 'src/digitalmodel/tests/test_data/catenary_riser/VesselTypes_Fatigue.yml', FEAmodel2], cfg)

                save_data.saveDataYaml(cfg, cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'])

            if cfg['default']['Analysis']['SCR']:
                for LoadingIndex in range(0, len(cfg['EnvironmentLoad'][FEAType])):
                    cfg['Analysis']['fe_filename'] = cfg['Analysis']['fe_folder'] + cfg['Analysis'][
                        'file_name'] + '_SCR_FE_' + cfg['EnvironmentLoad'][FEAType][LoadingIndex]['Wave'][
                            'WaveTrains'][0]['Name'] + '_FE.yml'
                    if cfg['default']['Analysis'][FEAType] == True:
                        FEAmodel1, FEAmodel2 = orcaflexModel(cfg, FEAType, LoadingIndex)
                        if FEAType == 'Extreme':
                            self.WriteOrcaflexModel(
                                [FEAmodel1, 'src/digitalmodel/tests/test_data/catenary_riser/VesselTypes_Extreme.yml', FEAmodel2], cfg)
                        elif FEAType == 'Fatigue':
                            self.WriteOrcaflexModel(
                                [FEAmodel1, 'src/digitalmodel/tests/test_data/catenary_riser/VesselTypes_Fatigue.yml', FEAmodel2], cfg)

                save_data.saveDataYaml(cfg, cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'], False)
        if self.riser_model.riser_shape == 'vertical':
            for LoadingIndex in range(0, len(cfg['EnvironmentLoad'][FEAType])):
                if shear7_flag:
                    cfg['Analysis']['fe_filename'] = cfg['Analysis']['fe_folder'] + cfg['Analysis'][
                        'file_name'] + '_shear7' + '_FE.yml'
                else:
                    cfg['Analysis'][
                        'fe_filename'] = cfg['Analysis']['fe_folder'] + cfg['Analysis']['file_name'] + '_' + cfg[
                            'EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][0]['Name'] + '_FE.yml'

                if cfg['default']['analysis']['loading_type'][FEAType] == True:
                    self.WriteOrcaflexModel([
                        self.model1, self.riser_model.cfg['default']['analysis']['host']['raos'][FEAType], self.model2
                    ])

            save_data.saveDataYaml(cfg, cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'], False)
            logging.info("Writing file successfully: {0}".format(cfg['Analysis']['result_folder'] +
                                                                 cfg['Analysis']['file_name']))

    def line_items_catenary(self, FEAType, LoadingIndex, data, model):
        # TODO Refactor
        if data['default']['Analysis']['SLWR']:
            absolute_orientation_angle_of_riser = \
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['Orientation'][2] \
                + data['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel']
            distance_from_hangoff_to_TDP = data['lazyWaveCatenaryResult']['Summary']['HangoffToTDP']['X'] \
                                           + data['LazyWaveCatenaryDefinition']['TDPToAnchor'] + \
                                           data['FEASettings']['AnchorAdjustment']['SLWR']
            model.update({
                "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzm, ConnectionDec, ConnectionGamma, ReleaseStage":                                            \
                    [["Vessel1", data['LazyWaveCatenaryDefinition']['Hangoff']['from_midship'],
                      data['LazyWaveCatenaryDefinition']['Hangoff']['from_centerline'],
                      data['LazyWaveCatenaryDefinition']['Hangoff']['above_keel'],
                      data['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel'],
                      180 - data['LazyWaveCatenaryDefinition']['declinationAngle'],
                      data['FEASettings']['EndOrientation']['SLWR']['A']['Gamma'], "~"], \
                     ["Anchored", distance_from_hangoff_to_TDP * math.cos(
                         math.radians(absolute_orientation_angle_of_riser)) +
                      data['LazyWaveCatenaryDefinition']['Hangoff']['from_midship']
                         , data['LazyWaveCatenaryDefinition']['Hangoff'][
                          'from_centerline'] + distance_from_hangoff_to_TDP * math.sin(
                         math.radians(absolute_orientation_angle_of_riser)), \
                      data['MainPipe']['InsulationSection']['OD'] * 0.0254 / 2,
                      data['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel'],
                      data['FEASettings']['EndOrientation']['SLWR']['B']['Declination'],
                      data['FEASettings']['EndOrientation']['SLWR']['B']['Gamma'], "~"]]})
        if data['default']['Analysis']['SCR']:
            absolute_orientation_angle_of_riser = \
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['Orientation'][2] \
                + data['simpleCatenaryDefinition']['lay_azimuth_to_vessel']
            distance_from_hangoff_to_TDP = data['catenaryResult']['X'] + data['simpleCatenaryDefinition']['TDPToAnchor'] \
                                           + data['FEASettings']['AnchorAdjustment']['SCR']
            model.update({
                "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzm, ConnectionDec, ConnectionGamma, ReleaseStage":                                            \
                    [["Vessel1", data['LazyWaveCatenaryDefinition']['Hangoff']['from_midship'],
                      data['LazyWaveCatenaryDefinition']['Hangoff']['from_centerline'],
                      data['LazyWaveCatenaryDefinition']['Hangoff']['above_keel'],
                      data['simpleCatenaryDefinition']['lay_azimuth_to_vessel'],
                      180 - data['simpleCatenaryDefinition']['declinationAngle'],
                      data['FEASettings']['EndOrientation']['SCR']['A']['Gamma'], "~"], \
                     ["Anchored", (distance_from_hangoff_to_TDP) * math.cos(
                         math.radians(absolute_orientation_angle_of_riser)) +
                      data['LazyWaveCatenaryDefinition']['Hangoff']['from_midship']
                         , data['LazyWaveCatenaryDefinition']['Hangoff'][
                          'from_centerline'] + distance_from_hangoff_to_TDP * math.sin(
                         math.radians(absolute_orientation_angle_of_riser)), \
                      data['MainPipe']['InsulationSection']['OD'] * 0.0254 / 2,
                      data['simpleCatenaryDefinition']['lay_azimuth_to_vessel'],
                      data['FEASettings']['EndOrientation']['SCR']['B']['Declination'],
                      data['FEASettings']['EndOrientation']['SCR']['B']['Gamma'], "~"]]})
        model.update({
            "ConnectionStiffnessX, ConnectionStiffnessY": [[
                data['FEASettings']['FlexJointStiffness']['ConnectionLabel'],
                data['FEASettings']['FlexJointStiffness']['ConnectionLabel']
            ], [0, "~"]]
        })
        if data['default']['Analysis']['SLWR']:
            SLWR_L_Before_tdp = data['lazyWaveCatenaryResult']['Summary']['BuoyancyToTouchDown']['S'] - 30 - \
                                data['FEASettings']['Mesh']['BeforeTDP']['L']
            if SLWR_L_Before_tdp > 0:
                model.update({"LineType, Length, TargetSegmentLength":                                            \
                                  [["FJExtension1", data['LazyWaveCatenaryDefinition']['TaperJoint']['L'],
                                    data['FEASettings']['Mesh']['Top']['Size']], \
                                   ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']], \
                                   ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size'] * 2], \
                                   ["MainPipe", data['FEASettings']['Mesh']['Top']['L'] -
                                    data['LazyWaveCatenaryDefinition']['TaperJoint']['L'] - 4,
                                    data['FEASettings']['Mesh']['Top']['Size'] * 2 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 4 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 8 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 16 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 24 * 2], \
                                   ["MainPipe", data['lazyWaveCatenaryResult']['Summary']['HangOffToBuoyancy']['S'] -
                                    data['FEASettings']['Mesh']['Top']['L'] - 80, 20], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 6], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 4], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["BuoyPipe", data['lazyWaveCatenaryResult']['Summary']['Buoyancy']['S'] - 4,
                                    data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 2], \
                                   ["MainPipe",
                                    data['lazyWaveCatenaryResult']['Summary']['BuoyancyToTouchDown']['S'] - 30 -
                                    data['FEASettings']['Mesh']['BeforeTDP']['L'], 5], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size'] * 4], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size'] * 2], \
                                   ["MainPipe", data['FEASettings']['Mesh']['BeforeTDP']['L'] - 10,
                                    data['FEASettings']['Mesh']['BeforeTDP']['Size']], \
                                   ["MainPipe", data['FEASettings']['Mesh']['AfterTDP']['L'] - 10,
                                    data['FEASettings']['Mesh']['AfterTDP']['Size']], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 4], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 8], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 16], \
                                   ["MainPipe", data['LazyWaveCatenaryDefinition']['TDPToAnchor'] - 30 -
                                    data['FEASettings']['Mesh']['AfterTDP']['L'], 20]]})
            else:
                model.update({"LineType, Length, TargetSegmentLength":                                            \
                                  [["FJExtension1", data['LazyWaveCatenaryDefinition']['TaperJoint']['L'],
                                    data['FEASettings']['Mesh']['Top']['Size']], \
                                   ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']], \
                                   ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size'] * 2], \
                                   ["MainPipe", data['FEASettings']['Mesh']['Top']['L'] -
                                    data['LazyWaveCatenaryDefinition']['TaperJoint']['L'] - 4,
                                    data['FEASettings']['Mesh']['Top']['Size'] * 2 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 4 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 8 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 16 * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 24 * 2], \
                                   ["MainPipe", data['lazyWaveCatenaryResult']['Summary']['HangOffToBuoyancy']['S'] -
                                    data['FEASettings']['Mesh']['Top']['L'] - 80, 20], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 6], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 4], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size'] * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["BuoyPipe", data['lazyWaveCatenaryResult']['Summary']['Buoyancy']['S'] - 4,
                                    data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size'] * 2], \
                                   ["MainPipe",
                                    data['lazyWaveCatenaryResult']['Summary']['BuoyancyToTouchDown']['S'] - 10,
                                    data['FEASettings']['Mesh']['BeforeTDP']['Size']], \
                                   ["MainPipe", data['FEASettings']['Mesh']['AfterTDP']['L'] - 10,
                                    data['FEASettings']['Mesh']['AfterTDP']['Size']], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 2], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 4], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 8], \
                                   ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 16], \
                                   ["MainPipe", data['LazyWaveCatenaryDefinition']['TDPToAnchor'] - 30 -
                                    data['FEASettings']['Mesh']['AfterTDP']['L'], 20]]})
        if data['default']['Analysis']['SCR']:
            model.update({"LineType, Length, TargetSegmentLength":                                            \
                              [["FJExtension1", data['LazyWaveCatenaryDefinition']['TaperJoint']['L'],
                                data['FEASettings']['Mesh']['Top']['Size']], \
                               ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']], \
                               ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size'] * 2], \
                               ["MainPipe",
                                data['FEASettings']['Mesh']['Top']['L'] -
                                data['LazyWaveCatenaryDefinition']['TaperJoint'][
                                    'L'] - 4, data['FEASettings']['Mesh']['Top']['Size'] * 2], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 4], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 8], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 16], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size'] * 24], \
                               ["MainPipe", data['catenaryResult']['S'] - data['FEASettings']['Mesh']['Top']['L'] -
                                data['FEASettings']['Mesh']['BeforeTDP']['L'] - 40, 5], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size'] * 2], \
                               ["MainPipe", data['FEASettings']['Mesh']['BeforeTDP']['L'] - 10,
                                data['FEASettings']['Mesh']['BeforeTDP']['Size']], \
                               ["MainPipe", data['FEASettings']['Mesh']['AfterTDP']['L'] - 10,
                                data['FEASettings']['Mesh']['AfterTDP']['Size']], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 2], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 4], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 8], \
                               ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size'] * 16], \
                               ["MainPipe", data['simpleCatenaryDefinition']['TDPToAnchor'] - 30 -
                                data['FEASettings']['Mesh']['AfterTDP']['L'], 20]]})

    def variable_data(self):
        # variable_data_1 = {"Type": "BendingConnectionStiffness",
        #                "Name": "FJProfile",
        #                "FlexJointStiffness": data['FEASettings']['FlexJointStiffness'][FEAType]}
        # variable_data_2 = {"Type": "LineTypeOuterDiameter",
        #                "Name": "FJExtension",
        #                "ProfileLength": data['LazyWaveCatenaryDefinition']['TaperJoint']['L'],
        #                "ThickEndOD": (data['MainPipe']['SteelSection']['ID'] + 2 *
        #                               data['LazyWaveCatenaryDefinition']['TaperJoint']['ThickendThickness']) * 0.0254,
        #                "ThinEndOD": (data['MainPipe']['SteelSection']['OD']) * 0.0254}
        # customData.append(variable_data_1)
        # customData.append(variable_data_2)
        pass
