# Standard library imports
import logging
import pandas as pd

# Third party imports
import OrcFxAPI


class OrcaFlexObjects:
    def __init__(self) -> None:
        pass

    def get_orcaflex_objects(self, model_dict, cfg):
        model = model_dict["model"]
        OrcFXAPIObject = self.get_OrcFXAPIObject(model, cfg)

        TimePeriod = self.get_SimulationPeriod(cfg, model_dict)

        TimePeriod = self.get_SimulationPeriod(cfg, model_dict)

        ArcLengthArray, arclengthRange = self.get_arc_length_objects(cfg)

        objectExtra = None
        if OrcFXAPIObject is not None:
            objectExtra, arclengthRange_objectExtra = self.get_objectExtra(
                cfg, OrcFXAPIObject
            )
            if arclengthRange is None:
                arclengthRange = arclengthRange_objectExtra
        objectExtra = None
        if OrcFXAPIObject is not None:
            objectExtra, arclengthRange_objectExtra = self.get_objectExtra(
                cfg, OrcFXAPIObject
            )
            if arclengthRange is None:
                arclengthRange = arclengthRange_objectExtra

        VariableName = None
        if "Variable" in cfg and OrcFXAPIObject is not None:
            VariableName = cfg["Variable"]

        Statistic_Type = None
        if "Statistic_Type" in cfg and OrcFXAPIObject is not None:
            Statistic_Type = cfg["Statistic_Type"]

        return (
            OrcFXAPIObject,
            TimePeriod,
            arclengthRange,
            objectExtra,
            VariableName,
            Statistic_Type,
        )

    def get_arc_length_objects(self, cfg):
        ArcLengthArray = []
        if "ArcLength" in cfg:
            ArcLengthArray = cfg["ArcLength"]

        try:
            arclengthRange = self.get_ArcLengthObject(ArcLengthArray, cfg)
        except:
            arclengthRange = self.get_ArcLengthObject([])

        return ArcLengthArray, arclengthRange

    def get_ArcLengthObject(self, ArcLength, cfg):

        arclengthRange = None
        if "objectExtra" in cfg and cfg["objectExtra"] is not None:
            if "End A" in cfg["objectExtra"][0]:
                arclengthRange = OrcFxAPI.oeEndA
            elif "End B" in cfg["objectExtra"][0]:
                arclengthRange = OrcFxAPI.oeEndB
            elif "Touchdown" in cfg["objectExtra"][0]:
                arclengthRange = OrcFxAPI.oeTouchdown

        if arclengthRange is None and ArcLength is None:
            arclengthRange = None
        elif type(ArcLength) is str:
            if "End A" in ArcLength:
                arclengthRange = OrcFxAPI.oeEndA
            elif "End B" in ArcLength:
                arclengthRange = OrcFxAPI.oeEndB
            elif "Touchdown" in ArcLength:
                arclengthRange = OrcFxAPI.oeTouchdown
            else:
                raise ValueError

        elif len(ArcLength) == 2:
            StartArcLength = ArcLength[0]
            EndArcLength = ArcLength[1]
            arclengthRange = OrcFxAPI.arSpecifiedArclengths(
                StartArcLength, EndArcLength
            )
        elif len(ArcLength) == 1:
            StartArcLength = ArcLength[0]
            arclengthRange = OrcFxAPI.arSpecifiedArclengths(
                StartArcLength, StartArcLength
            )
        else:
            arclengthRange = None

        return arclengthRange

    def get_OrcFXAPIObject(self, model, cfg):
        OrcFXAPIObject = None
        OrcFXAPIObject = None
        if "ObjectName" in cfg:
            objectName = cfg["ObjectName"]
        else:
            logging.debug(
                f"function input configuration does not have objectName: {cfg}"
            )
            raise Exception("function input configuration does not have objectName")
            logging.debug(
                f"function input configuration does not have objectName: {cfg}"
            )
            raise Exception("function input configuration does not have objectName")

        try:
            OrcFXAPIObject = model[objectName]
        except Exception as e:
            logging.debug(str(e))
            logging.debug(str(e))

        return OrcFXAPIObject

    def get_SimulationPeriod(self, cfg, model_dict):
        start_time = model_dict["start_time"]
        termination_time = model_dict["current_time"]

        TimePeriod = None
        if "SimulationPeriod" in cfg:
            SimulationPeriod = cfg["SimulationPeriod"]

            if type(SimulationPeriod) is str:
                if SimulationPeriod == "StaticState":
                    TimePeriod = OrcFxAPI.PeriodNum.StaticState
                elif SimulationPeriod == "WholeSimulation":
                    TimePeriod = OrcFxAPI.PeriodNum.WholeSimulation
                elif SimulationPeriod == "LatestWave":
                    TimePeriod = OrcFxAPI.PeriodNum.LatestWave
                else:
                    raise ValueError("Could not specify time period for simulation")
            elif type(SimulationPeriod) is list:
                if len(SimulationPeriod) == 2:
                    if SimulationPeriod[0] is not None:
                        start_time = SimulationPeriod[0]
                    if SimulationPeriod[1] is not None:
                        termination_time = SimulationPeriod[1]

                    TimePeriod = self.get_TimePeriodObject(
                        [start_time, termination_time]
                    )
                elif len(SimulationPeriod) == 1:
                    TimePeriod = self.get_TimePeriodObject(SimulationPeriod)

        return TimePeriod

    def get_TimePeriodObject(self, SimulationPeriod):
        """Gets an OrcaFlex time period object based on simulation period settings

        Args:
            SimulationPeriod: Simulation period configuration

        Returns:
            OrcaFlex time period object
        """
        if len(SimulationPeriod) == 2:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(
                SimulationPeriod[0], SimulationPeriod[1]
            )
        elif len(SimulationPeriod) == 1:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(
                SimulationPeriod[0], SimulationPeriod[0]
            )
        elif type(SimulationPeriod) is int:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(
                SimulationPeriod, SimulationPeriod
            )
        elif SimulationPeriod == "StaticState":
            TimePeriodObject = OrcFxAPI.PeriodNum.StaticState
        elif SimulationPeriod == "WholeSimulation":
            TimePeriodObject = OrcFxAPI.PeriodNum.WholeSimulation
        elif SimulationPeriod == "LatestWave":
            TimePeriodObject = OrcFxAPI.PeriodNum.LatestWave
        else:
            raise ValueError("Could not specify time period for simulation")

        return TimePeriodObject

    def get_objectExtra(self, cfg, OrcFXAPIObject):
        """
        There are 10s (84 or more) of object types in OrcaFlex.
        Example commands below:
            List: list(OrcFxAPI.ObjectType.__dict__.keys())
            Count: len(list(OrcFxAPI.ObjectType.__dict__.keys()))
            Print: print(list(OrcFxAPI.ObjectType.__dict__.keys())

            #TODO update ObjectExtra to using OrcFxAPI native types rather than external input
        """

        objectExtra = None
        arclengthRange = None

        if "Support" in cfg["Variable"]:
            if "SupportIndex" in cfg:
                objectExtra = OrcFxAPI.oeSupport(SupportIndex=cfg["SupportIndex"])
            else:
                logging.info(
                    "SupportIndex not implemented fully. Edit definition to get acceptance."
                )
                raise Exception(
                    "SupportIndex not implemented fully. Edit definition to get acceptance."
                )

        if cfg["Variable"] == "Rigid Pipe":
            objectExtra = self.get_objectExtra_rigid_pipe(cfg)

        if cfg["Variable"] == "Line":
            objectExtra = self.get_objectExtra_line(cfg)

        if (
            "objectExtra" in cfg
            and cfg["objectExtra"] is not None
            and len(cfg["objectExtra"]) > 0
        ):
            if cfg["objectExtra"][0] == "Section":
                if len(cfg["objectExtra"][1]) == 1:
                    objectExtra = OrcFxAPI.arSpecifiedSections(
                        FromSection=cfg["objectExtra"][1][0],
                        ToSection=cfg["objectExtra"][1][0],
                    )
                elif len(cfg["objectExtra"][1]) == 2:
                    objectExtra = OrcFxAPI.arSpecifiedSections(
                        FromSection=cfg["objectExtra"][1][0],
                        ToSection=cfg["objectExtra"][1][1],
                    )
            elif "End A" in cfg["objectExtra"][0]:
                objectExtra = OrcFxAPI.oeEndA
            elif "End B" in cfg["objectExtra"][0]:
                objectExtra = OrcFxAPI.oeEndB
            elif "Touchdown" in cfg["objectExtra"][0]:
                objectExtra = OrcFxAPI.oeTouchdown
            arclengthRange = objectExtra

        elif "ArcLength" in cfg and len(cfg["ArcLength"]) > 0:
            ArcLength = cfg["ArcLength"]
            if len(ArcLength) == 1:
                objectExtra = OrcFxAPI.oeLine(ArcLength[0])
            elif len(ArcLength) == 2:
                objectExtra = OrcFxAPI.oeLine(ArcLength[0], ArcLength[1])
            else:
                objectExtra = OrcFxAPI.oeLine([])

        if OrcFXAPIObject.type == OrcFxAPI.ObjectType.Vessel:
            objectExtra = self.get_objectExtra_vessel(cfg)

        return objectExtra, arclengthRange

    def get_objectExtra_vessel(self, cfg):
        if "Position" in cfg:
            Position = cfg["Position"]
        else:
            Position = [0, 0, 0]
            logging.debug("Position not defined. Defaulting to [0, 0, 0]")

        objectExtra = OrcFxAPI.oeVessel(Position[0], Position[1], Position[2])

        return objectExtra

    def get_objectExtra_rigid_pipe(self, cfg_time_series):
        objectExtra = None
        if cfg_time_series.__contains__("ArcLength"):
            ArcLength = cfg_time_series["ArcLength"][0]
            RadialPos_text = cfg_time_series["RadialPos"]
            if RadialPos_text == "Outer":
                RadialPos = 1
            elif RadialPos_text == "Inner":
                RadialPos = 0
            Theta = cfg_time_series["Theta"]
            objectExtra = OrcFxAPI.oeLine(
                ArcLength=ArcLength, RadialPos=RadialPos, Theta=Theta
            )
        else:
            Location = cfg_time_series["Location"]
            objectExtra = OrcFxAPI.oeEnvironment(Location[0], Location[1], Location[2])

        return objectExtra

    def get_input_data_variable_name(self, cfg, OrcFXAPIObject, VariableName):
        if "ObjectName" in cfg:
            objectName = cfg["ObjectName"]
        else:
            raise Exception("Model does not have the objectName")

        if objectName == "Environment":
            if type(VariableName) is list:
                VariableName_0 = VariableName[0]
                if len(VariableName) == 1:
                    SelectedCurrentIndex = 0
                    if "RefCurrent" in VariableName_0:
                        # SelectedCurrentIndex = OrcFXAPIObject.SelectedCurrentIndex
                        OrcFXAPIObject.SelectedCurrent = OrcFXAPIObject.ActiveCurrent
                    VariableName.append(SelectedCurrentIndex)

        return OrcFXAPIObject, VariableName

    def get_model_objects(self, model):
        object_df = pd.DataFrame(columns=["ObjectType", "ObjectName", "ObjectTypeName"])
        objects = model.objects
        object_df["ObjectType"] = [int(object.type) for object in objects]
        object_df["ObjectName"] = [object.name for object in objects]
        object_df["ObjectTypeName"] = [object.type.name for object in objects]

        output_dict = {"object_df": object_df}

        return output_dict

    def get_object_vars(self, cfg, model, object, objectExtra, ResultType):
        """
        Get the variable data for an object.
        """

        output_dict = {}
        var_df = pd.DataFrame(columns=["VarName", "VarUnits", "FullName"])

        try:
            vardetails = object.varDetails(objectExtra=objectExtra)
            var_df["VarName"] = [vardetail.VarName for vardetail in vardetails]
            var_df["VarUnits"] = [vardetail.VarUnits for vardetail in vardetails]
            var_df["FullName"] = [vardetail.FullName for vardetail in vardetails]
        except Exception as e:
            logging.debug(f"Error getting variable details: {e}")

        output_dict["var_df"] = var_df

        VarNames_parameter_keys = list(cfg["parameters"]["VarNames"].keys())
        VarNames = []
        if object.type.name in VarNames_parameter_keys:
            VarNames = cfg["parameters"]["VarNames"][object.type.name]
        else:
            VarNames = list(var_df["VarName"])

        output_dict["VarNames"] = VarNames

        return output_dict
