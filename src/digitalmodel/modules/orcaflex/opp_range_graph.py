import pandas as pd
import math
import copy

try:
    import OrcFxAPI
except Exception:
    print("OrcaFlex license not available. Run on different computer")


class OPPRangeGraph:
    def __init__(self) -> None:
        pass

    def get_RangeGraph(
        self, OrcFXAPIObject, TimePeriod, VariableName, arclengthRange, objectExtra
    ):
        try:
            output = OrcFXAPIObject.RangeGraph(
                VariableName, TimePeriod, arclengthRange=arclengthRange
            )
        except:
            output = OrcFXAPIObject.RangeGraph(
                VariableName,
                TimePeriod,
                objectExtra=objectExtra,
                arclengthRange=arclengthRange,
            )
        return output

    def postProcessRange(self, model, cfg, FileObjectName):

        # Range Graphs for a simulation
        RangeFile = []

        for RangeGraphIndex in range(0, len(cfg["RangeGraph"])):
            RangeDF = pd.DataFrame()
            # Read Object
            try:
                objectName = cfg["RangeGraph"][RangeGraphIndex]["ObjectName"]
                OrcFXAPIObject = model[objectName]
            except:
                OrcFXAPIObject = model[FileObjectName]

            SimulationPeriod = cfg["RangeGraph"][RangeGraphIndex]["SimulationPeriod"]
            TimePeriod = of_objects.get_TimePeriodObject(SimulationPeriod)
            VariableName = cfg["RangeGraph"][RangeGraphIndex]["Variable"]

            try:
                if len(cfg["RangeGraph"][RangeGraphIndex]["ArcLength"]) > 1:
                    StartArcLength = cfg["RangeGraph"][RangeGraphIndex]["ArcLength"][0]
                    EndArcLength = cfg["RangeGraph"][RangeGraphIndex]["ArcLength"][1]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength, EndArcLength
                        ),
                    )
                else:
                    StartArcLength = cfg["RangeGraph"][RangeGraphIndex]["ArcLength"][0]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(StartArcLength),
                    )
                    # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
            except:
                arclengthRange = None
                output = OrcFXAPIObject.RangeGraph(
                    VariableName, TimePeriod, arclengthRange=arclengthRange
                )
            # Assign Arc Length
            AdditionalDataName = "X"
            RangeDF[AdditionalDataName] = output.X

            for AdditionalDataIndex in range(
                0, len(cfg["RangeGraph"][RangeGraphIndex]["AdditionalData"])
            ):
                AdditionalDataName = cfg["RangeGraph"][RangeGraphIndex][
                    "AdditionalData"
                ][AdditionalDataIndex]
                RangeDF[VariableName] = getattr(output, AdditionalDataName)

                if VariableName == "API STD 2RD Method 1":
                    RangeDF[VariableName] = [
                        math.sqrt(x) for x in RangeDF[VariableName]
                    ]
            RangeFile.append(RangeDF)

        if (
            self.cfg["default"]["Analysis"]["RangeGraph"]["flag"]
            and self.cfg["default"]["Analysis"]["RangeGraph"][
                "add_effective_tension_to_cfg"
            ]
        ):
            self.add_result_to_cfg(RangeDF, VariableName)

        return RangeFile

    def add_result_to_cfg(self, RangeDF, VariableName):
        self.cfg["Analysis"]["X"] = RangeDF["X"].tolist()
        self.cfg["Analysis"][VariableName] = RangeDF[VariableName].tolist()
        self.cfg_array.append(copy.deepcopy(self.cfg))

    def process_range_graphs(self):
        if self.cfg["default"]["Analysis"]["RangeGraph"]["flag"]:
            vizualization = Visualization()
            # vizualization.orcaflex_range_plot(RangeAllFiles, self.cfg)
        else:
            print("No postprocessing plots per user request")
