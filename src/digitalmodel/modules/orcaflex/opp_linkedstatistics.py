
class OPPLinkedStatistics():
    
    def __init__(self) -> None:
        pass




    def legacy_code(self):

        # TODO LinkedStatistics using OrcaFlex. Code currently not working.
        VariableName = 'Effective Tension', 'Bend Moment'
        arclengthRange = OrcFxAPI.oeArcLength(25.0)
        SimulationPeriod = cfg["PostProcess"]['Summary'][SummaryIndex][
            'SimulationPeriod']
        TimePeriod = of_objects.get_TimePeriodObject(SimulationPeriod)
        stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod,
                                                arclengthRange)
        query = stats.Query('Effective Tension', 'Bend Moment')
        print(query.ValueAtMax)
        print(query.ValueAtMin)

    # Alternatively, use Range graphs for required quantities and obtain them using DFs. (easiest to customize)

