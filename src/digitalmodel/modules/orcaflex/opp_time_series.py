# Standard library imports
import logging

# Third party imports
try:
    import OrcFxAPI 
except:
    logging.debug("OrcFxAPI not available")

class OPPTimeSeries:
    
    def __init__(self) -> None:
        pass
        
    def get_time_series_from_orcaflex_run(self, model, cfg_time_series):
        """Gets time series data from an OrcaFlex run

        Args:
            model: OrcaFlex model object
            cfg_time_series: Configuration dictionary containing time series settings

        Returns:
            Time series data from the OrcaFlex analysis
        """
        time_series = None
        ObjectName = cfg_time_series['ObjectName']
        OrcFXAPIObject = model[ObjectName]
        SimulationPeriod = cfg_time_series['SimulationPeriod']
        TimePeriod = self.get_TimePeriodObject(SimulationPeriod)
        VariableName = cfg_time_series['Variable']
        
        if cfg_time_series.__contains__('ArcLength'):
            ArcLength = cfg_time_series['ArcLength'][0]
            RadialPos_text = cfg_time_series['RadialPos']
            if RadialPos_text == 'Outer':
                RadialPos = 1
            elif RadialPos_text == 'Inner':
                RadialPos = 0
            Theta = cfg_time_series['Theta']
            objectExtra = OrcFxAPI.oeLine(ArcLength=ArcLength,
                                        RadialPos=RadialPos,
                                        Theta=Theta)
        else:
            Location = cfg_time_series['Location']
            objectExtra = OrcFxAPI.oeEnvironment(Location[0], Location[1],
                                               Location[2])

        time_series = OrcFXAPIObject.TimeHistory(VariableName,
                                               TimePeriod,
                                               objectExtra=objectExtra)
        return time_series

    def get_TimePeriodObject(self, SimulationPeriod):
        """Gets an OrcaFlex time period object based on simulation period settings

        Args:
            SimulationPeriod: Simulation period configuration

        Returns:
            OrcaFlex time period object
        """
        if type(SimulationPeriod) is int:
            TimePeriodObject = SimulationPeriod
        elif SimulationPeriod == 'WhileSimulation':
            TimePeriodObject = OrcFxAPI.PeriodNum.WholeSimulation
        elif SimulationPeriod == 'LatestWave':
            TimePeriodObject = OrcFxAPI.PeriodNum.LatestWave
        elif len(SimulationPeriod) == 2:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(SimulationPeriod[0],
                                                      SimulationPeriod[1])
        elif len(SimulationPeriod) == 1:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(SimulationPeriod[0])
        else:
            raise ValueError("Could not specify time period for simulation")

        return TimePeriodObject

