# Standard library imports
from typing import Any, Dict


class OPPLinkedStatistics():
    
    def __init__(self) -> None:
        pass

    def get_linked_statistics(self, cfg: Dict[str, Any], model) -> Dict[str, Any]:
        ls_groups = cfg['linked_statistics_settings']['groups']
        for ls_group in ls_groups:
            ls_group_label = ls_group['Label']
            df = pd.DataFrame()
            for ls_cfg in ls_group['Columns']:
                ls_label = ls_cfg['Label']
                linked_statistics = self.get_linked_statistics_from_orcaflex_run(model, ls_cfg)

        return linked_statistics

    def get_linked_statistics_from_orcaflex_run(self, model, ls_cfg) -> Dict[str, Any]:
        # Get the linked statistics
        linked_statistics = model.get_linked_statistics(ls_cfg['VariableName'], ls_cfg['TimePeriod'], ls_cfg['ArclengthRange'])
        # Get the query
        query = linked_statistics.Query(ls_cfg['VariableName'])
        # Get the data
        data = query.Data
        return data

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

