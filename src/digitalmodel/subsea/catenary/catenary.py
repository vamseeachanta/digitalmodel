from typing import Dict, Any

from digitalmodel.subsea.catenary.catenary_equation import CatenaryCalculator
from digitalmodel.subsea.catenary.catenary_riser import catenary_riser

catenary_calculator = CatenaryCalculator()


class Catenary:
    """
    Class for handling catenary calculations and routing operations.
    """

    def __init__(self):
        pass

    def router(self, cfg: Dict[str, Any] = None) -> Any:
        """
        Route catenary operations based on the provided configuration.

        Args:
            cfg (Dict[str, Any], optional): Configuration dictionary
        """
        calculation = cfg.get(
            "calculation", cfg.get("meta", {}).get("calculation", None)
        )
        if not calculation:
            return cfg

        if calculation == "catenary":
            cfg = catenary_calculator.calculate(cfg)
        elif "catenary_riser" in calculation:
            cfg = catenary_riser(cfg)

        return cfg
