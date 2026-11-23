"""Legacy catenary riser geometry and configuration calculations."""

from typing import Dict, Any

from digitalmodel.subsea.catenary_riser.legacy.catenary_equation import CatenaryCalculator

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
        if not isinstance(cfg, dict):
            raise ValueError("calculation requires a configuration dictionary")
        calculation = cfg.get("calculation")
        if calculation is None:
            metadata = cfg.get("meta", {})
            if not isinstance(metadata, dict):
                raise ValueError("calculation metadata must be a dictionary")
            calculation = metadata.get("calculation")
        if calculation not in ("catenary", "catenary_riser"):
            raise ValueError("calculation must be catenary or catenary_riser")

        if calculation == "catenary":
            cfg = catenary_calculator.calculate(cfg)
        elif calculation == "catenary_riser":
            from .catenary_riser import catenary_riser
            cfg = catenary_riser(cfg)

        return cfg
