from digitalmodel.infrastructure.base_solvers.marine.ship.ship_fatigue_analysis import ShipFatigueAnalysis

sfa = ShipFatigueAnalysis()


class ShipDesign:
    """Ship design analysis controller.

    Routes ship design calculations to the appropriate analysis components,
    including fatigue analysis.
    """

    def __init__(self):
        """Initialize ShipDesign."""
        pass

    def router(self, cfg):
        """Route ship design analysis to the appropriate handler.

        Args:
            cfg: Configuration dictionary with analysis parameters.
        """
        sfa.router(cfg)
