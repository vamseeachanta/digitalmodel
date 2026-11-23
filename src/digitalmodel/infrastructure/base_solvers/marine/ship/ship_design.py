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

        Returns:
            The (possibly updated) configuration dictionary. The engine assigns
            ``cfg_base = ship_design.router(cfg_base)``; without a return the
            config would be nulled (cf. #757 for the rigging router).
        """
        cfg = sfa.router(cfg)
        return cfg
