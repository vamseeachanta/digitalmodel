from digitalmodel.infrastructure.common.ship_fatigue_analysis import ShipFatigueAnalysis

sfa = ShipFatigueAnalysis()


class ShipDesign:
    def __init__(self):
        pass

    def router(self, cfg):
        sfa.router(cfg)
