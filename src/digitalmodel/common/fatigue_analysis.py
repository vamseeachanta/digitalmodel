class FatigueAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg["inputs"]["software"] == "orcaflex":
            raise NotImplementedError
        elif cfg["inputs"]["software"] is None:
            pass
        return cfg

    def damage_from_stress_range(self, cfg):
        raise NotImplementedError
