from assetutilities.common.data import ReadFromExcel

from digitalmodel.specialized.rigging.rigging_components import Slings
from digitalmodel.specialized.rigging.rigging_components import Shackles

read_excel = ReadFromExcel()

slings = Slings()
shackles = Shackles()


class Rigging:
    def __init__(self):
        pass

    def get_rigging_groups(self, cfg=None):
        self.cfg = cfg
        rigging_groups = self.cfg.rigging["groups"]
        for rigging_group in rigging_groups:
            self.get_rigging_group(rigging_group)
        # Return the cfg so the engine's `cfg_base = ...router/get_rigging_groups`
        # assignment doesn't null the config (was returning None).
        return self.cfg

    def get_rigging_group(self, rigging_group=None):
        rigging_elements = rigging_group["elements"]
        for rigging in rigging_elements:
            if rigging["category"] == "sling":
                rigging_dict = self.get_sling(rigging)
            elif rigging["category"] == "shackle":
                rigging_dict = self.get_shackle(rigging)

    def get_sling(self, cfg=None):
        slings.get_sling(cfg)
        return 1

    def get_shackle(self, cfg=None):
        shackles.get_shackle(cfg)
        return 1
