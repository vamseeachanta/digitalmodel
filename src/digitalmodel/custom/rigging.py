import os
import pkgutil
import pandas as pd

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.data import ReadFromExcel
from digitalmodel.custom.rigging_components import Slings
from digitalmodel.custom.rigging_components import Shackles



read_excel = ReadFromExcel()

slings = Slings()
shackles = Shackles()


class Rigging:

    def __init__(self):
        pass

    def get_rigging_groups(self, cfg=None):
        self.cfg = cfg
        rigging_groups = self.cfg.rigging_groups
        for rigging_group in rigging_groups:
            self.get_rigging_group(rigging_group)

    def get_rigging_group(self, rigging_group=None):
        rigging_array = rigging_group['rigging_array']
        for rigging in rigging_array:
            if rigging['category'] == 'sling':
                rigging_dict = self.get_sling(rigging)
            elif rigging['category'] == 'shackle':
                rigging_dict = self.get_shackle(rigging)

    def get_sling(self, cfg=None):
        slings.get_sling(cfg)
        return 1

    def get_shackle(self, cfg=None):
        shackles.get_shackle(cfg)
        return 1