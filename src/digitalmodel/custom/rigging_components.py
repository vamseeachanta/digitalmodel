import os
import pkgutil
import pandas as pd

from digitalmodel.common.data import ReadFromExcel

read_excel = ReadFromExcel()


class Slings:

    def __init__(self):

        self.twinpathslings_datafile = pkgutil.get_data(
            'digitalmodel',
            'data/slings/' + 'MIS-ProductSheets_twinPathSlings.xlsx')

    def get_vendor_data(self, cfg=None):
        cfg_sling_data = {'io': self.twinpathslings_datafile}
        twinpathslings_data = read_excel.from_xlsx(cfg_sling_data)
        return twinpathslings_data

    def get_sling(self, cfg=None):
        twinpathslings_data = self.get_vendor_data()


class Shackles:

    def __init__(self):

        self.twinpathslings_datafile = pkgutil.get_data(
            'digitalmodel',
            'data/slings/' + 'MIS-ProductSheets_twinPathSlings.xlsx')

    def get_vendor_data(self, cfg=None):
        cfg_sling_data = {'io': self.twinpathslings_datafile}
        twinpathslings_data = read_excel.from_xlsx(cfg_sling_data)
        return twinpathslings_data

    def get_shackle(self, cfg=None):
        twinpathslings_data = self.get_vendor_data()
