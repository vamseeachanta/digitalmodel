import os
import pkgutil
import pandas as pd

from assetutilities.common.data import ReadFromExcel
from digitalmodel.custom.fea_model.LineType_components import LineType
from digitalmodel.custom.fea_model.line_components import Line

read_excel = ReadFromExcel()


class Slings:

    def __init__(self):

        self.twinpathslings_datafile = pkgutil.get_data(
            'digitalmodel',
            'data/slings/' + 'MIS-ProductSheets_twinPathSlings.xlsx')
        self.sling_data = self.get_vendor_data()

    def get_vendor_data(self, cfg=None):
        cfg_data = {'io': self.twinpathslings_datafile}
        excel_data = read_excel.from_xlsx(cfg_data)
        twinpathslings_data = excel_data['Sheet1']
        return {'twinpathslings': twinpathslings_data}

    def get_sling(self, cfg=None):
        sling = {}
        if cfg['subcategory'] in ['twinpathslings', 'polyester_endless_round']:
            sling = self.get_twinpathslings(cfg)
            sling_model = self.get_twinpathslings_model(sling)

        return sling, sling_model

    def get_twinpathslings(self, cfg=None):
        sling_data = self.sling_data['twinpathslings']
        sling = sling_data.loc[sling_data['part_number'] ==
                               cfg['part_number']].to_dict('records')[0]
        return sling

    def get_twinpathslings_model(self, sling=None):
        cfg_model = {}
        cfg_model['OD'] = sling['diameter']
        sling_model = self.get_twinpathslings_model({'cfg': cfg_model})

        return sling_model

    def GetLineTypeProperties(self, Asset):
        cfg = Asset
        line_type = LineType(cfg)
        fea_data = line_type.get_orcaflex_properties()

        return fea_data

    def GetLineProperties(self, Asset):
        cfg = Asset
        line = Line(cfg)
        fea_data = line.get_orcaflex_properties()

        return fea_data


class Shackles:

    def __init__(self):

        self.G2100_datafile = pkgutil.get_data(
            'digitalmodel', 'data/crosby/' + '204_subsea_shackles_g2100.xlsx')
        self.G2110_datafile = pkgutil.get_data(
            'digitalmodel', 'data/crosby/' + '204_subsea_shackles_g2110.xlsx')
        self.G2130_datafile = pkgutil.get_data(
            'digitalmodel',
            'data/crosby/' + '26_shackles_bolt_type_anchor_g2130.xlsx')
        self.shackle_data = self.get_vendor_data()

    def get_vendor_data(self, cfg=None):
        cfg_G2100_data = {'io': self.G2100_datafile}
        excel_data = read_excel.from_xlsx(cfg_G2100_data)
        G2100_data = excel_data['Sheet1']

        cfg_G2110_data = {'io': self.G2110_datafile}
        excel_data = read_excel.from_xlsx(cfg_G2110_data)
        G2110_data = excel_data['Sheet1']

        cfg_G2130_data = {'io': self.G2130_datafile}
        excel_data = read_excel.from_xlsx(cfg_G2130_data)
        G2130_data = excel_data['Sheet1']

        shackle_data = {
            'G2100': G2100_data,
            'G2110': G2110_data,
            'G2130': G2130_data
        }

        return shackle_data

    def get_shackle(self, cfg=None):
        shackle = {}
        if cfg['subcategory'] in ['twinpathslings', 'polyester_endless_round']:
            shackle = self.get_twinpathslings(cfg)

        return shackle

    def get_shackle_model(self, cfg=None):
        shackle_model_cfg = {}
        shackle_data = self.shackle_data[cfg['subcategory']]
        shackle = shackle_data.loc[shackle_data['part_number'] ==
                                   cfg['part_number']].to_dict('records')[0]
        return shackle
