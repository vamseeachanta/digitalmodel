import pandas as pd
from pyintegrity.common.yml_utilities import WorkingWithYAML

ww_yaml = WorkingWithYAML()


def ExcelRead(data):
    ww_yaml_cfg = {'filename': data['io']}
    data['io'] = ww_yaml.get_library_filename(ww_yaml_cfg)

    df = pd.read_excel(data['io'], sheet_name = data['sheet_name'], \
            skiprows=data['skiprows'], skipfooter= data['skipfooter'], \
            index_col=data['index_col'])

    return df
