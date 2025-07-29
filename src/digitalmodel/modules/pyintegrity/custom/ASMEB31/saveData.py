import json
from collections import OrderedDict

import yaml


def saveDataJson(data, fileName):
    with open(fileName+'.json', 'w') as f:
        json.dump(data, f)

def saveDataYaml(data, fileName, default_flow_style=False):
#     setup_yaml()
    if default_flow_style == None:
        with open(fileName+'.yml', 'w') as f:
            yaml.dump(data, f)
    else:
        with open(fileName+'.yml', 'w') as f:
            yaml.dump(data, f, default_flow_style=default_flow_style)
			

def saveDataFrame(df, fileName):
    df.to_csv('results\\ASMEB31\\Data\\'+fileName+'.csv')

def setup_yaml():
    """ https://stackoverflow.com/a/8661021 """
    represent_dict_order = lambda self, data:  self.represent_mapping('tag:yaml.org,2002:map', data.items())
    yaml.add_representer(OrderedDict, represent_dict_order)
