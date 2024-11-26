import json
from collections import OrderedDict

import oyaml as yaml

# import ruamel.yaml

noalias_dumper = yaml.dumper.SafeDumper
noalias_dumper.ignore_aliases = lambda self, data: True

def saveDataJson(data, fileName):
    with open(fileName+'.json', 'w') as f:
        json.dump(data, f)

def saveDataYaml(data, fileName, default_flow_style=False):
#     setup_yaml()
    if default_flow_style == None:
        with open(fileName+'.yml', 'w') as f:
            yaml.dump(data, f)
    elif default_flow_style == 'NonAlias':
        with open(fileName+'.yml', 'w') as f:
            yaml.dump(data, f, Dumper=noalias_dumper)
    elif default_flow_style == 'ruamel':
        with open(fileName+'.yml', 'w') as f:
            ruamel.yaml.dump(data, f)
    elif default_flow_style == 'round_trip_dump':
        with open(fileName+'.yml', 'w') as f:
            ruamel.yaml.round_trip_dump(data, f)
    else:
        with open(fileName+'.yml', 'w') as f:
            yaml.dump(data, f, default_flow_style=default_flow_style)

def saveDataFrame(df, fileName):
    df.to_csv(fileName+'.csv')

def setup_yaml():
    """ https://stackoverflow.com/a/8661021 """
    represent_dict_order = lambda self, data:  self.represent_mapping('tag:yaml.org,2002:map', data.items())
    yaml.add_representer(OrderedDict, represent_dict_order)
