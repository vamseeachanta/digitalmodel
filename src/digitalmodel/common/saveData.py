import json
from collections import OrderedDict

import yaml
from yaml import SafeDumper, Dumper, SafeLoader

noalias_dumper = yaml.dumper.SafeDumper
noalias_dumper.ignore_aliases = lambda self, data: True


def saveDataJson(data, fileName):
    with open(fileName + '.json', 'w') as f:
        json.dump(data, f)


class OrderedDumper(SafeDumper):

    def __init__(self, *args, **kwargs):
        super(OrderedDumper, self).__init__(*args, **kwargs)
        represent_dict_order = lambda self, data: self.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, data.items())
        self.add_representer(OrderedDict, represent_dict_order)


def dump_ordered_yaml(ordered_data, output_filename, Dumper=yaml.Dumper):

    class OrderedDumper(Dumper):
        pass

    class UnsortableList(list):

        def sort(self, *args, **kwargs):
            pass

    class UnsortableOrderedDict(OrderedDict):

        def items(self, *args, **kwargs):
            return UnsortableList(OrderedDict.items(self, *args, **kwargs))

    OrderedDumper.add_representer(
        UnsortableOrderedDict, yaml.representer.SafeRepresenter.represent_dict)
    with open(output_filename, "w") as f:
        yaml.dump(ordered_data, f, Dumper=OrderedDumper)


def dump_ordered(dictionary):
    """
    Dump ordered dictionary
    """
    yaml.add_representer(
        OrderedDict, lambda dumper, data: dumper.represent_mapping(
            'tag:yaml.org,2002:map', data.items()))

    return yaml.dump(dictionary)


def saveDataYaml(data, fileName, default_flow_style=False):
    #     setup_yaml()
    if default_flow_style == None:
        with open(fileName + '.yml', 'w') as f:
            yaml.dump(data, f)
    elif default_flow_style == 'NonAlias':
        with open(fileName + '.yml', 'w') as f:
            yaml.dump(data, f, Dumper=noalias_dumper)
    elif default_flow_style == 'OrderedDumper':
        with open(fileName + '.yml', 'w') as f:
            yaml.dump(data, f, Dumper=OrderedDumper)
    elif default_flow_style == 'ruamel':
        with open(fileName + '.yml', 'w') as f:
            ruamel.yaml.dump(data, f)
    elif default_flow_style == 'round_trip_dump':
        with open(fileName + '.yml', 'w') as f:
            ruamel.yaml.round_trip_dump(data, f)
    else:
        with open(fileName + '.yml', 'w') as f:
            yaml.dump(data, f, default_flow_style=default_flow_style)


def saveDataFrame(df, fileName):
    df.to_csv('results\\catenary\\' + fileName + '.csv')


def setup_yaml():
    """ https://stackoverflow.com/a/8661021 """
    represent_dict_order = lambda self, data: self.represent_mapping(
        'tag:yaml.org,2002:map', data.items())
    yaml.add_representer(OrderedDict, represent_dict_order)
