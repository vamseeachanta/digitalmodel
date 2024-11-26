import configparser
import os
from os.path import abspath, dirname

from custom.DNV_OS_F101.AttributeDict import AttributeDict


class ConfigurationManager():
    def __init__(self, fileName=None):
        self.fileName = fileName
        self.cf = self.get_project_config()

    def get_project_config(self):
        cf = configparser.ConfigParser()
        cf.optionxform = str
        # cf.read(os.path.join((abspath(dirname(dirname(__file__)))), 'config.ini'))
        cf.read(self.fileName)
        return cf

    def represents_int(self, s):
        try:
            int(s)
            return True
        except ValueError:
            return False

    def represents_float(self, s):
        try:
            float(s)
            return True
        except ValueError:
            return False

    def __adjust_dict_values_based_on_represented_type(self, input_dict):
        new_dict = {}

        for k,v in input_dict.items():
            if k.endswith("flag"):
                if v in ["1", "True", "true"]:
                    new_dict[k] = True
                elif v in ["0", "False", "false"]:
                    new_dict[k] = False
                else:
                    new_dict[k] = v
            elif self.represents_int(v):
                new_dict[k] = int(v)
            elif self.represents_float(v):
                new_dict[k] = float(v)
            else:
                new_dict[k] = v

        return new_dict

    def get_configured_values(self) -> object:

        configured_values = AttributeDict()

        for section in self.cf.sections():
            configured_values[section]=AttributeDict()
            adjusted_main_section_dict = self.__adjust_dict_values_based_on_represented_type(dict(self.cf[section].items()))
            configured_values[section].update(adjusted_main_section_dict)

        return configured_values

