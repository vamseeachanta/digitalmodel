"""YAML input file handling for API STD 2RD burst pressure analysis."""

import yaml


def ymlInput(defaultYml, updateYml):
    with open(defaultYml, "r") as ymlfile:
        cfg = yaml.safe_load(ymlfile)

    if updateYml != None:
        #  Update values file
        try:
            with open(updateYml, "r") as ymlfile:
                cfgUpdateValues = yaml.safe_load(ymlfile)
            #  Convert to logs
            # print(cfgUpdateValues)
            cfg.update(cfgUpdateValues)
        except:
            print(
                "Update Input file could not be loaded successfully. Running program default values"
            )

    return cfg
