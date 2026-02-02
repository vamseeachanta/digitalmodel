from collections.abc import Mapping

import yaml


def ymlInput(defaultYml, updateYml):

    with open(defaultYml, 'r') as ymlfile:
        cfg = yaml.safe_load(ymlfile)

    if updateYml != None:
        #  Update values file
        try:
            with open(updateYml, 'r') as ymlfile:
                cfgUpdateValues = yaml.safe_load(ymlfile)
    #  Convert to logs
    # print(cfgUpdateValues)
            cfg = update_deep(cfg, cfgUpdateValues)
        except:
            print(
                "Update Input file could not be loaded successfully. Running program default values"
            )

    return cfg


def update_deep(d, u):
    for k, v in u.items():
        # this condition handles the problem
        if not isinstance(d, Mapping):
            d = u
        elif isinstance(v, Mapping):
            r = update_deep(d.get(k, {}), v)
            d[k] = r
        else:
            d[k] = u[k]

    return d