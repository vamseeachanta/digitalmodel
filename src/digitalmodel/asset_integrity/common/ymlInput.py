"""YAML input file handling for asset integrity configurations."""

from collections.abc import Mapping
from pathlib import Path

import yaml


def ymlInput(defaultYml, updateYml):

    with Path(defaultYml).open('r', encoding='utf-8') as ymlfile:
        cfg = yaml.safe_load(ymlfile)
    if not isinstance(cfg, Mapping) or not cfg:
        raise ValueError("base configuration must be a nonempty mapping")

    if updateYml != None:
        #  Update values file
        try:
            with Path(updateYml).open('r', encoding='utf-8') as ymlfile:
                cfgUpdateValues = yaml.safe_load(ymlfile)
    #  Convert to logs
    # print(cfgUpdateValues)
            if not isinstance(cfgUpdateValues, Mapping) or not cfgUpdateValues:
                raise ValueError("overlay must be a nonempty mapping")
            cfg = update_deep(cfg, cfgUpdateValues)
        except (OSError, yaml.YAMLError, ValueError) as exc:
            raise ValueError("requested configuration overlay could not be loaded") from exc

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
