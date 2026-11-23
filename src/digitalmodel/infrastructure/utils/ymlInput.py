from collections.abc import Mapping

import yaml


def ymlInput(defaultYml, updateYml):
    """Load a YAML configuration file and optionally update it with values from another YAML file.

    Reads a default YAML configuration file and, if provided, merges in
    values from an update YAML file using deep dictionary update.

    Args:
        defaultYml: Path to the default YAML configuration file.
        updateYml: Path to the update YAML file, or None to skip updating.

    Returns:
        dict: The loaded (and optionally updated) configuration dictionary.
    """
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
    """Recursively update a nested dictionary with values from another dictionary.

    Performs a deep merge of dictionary `u` into dictionary `d`. Nested
    dictionaries are merged recursively rather than being overwritten.

    Args:
        d: The base dictionary to update.
        u: The dictionary containing update values.

    Returns:
        dict: The updated dictionary with values from `u` merged into `d`.
    """
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