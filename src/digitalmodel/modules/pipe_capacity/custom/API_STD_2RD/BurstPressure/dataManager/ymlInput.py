import yaml


def ymlInput(defaultYml, updateYml):
    with open(defaultYml, 'r') as ymlfile:
        cfg = yaml.load(ymlfile)

    if updateYml != None :
    #  Update values file
        try:
            with open(updateYml, 'r') as ymlfile:
                cfgUpdateValues = yaml.load(ymlfile)
    #  Convert to logs
            # print(cfgUpdateValues)
            cfg.update(cfgUpdateValues)
        except:
            print("Update Input file could not be loaded successfully. Running program default values")

    return cfg