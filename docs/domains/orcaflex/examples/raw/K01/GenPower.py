def genPwr(info):
    power = info.modelObject.TimeHistory('Generator power', info.period, info.objectExtra)
    return power * 0.944

def UserDefinedResults(model):
    return (
        {
            'ObjectType': OrcFxAPI.otTurbine,
            'Name': 'Generator power (94.4%)',
            'Units': '$P',
            'TimeDomainFunction': genPwr
        },
    )