import OrcFxAPI

model = OrcFxAPI.Model('Relative velocity.sim')
crane = model['Crane tip']
environment = model['Environment']

times = model.SampleTimes(OrcFxAPI.Period(1))
MaxRelVelocity = float('-inf')

for time in times:
    period = OrcFxAPI.SpecifiedPeriod(time, time)
    CraneTipVelocity = crane.TimeHistory('GZ-Velocity', period)[0]
    X = crane.TimeHistory('X', period)[0]
    Y = crane.TimeHistory('Y', period)[0]
    SeaZVelocity = environment.TimeHistory('Z Velocity', period, OrcFxAPI.oeEnvironment(X, Y, OrcFxAPI.OrcinaDefaultReal()))[0]

    RelVelocity = abs(SeaZVelocity-CraneTipVelocity)
    if RelVelocity >= MaxRelVelocity:
        MaxRelVelocity = RelVelocity

print '\nMaximum Relative Velocity is ', MaxRelVelocity, ' m/s'
raw_input('\n\nPress the enter key to end.')