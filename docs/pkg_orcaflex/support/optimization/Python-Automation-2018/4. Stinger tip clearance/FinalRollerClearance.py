import OrcFxAPI
import sys
from scipy.optimize import fsolve

model = OrcFxAPI.Model('E01 Explicit Geometry Stinger.dat')
vessel = model['Vessel1']
pipe = model['Pipe']

targetClearance = 0.2
supportNo = 11

def calcClearance(length): 
    pipe.Length[1] = length
    try:
        model.CalculateStatics()
    except:
        sys.exit('Static calculation has failed.')

    clearance = vessel.StaticResult('Support Contact Clearance',  
                                   OrcFxAPI.oeSupport(supportNo))

    print('\nCurrent section length = {}'.format(length[0]))
    print('\nCurrent section length = ', length[0])
    print('   Clearance = {}'.format(clearance))
    
    return clearance - targetClearance

initialLength = pipe.Length[1]
optimisation = fsolve(calcClearance, initialLength, full_output = True)

print('\n\nCompleted in', optimisation[1]['nfev'], 'iterations.')
print('Total pipe length =', round(pipe.CumulativeLength[-1], 3), 'm')

model.SaveSimulation('Results.sim')