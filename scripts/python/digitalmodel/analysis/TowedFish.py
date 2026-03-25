import OrcFxAPI
import time
from scipy.optimize import fsolve

model = OrcFxAPI.Model('Towed Fish.dat')
fish = model['Towfish']
towline = model['Tow Line']

targetDepth = -85
initialLength = towline.Length[0]
iteration = 0

def calcDepth(Length): #function to run statics and find current depth of towfish
	towline.Length[0] = Length #updates the line length in the model
	model.CalculateStatics() #runs statics in OrcaFlex
	global iteration
	iteration += 1
	print '\nIteration #', iteration,'. \n   Tow Line Length =', Length, '\n   Fish Depth =', fish.StaticResult('Z')
	return fish.StaticResult('Z') - targetDepth #calculates the error
   
# main code
start = time.clock()
fsolve(calcDepth, initialLength)
print '\n\nCompleted in', time.clock()-start, 's, ', iteration, 'iterations.'
model.SaveSimulation('Results.sim')
raw_input('\n\nPress the enter key to end.')