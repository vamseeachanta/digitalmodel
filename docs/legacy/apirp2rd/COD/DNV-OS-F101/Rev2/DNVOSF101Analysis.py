'''
Author : Vamsee Achanta
Date: 2018-07-12
Objective: To define pipe attributes (data) for offshore structural analysis
'''

import logging

from RigidPipe import DNVOSF101Pipe
from setLogging import setLogging

from ConfigurationManager import ConfigurationManager

# Set Default settings
appConfigurations = ConfigurationManager('config.ini')
appconfigData = appConfigurations.get_configured_values()
logging.debug(appconfigData.defaults.logLevel)
setLogging(appconfigData.defaults.logLevel)

# Define Project Pipe Data
MecorPipe = DNVOSF101Pipe(42, 0.625)
DNVOSF101Pipe.updateThicknessFabricationTolerance(MecorPipe, 0.1)
DNVOSF101Pipe.updateCorrosionAllowance(MecorPipe, 0.2)
DNVOSF101Pipe.updateErosionAllowance(MecorPipe, 0.0)
DNVOSF101Pipe.updateOvality(MecorPipe)

# Burst Calculation
DNVOSF101Pipe.temperatureDeratingValues(MecorPipe)
DNVOSF101Pipe.updateStrengthValues(MecorPipe)
DNVOSF101Pipe.DNVThickness(MecorPipe)
DNVOSF101Pipe.pressureContainment(MecorPipe, 'installation', 'high')
print("Burst margin is: {}" .format(MecorPipe.incidentPressureburstcheckMargin))
print("Burst Check Pass is: {}" .format(MecorPipe.testPressureburstCheckPass))

DNVOSF101Pipe.collapse(MecorPipe, MecorPipe.thickness1['installation'], "UOE")
print("collapse pressure is: {}" .format(MecorPipe.collapsePressure))

# This is an experiment method to solve same equation using polynomials. Choosing a single
# solution is difficult and more understanding and logic are required. 
DNVOSF101Pipe.localBucklingUnderExternalPressure(MecorPipe, MecorPipe.thickness1['installation'], "UOE")
print("Local Buckling Under External Pressure margin is: {}" .format(MecorPipe.localBucklingExternalPressureCheckMargin))
print("Local Buckling Under External Pressure Check Pass is: {}" .format(MecorPipe.localBucklingExternalPressureCheckPass))


DNVOSF101Pipe.propagationBuckling(MecorPipe, MecorPipe.thickness1['installation'], "UOE", "high", "ULS", "Cold")
print("Propagation Buckling margin is: {}" .format(MecorPipe.propagationBucklingCheckMargin))
print("Propagation Buckling Check Pass is: {}" .format(MecorPipe.propagationBucklingCheckPass))

DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled(MecorPipe, MecorPipe.thickness1['installation'], "UOE", "installation", "high", "ULS", "Cold", "Other")
print("Local Buckling due to Combined Loading Load Controlled margin is: {}" .format(MecorPipe.localBucklingCombinedLoadingLoadControlledCheckMargin))
print("Local Buckling due to Combined Loading Load Controlled Check Pass is: {}" .format(MecorPipe.localBucklingCombinedLoadingLoadControlledCheckPass))

DNVOSF101Pipe.localBucklingCombinedLoadingDisplacementControlled(MecorPipe, MecorPipe.thickness1['installation'], "UOE", "installation", "high", "ULS", "Cold", "Other")
print("Local Buckling due to Combined Loading Displacement controlled margin is: {}" .format(MecorPipe.localBucklingCombinedLoadingDisplacementControlledCheckMargin))
print("Local Buckling due to Combined Loading Displacement controlled Check Pass is: {}" .format(MecorPipe.localBucklingCombinedLoadingDisplacementControlledCheckPass))
