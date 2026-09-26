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
ProjectPipe = DNVOSF101Pipe(42, 0.625)
DNVOSF101Pipe.updateThicknessFabricationTolerance(ProjectPipe, 0.1)
DNVOSF101Pipe.updateCorrosionAllowance(ProjectPipe, 0.2)
DNVOSF101Pipe.updateErosionAllowance(ProjectPipe, 0.0)
DNVOSF101Pipe.updateOvality(ProjectPipe)

# Burst Calculation
DNVOSF101Pipe.temperatureDeratingValues(ProjectPipe)
DNVOSF101Pipe.updateStrengthValues(ProjectPipe)
DNVOSF101Pipe.DNVThickness(ProjectPipe)
DNVOSF101Pipe.pressureContainment(ProjectPipe, 'installation', 'high')
print("Burst margin is: {}" .format(ProjectPipe.incidentPressureburstcheckMargin))
print("Burst Check Pass is: {}" .format(ProjectPipe.testPressureburstCheckPass))

DNVOSF101Pipe.collapse(ProjectPipe, ProjectPipe.thickness1['installation'], "UOE")
print("collapse pressure is: {}" .format(ProjectPipe.collapsePressure))

# This is an experiment method to solve same equation using polynomials. Choosing a single
# solution is difficult and more understanding and logic are required. 
DNVOSF101Pipe.localBucklingUnderExternalPressure(ProjectPipe, ProjectPipe.thickness1['installation'], "UOE")
print("Local Buckling Under External Pressure margin is: {}" .format(ProjectPipe.localBucklingExternalPressureCheckMargin))
print("Local Buckling Under External Pressure Check Pass is: {}" .format(ProjectPipe.localBucklingExternalPressureCheckPass))


DNVOSF101Pipe.propagationBuckling(ProjectPipe, ProjectPipe.thickness1['installation'], "UOE", "high", "ULS", "Cold")
print("Propagation Buckling margin is: {}" .format(ProjectPipe.propagationBucklingCheckMargin))
print("Propagation Buckling Check Pass is: {}" .format(ProjectPipe.propagationBucklingCheckPass))

DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled(ProjectPipe, ProjectPipe.thickness1['installation'], "UOE", "installation", "high", "ULS", "Cold", "Other")
print("Local Buckling due to Combined Loading Load Controlled margin is: {}" .format(ProjectPipe.localBucklingCombinedLoadingLoadControlledCheckMargin))
print("Local Buckling due to Combined Loading Load Controlled Check Pass is: {}" .format(ProjectPipe.localBucklingCombinedLoadingLoadControlledCheckPass))

DNVOSF101Pipe.localBucklingCombinedLoadingDisplacementControlled(ProjectPipe, ProjectPipe.thickness1['installation'], "UOE", "installation", "high", "ULS", "Cold", "Other")
print("Local Buckling due to Combined Loading Displacement controlled margin is: {}" .format(ProjectPipe.localBucklingCombinedLoadingDisplacementControlledCheckMargin))
print("Local Buckling due to Combined Loading Displacement controlled Check Pass is: {}" .format(ProjectPipe.localBucklingCombinedLoadingDisplacementControlledCheckPass))
