import logging
import math
# import matplotlib.pyplot as pyplot
# import munch

from APIRP2RD import APIRP2RDPipe
from setLogging import setLogging


from ConfigurationManager import ConfigurationManager

# Set Default settings
appConfigurations = ConfigurationManager('config.ini')
appconfigData = appConfigurations.get_configured_values()
setLogging(appconfigData.defaults.logLevel)
logging.debug(appconfigData.defaults.logLevel)

# Define Project Pipe Data
MecorPipe = APIRP2RDPipe(0.24765,0.034925,0.034925,0,0,0,0,5.52E+08)
APIRP2RDPipe.updateAllowableStressFactor(MecorPipe, 0.666)
APIRP2RDPipe.updateDesignCaseFactor(MecorPipe, 1)


APIRP2RDPipe.self.pipeNominalID(MecorPipe)
print("pipeNominalInsideDiameter : ","{:.4e}".format(self.pipeNominalID))
#print("SigmaA : ","{:.3e}".format(self.SigmaA))

print('Finished Analysis')
