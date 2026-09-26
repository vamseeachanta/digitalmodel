import logging
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
ProjectPipe = APIRP2RDPipe(0.24765,0.034925,0.034925,0,0,0,0,5.52E+08)
APIRP2RDPipe.updateAllowableStressFactor(ProjectPipe, 0.666)
APIRP2RDPipe.updateDesignCaseFactor(ProjectPipe, 1)


APIRP2RDPipe.self.pipeNominalID(ProjectPipe)
print("pipeNominalInsideDiameter : ","{:.4e}".format(self.pipeNominalID))
#print("SigmaA : ","{:.3e}".format(self.SigmaA))

print('Finished Analysis')
