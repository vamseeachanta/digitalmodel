# Standard library imports

# Third party imports
from assetutilities.common.data_exploration import DataExploration
from assetutilities.common.file_management import FileManagement

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities
from digitalmodel.custom.aqwa.ef_server.AqwaServerMgr import *

fm = FileManagement()
au = AqwaUtilities()
de = DataExploration()

class AqwaRAOs: 

    def __init__(self) -> None:
        pass

    def rao_router(self, cfg: dict) -> None:
        pass

    #TODO
    # SPlit .dat into DECKs (Status: manual_process)
    # Define weight, inertia, etc (DECK ?, Deck ??)
    # Run AQWA
    # Get hydrostatic output
    # Get RAOs
    # Get RAOs output and identify peaks
    # Ensure frequency resoultion is sufficient around peaks
    # Define frequency independent damping
    # Rerun Diffraction analysis
    # Plot RAOs
    # Plot RAOs comparison

