# Standard library imports

# Third party imports
from assetutilities.common.yml_utilities import WorkingWithYAML

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities

wwy = WorkingWithYAML()

au = AqwaUtilities()
de = DataExploration()

class AqwaRAOs: 

    def __init__(self) -> None:
        pass

    def rao_router(self, cfg: dict) -> None:
        self.split_dat_to_decks(cfg)

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

    def split_dat_to_decks(self, cfg: dict) -> None:
        template_split_to_decks = cfg['analysis_settings']['split_to_decks']['template']
        
        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_split_to_decks,
            'library_name': library_name
        }


        template_yaml = wwy.get_library_yaml_file(library_yaml_cfg)
        # template_yaml ['Analysis'] = custom_analysis_dict
        template_yaml = AttributeDict(template_yaml )
     
        dat_files = cfg['file_management']['input_files']['DAT']
        for dat_file in dat_files:
            
            decks = au.split_dat_to_decks(dat_file)
            cfg['file_management']['input_files']['DECK'] = decks
