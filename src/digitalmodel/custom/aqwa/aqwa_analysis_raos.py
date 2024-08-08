# Standard library imports
import os


# Third party imports
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.yml_utilities import WorkingWithYAML
from assetutilities.engine import engine as au_engine

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities

wwy = WorkingWithYAML()

au = AqwaUtilities()

class AqwaRAOs: 

    def __init__(self) -> None:
        pass

    def rao_router(self, cfg: dict) -> None:
        self.split_dat_to_decks(cfg)

    #TODO
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
        self.create_decks_directory(cfg)
        template_yaml = self.get_template_SplitToDeck(cfg)

        au_engine(inputfile=None, cfg=template_yaml, config_flag=False)

    def get_template_SplitToDeck(self, cfg):
        template_split_to_decks = cfg['analysis_settings']['split_to_decks']['template']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_split_to_decks,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)

        # template_yaml ['Analysis'] = custom_analysis_dict
        template_yaml = AttributeDict(template_yaml )
        template_yaml["Analysis"] = cfg["Analysis"].copy()
        template_yaml["file_management"] = cfg["file_management"].copy()

        return template_yaml

    def create_decks_directory(self, cfg):
        file_management_input_directory = cfg['Analysis']['file_management_input_directory']
        file_management_output_directory = os.path.join(file_management_input_directory, 'decks')
        if not os.path.exists(file_management_output_directory):
            os.makedirs(file_management_output_directory)

        cfg['Analysis']['file_management_output_directory'] = file_management_output_directory
        cfg['file_management']['files']['output_directory'] = file_management_output_directory