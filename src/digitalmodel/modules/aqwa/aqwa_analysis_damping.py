# Standard library imports
import pathlib

# Third party imports
from assetutilities.common.update_deep import AttributeDict

# Reader imports
from digitalmodel.custom.aqwa.aqwa_analysis_raos import AqwaRAOs

aq_raos = AqwaRAOs()

class AqwaDamping:

    def __init__(self) -> None:
        pass

    def router(self, cfg: dict) -> None:
        self.run_all_files(cfg)

    def run_all_files(self, cfg: dict) -> None:
        input_files = cfg['file_management']['input_files']['DAT']
        
        damping_output = []
        for file in input_files:
            filename_pattern = pathlib.Path(file).stem
            cfg_temp = AttributeDict(cfg.copy())

            analysis_settings_key = 'damping'
            cfg_temp.analysis_settings[analysis_settings_key]['output']['filename'] = filename_pattern
            cfg_temp, damping_cfg = aq_raos.derive_damping(cfg_temp, analysis_settings_key)
            cfg_temp,  additional_damping_values= aq_raos.prepare_damping(cfg_temp, damping_cfg, analysis_settings_key)

            damping_output.append({filename_pattern: cfg_temp.analysis_settings[analysis_settings_key]['output']['damping']})

        cfg['analysis_settings'][analysis_settings_key]['output']['damping'] = damping_output