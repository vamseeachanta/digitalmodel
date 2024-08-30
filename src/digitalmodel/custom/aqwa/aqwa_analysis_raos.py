# Standard library imports
import logging
import math
import os

# Third party imports
import pandas as pd
from assetutilities.common.file_management import FileManagement
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.yml_utilities import WorkingWithYAML  # noqa
from assetutilities.engine import engine as au_engine
from scipy import interpolate

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities  # noqa
from digitalmodel.engine import engine as dm_engine


wwy = WorkingWithYAML()

au = AqwaUtilities()
fm = FileManagement()

class AqwaRAOs: 

    def __init__(self) -> None:
        pass

    def rao_router(self, cfg: dict) -> None:
        self.split_dat_to_decks(cfg)
        self.prepare_decks(cfg)
        cfg = self.prepare_no_damping_runs(cfg)
        self.run_raos(cfg)
        damping_cfg = self.extract_damping(cfg)
        self.prepare_damping(cfg, damping_cfg)

        # cfg = self.prepare_damping_runs(cfg, additional_damping)

        return cfg

    def split_dat_to_decks(self, cfg: dict) -> None:
        self.create_decks_directory(cfg)
        template_yaml = self.get_template_SplitToDeck(cfg)

        au_engine(inputfile=None, cfg=template_yaml, config_flag=False)

    def prepare_decks(self, cfg: dict) -> None:
        self.create_decks_directory(cfg)

        drafts = cfg['analysis_settings']['drafts']
        for draft in drafts:
            template_yaml = self.get_template_prepare_decks(cfg, draft)
            dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)

    def prepare_no_damping_runs(self, cfg: dict) -> None:
        template_yaml = self.get_template_no_damping_runs(cfg)
        cfg_temp = au_engine(inputfile=None, cfg=template_yaml, config_flag=False)

        no_damp_filename = cfg_temp[cfg_temp['basename']]['output_filename'][0][0]
        cfg['analysis_settings']['damp_no']['output'] = {'filename': no_damp_filename}

        return cfg

    def get_template_SplitToDeck(self, cfg):
        template_file_name = cfg['analysis_settings']['split_to_decks']['template']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)

        template_yaml = AttributeDict(template_yaml )
        template_yaml["Analysis"] = cfg["Analysis"].copy()
        template_yaml["file_management"] = cfg["file_management"].copy()

        return template_yaml

    def get_template_prepare_decks(self, cfg, draft):
        template_file_name = draft['template']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)
        template_yaml = AttributeDict(template_yaml )
        template_yaml['inputs'] = draft['inputs'].copy()
        template_yaml["Analysis"] = cfg["Analysis"].copy()
        template_yaml["file_management"] = cfg["file_management"].copy()

        return template_yaml

    def get_template_no_damping_runs(self, cfg):
        no_damping_cfg = cfg['analysis_settings']['damp_no'].copy()
        template_file_name = no_damping_cfg['template']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)
        template_yaml = AttributeDict(template_yaml)
        template_yaml['input'] = no_damping_cfg['input']
        template_yaml["Analysis"] = cfg["Analysis"].copy()
        template_yaml["file_management"] = cfg["file_management"].copy()
        
        input_cfg = template_yaml['input']
        analysis_root_folder = cfg['Analysis']['analysis_root_folder']

        for input in input_cfg:
            input_files_cfg = input['input_files']
            for input_files_array in input_files_cfg:
                for file_idx in range(0, len(input_files_array)):
                    input_file = input_files_array[file_idx]
                    is_file_valid, relative_input_file = is_file_valid_func(input_file, analysis_root_folder)
                    if is_file_valid:
                        input_files_array[file_idx] = relative_input_file
                    else:
                        library_filename_cfg = {
                            'filename': input_file,
                            'library_name': library_name
                        }
                        library_filename = wwy.get_library_filename(library_filename_cfg)
                        if os.path.isfile(library_filename):
                            input_files_array[file_idx] = library_filename
                        else:
                            raise FileNotFoundError(f"File {input_file} not found in {analysis_root_folder} or {module_path}")

        return template_yaml

    def get_template_damping_runs(self, cfg):
        ad_damping_cfg = cfg['analysis_settings']['damp_ad'].copy()
        template_file_name = ad_damping_cfg['template']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)
        template_yaml = AttributeDict(template_yaml)
        template_yaml['input'] = ad_damping_cfg['input']
        template_yaml["Analysis"] = cfg["Analysis"].copy()
        template_yaml["file_management"] = cfg["file_management"].copy()
        
        input_cfg = template_yaml['input']
        analysis_root_folder = cfg['Analysis']['analysis_root_folder']

        for input in input_cfg:
            input_files_cfg = input['input_files']
            for input_files_array in input_files_cfg:
                for file_idx in range(0, len(input_files_array)):
                    input_file = input_files_array[file_idx]
                    is_file_valid, relative_input_file = is_file_valid_func(input_file, analysis_root_folder)
                    if is_file_valid:
                        input_files_array[file_idx] = relative_input_file
                    else:
                        library_filename_cfg = {
                            'filename': input_file,
                            'library_name': library_name
                        }
                        library_filename = wwy.get_library_filename(library_filename_cfg)
                        if os.path.isfile(library_filename):
                            input_files_array[file_idx] = library_filename
                        else:
                            raise FileNotFoundError(f"File {input_file} not found in {analysis_root_folder} or {module_path}")

        return template_yaml

    def create_decks_directory(self, cfg):
        file_management_input_directory = cfg['Analysis']['file_management_input_directory']
        file_management_output_directory = os.path.join(file_management_input_directory, 'decks')
        if not os.path.exists(file_management_output_directory):
            os.makedirs(file_management_output_directory)

        cfg['Analysis']['file_management_output_directory'] = file_management_output_directory
        cfg['file_management']['files']['output_directory'] = file_management_output_directory

    def run_raos(self, cfg):
        #TODO - Run AQWA
        print("Running AQWA RAO Runs manually.")
        pass


    def extract_damping(self, cfg):
        natural_period = self.get_natural_period(cfg)
        percent_critical_damping = self.get_percent_critical_damping(cfg)
        absolute_damping = self.get_absolute_damping(cfg)
        no_damp_raos = self.get_no_damp_raos(cfg)

        damping_cfg = {
            'natural_period': natural_period, 
            'percent_critical_damping': percent_critical_damping,
            'absolute_damping': absolute_damping,
            'no_damp_raos': no_damp_raos
        }
        
        return damping_cfg

    def get_natural_period(self, cfg):
        logging.info("Extracting natural period values")
        template_yaml = self.get_template_natural_period(cfg)
        # template_yaml = fm.router(template_yaml)
        
        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        cfg['analysis_settings']['damping']['output'] = {'natural_period': cfg_temp[cfg_temp['basename']]}
        
        csv_file = cfg['analysis_settings']['damping']['output']['natural_period']['csv_filename'][0]
        natural_period = pd.read_csv(csv_file)
        
        return natural_period

    def get_percent_critical_damping(self, cfg):
        logging.info("Extracting critical damping values")
        template_yaml = self.get_template_critical_damping(cfg)

        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        cfg['analysis_settings']['damping']['output'] = {'critical_damping': cfg_temp[cfg_temp['basename']]}
        
        csv_file = cfg['analysis_settings']['damping']['output']['critical_damping']['csv_filename'][0]
        percent_critical_damping = pd.read_csv(csv_file)
        
        return percent_critical_damping

    def get_absolute_damping(self, cfg):
        logging.info("Extracting absolute damping values")
        template_yaml = self.get_template_absolute_damping(cfg)
        
        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        absolute_damping_cfg = {'absolute_damping': cfg_temp[cfg_temp['basename']]}
        cfg['analysis_settings']['damping']['output'].update(absolute_damping_cfg)
        
        csv_file = cfg['analysis_settings']['damping']['output']['absolute_damping']['csv_filename']
        absolute_damping = pd.read_csv(csv_file)

        return absolute_damping

    def get_no_damp_raos(self, cfg):
        logging.info("Extracting RAOs with no damping")
        template_yaml = self.get_template_no_damp_raos(cfg)

        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        raos_cfg = {'raos': cfg_temp[cfg_temp['basename']]}
        cfg['analysis_settings']['damping']['output'].update(raos_cfg)

        csv_file = cfg['analysis_settings']['damping']['output']['raos']['csv_filename']
        no_damp_raos = pd.read_csv(csv_file)
        
        return no_damp_raos

    def get_template_natural_period(self, cfg):
        logging.info("Extracting natural period values")
        template_file_name = cfg['analysis_settings']['damping']['template']['natural_period']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)

        template_yaml = AttributeDict(template_yaml )
        template_yaml["Analysis"] = cfg["Analysis"].copy()

        template_yaml['file_management']['files']['files_in_current_directory']['directory'] = cfg['Analysis']['file_management_input_directory']

        return template_yaml

    def get_template_critical_damping(self, cfg):
        #TODO Fix to get complete critical damping values. Currently only getting half the values from single table.
        template_file_name = cfg['analysis_settings']['damping']['template']['critical_damping']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)

        template_yaml = AttributeDict(template_yaml )
        template_yaml["Analysis"] = cfg["Analysis"].copy()

        template_yaml['file_management']['files']['files_in_current_directory']['directory'] = cfg['Analysis']['file_management_input_directory']

        return template_yaml
    
    def get_template_absolute_damping(self, cfg):
        template_file_name = cfg['analysis_settings']['damping']['template']['absolute_damping']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)

        template_yaml = AttributeDict(template_yaml )
        template_yaml["Analysis"] = cfg["Analysis"].copy()

        template_yaml['file_management']['files']['files_in_current_directory']['directory'] = cfg['Analysis']['file_management_input_directory']

        return template_yaml
    
    def get_template_no_damp_raos(self, cfg):
        template_file_name = cfg['analysis_settings']['damping']['template']['no_damp_raos']

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': template_file_name,
            'library_name': library_name
        }

        template_yaml = wwy.get_library_yaml_file(library_file_cfg)

        template_yaml = AttributeDict(template_yaml )
        template_yaml["Analysis"] = cfg["Analysis"].copy()

        template_yaml['file_management']['files']['files_in_current_directory']['directory'] = cfg['Analysis']['file_management_input_directory']

        return template_yaml

    def prepare_damping(self, cfg, damping_cfg):
        natural_period = damping_cfg['natural_period']
        percent_critical_damping = damping_cfg['percent_critical_damping']
        absolute_damping = damping_cfg['absolute_damping']
        no_damp_raos = damping_cfg['no_damp_raos']

        peak_periods = self.get_natural_periods(natural_period)
        additional_damping_values = self.get_damping_values(cfg, percent_critical_damping, absolute_damping, peak_periods)

        self.prepare_damping_runs(cfg, additional_damping_values)

    def prepare_damping_runs(self, cfg, additional_damping_values):
        self.prepare_damping_decks(cfg, additional_damping_values)

        template_yaml = self.get_template_damping_runs(cfg)
        cfg_temp = au_engine(inputfile=None, cfg=template_yaml, config_flag=False)

        ad_damp_filename = cfg_temp[cfg_temp['basename']]['output_filename'][0][0]
        cfg['analysis_settings']['damp_ad']['output'] = {'filename': ad_damp_filename}

        return cfg

    def prepare_damping_decks(self, cfg, additional_damping_values):
        self.create_decks_directory(cfg)

        deck_input = cfg['analysis_settings']['damping']['additional_damping']
        deck_input['inputs'][1]['data']['raw']['added_damping']['rxx'] = additional_damping_values['roll']
        deck_input['inputs'][1]['data']['raw']['added_damping']['ryy'] = additional_damping_values['pitch']

        template_yaml = self.get_template_prepare_decks(cfg, deck_input)
        dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)

    def get_natural_periods(self, natural_period):
        roll = round(float(natural_period['ROLL(RX)'].mean()), 3)
        pitch = round(float(natural_period['PITCH(RY)'].mean()), 3)
        heave = round(float(natural_period['HEAVE(Z)'].mean()), 3)

        omega = {
            'roll': roll,
            'pitch': pitch,
            'heave': heave
        }
        
        frequency = {
            'roll': round(roll/2/math.pi, 3),
            'pitch': round(pitch/2/math.pi, 3),
            'heave': round(heave/2/math.pi, 3)
        }
        
        period = {
            'roll': round(1/frequency['roll'], 3),
            'pitch': round(1/frequency['pitch'], 3),
            'heave': round(1/frequency['heave'], 3)
        }

        peak_response = {
            'omega': omega,
            'frequency': frequency,
            'period': period
        }
        return peak_response

    def get_damping_values(self, cfg, percent_critical_damping, absolute_damping, peak_periods):
        roll_additional_damping = self.get_roll_additional_damping(cfg, percent_critical_damping, absolute_damping, peak_periods)
        pitch_additional_damping = self.get_pitch_additional_damping(cfg, percent_critical_damping, absolute_damping, peak_periods)

        damping_values = {
            'roll': roll_additional_damping,
            'pitch': pitch_additional_damping,
        }

        return damping_values

    def get_roll_additional_damping(self, cfg, percent_critical_damping, absolute_damping, peak_periods):
        roll_peak_omega = peak_periods['omega']['roll']

        x_values = percent_critical_damping['(RAD/S)']
        y_values = percent_critical_damping['ROLL(RX)']
        f = interpolate.interp1d(x_values, y_values, fill_value='extrapolate')
        roll_critical_damping = round(float(f(roll_peak_omega)), 3)
    
        x_values = absolute_damping['frequency']
        y_values = absolute_damping['RADIATION DAMPING ROLL(X) _ROLL(X)']
        f = interpolate.interp1d(x_values, y_values, fill_value='extrapolate')
        roll_absolute_damping = round(float(f(roll_peak_omega)), 3)

        target_critical_damping = cfg['analysis_settings']['damping']['target_damping']['roll']
        
        additional_damping = (target_critical_damping - roll_critical_damping)*roll_absolute_damping/roll_critical_damping*math.pi/180
        
        return additional_damping
    
    def get_pitch_additional_damping(self, cfg, percent_critical_damping, absolute_damping, peak_periods):
        pitch_peak_omega = peak_periods['omega']['pitch']

        x_values = percent_critical_damping['(RAD/S)']
        y_values = percent_critical_damping['PITCH(RY)']
        f = interpolate.interp1d(x_values, y_values, fill_value='extrapolate')
        pitch_critical_damping = round(float(f(pitch_peak_omega)), 3)

        x_values = absolute_damping['frequency']
        y_values = absolute_damping['RADIATION DAMPING PITCH(Y) _PITCH(Y)']
        f = interpolate.interp1d(x_values, y_values, fill_value='extrapolate')
        pitch_absolute_damping = round(float(f(pitch_peak_omega)), 3)

        target_critical_damping = cfg['analysis_settings']['damping']['target_damping']['pitch']

        additional_damping = (target_critical_damping - pitch_critical_damping)*pitch_absolute_damping/pitch_critical_damping*math.pi/180

        return additional_damping

