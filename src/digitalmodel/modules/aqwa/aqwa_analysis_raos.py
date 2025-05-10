# Standard library imports
import logging
import math
import os

# Third party imports
import pandas as pd
from assetutilities.common.file_management import FileManagement
from assetutilities.common.update_deep import AttributeDict, update_deep_dictionary
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.visualization.visualization_templates_matplotlib import (
    VisualizationTemplates,
)
from assetutilities.common.yml_utilities import WorkingWithYAML  # noqa
from assetutilities.engine import engine as au_engine

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities  # noqa
from digitalmodel.engine import engine as dm_engine  # noqa
from scipy import interpolate

viz_templates = VisualizationTemplates()

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
        analysis_settings_key='damp_no'
        cfg, damping_cfg_no_damp = self.derive_damping(cfg, analysis_settings_key)
        if damping_cfg_no_damp['is_file_valid']:
            cfg,  additional_damping_values= self.prepare_damping(cfg, damping_cfg_no_damp, analysis_settings_key)
            self.prepare_damping_runs(cfg, additional_damping_values)
            analysis_settings_key='damp_ad'
            cfg, damping_cfg_damp_ad = self.derive_damping(cfg, analysis_settings_key)
            cfg,  additional_damping_values= self.prepare_damping(cfg, damping_cfg_damp_ad, analysis_settings_key)
            cfg = self.get_raos(cfg)
            cfg = self.plot_raos(cfg)
        else:
            logging.error("No file found. Skipping damping preparation.")
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

    def derive_damping(self, cfg, analysis_settings_key):
        filename_pattern = cfg.analysis_settings[analysis_settings_key]['output']['filename']
        natural_period, is_file_valid = self.get_natural_period_from_lis_file(cfg, filename_pattern)
        percent_critical_damping, is_file_valid = self.get_percent_critical_damping(cfg, filename_pattern)
        absolute_damping = self.get_absolute_damping(cfg, filename_pattern)

        damping_cfg = {
            'natural_period': natural_period, 
            'percent_critical_damping': percent_critical_damping,
            'absolute_damping': absolute_damping,
            'is_file_valid' : is_file_valid
        }

        return cfg, damping_cfg

    def get_natural_period_from_lis_file(self, cfg, filename_pattern):
        logging.info("Extracting natural period values")
        template_yaml = self.get_template_natural_period(cfg)
        template_yaml['file_management']['files']['files_in_current_directory']['filename_pattern'] = filename_pattern

        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        cfg['analysis_settings']['damping']['output'] = {'natural_period': cfg_temp[cfg_temp['basename']]}
        csv_file = cfg['analysis_settings']['damping']['output']['natural_period']['csv_filename'][0]

        try:
            is_file_valid = True
            natural_period = pd.read_csv(csv_file)
            if 'drafts' in cfg['analysis_settings']:
                no_of_frequencies = self.get_no_of_frequencies(cfg, filename_pattern)
                if len(natural_period) >= no_of_frequencies:
                    natural_period = natural_period[:no_of_frequencies].copy()
                else:
                    logging.error("Natural period values not found for all frequencies.")
        except Exception:
            is_file_valid = False
            logging.error(f"File {csv_file} not found or empty.")
            natural_period = pd.DataFrame()

        return natural_period, is_file_valid

    def get_no_of_frequencies(self, cfg, filename_pattern):
        no_of_frequencies = None
        input_array = cfg['analysis_settings']['drafts'][0]['inputs']
        for input in input_array:
            if input['data']['category'] == 6:
                no_of_frequencies = len(input['data']['raw']['period'])

        return no_of_frequencies

    def get_directions_labels(self):
        no_of_directions = [0, 45, 90, 135, 180, 225, 270, 315, 360]

        return no_of_directions

    def get_percent_critical_damping(self, cfg, filename_pattern):
        logging.info("Extracting critical damping values")
        template_yaml = self.get_template_critical_damping(cfg)
        template_yaml['file_management']['files']['files_in_current_directory']['filename_pattern'] = filename_pattern

        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        cfg['analysis_settings']['damping']['output'] = {'critical_damping': cfg_temp[cfg_temp['basename']]}
        
        csv_file = cfg['analysis_settings']['damping']['output']['critical_damping']['csv_filename'][0]

        try:
            is_file_valid = True
            percent_critical_damping = pd.read_csv(csv_file)
            if 'drafts' in cfg['analysis_settings']:
                no_of_frequencies = self.get_no_of_frequencies(cfg, filename_pattern)
                if len(percent_critical_damping) >= no_of_frequencies:
                    percent_critical_damping = percent_critical_damping[:no_of_frequencies].copy()
                else:
                    logging.error("Natural period values not found for all frequencies.")
        except Exception:
            logging.error(f"File {csv_file} not found or empty.")
            percent_critical_damping = pd.DataFrame()

        return percent_critical_damping, is_file_valid

    def get_absolute_damping(self, cfg, filename_pattern):
        logging.info("Extracting absolute damping values")
        template_yaml = self.get_template_absolute_damping(cfg)
        template_yaml['file_management']['files']['files_in_current_directory']['filename_pattern'] = filename_pattern

        cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
        absolute_damping_cfg = {'absolute_damping': cfg_temp[cfg_temp['basename']]}
        cfg['analysis_settings']['damping']['output'].update(absolute_damping_cfg)
        
        csv_file = cfg['analysis_settings']['damping']['output']['absolute_damping']['csv_filename']
        try:
            absolute_damping = pd.read_csv(csv_file)
        except Exception:
            logging.error(f"File {csv_file} not found or empty.")
            absolute_damping = pd.DataFrame()

        return absolute_damping

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

    def get_template_raos(self, cfg):
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

    def prepare_damping(self, cfg, damping_cfg, analysis_settings_key):
        natural_period = damping_cfg['natural_period']
        percent_critical_damping = damping_cfg['percent_critical_damping']
        absolute_damping = damping_cfg['absolute_damping']

        additional_damping_values = {}
        if natural_period.empty or percent_critical_damping.empty or absolute_damping.empty:
            logging.error("One or more damping files are empty. Skipping damping preparation.")
        else:
            peak_periods = self.get_natural_periods(natural_period)
            additional_damping_values = self.get_damping_values(cfg, percent_critical_damping, absolute_damping, peak_periods, analysis_settings_key)

        return cfg, additional_damping_values

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

        if roll == 0:
            roll = 0.04
        if pitch == 0:
            pitch = 0.04
        if heave == 0:
            heave = 0.04

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

    def get_damping_values(self, cfg, percent_critical_damping, 
                           absolute_damping, peak_periods, analysis_settings_key):

        cfg_additional_damping = {}
        cfg_additional_damping['peak_omega_key']  = 'roll'
        critical_damping_dict = {'x': '(RAD/S)', 'y': 'ROLL(RX)'}
        absolute_damping_dict = {'x': 'frequency', 'y': 'RADIATION DAMPING ROLL(X) _ROLL(X)'}
        cfg_additional_damping['critical_damping'] = critical_damping_dict
        cfg_additional_damping['absolute_damping'] = absolute_damping_dict

        roll_additional_damping = self.get_additional_damping_by_cfg(cfg, 
                                                                     cfg_additional_damping, percent_critical_damping, 
                                                                     absolute_damping, peak_periods, analysis_settings_key)
        
        cfg_additional_damping['peak_omega_key']  = 'pitch'
        critical_damping_dict = {'x': '(RAD/S)', 'y': 'PITCH(RY)'}
        absolute_damping_dict = {'x': 'frequency', 'y': 'RADIATION DAMPING PITCH(Y) _PITCH(Y)'}
        cfg_additional_damping['critical_damping'] = critical_damping_dict
        cfg_additional_damping['absolute_damping'] = absolute_damping_dict

        pitch_additional_damping = self.get_additional_damping_by_cfg(cfg, cfg_additional_damping, percent_critical_damping, absolute_damping, peak_periods, analysis_settings_key)

        damping_values = {
            'roll': roll_additional_damping,
            'pitch': pitch_additional_damping,
        }

        return damping_values

    def get_additional_damping_by_cfg(self, cfg, cfg_additional_damping, percent_critical_damping, 
                                     absolute_damping, peak_periods, analysis_settings_key):
        peak_omega_key = cfg_additional_damping['peak_omega_key']
        critical_damping_dict = cfg_additional_damping['critical_damping']
        absolute_damping_dict = cfg_additional_damping['absolute_damping']

        peak_omega = peak_periods['omega'][peak_omega_key]

        x_values = percent_critical_damping[critical_damping_dict['x']]
        if peak_omega > x_values.max():
            peak_omega = x_values.max()
        elif peak_omega < x_values.min():
            peak_omega = x_values.min()
        y_values = percent_critical_damping[critical_damping_dict['y']]
        f = interpolate.interp1d(x_values, y_values, fill_value='nearest')

        current_critical_damping = round(float(f(peak_omega)), 3)

        x_values = absolute_damping[absolute_damping_dict['x']]
        if peak_omega > x_values.max():
            peak_omega = x_values.max()
        elif peak_omega < x_values.min():
            peak_omega = x_values.min()
        y_values = absolute_damping[absolute_damping_dict['y']]
        f = interpolate.interp1d(x_values, y_values, fill_value='nearest')
        current_absolute_damping = round(float(f(peak_omega)), 3)

        target_critical_damping = cfg['analysis_settings']['damping']['target_damping'][peak_omega_key]

        if current_critical_damping > 0:
            additional_damping = self.get_additional_damping(current_critical_damping, current_absolute_damping, target_critical_damping)
        elif current_critical_damping == 0:
            additional_damping = current_absolute_damping
        else:
            raise NotImplementedError("Current critical damping is negative. No Logic implemented for negative damping.")

        damping_output = {'current_critical_damping': current_critical_damping,
                        'current_absolute_damping': current_absolute_damping,
                        'target_critical_damping': target_critical_damping,
                        'additional_damping': additional_damping}

        cfg['analysis_settings'][analysis_settings_key]['output'] = update_deep_dictionary(cfg['analysis_settings'][analysis_settings_key]['output'], {'damping': {peak_omega_key: damping_output}})

        return additional_damping

    def get_additional_damping(self, current_critical_damping, current_absolute_damping, target_critical_damping):
            additional_damping = target_critical_damping / current_critical_damping * current_absolute_damping - current_absolute_damping
            additional_damping = round(additional_damping, 1)
            return additional_damping

    def get_raos(self, cfg):
        rao_plot_cfg = {}
        rao_plot_cfg['rao_key'] = 'damp_no'
        rao_plot_cfg['title_text'] = 'No Damping'
        cfg, no_damp_raos = self.get_raos_by_cfg(cfg, rao_plot_cfg)
        rao_plot_cfg = {}
        rao_plot_cfg['rao_key'] = 'damp_ad'
        rao_plot_cfg['title_text'] = 'With Damping'
        cfg, damp_raos = self.get_raos_by_cfg(cfg, rao_plot_cfg)

        return cfg

    def get_raos_by_cfg(self, cfg, rao_plot_cfg):
        rao_key = rao_plot_cfg['rao_key']
        title_text = rao_plot_cfg['title_text']
        filename_pattern = cfg.analysis_settings[rao_key]['output']['filename']
        directions = cfg['analysis_settings'][rao_key]['directions']
        logging.info(f"Extracting RAOs for: {title_text}")

        template_yaml = self.get_template_raos(cfg)
        sheetname_base = template_yaml['result'][0]['inject_into']['sheetname']
        csv_file_dict = {}
        for direction in directions:
            template_yaml['file_management']['files']['files_in_current_directory']['filename_pattern'] = filename_pattern
            template_yaml['result'][0]['groups'][0]['second_level'] = [direction]
            template_yaml['result'][0]['inject_into']['sheetname'] = sheetname_base + '_' + str(direction)
            cfg_temp = dm_engine(inputfile=None, cfg=template_yaml, config_flag=False)
            raos_cfg = {'raos': cfg_temp[cfg_temp['basename']]}
            cfg['analysis_settings']['damping']['output'].update(raos_cfg)

            csv_file = cfg['analysis_settings']['damping']['output']['raos']['csv_filename']
            try:
                raos = pd.read_csv(csv_file)
                no_damp_raos_keys = list(raos.keys())
                renamed_columns = [no_damp_raos_keys[0]] + ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw'] + [no_damp_raos_keys[-1]]
                rename_columns_dict = dict(zip(no_damp_raos_keys, renamed_columns))
                raos.rename(columns=rename_columns_dict, inplace=True)
                raos['Period'] = 1/raos['frequency']*2*math.pi
                raos.to_csv(csv_file, index=False)
            except Exception:
                logging.error(f"File {csv_file} not found or empty.")
                raos = pd.DataFrame()

            csv_file_dict.update({direction: csv_file})

        cfg['analysis_settings'][rao_key]['output'].update({'raos': {'csv_filename': csv_file_dict}})

        return cfg, raos


    def plot_raos(self, cfg):
        rao_plot_cfg = {}
        rao_plot_cfg['rao_key'] = 'damp_no'
        rao_plot_cfg['title_text'] = 'No Damping'
        self.plot_raos_by_cfg(cfg, rao_plot_cfg)

        rao_plot_cfg = {}
        rao_plot_cfg['rao_key'] = 'damp_ad'
        rao_plot_cfg['title_text'] = 'With Damping'
        self.plot_raos_by_cfg(cfg, rao_plot_cfg)

        return cfg

    def plot_raos_by_cfg(self, cfg, rao_plot_cfg):
        rao_key = rao_plot_cfg['rao_key']
        title_text = rao_plot_cfg['title_text']
        directions_labels = self.get_directions_labels()
        no_damp_csv_file_dict = cfg['analysis_settings'][rao_key]['output']['raos']['csv_filename']

        csv_groups = []
        for direction, csv_file in no_damp_csv_file_dict.items():
            direction_value = directions_labels[direction-1]
            csv_group = {'label': f'Heading: {direction_value}', 'file_name': csv_file}
            csv_groups.append(csv_group)

            plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())
            plot_yml['data']['groups'] = [csv_group]

            columns= { 'x': ['Period'], 'y': ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw'] }
            plot_yml['master_settings']['groups']['columns'] = columns

            filename_prefix = cfg['analysis_settings'][rao_key]['output']['filename']
            settings = {'file_name':  f'{filename_prefix}_{direction_value:03d}',
                        'title': f'RAOs, {title_text}, Heading Dir: {direction_value}',
                        'xlabel': 'Period (s)',
                        'ylabel': 'm/m or deg/m'}
            plot_yml['settings'].update(settings)
            au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

        plot_yml = viz_templates.get_xy_line_csv(cfg['Analysis'].copy())
        plot_yml['data']['groups'] = csv_groups
        plot_yml['master_settings']['groups']['columns'] = columns
        settings = {'file_name':  f'{filename_prefix}_all_dir',
                    'title': f'RAOs, {title_text}, Direction Comparison',
                    'xlabel': 'Period (s)',
                    'ylabel': 'm/m or deg/m'}
        plot_yml['settings'].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

