import os
import logging
import glob
import yaml
import pkgutil
try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")
import shutil

from assetutilities.common.data import SaveData

save_data = SaveData()


class OrcaflexIterativeRuns():

    def __init__(self):
        self.iterative_filenames = []

    def prepare_iterative_runs(self, cfg):
        self.cfg = cfg
        cfg['Analysis']['input_files']['iterate'] = []

        cfg_files = cfg['Files']['data'].copy()
        for i in range(0, len(cfg_files)):
            filename = cfg['Analysis']['input_files']['with_ext'][i]

            if os.path.isfile(filename):
                if 'iterate_yml' in cfg_files[0]:
                    logging.info(
                        f'Iteration analysis for File: {filename} ... START')
                    filename_copy = self.get_filename_copy(filename)
                    self.make_copy_per_iterative_guideline(
                        filename, filename_copy)
                    cfg['Analysis']['input_files']['iterate'].append(
                        filename_copy)

                    iterate_cfg = cfg_files[0]['iterate_yml'].copy()
                    iterative_yml = self.get_iterative_yaml(
                        iterate_cfg, filename_copy)

                    self.save_iterative_file(filename_copy, iterative_yml)
                    logging.info(
                        f'Iteration analysis for File: {filename} ... COMPLETE')
                else:
                    logging.info(
                        f'Iteration analysis for File: {filename} ... SKIP')
            else:
                logging.error(f'File {filename_copy} not found ... FAIL')

    def make_copy_per_iterative_guideline(self, filename, filename_copy):
        shutil.copyfile(filename, filename_copy)
        logging.error(f'File copy for: {filename} ... PASS')

    def get_filename_copy(self, filename):
        filename_components = self.get_filename_components(filename)
        file_base_orcaflex = filename_components['file_base_orcaflex']
        file_base_orcaflex_copy = '_' + file_base_orcaflex
        filename_copy = file_base_orcaflex_copy
        filename_copy = os.path.join(filename_components['filepath'],
                                     filename_copy)

        return filename_copy

    def save_iterative_file(self, filename, iterative_yml):
        iterative_filename = self.get_iterative_filename(filename)
        self.iterative_filenames.append(iterative_filename)

        save_data.saveDataYaml(iterative_yml,
                               iterative_filename['iterative_filename_no_ext'],
                               default_flow_style=False)

    def get_iterative_filename(self, filename):
        related_files, related_files_variations = self.find_related_files(
            filename)
        logging.info(f'related_files: {related_files}')
        logging.info(f'related_files_variations: {related_files_variations}')

        file_ext = filename.split('.')[-1]
        file_base = filename.replace(f'.{file_ext}', '')
        if len(related_files) > 1:
            logging.warning(
                "Multiple files found ... FIles will be OVERWRITTEN")

        iterative_filename_with_ext = file_base + '_01' + '.' + file_ext
        iterative_filename_no_ext = file_base + '_01'

        iterative_filename = {
            'iterative_filename_no_ext': iterative_filename_no_ext,
            'iterative_filename_with_ext': iterative_filename_with_ext
        }

        return iterative_filename

    def find_related_files(self, filename):
        file_ext = filename.split('.')[-1]
        file_base = filename.replace(f'.{file_ext}', '')
        related_files = glob.glob(f'{file_base}*.{file_ext}')

        related_files_variations = []
        for related_file in related_files:
            related_file = related_file.replace('file_base', '')
            related_file = related_file.replace('file_ext', '')
            related_files_variations.append(related_file)

        return related_files, related_files_variations

    def get_iterative_yaml(self, iterate_cfg, filename):
        iterate_cfg_yml = iterate_cfg['name']
        if os.path.isfile(iterate_cfg_yml):
            with open(iterate_cfg_yml, 'r') as ymlfile:
                iterative_yml = yaml.load(ymlfile, Loader=yaml.Loader)
        else:
            data = pkgutil.get_data('digitalmodel', iterate_cfg['name'])
            iterative_yml = yaml.safe_load(data)

        filename_components = self.get_filename_components(filename)
        file_base_orcaflex = filename_components['file_base_orcaflex']
        iterative_yml['BaseFile'] = file_base_orcaflex

        self.iterate_add_keys(iterative_yml, iterate_cfg)
        self.iterate_delete_keys(iterative_yml, iterate_cfg)
        self.iterate_replace_values(iterative_yml, iterate_cfg)

        return iterative_yml

    def iterate_add_keys(self, iterative_yml, iterate_cfg):
        if len(iterate_cfg['add']) > 0:
            raise (Exception("CODE NOT IMPLEMENTED"))

    def iterate_delete_keys(self, iterative_yml, iterate_cfg):
        if len(iterate_cfg['delete']) > 0:
            raise (Exception("CODE NOT IMPLEMENTED"))

    def iterate_replace_values(self, iterative_yml, iterate_cfg):
        if len(iterate_cfg['replace']) > 0:
            raise (Exception("CODE NOT IMPLEMENTED"))

    def get_filename_components(self, filename):
        file_ext = filename.split('.')[-1]
        file_base = filename.replace(f'.{file_ext}', '')
        file_base_orcaflex = file_base.split('\\')[-1]
        file_base_orcaflex = file_base_orcaflex + '.' + file_ext
        filepath = os.path.split(filename)[0]
        filename_components = {
            'file_base_orcaflex': file_base_orcaflex,
            'file_ext': file_ext,
            'filename': filename,
            'filepath': filepath
        }
        return filename_components

    def run_iterative_simulations(self):
        for i in range(0, len(self.iterative_filenames)):
            iterative_filename = self.iterative_filenames[i]
            filename_with_ext = iterative_filename[
                'iterative_filename_with_ext']
            filename_save_data = self.cfg['Analysis']['input_files'][
                'with_ext'][i]
            file_ext = filename_save_data.split('.')[-1]
            filename_save_simulation = filename_save_data.replace(
                file_ext, 'sim')
            logging.info(f'Running iterative file: {filename_with_ext}')

            model = OrcFxAPI.Model()
            model.LoadData(filename_with_ext)

            model.RunSimulation()

            model.SaveSimulation(filename_save_simulation)
            if self.cfg['orcaflex']['iterate']['overwrite_data']:
                model.SaveData(filename_save_data)

            logging.info(
                f'Finished running iterative file: {filename_with_ext}')
