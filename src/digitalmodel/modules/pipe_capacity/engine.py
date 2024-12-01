import os
import sys

from assetutilities.common.data import SaveData
from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.ApplicationManager import ConfigureApplicationInputs

from pipecapacity.pipe_capacity import pipe_capacity

save_data = SaveData()
library_name = 'pipecapacity'


def engine(inputfile=None):
    inputfile = validate_arguments_run_methods(inputfile)

    cfg = ymlInput(inputfile, updateYml=None)
    cfg = AttributeDict(cfg)

    basename = cfg['basename']
    application_manager = ConfigureApplicationInputs(basename)
    application_manager.configure(cfg, library_name)
    if cfg is None:
        raise ValueError("cfg is None")

    if basename in ['pipecapacity']:
        cfg_base = pipe_capacity(application_manager.cfg)

    else:
        raise (
            Exception(f'Analysis for basename: {basename} not found. ... FAIL'))

    save_cfg(cfg_base=cfg_base)

    return cfg_base


def validate_arguments_run_methods(inputfile):
    '''
    Validate inputs for following run methods:  
    - module (i.e. python -m digitalmodel input.yml)
    - from python file (i.e. )
    '''

    if len(sys.argv) > 1 and inputfile is not None:
        raise (Exception(
            '2 Input files provided via arguments & function. Please provide only 1 file ... FAIL'
        ))

    if len(sys.argv) > 1:
        if not os.path.isfile(sys.argv[1]):
            raise (FileNotFoundError(
                f'Input file {sys.argv[1]} not found ... FAIL'))
        else:
            inputfile = sys.argv[1]

    else:
        if not os.path.isfile(inputfile):
            raise (
                FileNotFoundError(f'Input file {inputfile} not found ... FAIL'))
        else:
            sys.argv.append(inputfile)

    return inputfile


def save_cfg(cfg_base):
    output_dir = cfg_base.Analysis['analysis_root_folder']

    filename = cfg_base.Analysis['file_name']
    filename_path = os.path.join(output_dir, filename)

    save_data.saveDataYaml(cfg_base, filename_path, default_flow_style=False)
