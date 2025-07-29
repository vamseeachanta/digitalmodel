import os
import sys

from pyintegrity.common.data import SaveData
from pyintegrity.common.yml_utilities import ymlInput
from pyintegrity.common.update_deep import AttributeDict
from pyintegrity.common.ApplicationManager import ConfigureApplicationInputs
from pyintegrity.fracture_mechanics import fracture_mechanics
from pyintegrity.API579 import API579

save_data = SaveData()


def engine(inputfile=None):
    inputfile = validate_arguments_run_methods(inputfile)

    cfg = ymlInput(inputfile, updateYml=None)
    cfg = AttributeDict(cfg)

    basename = cfg['basename']
    application_manager = ConfigureApplicationInputs(basename)
    application_manager.configure(cfg)
    if cfg is None:
        raise ValueError("cfg is None")

    try:
        if basename == 'fracture_mechanics':
            cfg_base = fracture_mechanics(application_manager.cfg)
        elif basename == 'API579':
            cfg_base = API579(application_manager.cfg)
        else:
            raise (Exception(
                f'Analysis for basename: {basename} not found. ... FAIL'))
    except Exception as e:
        cfg_base = application_manager.cfg.copy()
        cfg_base.update({'Result': {'Exception': str(e)}})
        cfg_base = AttributeDict(cfg_base)
        raise e
    finally:
        save_cfg(cfg_base=cfg_base)

    return cfg_base


def validate_arguments_run_methods(inputfile):
    '''
    Validate inputs for following run methods:  
    - module (i.e. python -m pyintegrity input.yml)
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

    if len(sys.argv) <= 1:
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
