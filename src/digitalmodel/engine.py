import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.common.data import CopyAndPasteFiles
from digitalmodel.catenary_riser import catenary_riser
from digitalmodel.vertical_riser import vertical_riser
from digitalmodel.orcaflex_analysis import orcaflex_analysis
from digitalmodel.custom.orcaflex_modal_analysis import OrcModalAnalysis
from digitalmodel.custom.umbilical_analysis_components import UmbilicalAnalysis
from digitalmodel.custom.rigging import Rigging
from digitalmodel.common.code_dnvrph103_hydrodynamics_rectangular import DNVRPH103_hydrodynamics_rectangular
from digitalmodel.common.code_dnvrph103_hydrodynamics_circular import DNVRPH103_hydrodynamics_circular
from digitalmodel.custom.orcaflex_post_process import orcaflex_post_process
from digitalmodel.custom.rao_analysis import RAOAnalysis


def engine(inputfile=None):
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

    cfg = ymlInput(inputfile, updateYml=None)
    cfg = AttributeDict(cfg)

    basename = cfg['basename']
    application_manager = ConfigureApplicationInputs(basename)
    application_manager.configure(cfg)

    if basename in ['simple_catenary_riser', 'catenary_riser']:
        cfg_base = catenary_riser(application_manager.cfg)
    elif basename == 'vertical_riser':
        cfg_base = vertical_riser(application_manager.cfg)
    elif basename == 'orcaflex_analysis':
        cfg_base = orcaflex_analysis(application_manager.cfg)
    elif basename == 'modal_analysis':
        oma = OrcModalAnalysis()
        cfg_base = oma.run_modal_analysis(application_manager.cfg)
    elif basename == 'copy_and_paste':
        cpf = CopyAndPasteFiles()
        cfg_base = cpf.iterate_all_cfgs(application_manager.cfg)
    elif basename == 'umbilical_end':
        ua = UmbilicalAnalysis()
        ua.perform_analysis(application_manager.cfg)
    elif basename == 'orcaflex_post_process':
        opp = orcaflex_post_process()
        cfg_base = opp.post_process_router(application_manager.cfg)
    elif basename == 'rigging':
        rigging = Rigging()
        cfg_base = rigging.get_rigging_groups(application_manager.cfg)
    elif basename == 'code_dnvrph103':
        if application_manager.cfg['inputs']['shape'] == 'rectangular':
            code_dnvrph103 = DNVRPH103_hydrodynamics_rectangular()
        elif application_manager.cfg['inputs']['shape'] == 'circular':
            code_dnvrph103 = DNVRPH103_hydrodynamics_circular()
        cfg_base = code_dnvrph103.get_orcaflex_6dbuoy(application_manager.cfg)
    elif basename == 'rao_analysis':
        rao = RAOAnalysis()
        cfg_base = rao.read_orcaflex_raos(application_manager.cfg)
        rao.plot_amplitudes()
    else:
        raise (
            Exception(f'Analysis for basename: {basename} not found. ... FAIL'))

    return cfg_base
