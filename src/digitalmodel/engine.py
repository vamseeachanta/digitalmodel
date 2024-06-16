import os
import sys

from assetutilities.common.ApplicationManager import ConfigureApplicationInputs
from assetutilities.common.data import CopyAndPasteFiles, SaveData
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.yml_utilities import ymlInput

from digitalmodel.aqwa import Aqwa
from digitalmodel.catenary_riser import catenary_riser
from digitalmodel.common.cathodic_protection import CathodicProtection
from digitalmodel.common.code_dnvrph103_hydrodynamics_circular import (
    DNVRPH103_hydrodynamics_circular,
)
from digitalmodel.common.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)
from digitalmodel.common.fatigue_analysis import FatigueAnalysis
from digitalmodel.common.ship_design import ShipDesign
from digitalmodel.custom.orcaflex_file_management import OrcaflexFileManagement
from digitalmodel.custom.orcaflex_installation import OrcInstallation
from digitalmodel.custom.orcaflex_modal_analysis import OrcModalAnalysis
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

from digitalmodel.custom.rao_analysis import RAOAnalysis
from digitalmodel.custom.transformation import Transformation
from digitalmodel.custom.umbilical_analysis_components import UmbilicalAnalysis
from digitalmodel.orcaflex_analysis import orcaflex_analysis
from digitalmodel.vertical_riser import vertical_riser

save_data = SaveData()
ou = OrcaflexUtilities()
library_name = "digitalmodel"


def engine(inputfile: str = None) -> dict:
    inputfile = validate_arguments_run_methods(inputfile)

    cfg = ymlInput(inputfile, updateYml=None)
    cfg = AttributeDict(cfg)
    if cfg is None:
        raise ValueError("cfg is None")

    basename = cfg["basename"]
    application_manager = ConfigureApplicationInputs(basename)
    application_manager.configure(cfg, library_name)
    cfg_base = application_manager.cfg

    if basename in ["simple_catenary_riser", "catenary_riser"]:
        cfg_base = catenary_riser(cfg_base)
    elif basename == "vertical_riser":
        cfg_base = vertical_riser(cfg_base)
    elif basename in ["orcaflex_analysis", "orcaflex_post_process"]:
        if "file_management" in cfg_base and cfg["file_management"]["flag"]:
            cfg_base = ou.file_management(cfg_base)
        cfg_base = orcaflex_analysis(cfg_base)
    elif basename in ["aqwa"]:
        aqwa = Aqwa()
        cfg_base = aqwa.router(cfg_base)
    elif basename == "modal_analysis":
        oma = OrcModalAnalysis()
        cfg_base = oma.run_modal_analysis(cfg_base)
    elif basename == "copy_and_paste":
        cpf = CopyAndPasteFiles()
        cfg_base = cpf.iterate_all_cfgs(cfg_base)
    elif basename == "umbilical_analysis":
        ua = UmbilicalAnalysis()
        cfg_base = ua.perform_analysis(cfg_base)
    elif basename in ["orcaflex_file_management", "orcaflex_file_preparation"]:
        ofm = OrcaflexFileManagement()
        cfg_base = ofm.file_management(cfg_base)
    elif basename == "rigging":
        from digitalmodel.custom.rigging import Rigging

        rigging = Rigging()
        cfg_base = rigging.get_rigging_groups(cfg_base)
    elif basename == "code_dnvrph103":
        if cfg_base["inputs"]["shape"] == "rectangular":
            code_dnvrph103 = DNVRPH103_hydrodynamics_rectangular()
        elif cfg_base["inputs"]["shape"] == "circular":
            code_dnvrph103 = DNVRPH103_hydrodynamics_circular()
        cfg_base = code_dnvrph103.get_orcaflex_6dbuoy(cfg_base)
    elif basename == "rao_analysis":
        rao = RAOAnalysis()
        cfg_base = rao.read_orcaflex_displacement_raos(cfg_base)
        cfg_base = rao.read_orcaflex_displacement_raos(cfg_base)
    elif basename == "installation":
        orc_install = OrcInstallation()
        if cfg_base["structure"]["flag"]:
            cfg_base = orc_install.create_model_for_water_depth(cfg_base)
    elif basename == "ship_design":
        ship_design = ShipDesign()
        cfg_base = ship_design.router(cfg_base)
    elif basename == "fatigue_analysis":
        fatigue_analysis = FatigueAnalysis()
        cfg_base = fatigue_analysis.router(cfg_base)
    elif basename == "cathodic_protection":
        cp = CathodicProtection()
        cfg_base = cp.router(cfg_base)
    elif basename == "transformation":
        trans = Transformation()
        cfg_base = trans.router(cfg_base)

    else:
        raise (Exception(f"Analysis for basename: {basename} not found. ... FAIL"))

    save_cfg(cfg_base=cfg_base)

    return cfg_base


def validate_arguments_run_methods(inputfile):
    """
    Validate inputs for following run methods:
    - module (i.e. python -m digitalmodel input.yml)
    - from python file (i.e. )
    """

    if len(sys.argv) > 1 and inputfile is not None:
        raise (Exception("2 Input files provided via arguments & function. Please provide only 1 file ... FAIL"))

    if len(sys.argv) > 1:
        if not os.path.isfile(sys.argv[1]):
            raise (FileNotFoundError(f"Input file {sys.argv[1]} not found ... FAIL"))
        else:
            inputfile = sys.argv[1]

    if len(sys.argv) <= 1:
        if not os.path.isfile(inputfile):
            raise (FileNotFoundError(f"Input file {inputfile} not found ... FAIL"))
        else:
            sys.argv.append(inputfile)
    return inputfile


def save_cfg(cfg_base):
    output_dir = cfg_base.Analysis["analysis_root_folder"]

    filename = cfg_base.Analysis["file_name"]
    filename_path = os.path.join(output_dir, "results", filename)

    save_data.saveDataYaml(cfg_base, filename_path, default_flow_style=False)
