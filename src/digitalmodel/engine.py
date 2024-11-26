# Standard library imports
import logging
import os
import sys

# Third party imports
from assetutilities.common.ApplicationManager import ConfigureApplicationInputs
from assetutilities.common.data import CopyAndPasteFiles, SaveData
from assetutilities.common.file_management import FileManagement
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.yml_utilities import ymlInput

# Reader imports
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
from digitalmodel.modules.orcaflex.orcaflex_analysis import orcaflex_analysis
from digitalmodel.modules.orcaflex.orcaflex_file_management import (
    OrcaflexFileManagement,
)
from digitalmodel.modules.orcaflex.orcaflex_installation import OrcInstallation
from digitalmodel.modules.orcaflex.orcaflex_modal_analysis import OrcModalAnalysis
from digitalmodel.modules.orcaflex.umbilical_analysis_components import (
    UmbilicalAnalysis,
)
from digitalmodel.modules.pipeline.pipeline import Pipeline
from digitalmodel.modules.rao_analysis.rao_analysis import RAOAnalysis
from digitalmodel.modules.time_series.time_series_analysis import TimeSeriesAnalysis
from digitalmodel.modules.transformation.transformation import Transformation
from digitalmodel.modules.viv_analysis.viv_analysis import VIVAnalysis
from digitalmodel.vertical_riser import vertical_riser

library_name = "digitalmodel"
save_data = SaveData()

def engine(inputfile: str = None, cfg: dict = None, config_flag: bool = True) -> dict:
    if cfg is None:
        inputfile = validate_arguments_run_methods(inputfile)
        cfg = ymlInput(inputfile, updateYml=None)
        cfg = AttributeDict(cfg)
        if cfg is None:
            raise ValueError("cfg is None")

    basename = cfg["basename"]
    application_manager = ConfigureApplicationInputs(basename)
    application_manager.configure(cfg, library_name)

    if config_flag:
        fm = FileManagement()
        cfg_base = application_manager.cfg
        cfg_base = fm.router(cfg_base)
    else:
        cfg_base = cfg

    logging.info(f"{basename}, application ... START")

    if basename in ["simple_catenary_riser", "catenary_riser"]:
        cfg_base = catenary_riser(cfg_base)
    elif basename == "vertical_riser":
        cfg_base = vertical_riser(cfg_base)
    elif basename in ["orcaflex_analysis", "orcaflex_post_process"]:
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
        # Reader imports
        # Reader imports
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
    elif basename == "pipeline":
        pl = Pipeline()
        cfg_base = pl.router(cfg_base)
    elif basename == "viv_analysis":
        viv = VIVAnalysis()
        cfg_base = viv.router(cfg_base)
    elif basename == "time_series":
        tsa = TimeSeriesAnalysis()
        cfg_base = tsa.router(cfg_base)
    elif basename == "gis":
        tsa = TimeSeriesAnalysis()
        cfg_base = tsa.router(cfg_base)

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
