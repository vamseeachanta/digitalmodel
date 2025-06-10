import logging

from loguru import logger
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.ApplicationManager import ConfigureApplicationInputs
from assetutilities.common.data import CopyAndPasteFiles
from assetutilities.common.file_management import FileManagement
from assetutilities.common.yml_utilities import WorkingWithYAML
from assetutilities.common.yml_utilities import WorkingWithYAML
# Reader imports
from digitalmodel.aqwa import Aqwa
from digitalmodel.modules.vertical_riser.vertical_riser import vertical_riser
from digitalmodel.common.cathodic_protection import CathodicProtection
from digitalmodel.common.code_dnvrph103_hydrodynamics_circular import (
    DNVRPH103_hydrodynamics_circular,
)
from digitalmodel.common.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)
from digitalmodel.common.fatigue_analysis import FatigueAnalysis
from digitalmodel.common.ship_design import ShipDesign
from digitalmodel.common.fatigue_analysis import FatigueAnalysis
from digitalmodel.common.ship_design import ShipDesign
from digitalmodel.modules.orcaflex.orcaflex import OrcaFlex

from digitalmodel.modules.orcaflex.orcaflex import OrcaFlex

from digitalmodel.modules.orcaflex.orcaflex_file_management import (
    OrcaflexFileManagement,
)
from digitalmodel.modules.orcaflex.orcaflex_installation import OrcInstallation
from digitalmodel.modules.orcaflex.orcaflex_modal_analysis import OrcModalAnalysis
from digitalmodel.modules.orcaflex.umbilical_analysis_components import (
    UmbilicalAnalysis,
)
from digitalmodel.modules.pipe_capacity.pipe_capacity import PipeCapacity
from digitalmodel.modules.pipeline.pipeline import Pipeline
from digitalmodel.modules.rao_analysis.rao_analysis import RAOAnalysis
from digitalmodel.modules.time_series.time_series_analysis import TimeSeriesAnalysis
from digitalmodel.modules.transformation.transformation import Transformation
from digitalmodel.modules.viv_analysis.viv_analysis import VIVAnalysis
from digitalmodel.modules.vertical_riser import vertical_riser
from digitalmodel.modules.viv_analysis.viv_analysis import VIVAnalysis
from digitalmodel.modules.mooring.mooring import Mooring
from digitalmodel.modules.mooring.mooring import Mooring

library_name = "digitalmodel"
wwyaml = WorkingWithYAML()

app_manager = ConfigureApplicationInputs()
wwyaml = WorkingWithYAML()

app_manager = ConfigureApplicationInputs()

def engine(inputfile: str = None, cfg: dict = None, config_flag: bool = True) -> dict:
    cfg_argv_dict = {}
    cfg_argv_dict = {}
    if cfg is None:
        inputfile, cfg_argv_dict = app_manager.validate_arguments_run_methods(inputfile)
        cfg = wwyaml.ymlInput(inputfile, updateYml=None)
        cfg = AttributeDict(cfg)
        if cfg is None:
            raise ValueError("cfg is None")

    if 'basename' in cfg:
        basename = cfg["basename"]
    elif 'meta' in cfg:
        basename = cfg["meta"]["basename"]
    else:
        raise ValueError("basename not found in cfg")

    if config_flag:
        fm = FileManagement()
        cfg_base = app_manager.configure(cfg, library_name, basename, cfg_argv_dict)
        cfg_base = fm.router(cfg_base)
        result_folder_dict, cfg_base = app_manager.configure_result_folder(None, cfg_base)
    else:
        cfg_base = cfg

    logger.info(f"{basename}, application ... START")

    if "catenary" in basename:
        from digitalmodel.modules.catenary.catenary import Catenary
        catenary = Catenary()
        cfg_base = catenary.router(cfg_base)
    if "catenary" in basename:
        from digitalmodel.modules.catenary.catenary import Catenary
        catenary = Catenary()
        cfg_base = catenary.router(cfg_base)
    elif basename == "vertical_riser":
        cfg_base = vertical_riser(cfg_base)
    elif basename in ["orcaflex", "orcaflex_analysis", "orcaflex_post_process"]:
        ofx = OrcaFlex()
        cfg_base = ofx.router(cfg_base)
    elif basename in ["orcaflex", "orcaflex_analysis", "orcaflex_post_process"]:
        ofx = OrcaFlex()
        cfg_base = ofx.router(cfg_base)
    elif basename in ["aqwa"]:
        aqwa = Aqwa()
        cfg_base = aqwa.router(cfg_base)
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
    elif basename == "installation":
        orc_install = OrcInstallation()
        if cfg_base["structure"]["flag"]:
            cfg_base = orc_install.create_model_for_water_depth(cfg_base)
    elif basename == "ship_design":
        ship_design = ShipDesign()
        cfg_base = ship_design.router(cfg_base)
    elif basename == "ship_design_aqwa":
        ship_design = ShipDesign()
        cfg_base = cfg_base = ship_design.router(cfg_base)
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
    elif basename == "pipe_capacity":
        pc = PipeCapacity()
        cfg_base = pc.router(cfg_base)
    elif basename == "viv_analysis":
        viv = VIVAnalysis()
        cfg_base = viv.router(cfg_base)
    elif basename == "time_series":
        tsa = TimeSeriesAnalysis()
        cfg_base = tsa.router(cfg_base)
    elif basename == "gis":
        tsa = TimeSeriesAnalysis()
        cfg_base = tsa.router(cfg_base)

    elif basename == "plate_buckling":
        pb = PlateBuckling()
        cfg_base = pb.router(cfg_base)

    elif basename == "mooring":
        mooring = Mooring()
        cfg_base = mooring.router(cfg_base)

    elif basename == "mooring":
        mooring = Mooring()
        cfg_base = mooring.router(cfg_base)

    else:
        raise (Exception(f"Analysis for basename: {basename} not found. ... FAIL"))

    logger.debug(f"{basename}, application ... END")
    app_manager.save_cfg(cfg_base=cfg_base)

    return cfg_base

