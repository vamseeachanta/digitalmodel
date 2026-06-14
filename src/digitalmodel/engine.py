import os
import sys

from assetutilities.common.ApplicationManager import ConfigureApplicationInputs
from assetutilities.common.data import CopyAndPasteFiles
from assetutilities.common.file_management import FileManagement
from assetutilities.common.update_deep import AttributeDict
from assetutilities.common.yml_utilities import WorkingWithYAML

# Reader imports
from digitalmodel.hydrodynamics.aqwa import Aqwa
from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import (
    CathodicProtection,
)
from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_circular import (
    DNVRPH103_hydrodynamics_circular,
)
from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)
from digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis import (
    FatigueAnalysis,
)
from digitalmodel.infrastructure.base_solvers.marine.ship.ship_design import ShipDesign
from digitalmodel.subsea.mooring_analysis import MooringDesigner
from digitalmodel.solvers.orcaflex.orcaflex import OrcaFlex
from digitalmodel.solvers.orcaflex.orcaflex_file_management import (
    OrcaflexFileManagement,
)
from digitalmodel.solvers.orcaflex.orcaflex_installation import OrcInstallation
from digitalmodel.solvers.orcaflex.orcaflex_modal_analysis import OrcModalAnalysis
from digitalmodel.solvers.orcaflex.umbilical_analysis_components import (
    UmbilicalAnalysis,
)
from digitalmodel.structural.pipe_capacity.pipe_capacity import PipeCapacity
from digitalmodel.subsea.pipeline.pipeline import Pipeline
from digitalmodel.marine_ops.ct_hydraulics.ct_hydraulics import CTHydraulics
from digitalmodel.hydrodynamics.rao_analysis.rao_analysis import RAOAnalysis
from digitalmodel.signal_processing.time_series.time_series_analysis import (
    TimeSeriesAnalysis,
)
from digitalmodel.infrastructure.transformation.transformation import Transformation

# from digitalmodel.subsea.vertical_riser.vertical_riser import vertical_riser
from digitalmodel.subsea.viv_analysis.viv_analysis import VIVAnalysis
from digitalmodel.infrastructure.base_solvers.structural.plate_buckling import (
    PlateBuckling,
)
from loguru import logger
from digitalmodel.solvers.orcaflex.output_control import (
    OutputController,
    get_output_level_from_argv,
)

library_name = "digitalmodel"
wwyaml = WorkingWithYAML()
app_manager = ConfigureApplicationInputs()


def _running_under_pytest() -> bool:
    return (
        "PYTEST_CURRENT_TEST" in os.environ
        or "pytest" in sys.modules
        or any(name.startswith("_pytest") for name in sys.modules)
        or any("pytest" in arg or "_pytest" in arg for arg in sys.argv)
    )


def engine(inputfile: str = None, cfg: dict = None, config_flag: bool = True) -> dict:
    cfg_argv_dict = {}
    if cfg is None:
        try:
            inputfile, cfg_argv_dict = app_manager.validate_arguments_run_methods(
                inputfile
            )
        except Exception as exc:
            if (
                inputfile is None
                or not _running_under_pytest()
                or "2 Input files provided" not in str(exc)
            ):
                raise
            cfg_argv_dict = {}
        cfg = wwyaml.ymlInput(inputfile, updateYml=None)
        cfg = AttributeDict(cfg)
        # Track config file path for relative path resolution
        if inputfile and os.path.exists(inputfile):
            cfg["_config_file_path"] = os.path.abspath(inputfile)
            cfg["_config_dir_path"] = os.path.dirname(os.path.abspath(inputfile))
        logger.info(f"Engine set config dir: {cfg.get('_config_dir_path')}")
        if cfg is None:
            raise ValueError("cfg is None")

    if "basename" in cfg:
        basename = cfg["basename"]
    elif "meta" in cfg:
        basename = cfg["meta"]["basename"]
    else:
        raise ValueError("basename not found in cfg")

    if config_flag:
        fm = FileManagement()
        if inputfile is not None and _running_under_pytest():
            original_argv = sys.argv[:]
            sys.argv = [original_argv[0], inputfile]
            try:
                cfg_base = app_manager.configure(
                    cfg, library_name, basename, cfg_argv_dict, inputfile=inputfile
                )
            finally:
                sys.argv = original_argv
        else:
            cfg_base = app_manager.configure(
                cfg, library_name, basename, cfg_argv_dict, inputfile=inputfile
            )
        # Preserve config file path from original cfg
        if "_config_file_path" in cfg:
            cfg_base["_config_file_path"] = cfg["_config_file_path"]
        if "_config_dir_path" in cfg:
            cfg_base["_config_dir_path"] = cfg["_config_dir_path"]
        cfg_base = fm.router(cfg_base)
        result_folder_dict, cfg_base = app_manager.configure_result_folder(
            None, cfg_base
        )
    else:
        cfg_base = cfg

    # Apply output control settings from command line
    output_level = get_output_level_from_argv()
    if output_level == OutputController.QUIET:
        cfg_base["quiet"] = True
        cfg_base["verbose"] = False
    elif output_level == OutputController.VERBOSE:
        cfg_base["quiet"] = False
        cfg_base["verbose"] = True

    logger.info(f"{basename}, application ... START")

    if "catenary" in basename:
        from digitalmodel.subsea.catenary_riser.legacy.catenary import Catenary

        catenary = Catenary()
        cfg_base = catenary.router(cfg_base)
    elif basename == "vertical_riser":
        cfg_base = vertical_riser(cfg_base)
    elif basename in ["orcaflex", "orcaflex_analysis", "orcaflex_post_process"]:
        ofx = OrcaFlex()
        cfg_base = ofx.router(cfg_base)
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
        from digitalmodel.specialized.rigging.rigging import Rigging

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
    elif basename == "pipe_capacity":
        pc = PipeCapacity()
        cfg_base = pc.router(cfg_base)
    elif basename == "wall_thickness":
        from digitalmodel.structural.wall_thickness_quickcheck import (
            WallThicknessQuickCheck,
        )

        wt = WallThicknessQuickCheck()
        cfg_base = wt.router(cfg_base)
    elif basename == "API579":
        from digitalmodel.asset_integrity.API579 import API579

        result_folder = cfg_base.get("Analysis", {}).get("result_folder")
        if result_folder and not str(result_folder).endswith(os.sep):
            cfg_base["Analysis"]["result_folder"] = f"{result_folder}{os.sep}"
        cfg_base = API579(cfg_base)
    elif basename == "ct_hydraulics":
        ct_hydraulics = CTHydraulics()
        cfg_base = ct_hydraulics.router(cfg_base)
    elif basename == "viv_analysis":
        viv = VIVAnalysis()
        cfg_base = viv.router(cfg_base)
    elif basename == "parametric_run":
        from digitalmodel.workflows.parametric_run import router as parametric_run

        cfg_base = parametric_run(cfg_base)
    elif basename == "stress_strain":
        from digitalmodel.structural.stress.workflow import router as stress_strain

        cfg_base = stress_strain(cfg_base)
    elif basename == "riser_stackup":
        from digitalmodel.drilling_riser.workflow import router as riser_stackup

        cfg_base = riser_stackup(cfg_base)
    elif basename == "sn_curve":
        from digitalmodel.fatigue.workflow import router as sn_curve

        cfg_base = sn_curve(cfg_base)
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
        logger.info("Mooring analysis routed to mooring_analysis module")
        from digitalmodel.subsea.mooring_analysis.cli import cli as mooring_cli

        raise NotImplementedError(
            "Mooring via engine requires mooring_analysis CLI or direct API. "
            "Use: python -m digitalmodel.subsea.mooring_analysis.cli"
        )
    elif basename == "artificial_lift":
        from digitalmodel.marine_ops.artificial_lift.dynacard.solver import (
            DynacardWorkflow,
        )

        al = DynacardWorkflow()
        cfg_base = al.router(cfg_base)
    elif basename == "digitalmarketing":
        from digitalmodel.specialized.digitalmarketing.digitalmarketing import (
            DigitalMarketing,
        )

        dm = DigitalMarketing()
        cfg_base = dm.router(cfg_base)
    elif basename == "on_bottom_stability":
        from digitalmodel.subsea.pipeline.integrity_workflows import (
            run_on_bottom_stability,
        )

        cfg_base = run_on_bottom_stability(cfg_base)
    elif basename == "free_span":
        from digitalmodel.subsea.pipeline.integrity_workflows import run_free_span

        cfg_base = run_free_span(cfg_base)
    elif basename == "lateral_buckling":
        from digitalmodel.subsea.pipeline.integrity_workflows import (
            run_lateral_buckling,
        )

        cfg_base = run_lateral_buckling(cfg_base)
    elif basename == "upheaval_buckling":
        from digitalmodel.subsea.pipeline.integrity_workflows import (
            run_upheaval_buckling,
        )

        cfg_base = run_upheaval_buckling(cfg_base)
    elif basename == "diffraction":
        from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

        diffraction = DiffractionWorkflow()
        cfg_base = diffraction.router(cfg_base)
    elif basename == "fpso_mooring":
        from digitalmodel.subsea.mooring_analysis.fpso_workflow import (
            FPSOMooringWorkflow,
        )

        fmw = FPSOMooringWorkflow()
        cfg_base = fmw.router(cfg_base)
    elif basename == "jacket_checks":
        from digitalmodel.structural.jacket_topside.workflow import JacketChecksWorkflow

        jacket_checks = JacketChecksWorkflow()
        cfg_base = jacket_checks.router(cfg_base)
    elif basename == "ocimf":
        from digitalmodel.marine_ops.marine_analysis.environmental_loading.workflow import (
            OCIMFWorkflow,
        )

        ocimf = OCIMFWorkflow()
        cfg_base = ocimf.router(cfg_base)
    elif basename == "naval_arch":
        from digitalmodel.naval_architecture.workflow import NavalArchitectureWorkflow

        naval_arch = NavalArchitectureWorkflow()
        cfg_base = naval_arch.router(cfg_base)
    elif basename == "geotechnical":
        from digitalmodel.geotechnical.workflow import GeotechnicalWorkflow

        geotechnical = GeotechnicalWorkflow()
        cfg_base = geotechnical.router(cfg_base)
    elif basename == "production":
        from digitalmodel.production_engineering.workflow import (
            ProductionEngineeringWorkflow,
        )

        production = ProductionEngineeringWorkflow()
        cfg_base = production.router(cfg_base)
    elif basename == "well_bore_design":
        from digitalmodel.well.workflow import run_well_bore_design

        cfg_base = run_well_bore_design(cfg_base)
    elif basename == "well_hydraulics":
        from digitalmodel.well.workflow import run_well_hydraulics

        cfg_base = run_well_hydraulics(cfg_base)
    elif basename == "rop_analysis":
        from digitalmodel.well.workflow import run_rop_analysis

        cfg_base = run_rop_analysis(cfg_base)
    elif basename == "tubular_design":
        from digitalmodel.well.workflow import run_tubular_design

        cfg_base = run_tubular_design(cfg_base)
    else:
        raise (Exception(f"Analysis for basename: {basename} not found. ... FAIL"))

    logger.debug(f"{basename}, application ... END")
    app_manager.save_cfg(cfg_base=cfg_base)

    return cfg_base
