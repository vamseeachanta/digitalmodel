import sys

# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.hydrodynamics.aqwa.aqwa_post_process import AqwaPostProcess
from digitalmodel.hydrodynamics.aqwa.aqwa_pre_process import AqwaPreProcess
from digitalmodel.hydrodynamics.aqwa.mes_files import MesFiles

# Placeholder to support test patching; real class imported lazily in router
ViscousDampingDetermination = None  # type: ignore

a_post = AqwaPostProcess()
a_pre = AqwaPreProcess()
mes_files = MesFiles()


class Aqwa:
    def __init__(self):
        pass

    @staticmethod
    def _resolve_attr(name, fallback):
        module = sys.modules.get("digitalmodel.aqwa")
        if module is not None and hasattr(module, name):
            return getattr(module, name)
        return fallback

    def router(self, cfg):

        cfg = self.get_cfg_with_master_data(cfg)

        if cfg["type"]["preprocess"]:
            pre_handler = self._resolve_attr("a_pre", a_pre)
            pre_handler.pre_process_router(cfg)

        if cfg["type"]["analysis"]:
            analysis_settings = cfg.get("analysis_settings", {})
            viscous_cfg = analysis_settings.get("viscous_damping") if analysis_settings else None

            if isinstance(viscous_cfg, dict) and viscous_cfg.get("enabled", True):
                orchestrator_cls = globals().get("ViscousDampingDetermination")
                if orchestrator_cls is None:
                    orchestrator_cls = self._resolve_attr(
                        "ViscousDampingDetermination", None
                    )

                if orchestrator_cls is None:
                    from digitalmodel.hydrodynamics.aqwa.viscous_damping_determination import (
                        ViscousDampingDetermination as _ViscousDampingDetermination,
                    )

                    orchestrator_cls = _ViscousDampingDetermination
                    globals()["ViscousDampingDetermination"] = orchestrator_cls
                    module = sys.modules.get("digitalmodel.aqwa")
                    if module is not None:
                        setattr(module, "ViscousDampingDetermination", orchestrator_cls)

                orchestrator = orchestrator_cls()

                config_source = (
                    viscous_cfg.get("config")
                    or viscous_cfg.get("config_path")
                    or viscous_cfg.get("config_file")
                    or viscous_cfg.get("config_source")
                )

                if config_source is None:
                    config_source = viscous_cfg

                results_directory = (
                    viscous_cfg.get("results_directory")
                    or cfg.get("file_management", {}).get("output_directory")
                )

                run_result = orchestrator.run(config_source, results_directory)
                viscous_cfg["results"] = run_result
            else:
                try:
                    from digitalmodel.custom.aqwa.aqwa_analysis import AqwaAnalysis  # type: ignore
                except ModuleNotFoundError:
                    from digitalmodel.hydrodynamics.aqwa.aqwa_analysis import AqwaAnalysis  # type: ignore

                a_analysis = AqwaAnalysis()
                a_analysis.analysis_router(cfg)
        if "mes" in cfg and cfg["mes"]["flag"]:
            mes_handler = self._resolve_attr("mes_files", mes_files)
            mes_handler.router(cfg)

        if cfg["type"]["results"]:
            post_handler = self._resolve_attr("a_post", a_post)
            cfg = post_handler.post_process_router(cfg)

        return cfg

    def get_cfg_with_master_data(self, cfg):
        updater = self._resolve_attr("update_deep_dictionary", update_deep_dictionary)
        if "summary_settings_master" in cfg:
            summary_settings_master = cfg["summary_settings_master"].copy()
            summary_settings = cfg["summary_settings"]

            for group_index in range(0, len(summary_settings["groups"])):
                group = summary_settings["groups"][group_index].copy()
                group = updater(
                    summary_settings_master["groups"][0], group
                )
                summary_settings["groups"][group_index] = group.copy()

        if "settings_master" in cfg:
            keychain = cfg["settings_master"]["keychain"]
            settings_master = cfg["settings_master"].copy()

            groups = cfg[keychain[0]]

            for group_index in range(0, len(groups)):
                group = groups[group_index].copy()
                group = updater(settings_master, group)
                cfg[keychain[0]][group_index] = updater(
                    cfg[keychain[0]][group_index], group.copy()
                )

        return cfg
