# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.modules.aqwa.aqwa_post_process import AqwaPostProcess
from digitalmodel.modules.aqwa.aqwa_pre_process import AqwaPreProcess
from digitalmodel.modules.aqwa.mes_files import MesFiles

a_post = AqwaPostProcess()
a_pre = AqwaPreProcess()
mes_files = MesFiles()


class Aqwa:
    def __init__(self):
        pass

    def router(self, cfg):

        cfg = self.get_cfg_with_master_data(cfg)

        if cfg["type"]["preprocess"]:
            a_pre.pre_process_router(cfg)

        if cfg["type"]["analysis"]:
            # Reader imports
            from digitalmodel.custom.aqwa.aqwa_analysis import AqwaAnalysis

            a_analysis = AqwaAnalysis()
            a_analysis.analysis_router(cfg)
        if "mes" in cfg and cfg["mes"]["flag"]:
            mes_files.router(cfg)

        if cfg["type"]["results"]:
            cfg = a_post.post_process_router(cfg)

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if "summary_settings_master" in cfg:
            summary_settings_master = cfg["summary_settings_master"].copy()
            summary_settings = cfg["summary_settings"]

            for group_index in range(0, len(summary_settings["groups"])):
                group = summary_settings["groups"][group_index].copy()
                group = update_deep_dictionary(
                    summary_settings_master["groups"][0], group
                )
                summary_settings["groups"][group_index] = group.copy()

        if "settings_master" in cfg:
            keychain = cfg["settings_master"]["keychain"]
            settings_master = cfg["settings_master"].copy()

            groups = cfg[keychain[0]]

            for group_index in range(0, len(groups)):
                group = groups[group_index].copy()
                group = update_deep_dictionary(settings_master, group)
                cfg[keychain[0]][group_index] = update_deep_dictionary(
                    cfg[keychain[0]][group_index], group.copy()
                )

        return cfg
