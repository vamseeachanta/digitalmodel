# Reader imports
from digitalmodel.custom.aqwa.aqwa_analysis_ef_server import AqwaEFServer
from digitalmodel.custom.aqwa.aqwa_analysis_raos import AqwaRAOs
from digitalmodel.custom.aqwa.aqwa_analysis_damping import AqwaDamping

aq_ef = AqwaEFServer()
aq_raos = AqwaRAOs()
aq_damping = AqwaDamping()


class AqwaAnalysis:
    def __init__(self):
        pass

    def analysis_router(self, cfg):

        if cfg["analysis_settings"]["method"] == "ef_server":
            aq_ef.ef_server_router(cfg)

        elif cfg["analysis_settings"]["method"] == "raos":
            cfg = aq_raos.rao_router(cfg)

        elif cfg["analysis_settings"]["method"] == "damping":
            cfg = aq_damping.router(cfg)

        return cfg
