import logging

from digitalmodel.aqwa.aqwa_analysis_damping import AqwaDamping
from digitalmodel.aqwa.aqwa_analysis_ef_server import AqwaEFServer
from digitalmodel.aqwa.aqwa_analysis_raos import AqwaRAOs


class AqwaAnalysis:
    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def analysis_router(self, cfg):
        analysis_settings = cfg.get("analysis_settings", {})
        method = analysis_settings.get("method")

        if not method:
            raise ValueError("AQWA analysis_settings.method is required to run analysis.")

        method_key = str(method).lower()

        if method_key in ("raos", "rao"):
            cfg = AqwaRAOs().rao_router(cfg)
        elif method_key in ("damping", "viscous_damping"):
            AqwaDamping().router(cfg)
        elif method_key in (
            "ef_server",
            "external_forces",
            "external-force",
            "external_forces_server",
        ):
            AqwaEFServer().ef_server_router(cfg)
        else:
            raise ValueError(f"Unsupported AQWA analysis method: {method}")

        cfg["results"] = {
            "status": "completed",
            "method": method_key,
        }
        return cfg
