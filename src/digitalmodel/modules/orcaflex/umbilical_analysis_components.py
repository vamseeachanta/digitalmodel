
from assetutilities.common.data import SaveData


from digitalmodel.modules.orcaflex.orcaflex_linetypes import OrcaflexLineTypes
from digitalmodel.modules.orcaflex.umbilical_installation_IV_to_host import (
    InstallationVtoHost,
)
from digitalmodel.modules.orcaflex.umbilical_installation_lower_2nd_end import (
    Lower2ndEnd,
)

save_data = SaveData()
olt = OrcaflexLineTypes()
iv_to_host = InstallationVtoHost()
lower_2nd_end = Lower2ndEnd()


class UmbilicalAnalysis:
    def __init__(self):
        pass

    def perform_analysis(self, cfg):
        if cfg["installation_phases"]:
            # TODO program assumes host at 0 deg and No Y coordinate for installation vessel reference point.
            # Program this feature for future analysis
            self.installation_phases(cfg)
        elif cfg["line_properties"]:
            olt.get_umbilical_lines(cfg)
        else:
            raise NotImplementedError("Analysis not implemented.")

        return cfg

    def first_end_analysis(self):
        pass

    def second_end_analysis(self):
        pass

    def installation_phases(self, cfg):
        for phase in cfg["installation"]["phase"]:
            if cfg["installation"]["phase_type"] == "installationV_to_host":
                iv_to_host.installation_phase(cfg, phase)
            elif cfg["installation"]["phase_type"] == "2nd_end":
                lower_2nd_end.installation_phase(cfg, phase)

        return cfg
