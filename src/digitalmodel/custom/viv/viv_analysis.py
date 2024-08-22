# Standard library imports
import logging

# Reader imports
from digitalmodel.common.pipe_properties import PipeProperties
from digitalmodel.custom.viv.viv_analysis_legacy import (
    viv_analysis as VIVAnalysisLegacy,
)

from digitalmodel.custom.viv.viv_tubular_members import VIVTubularMembers

viv_tubular = VIVTubularMembers()
pp = PipeProperties()

class VIVAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):
        cfg = self.get_pipe_properties(cfg)
        if cfg['calculation']['name']:
            logging.error("Assess tubular members for VIV analysis")
            cfg = viv_tubular.analyze(cfg)
        else:
            logging.error("No calculation name found in the configuration file")
            viv_legacy = VIVAnalysisLegacy()
            cfg = viv_legacy.analyze(cfg)
        
        return cfg
    
    def get_pipe_properties(self, cfg):
        pipe_crossection = cfg['pipeline']['crossection']
        cfg = pp.get_properties(cfg, pipe_crossection)
        
        return cfg