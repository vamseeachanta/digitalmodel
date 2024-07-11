# Reader imports

# Reader imports
from digitalmodel.common.pipe_properties import PipeProperties
from digitalmodel.custom.pipeline.lateral_buckling import LateralBuckling
from digitalmodel.custom.pipeline.thermal_buckling import ThermalBuckling
from digitalmodel.custom.pipeline.upheaval_buckling import UpheavalBuckling

lb = LateralBuckling()
tb = ThermalBuckling()
ub = UpheavalBuckling()
pp = PipeProperties()

class Pipeline:
    def __init__(self):
        pass

    def router(self, cfg=None):
        cfg = self.get_pipe_properties(cfg)
        if cfg['calculation']['name'] == 'lateral_buckling':
            lb.run( cfg )
        elif cfg['calculation']['name'] == 'thermal_buckling':
            tb.run( cfg )
        elif cfg['calculation']['name'] == 'upheaval_buckling':
            ub.run( cfg )
        else:
            raise ValueError("calculation not implemented")
        
        return cfg
    
    def get_pipe_properties(self, cfg):
        pipe_crossection = cfg['pipeline']['crossection']
        cfg = pp.get_properties(cfg, pipe_crossection)
        
        return cfg