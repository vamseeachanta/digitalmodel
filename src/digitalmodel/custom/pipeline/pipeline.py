# Reader imports

# Reader imports
from digitalmodel.common.pipe_properties import PipeProperties
from digitalmodel.custom.pipeline.lateral_buckling import LateralBuckling

lb = LateralBuckling()
pp = PipeProperties()

class Pipeline:
    def __init__(self):
        pass

    def router(self, cfg=None):
        cfg = self.get_pipe_properties(cfg)
        if cfg['calculation']['name'] == 'lateral_buckling':
            lb.run( cfg )
        elif cfg['calculation']['name'] == 'thermal_buckling':
            pass
        else:
            raise ValueError("calculation not implemented")
        
        return cfg
    
    def get_pipe_properties(self, cfg):
        pipe_crossection = cfg['pipeline']['crossection']
        cfg = pp.get_properties(cfg, pipe_crossection)
        
        return cfg