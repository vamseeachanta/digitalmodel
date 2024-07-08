# Reader imports

from digitalmodel.custom.pipeline.lateral_buckling import LateralBuckling
from digitalmodel.custom.pipe_properties import PipeProperties

lb = LateralBuckling()
pp = PipeProperties()

class Pipeline:
    def __init__(self):
        pass

    def router(self, cfg=None):
        self.basic_properties(cfg)
        if cfg['calculation']['name'] == 'lateral_buckling':
            lb.run( cfg )
        elif cfg['calculation']['name'] == 'thermal_buckling':
            pass
        else:
            raise ValueError("calculation not implemented")
        
        return cfg
    
    def basic_properties(self, cfg):
        pp.get_properties(cfg)