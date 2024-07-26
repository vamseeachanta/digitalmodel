# Standard library imports

# Third party imports
from assetutilities.common.visualization.visualization_templates import (
    VisualizationTemplates,
)

# Reader imports
from digitalmodel.custom.pipeline.buckling_common import CommonBucklingCaculations

viz_templates = VisualizationTemplates()
cbc = CommonBucklingCaculations()
class Pressureloss():
    
    def __init__(self):
        pass

    def run(self, cfg):
        
        pipe_data = self.get_pipe_data(cfg)
        calci = self.calci(cfg,pipe_data)
        

    def get_pipe_data(self, cfg):

        acceleration_due_to_gravity = cfg['pipe_data']['acceleration']
        water_depth = cfg['pipe_data']['water_depth']

        depth_in_meters = water_depth * 0.3048

        pipe = {'water_depth': depth_in_meters}
        return pipe
    def calci(self, cfg,pipe_data):

        a = pipe_data['water_depth']
        b = cfg['pipe_data'] * 2

