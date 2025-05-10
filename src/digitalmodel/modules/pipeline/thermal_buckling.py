# Standard library imports

# Third party imports
from assetutilities.common.visualization.visualization_templates_matplotlib import (
    VisualizationTemplates,
)

viz_templates = VisualizationTemplates()

class ThermalBuckling:
    
    def __init__(self):
        pass

    def run(self, cfg):

        friction_force = self.get_friction_force(cfg)
        cfg['pipeline']['friction_force'] = friction_force
        lateral_buckling_df = self.get_upheaval_buckling(cfg)
        
