# Standard library imports
import math

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
        fluid_properties = self.get_fluid_properties(cfg)
        flow_parameters = self.flow_parameters(cfg, pipe_data, fluid_properties)
        

    def get_pipe_data(self, cfg):

        acceleration_due_to_gravity = cfg['pipe_data']['acceleration']
        water_depth = cfg['pipe_data']['water_depth']

        depth_in_meters = water_depth * 0.3048

        inner_diameter = cfg['pipe_data']['Nominal_Id']*0.0254
        internal_area = math.pi/4 * (inner_diameter)**2
        roughness = cfg['pipe_data']['roughness']*0.001
        e_by_D = roughness/inner_diameter

        pipe_data = { 'acceleration': acceleration_due_to_gravity, 'depth_in_meters': depth_in_meters,
                      'inner_diameter': round(inner_diameter,3),'internal_area': round(internal_area,3),
                      'roughness': roughness,'e_by_D': round(e_by_D,4)
                    }
        return pipe_data
    
    def get_fluid_properties(self, cfg):

        density = cfg['pipe_data']['mud_weight'] *119.8264
        viscosity = cfg['pipe_data']['viscosity'] * 0.001

        fluid_properties = {'density': round(density,2), 'viscosity': viscosity}
        return fluid_properties
    
    def flow_parameters(self, cfg, pipe_data,fluid_properties):
        
        flow_rate = cfg['pipe_data']['flow_rate'] * 0.1589873 / 60
        U = round(flow_rate / pipe_data['internal_area'],1)
        reynolds_number = U * fluid_properties['density'] * pipe_data['inner_diameter']/ fluid_properties['viscosity']

        friction_factor_laminar = 64 / reynolds_number
        factor_1 = math.log(6.9/ reynolds_number+ (pipe_data['e_by_D']/3.7) ** 1.11)
        factor_2 = -1.8 * factor_1
        friction_factor_turbulent =( 1 / factor_2 ) ** 2
        

        if reynolds_number > 3000:
            friction_loss = friction_factor_turbulent * pipe_data['depth_in_meters'] / pipe_data['inner_diameter'] * U ** 2 /2 / pipe_data['acceleration']

        else:
            friction_loss = friction_factor_laminar * pipe_data['depth_in_meters'] / pipe_data['inner_diameter'] * U ** 2 /2 / pipe_data['acceleration']

        friction_loss_in_mpA = fluid_properties['density'] * pipe_data['acceleration'] * friction_loss / 1000000

        kinetic_energy = U ** 2 / 2 / pipe_data['acceleration'] 
        potential_energy = pipe_data['depth_in_meters']
        total_energy = kinetic_energy + potential_energy + friction_loss

        flow_parameters = {'flow_rate': round(flow_rate,3), 'U': U, 'reynolds_number': round(reynolds_number,2),
                           'friction_loss': round(friction_loss,2), 'friction_loss_in_mpA': round(friction_loss_in_mpA,2),
                           'kinetic_energy': round(kinetic_energy,2), 'potential_energy': potential_energy,
                           'total_energy': round(total_energy,2)
                          }
        return flow_parameters


       
    

