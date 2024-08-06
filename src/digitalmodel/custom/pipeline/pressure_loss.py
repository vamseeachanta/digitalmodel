# Standard library imports
import math
import sympy as sp
from sympy import init_printing
init_printing()

from sympy import init_session
init_session()
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

        cfg['pressure_loss'] = {'flow_parameters': flow_parameters}
        return cfg 
        

    def get_pipe_data(self, cfg):

        water_depth = cfg['pipe_data']['water_depth'] 
        acceleration = cfg['pipe_data']['acceleration']
        inner_diameter = cfg['pipe_data']['Nominal_Id']
        roughness = cfg['pipe_data']['roughness']

        w= sp.Symbol('w')
        w = water_depth

        a = sp.Symbol('a')
        a = acceleration

        i = sp.Symbol('i')
        i = inner_diameter

        r = sp.Symbol('r')
        r = roughness

        w_in_meters = w * 0.3048
        w_in_meters = round(float(w_in_meters),1)
        i_in_meters = i *0.0254
        i_in_meters = round(float(i_in_meters),3)
        r_in_meters = r * 0.001

        internal_area = sp.pi/4 * (i_in_meters)**2
        internal_area = round(float(internal_area),4)
        e_by_D = r_in_meters / i_in_meters
        e_by_D = round(float(e_by_D),5)

        pipe_data = { 'acceleration': round(a,2), 'depth_in_meters': w_in_meters,
                      'inner_diameter': i_in_meters,'internal_area':internal_area,
                      'roughness': r_in_meters,'e_by_D': e_by_D
                    }
        return pipe_data
    
    def get_fluid_properties(self, cfg):

        density = cfg['pipe_data']['mud_weight'] 
        viscosity = cfg['pipe_data']['viscosity'] 
        d = sp.Symbol('d')
        d = density
        v = sp.Symbol('v')
        v = viscosity

        d = d * 119.8264
        v = v * 0.001

        fluid_properties = {'density': round(d,2), 'viscosity': v}
        return fluid_properties
    
    def flow_parameters(self, cfg, pipe_data,fluid_properties):
        
        flow_rate = cfg['pipe_data']['flow_rate']
        internal_area = pipe_data['internal_area']
        density = fluid_properties['density']
        inner_diameter = pipe_data['inner_diameter']
        viscosity = fluid_properties['viscosity']
        depth = pipe_data['depth_in_meters']
        acceleration = pipe_data['acceleration']
        
        fr, ia, de, id, v, dm, ac = sp.symbols('fr ia de id v dm ac')
        fr = flow_rate
        ia = internal_area
        de = density
        id = inner_diameter
        v = viscosity
        dm = depth
        ac = acceleration


        fr = fr * 0.1589873 / 60
        U = fr /ia
        U = round(float(U),2)
        reynolds_number = (U * de * id)/ v
        reynolds_number = round(float(reynolds_number),2)

        friction_factor_laminar = 64 / reynolds_number
        friction_factor_laminar = round(float(friction_factor_laminar),3)
        
        a = -1.8
        b = 6.9
        reynolds_number = round(reynolds_number,2)
        d = pipe_data['e_by_D']
        e = 3.7
        f = 1.11

        expression_1 = sp.log((b / reynolds_number) + (d / e) ** f, 10)
        expression_2 = a * expression_1
        expression_3 = (1 / expression_2 ) ** 2

        simplified_expression = sp.simplify(expression_3)
        friction_factor_turbulent = simplified_expression.evalf()
        friction_factor_turbulent = round(float(friction_factor_turbulent),3)

        if reynolds_number > 3000:
            friction_loss = friction_factor_turbulent * dm / id * U ** 2 /2 / ac
            friction_loss = round(float(friction_loss),2)

        else:
            friction_loss = friction_factor_laminar * dm / id * U ** 2 /2 / ac

        friction_loss_in_mpA = de * ac * friction_loss / 1000000

        kinetic_energy = U ** 2 / 2 / ac
        kinetic_energy = round(float(kinetic_energy),2) 
        potential_energy = dm
        total_energy = kinetic_energy + potential_energy + friction_loss
        total_energy = round(float(total_energy),2)

        flow_parameters = {'flow_rate': round(fr,3), 'U': U, 'reynolds_number': reynolds_number,
                           'friction_loss': friction_loss,'friction_factor_laminar':friction_factor_laminar,
                           'friction_factor_turbulent':friction_factor_turbulent,'kinetic_energy': kinetic_energy,
                           'potential_energy': potential_energy,'total_energy': total_energy
                          }
        return flow_parameters
    



       
    

