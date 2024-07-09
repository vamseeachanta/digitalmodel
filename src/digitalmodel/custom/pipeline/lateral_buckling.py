
class LateralBuckling:
    
    def __init__(self):
        pass

    def run(self, cfg):
        friction_force = self.get_friction_force(cfg)
        cfg['pipeline']['friction_force'] = friction_force
        self.get_stress(cfg)

    def get_friction_force(self, cfg):
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        
        mass_in_water = system_properties['equivalent']['mass']['water']['with_internal_fluid']
        friction_cfg = cfg['pipeline']['soil']
        
        axial_friction_force = mass_in_water * friction_cfg['axial_breakout_friction_coeff']
        lateral_friction_force = mass_in_water * friction_cfg['lateral_breakout_friction_coeff']
        
        friction = {'axial': axial_friction_force, 'lateral': lateral_friction_force}

        return friction
    
    def get_stress(self, cfg):
        pipe_properties = cfg['pipeline']['pipe_properties']
        system_properties = cfg['pipeline']['system_properties']
        friction = self.get_friction_force(cfg)

        
        
        stress = {}
        
        tension_cfg  = cfg['pipeline']['tension']
        
        pressure = system_properties['internal_fluid']['pressure']
        Ai = system_properties['section']['Ai']
        logitudinal_force = tension_cfg['start'] - tension_cfg['lay_tension'] + pressure * Ai - friction*(length)
        longitudinal_stress = tension_cfg['start']
        stress['axial'] = friction['axial'] / system_properties['equivalent']['EA']
        stress['lateral'] = friction['lateral'] / system_properties['equivalent']['EA']
        
        return stress