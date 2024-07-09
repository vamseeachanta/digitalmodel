
class DNV_RP_F103:

    
    def __init__(self):
        pass

    def router(self, cfg):

        if cfg['inputs']['calculation_type'] == 'DNV_RP_F103_2010':
            self.DNV_RP_F103_2010(cfg)
        if cfg['inputs']['calculation_type'] == 'DNV_RP_F103_2019':
            self.DNV_RP_F103_2019(cfg)
        else:
            raise (Exception(f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"))

        return cfg


    def DNV_RP_F103_2010(self, cfg):
        """
        
        structure_area = self.get_structure_area(cfg)
        breakdown_factor = self.assess_coating_breakdown(cfg) (DONE)
        current_density = self.get_design_current_density(cfg)
        current_demand = self.get_current_demand(cfg, structure_area, current_density)

        anode_capacity = self.get_anode_current_capacity(cfg)
        
        anodes_required = self.get_anodes_required(cfg,breakdown_factor,current_demand,anode_capacity)
        
        anode_initial_check = self.get_anode_initial_check(cfg,anode_capacity,breakdown_factor,current_demand, anodes_required)
        
        anode_final_check = self.anode_final_check(cfg,anode_capacity,breakdown_factor,anode_initial_check, anodes_required,current_demand)

        seawater_resistivity = self.get_seawater_resistivity(cfg)

        """

        breakdown_factor = self.get_breakdown_factors(cfg)

    def get_breakdown_factors(self, cfg):
        design = cfg['inputs']['design']
        structure = cfg['inputs']['structure']
        coating = structure['coating_properties']

        a = coating['breakdown']['regular']['a']
        b = coating['breakdown']['regular']['b']
        fcm = a + 0.5*b*design['life']
        fcf = a + b*design['life']
        breakdown_factor_regular = {'mean': fcm, 'final': fcf}

        a = coating['breakdown']['field_joint']['a']
        b = coating['breakdown']['field_joint']['b']
        fcm = a + 0.5*b*design['life']
        fcf = a + b*design['life']
        breakdown_factor_field_joint = {'mean': fcm, 'final': fcf}

        breakdown_factor = {'regular': breakdown_factor_regular, 'field_joint': breakdown_factor_field_joint}

        return breakdown_factor