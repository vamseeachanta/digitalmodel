import math

class DNV_RP_F103:

    
    def __init__(self):
        pass

    def router(self, cfg):

        if cfg['inputs']['calculation_type'] == 'DNV_RP_F103_2010':
            self.DNV_RP_F103_2010(cfg)
        elif cfg['inputs']['calculation_type'] == 'DNV_RP_F103_2019':
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
        
        anode_mass = self.calculate_anode_mass(cfg)
        surface_area_protected = self.get_surface_area(cfg)

        current_demand = self.get_current_demand(cfg,breakdown_factor,surface_area_protected)
        bracelet_anode_mass = self.get_required_anode_mass(cfg,current_demand,anode_mass)


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
        breakdown_factor_field_joint = {'mean': fcm, 'final': round(fcf,2)}

        breakdown_factor = {'regular': breakdown_factor_regular, 'field_joint': breakdown_factor_field_joint}

        return breakdown_factor
    
    def calculate_anode_mass(self,cfg):
        """
        Anode mass calculation
        """
        structure = cfg['inputs']['structure']
        anode = cfg['inputs']['anode']
        coating = structure['coating_properties']

        outer_diameter = structure['dimensions']['Nominal_OD']+ 2*coating['coatings']['thickness']+2*anode['physical_properties']['thickness']
        inner_diameter = structure['dimensions']['Nominal_OD']+ 2*coating['coatings']['thickness']  

        density = anode['physical_properties']['density']
        
        # convert to SI units
        length = anode['physical_properties']['length']
        length = length * 0.0254
        thickness = anode['physical_properties']['thickness']
        thickness = thickness * 0.0254
        half_shell_gap = anode['physical_properties']['half_shell_gap']
        half_shell_gap = half_shell_gap * 0.0254
        outer_diameter = outer_diameter * 0.0254 

        mass = density* length* thickness* (math.pi*(outer_diameter-thickness)-2*half_shell_gap)

        anode_mass = {'anode_mass': round(mass,2), 'outer_diameter': outer_diameter, 'inner_diameter': inner_diameter}
        
        return anode_mass
    
    def get_surface_area(self, cfg):
        structure = cfg['inputs']['structure']
        
        field_joint_length = 2 * structure['dimensions']['length']['cutback']*(structure['dimensions']['length']['total']/structure['dimensions']['length']['joint'])
        field_joint_length = math.ceil(field_joint_length)

        # convert to SI units
        OD = structure['dimensions']['Nominal_OD']
        OD = OD * 0.0254
        length = structure['dimensions']['length']['total']
        area_pipeline = math.pi * OD *(length-field_joint_length)

        area_field_joint = math.pi * OD *(field_joint_length)

        surface_area = {'surface_area_pipeline': round(area_pipeline,2), 'surface_area_field_joint': round(area_field_joint,3)}
        
        return surface_area
    
    def get_current_demand(self, cfg,breakdown_factor,surface_area_protected):
        """
        Current demand calculation
        """
        structure = cfg['inputs']['structure']
        design = cfg['inputs']['design']

        mean = surface_area_protected['surface_area_pipeline']*breakdown_factor['regular']['mean']*structure['electrical']['anode_mean_current_density']*design['factor']+surface_area_protected['surface_area_field_joint']*breakdown_factor['field_joint']['mean']*structure['electrical']['anode_mean_current_density']*design['factor']

        final = surface_area_protected['surface_area_pipeline']*breakdown_factor['regular']['final']*structure['electrical']['anode_mean_current_density']*design['factor']+surface_area_protected['surface_area_field_joint']*breakdown_factor['field_joint']['final']*structure['electrical']['anode_mean_current_density']*design['factor']

        current_demand = {'mean_current_demand': round(mean,3), 'final_current_demand': round(final,3)}

        return current_demand
    
    def get_required_anode_mass(self, cfg,current_demand,anode_mass):
        """
        this is used to calculate anode mass of 'bracelet type' 
        """
        design = cfg['inputs']['design']
        anode = cfg['inputs']['anode']

        mass = current_demand['mean_current_demand']*design['life']/anode['utilisation_factor']*anode['current_capacity']
        number = mass / anode_mass['anode_mass']

        anode_mass = {'anode_mass_bracelet_type': round(mass,1),'anode_number': round(number,1)}
        return anode_mass

    

        

        

        
        
    

        
         
    
