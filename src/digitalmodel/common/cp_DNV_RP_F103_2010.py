# Standard library imports
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
        bracelet_anode_mass = self.get_required_bracelet_anode_mass(cfg,current_demand)

        final_current_requirement = self.get_final_current_requirement(cfg,anode_mass,current_demand)
        max_pipe_length_check = self.max_pipe_length_check(cfg,breakdown_factor,final_current_requirement,current_demand,bracelet_anode_mass)
        final_check = self.final_check(cfg,max_pipe_length_check,bracelet_anode_mass,final_current_requirement,current_demand)

        cfg['cathodic_protection'] = {'breakdown_factor': breakdown_factor, 'anode_mass': anode_mass, 'surface_area_protected': surface_area_protected, 'current_demand': current_demand, 'bracelet_anode_mass': bracelet_anode_mass}

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

        current_demand = {'mean': round(mean,3), 'final': round(final,3)}

        return current_demand
    
    def get_required_bracelet_anode_mass(self, cfg, current_demand):
        """
        this is used to calculate anode mass of 'bracelet type' 
        """
        design = cfg['inputs']['design']
        anode_cfg = cfg['inputs']['anode']
        design_life_in_years = design['life']
        design_life_in_hours = design_life_in_years * 8760
        anode_net_weight = anode_cfg['physical_properties']['net_weight']

        mass = current_demand['mean']*design_life_in_hours/(anode_cfg['utilisation_factor']*anode_cfg['current_capacity'])
        number = int(math.ceil(mass / anode_net_weight))

        anode_mass_bracelet = {'anode_mass': round(mass,1),'anode_number': round(number,1)}
        return anode_mass_bracelet
    
    def get_final_current_requirement(self, cfg,anode_mass,current_demand):
        
        structure = cfg['inputs']['structure']
        anode_cfg = cfg['inputs']['anode']
        environment_cfg = cfg['inputs']['environment']

        length_in_meters = anode_cfg['physical_properties']['length']*0.0254
        inner_diameter_in_meters = anode_mass['inner_diameter']*0.0254
        half_shell_gap_in_meters = anode_cfg['physical_properties']['half_shell_gap']*0.0254

        surface_area = length_in_meters*(math.pi*inner_diameter_in_meters- 2* half_shell_gap_in_meters)

        resistivity = round(0.315 * environment_cfg['seawater_resistivity']['input']/(math.sqrt(surface_area)),3)

        anode_current = (structure['electrical']['material_protective_potential']-(structure['electrical']['anode_potential']))/resistivity

        anode_number = anode_current/current_demand['final']

        final_current_requirement = {'anode':{'surface_area': round(surface_area,3), 'resistivity':resistivity,'current_output':round(anode_current,3),'number': round(anode_number,1)}}

        return final_current_requirement
    
    def max_pipe_length_check(self, cfg,breakdown_factor,final_current_requirement,current_demand,bracelet_anode_mass):
        
        structure_cfg = cfg['inputs']['structure']
        design_cfg = cfg['inputs']['design']
        
        # input values
        d= structure_cfg['dimensions']['Nominal_WT']* 0.0254
        D= round(structure_cfg['dimensions']['Nominal_OD']* 0.0254,4)
        Res = structure_cfg['electrical']['resistivity']
        icm = structure_cfg['electrical']['anode_mean_current_density']
        k = design_cfg['factor']
        Raf_bracelet = final_current_requirement['anode']['resistivity']
        Icf = current_demand['final']
        L = structure_cfg['dimensions']['length']['total']
        Ec = structure_cfg['electrical']['material_protective_potential']
        Ea = structure_cfg['electrical']['anode_potential']
        mass = bracelet_anode_mass['anode_number']
        number = final_current_requirement['anode']['number']

        breakdown_factor = round(breakdown_factor['regular']['final']+ 2*structure_cfg['dimensions']['length']['cutback']/structure_cfg['dimensions']['length']['joint']*breakdown_factor['field_joint']['final'],4)
        
        calc_1 = d * (D-d) / (Res * D * breakdown_factor* icm * k)
        calc_2 = (Raf_bracelet* Icf) / L
        calc_3 = ((Raf_bracelet*Icf)/L)**2
        calc_4 = (((Res *icm *breakdown_factor* D)/(d*(D-d))) * (Ec-(Ea)))
        calc_5 = math.sqrt((calc_3) + (calc_4))
        
        pipe_length = calc_1 * (calc_2 + calc_5)
        anode_number = L / pipe_length
        spacing = math.ceil(L /(max(anode_number,mass,number)))

        spacing_joints = spacing/structure_cfg['dimensions']['length']['joint']
        final_spacing = spacing_joints * structure_cfg['dimensions']['length']['joint']
        final_number = math.ceil(L / final_spacing)

        max_pipe_length_check = {'max_pipe_length': round(pipe_length,2), 'anode':{'final_spacing': round(final_spacing,1), 'final_number': round(final_number,1)}}
        return max_pipe_length_check
    
    def final_check(self, cfg,max_pipe_length_check,bracelet_anode_mass,final_current_requirement,current_demand):
        
        anode_cfg = cfg['inputs']['anode']

        if max_pipe_length_check['anode']['final_number'] * anode_cfg['physical_properties']['net_weight'] > bracelet_anode_mass['anode_mass']:
           mass_requirement = " Acceptable"
        else:
            mass_requirement = " Not Acceptable"
        if max_pipe_length_check['anode']['final_number'] * final_current_requirement['anode']['current_output'] > current_demand['final']:
            current_requirement = " Acceptable"
        else:
            current_requirement = " Not Acceptable"
        if max_pipe_length_check['max_pipe_length'] > max_pipe_length_check['anode']['final_spacing']:
            pipe_check_length = " Acceptable"
        else:
            pipe_check_length = " Not Acceptable"
         
        final_check = {'mass_requirement_check': mass_requirement, 'current_requirement_check': current_requirement, 'pipe_length_check': pipe_check_length}
        
        return final_check

    

    


        
      

    

        

        

        
        
    

        
         
    
    
