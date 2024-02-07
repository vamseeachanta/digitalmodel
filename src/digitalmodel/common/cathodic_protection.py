
class CathodicProtection():

    def __init__(self):
        pass

    def router(self, cfg):
        if cfg['inputs']['calculation_type'] == 'ABS_gn_ships_2018':
            self.ABS_gn_ships_2018(cfg)
        else:
            raise (Exception(f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"))
        

        return cfg
    
    def ABS_gn_ships_2018(self, cfg):
        """
        This method is used to calculate the cathodic protection for ABS gn ships 2018
        """
        pass

    def get_design_current_density(self, cfg):
        """
        This method is used to calculate the design current density
        """
        pass
    
    
    def get_required_current_density(self, cfg):
        """
        This method is used to calculate the required current density
        """
        pass


    
        
    def get_anodes_required(self, cfg):
        """
        This method is used to calculate weight and no of anodes required
        """
        pass
    
    def anode_initial_check(self, cfg):
        """
        This method is used to check the initial anode
        """
        pass


    def anode_final_check(self, cfg):
        
        """
        This method is used to check the final anode
        """
        pass
    