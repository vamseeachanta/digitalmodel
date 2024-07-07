
class DNV_RP_F103():

    
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg['inputs']['calculation_type'] == 'DNV_RP_F103_2010':
            self.DNV_RP_F103_2010(cfg)
        else:
            raise (Exception(f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"))

        return cfg


    def DNV_RP_F103_2010(self, cfg):
        """
        This method is used to calculate the cathodic protection for ABS gn ships 2018
        """
        
        pass
        #TODO implement the method