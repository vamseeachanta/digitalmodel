class CathodicProtection:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg["inputs"]["calculation_type"] == "ABS_gn_ships_2018":
            self.ABS_gn_ships_2018(cfg)
        else:
            raise (
                Exception(
                    f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"
                )
            )

        return cfg

    def ABS_gn_ships_2018(self, cfg):
        """
        This method is used to calculate the cathodic protection for ABS gn ships 2018
        """
        anode_current_capacity = self.get_anode_current_capacity(cfg)
        my_value = (
            cfg.inputs["design_data"]["seawater_max_temperature"]
            * cfg.inputs["design_data"]["design_life"]
        )

    def get_anode_current_capacity(self, cfg):
        """
        This method is used to calculate the anode current capacity
        """
        if cfg["inputs"]["anode"] == "zinc":
            anode_current_capacity = 780
        elif cfg["inputs"]["anode"] == "aluminium":
            anode_current_capacity = 2000 - 27 * (
                cfg["inputs"]["design_data"]["seawater_max_temperature"] - 20
            )

        return anode_current_capacity

    def get_seawater_resistivity(self, cfg):
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
