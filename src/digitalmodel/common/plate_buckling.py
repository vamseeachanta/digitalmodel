import math
class PlateBuckling():

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
        resistance = self.get_resistance(cfg)
        FEA_stress= self.get_FEA_stress(cfg)
        characteristic_resistance = self.get_characteristic_resistance(cfg,resistance)
        buckling_coefficient = self.get_buckling_coefficient(cfg,resistance)
    
    def get_resistance(self,cfg):
        
        plate_cfg = cfg['inputs']['plate_1']
        length = round(plate_cfg['length']/0.3048,2) #ft
        breadth = round(plate_cfg['breadth']/0.3048,2) #ft
        thickness = round(plate_cfg['thickness']/0.3048,3) #ft
        water_depth = round(plate_cfg['water_depth']/0.3048,2) #ft

        s_by_l = breadth/length
        l_by_s = 1/ s_by_l
        c = 2- s_by_l
        t_by_s = thickness/ breadth
        s_by_t = 1/ t_by_s
        l_by_t = length/ thickness

        resistance = { 's/l': round(s_by_l,2), 'l/s': round(l_by_s,2), 'c':round(c,2), 't/s':round(t_by_s,2),
                       's/t':round(s_by_t,2), 'l/t':round(l_by_t,2), 'yield_strength': plate_cfg['yield_strength']
                     }
        return resistance
    
    def get_FEA_stress(self,cfg):
        
        stress_cfg = cfg['inputs']['plate_1']
        longtudinal_stress = stress_cfg['longtudinal_stress']
        transverse_stress = stress_cfg['transverse_stress']
        shear_stress = stress_cfg['shear_stress']

        vonmises_stress = math.sqrt(longtudinal_stress*longtudinal_stress + transverse_stress*transverse_stress
                                     -(longtudinal_stress*transverse_stress) +
                                    (3*shear_stress*shear_stress))
        FEA_stress = {'vonmises_stress': round(vonmises_stress,2)}
        return FEA_stress
    
    def get_characteristic_resistance(self,cfg,resistance):

        normal_stress_σkx = resistance['yield_strength']
        normal_stress_τk = normal_stress_σkx/math.sqrt(3) 
        
        characteristic_resistance = {'normal_stress': round(normal_stress_τk,2)}
        return characteristic_resistance
    
    def get_buckling_coefficient(self,cfg,resistance):
        
        key_value = resistance['s/l']
        buckling_coefficient_transverse_stress = (1+key_value * key_value)
        buckling_coefficient_transverse_stress = buckling_coefficient_transverse_stress*buckling_coefficient_transverse_stress
        buckling_coefficient_shear_stress = 5.34 + 4* key_value * key_value
        
        buckling_coefficient = {'buckling_coefficient_transverse_stress': round(buckling_coefficient_transverse_stress,2),
                                'buckling_coefficient_shear_stress': round(buckling_coefficient_shear_stress,2)}
        return buckling_coefficient







        
