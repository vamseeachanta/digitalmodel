import math
class PlateBuckling():

    def __init__(self) -> None:
        pass

    def router(self, cfg):
        if cfg['inputs']['calculation_type'] == 'DNV_rp_C201':
            cfg = self.DNV_rp_C201(cfg)
        else:
            raise (Exception(f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"))

        return cfg

    def DNV_rp_C201(self, cfg):
        """
        This method is used to calculate the cathodic protection for ABS gn ships 2018
        """
        plate_properties = self.get_plate_properties(cfg)
        FEA_stress= self.get_FEA_stress(cfg)
        characteristic_resistance = self.get_characteristic_resistance(cfg,plate_properties)
        buckling_coefficient = self.get_buckling_coefficient(cfg,plate_properties)
        elastic_buckling_resistance = self.get_elastic_resistance(cfg,plate_properties,buckling_coefficient)
        reduced_slender_ratio = self.reduced_slenders_ratio(cfg,elastic_buckling_resistance,characteristic_resistance,plate_properties,FEA_stress)
        buckling_resistance_serviceability = self.buckling_resistance_serviceability(cfg,characteristic_resistance,reduced_slender_ratio,plate_properties)
        usage_factor_serviceabilty = self.usage_factor_serviceability_check(cfg,FEA_stress,buckling_resistance_serviceability)

        return cfg

    def get_plate_properties(self,cfg):
        
        plate_cfg = cfg['inputs']['plate_1']
        length = round(plate_cfg[0]['length']/0.3048,2) #ft
        breadth = round(plate_cfg[1]['breadth']/0.3048,2) #ft
        thickness = round(plate_cfg[2]['thickness']/0.3048,3) #ft
        water_depth = round(plate_cfg[3]['water_depth']/0.3048,2) #ft

        s_by_l = breadth/length
        l_by_s = 1/ s_by_l
        c = 2- s_by_l
        t_by_s = thickness/ breadth
        s_by_t = 1/ t_by_s
        l_by_t = length/ thickness

        plate_properties = { 's/l': round(s_by_l,2), 'l/s': round(l_by_s,2), 'c':round(c,2), 't/s':round(t_by_s,2),
                       's/t':round(s_by_t,2), 'l/t':round(l_by_t,2), 'yield_strength': plate_cfg[4]['yield_strength'],
                     'young_modulus':plate_cfg[6]['young_modulus'], 'poission_ratio':plate_cfg[5]['poission_ratio']}
        return plate_properties
    
    def get_FEA_stress(self,cfg):
        
        stress_cfg = cfg['inputs']['plate_1']
        longtudinal_stress = stress_cfg[7]['longtudinal_stress']
        transverse_stress = stress_cfg[9]['transverse_stress']
        shear_stress = stress_cfg[8]['shear_stress']

        vonmises_stress = math.sqrt(longtudinal_stress * longtudinal_stress + transverse_stress * transverse_stress
                                     -(longtudinal_stress * transverse_stress) +
                                    (3 * shear_stress*shear_stress))
        FEA_stress = {'vonmises_stress': round(vonmises_stress,2),
                      'longtudinal_stress':round(longtudinal_stress,2),
                      'transverse_stress':round(transverse_stress,2),'shear_stress':round(shear_stress,2)}
        return FEA_stress
    
    def get_characteristic_resistance(self,cfg,plate_properties):

        normal_stress_σkx = plate_properties['yield_strength']
        normal_stress_τk = normal_stress_σkx/math.sqrt(3) 
        
        characteristic_resistance = {'normal_stress_τk': round(normal_stress_τk,2),
                                     'normal_stress_σkx':round(normal_stress_σkx,2)}
        return characteristic_resistance
    
    def get_buckling_coefficient(self,cfg,plate_properties):
        
        buckling_cfg = cfg['inputs']['plate_1']
        key_value = plate_properties['s/l']
        buckling_coefficient_transverse_stress = (1+key_value * key_value)
        buckling_coefficient_transverse_stress = buckling_coefficient_transverse_stress*buckling_coefficient_transverse_stress
        buckling_coefficient_shear_stress = 5.34 + 4* key_value * key_value
        
        buckling_coefficient = {'buckling_coefficient_transverse_stress': round(buckling_coefficient_transverse_stress,2),
                                'buckling_coefficient_shear_stress': round(buckling_coefficient_shear_stress,2),
                                'buckling_coefficient_longtudinal_stress':buckling_cfg[10]['buckling_coefficient']}
        return buckling_coefficient

    def get_elastic_resistance(self,cfg,plate_properties,buckling_coefficient):

        coefficient = 3.14159265358979 * 3.14159265358979 * plate_properties['young_modulus']/12/(1-plate_properties['poission_ratio']
                                                                                                  *plate_properties['poission_ratio'])
        
        elastic_resistance_longtudinal_stress = coefficient * buckling_coefficient['buckling_coefficient_longtudinal_stress'] * plate_properties['t/s'] *plate_properties['t/s']
        
        elastic_resistance_transverse_stress = coefficient * buckling_coefficient['buckling_coefficient_transverse_stress'] * plate_properties['t/s'] *plate_properties['t/s']

        elastic_resistance_shear_stress = coefficient * buckling_coefficient['buckling_coefficient_shear_stress'] * plate_properties['t/s'] *plate_properties['t/s']

        elastic_buckling_resistance = {'elastic_resistance_longtudinal_stress':round(elastic_resistance_longtudinal_stress,1),
                                       'elastic_resistance_transverse_stress':round(elastic_resistance_transverse_stress,2),
                                       'elastic_resistance_shear_stress':round(elastic_resistance_shear_stress,2)}
        return elastic_buckling_resistance
    
    def reduced_slenders_ratio(self,cfg,elastic_buckling_resistance,characteristic_resistance,plate_properties,FEA_stress):

        reduced_ratio_longtudinal_direction = math.sqrt(characteristic_resistance['normal_stress_σkx']/elastic_buckling_resistance['elastic_resistance_longtudinal_stress'])
        reduced_ratio_transverse_direction = math.sqrt(characteristic_resistance['normal_stress_σkx']/elastic_buckling_resistance['elastic_resistance_transverse_stress'])
        reduced_ratio_shear_direction = math.sqrt(characteristic_resistance['normal_stress_τk']/elastic_buckling_resistance['elastic_resistance_shear_stress'])

        equivalent_reduced_slenderness_ratio = math.sqrt(plate_properties['yield_strength']*FEA_stress['vonmises_stress']((FEA_stress['longtudinal_stress']/
                                               elastic_buckling_resistance['elastic_resistance_longtudinal_stress']** plate_properties['c'])+
                                               (FEA_stress['transverse_stress']/elastic_buckling_resistance['elastic_resistance_transverse_stress']** plate_properties['c'])+
                                               (FEA_stress['shear_stress']/elastic_buckling_resistance['elastic_resistance_shear_stress']** plate_properties['c'])** (1/plate_properties['c'])))

        reduced_slender_ratio = {'reduced_ratio_longtudinal_direction':round(reduced_ratio_longtudinal_direction,2),
                                 'reduced_ratio_transverse_direction':round(reduced_ratio_transverse_direction,2),
                                 'reduced_ratio_shear_direction':round(reduced_ratio_shear_direction,2),
                                 'equivalent_reduced_slenderness_ratio':round(equivalent_reduced_slenderness_ratio,2)
                                 }
        return reduced_slender_ratio
    
    def buckling_resistance_serviceability(self,cfg,characteristic_resistance,reduced_slender_ratio,plate_properties):

        buckling_resistance_longtudinal_direction = characteristic_resistance['normal_stress_σkx']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_longtudinal_direction'] **4)

        buckling_resistance_transverse_direction = characteristic_resistance['normal_stress_σkx']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_transverse_direction'] **4)

        buckling_resistance_shear_direction = characteristic_resistance['normal_stress_τk']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_shear_direction'] **4)

        equivalent_stress = plate_properties['yield_strength']/math.sqrt(1+reduced_slender_ratio['equivalent_reduced_slenderness_ratio']**4)
        
        buckling_resistance_serviceability ={'buckling_resistance_longtudinal_direction':round(buckling_resistance_longtudinal_direction,2),
                                             'buckling_resistance_transverse_direction':round(buckling_resistance_transverse_direction,2),
                                             'buckling_resistance_shear_direction':round(buckling_resistance_shear_direction,2),
                                             'equivalent_stress':round(equivalent_stress,2)
                                             }
        
        return buckling_resistance_serviceability
    
    def usage_factor_serviceability_check(self,cfg,FEA_stress,buckling_resistance_serviceability):

        usage_longtudinal_direction = FEA_stress['longtudinal_stress']/buckling_resistance_serviceability['buckling_resistance_longtudinal_direction']

        usage_transverse_direction = FEA_stress['transverse_stress']/buckling_resistance_serviceability['buckling_resistance_transverse_direction']

        usage_shear_direction = FEA_stress['shear_stress']/buckling_resistance_serviceability['buckling_resistance_shear_direction']

        usage_equivalent_direction = FEA_stress['vonmises_stress']/buckling_resistance_serviceability['equivalent_stress']

        usage_factor_serviceability = {'usage_longtudinal_direction':round(usage_longtudinal_direction,2),
                                       'usage_transverse_direction':round(usage_transverse_direction,2),
                                       'usage_shear_direction':round(usage_shear_direction,2),
                                       'usage_equivalent_direction':round(usage_equivalent_direction,2)
                                       }
        return usage_factor_serviceability



        
