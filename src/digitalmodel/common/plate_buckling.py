import math
import logging

class PlateBuckling():

    def __init__(self) -> None:
        pass

    def router(self, cfg):
        if cfg['calculation_type'] == 'DNV_rp_C201':
            plates = cfg['groups']
            plate_buckling_result_array = []
            # for plate_cfg in plates:
            for plate_idx in range(0, len(plates)):
                plate_cfg = plates[plate_idx]
                plate_buckling_result = self.run_plate_DNV_rp_C201(cfg, plate_cfg)
                plate_buckling_result_array.append(plate_buckling_result)
        
        else:
            logging.error(f"Calculation type: {cfg['calculation_type']} not IMPLEMENTED. ... FAIL")
            raise (Exception(f"Calculation type: {cfg['calculation_type']} not IMPLEMENTED. ... FAIL"))

        cfg[cfg['basename']] = plate_buckling_result_array

        return cfg
    
    def run_plate_DNV_rp_C201(self, cfg, plate_cfg):
        """
        This method is used to calculate plate buckling using DNV-RP-C201 Guidelines
        """
        plate_properties = self.get_plate_properties(plate_cfg)
        FEA_stress= self.get_FEA_stress(plate_cfg)
        characteristic_resistance = self.get_characteristic_resistance(cfg,plate_properties)
        buckling_coefficient = self.get_buckling_coefficient(plate_cfg,plate_properties)
        elastic_buckling_resistance = self.get_elastic_resistance(cfg,plate_properties,buckling_coefficient)
        reduced_slender_ratio = self.reduced_slenders_ratio(cfg,elastic_buckling_resistance,characteristic_resistance,plate_properties,FEA_stress)
        buckling_resistance_serviceability = self.buckling_resistance_serviceability(cfg,characteristic_resistance,reduced_slender_ratio,plate_properties)
        usage_factor_serviceabilty = self.usage_factor_serviceability_check(cfg,FEA_stress,buckling_resistance_serviceability)
        
        resistance_ultimate = self.resistance_ultimate_check(cfg,reduced_slender_ratio,characteristic_resistance)
        usage_factor_ultimate = self.usage_factor_ultimate_check(cfg,resistance_ultimate,FEA_stress)
        
        coefficient_factor = self.buckling_coefficient(cfg,plate_properties)
        elastic_resistance = self.elastic_resistance(cfg,plate_properties,elastic_buckling_resistance,coefficient_factor)
        reduced_ratio = self.get_reduced_ratio(cfg,elastic_resistance,characteristic_resistance,FEA_stress,plate_properties)
        characteristic_buckling_serviceability = self.get_characteristic_buckling_serviceability(cfg,characteristic_resistance,reduced_ratio,plate_properties,reduced_slender_ratio)
        usage_factor_serviceabilty_check = self.usage_factor(cfg,FEA_stress,characteristic_buckling_serviceability)
        
        char_resistance_ultimate = self.get_resistance_ultimate_check(cfg,reduced_ratio,characteristic_resistance)
        usage_factor_ultimate_check = self.usage_factor_ultimate(cfg,FEA_stress,char_resistance_ultimate)
        
        stress_longtudinal = self.get_stress_longitudinal_direction(cfg,plate_properties)
        stress_transverse = self.get_stress_transverse_direction(cfg,plate_properties)
        stress_shear = self.get_stress_shear_direction(cfg,plate_properties)
        stress_bi_axial = self.get_bi_axial_direction(cfg,plate_properties,stress_longtudinal,stress_transverse,stress_shear,FEA_stress)
        dnv_rp_usage_factor = self.DNV_RP_C201_usage_factor(cfg,FEA_stress,stress_longtudinal,stress_transverse,stress_shear,stress_bi_axial)

        plate_buckling_result = {'usage_factor_ultimate_check':usage_factor_ultimate_check,'usage_factor_serviceabilty':usage_factor_serviceabilty,
                                 'usage_factor_serviceabilty_check':usage_factor_serviceabilty_check,'usage_factor_ultimate':usage_factor_ultimate,
                                 'dnv_rp_usage_factor':dnv_rp_usage_factor}

        return plate_buckling_result


    
    def get_plate_properties(self, plate_cfg):
        
        length = round(plate_cfg['length']/0.3048,2) #ft
        breadth = round(plate_cfg['breadth']/0.3048,2) #ft
        thickness = round(plate_cfg['thickness']/0.3048,3) #ft
        yield_strength = plate_cfg['yield_strength']
        young_modulus = plate_cfg['young_modulus']
        poisson_ratio = plate_cfg['poisson_ratio']
        water_depth = round(plate_cfg['water_depth']/0.3048,2) #ft

        s_by_l = breadth/length
        l_by_s = 1/ s_by_l
        c = 2- s_by_l
        t_by_s = thickness/ breadth
        s_by_t = 1/ t_by_s
        l_by_t = length/ thickness

        plate_properties = { 's/l': round(s_by_l,2), 'l/s': round(l_by_s,2), 'c':round(c,2), 't/s':round(t_by_s,2),
                       's/t':round(s_by_t,2), 'l/t':round(l_by_t,2), 'yield_strength': yield_strength,'breadth':round(breadth,2),
                     'young_modulus': young_modulus,'length':round(length,2), 'poisson_ratio': poisson_ratio, 'water_depth': water_depth,'thickness':round(thickness,3)}
        
        return plate_properties
    
    def get_FEA_stress(self, plate_cfg):
        
        longtudinal_stress = plate_cfg['longtudinal_stress']
        transverse_stress = plate_cfg['transverse_stress']
        shear_stress = plate_cfg['shear_stress']

        vonmises_stress = math.sqrt(longtudinal_stress * longtudinal_stress + transverse_stress * transverse_stress
                                     -(longtudinal_stress * transverse_stress) +
                                    (3 * shear_stress*shear_stress))
        FEA_stress = {'vonmises_stress': round(vonmises_stress,2),
                      'longtudinal_stress':round(longtudinal_stress,2),
                      'transverse_stress':round(transverse_stress,2),'shear_stress':round(shear_stress,2)}
        return FEA_stress
    
    def get_characteristic_resistance(self, cfg,plate_properties):

        normal_stress_kx = plate_properties['yield_strength']
        normal_stress_tk = normal_stress_kx/math.sqrt(3) 
        
        characteristic_resistance = {'normal_stress_tk': round(normal_stress_tk,2),
                                     'normal_stress_kx':round(normal_stress_kx,2)}
        return characteristic_resistance

    def get_buckling_coefficient(self, plate_cfg,plate_properties):

        key_value = plate_properties['s/l']
        transverse_stress = (1+key_value * key_value)
        transverse_stress = transverse_stress*transverse_stress
        shear_stress = 5.34 + 4* key_value * key_value

        buckling_coefficient = {'buckling_coefficient_transverse_stress': round(transverse_stress,2),
                                'buckling_coefficient_shear_stress': round(shear_stress,2),
                                'buckling_coefficient_longtudinal_stress':plate_cfg['buckling_coefficient']}
        return buckling_coefficient

    def get_elastic_resistance(self,cfg,plate_properties,buckling_coefficient):

        coefficient = 3.14159265358979 ** 2 * plate_properties['young_modulus']/12/(1-plate_properties['poisson_ratio'] ** 2)

        longtudinal_stress = coefficient * buckling_coefficient['buckling_coefficient_longtudinal_stress'] * plate_properties['t/s'] *plate_properties['t/s']

        transverse_stress = coefficient * buckling_coefficient['buckling_coefficient_transverse_stress'] * plate_properties['t/s'] *plate_properties['t/s']

        shear_stress = coefficient * buckling_coefficient['buckling_coefficient_shear_stress'] * plate_properties['t/s'] *plate_properties['t/s']

        elastic_buckling_resistance = {'elastic_resistance_longtudinal_stress':round(longtudinal_stress,1),
                                       'elastic_resistance_transverse_stress':round(transverse_stress,2),
                                       'elastic_resistance_shear_stress':round(shear_stress,2),'coefficient':round(coefficient,2)}
        return elastic_buckling_resistance

    def reduced_slenders_ratio(self,cfg,elastic_buckling_resistance,characteristic_resistance,plate_properties,FEA_stress):

        longtudinal_direction = math.sqrt(characteristic_resistance['normal_stress_kx']/elastic_buckling_resistance['elastic_resistance_longtudinal_stress'])
        transverse_direction = math.sqrt(characteristic_resistance['normal_stress_kx']/elastic_buckling_resistance['elastic_resistance_transverse_stress'])
        shear_direction = math.sqrt(characteristic_resistance['normal_stress_tk']/elastic_buckling_resistance['elastic_resistance_shear_stress'])

        equivalent_ratio = math.sqrt((plate_properties['yield_strength']/FEA_stress['vonmises_stress'])*((FEA_stress['longtudinal_stress']/
                                               elastic_buckling_resistance['elastic_resistance_longtudinal_stress'])** plate_properties['c']+
                                               (FEA_stress['transverse_stress']/elastic_buckling_resistance['elastic_resistance_transverse_stress'])** plate_properties['c']+
                                               (FEA_stress['shear_stress']/elastic_buckling_resistance['elastic_resistance_shear_stress'])** plate_properties['c'])** (1/plate_properties['c']))

        reduced_slender_ratio = {'reduced_ratio_longtudinal_direction':round(longtudinal_direction,2),
                                 'reduced_ratio_transverse_direction':round(transverse_direction,2),
                                 'reduced_ratio_shear_direction':round(shear_direction,2),
                                 'equivalent_slenderness_ratio':round(equivalent_ratio,4)
                                 }
        return reduced_slender_ratio

    def buckling_resistance_serviceability(self,cfg,characteristic_resistance,reduced_slender_ratio,plate_properties):

        longtudinal_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_longtudinal_direction'] **4)

        transverse_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_transverse_direction'] **4)

        shear_direction = characteristic_resistance['normal_stress_tk']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_shear_direction'] **4)

        equivalent_stress = plate_properties['yield_strength']/math.sqrt(1+reduced_slender_ratio['equivalent_slenderness_ratio']**4)

        buckling_resistance_serviceability ={'buckling_resistance_longtudinal_direction':round(longtudinal_direction,2),
                                             'buckling_resistance_transverse_direction':round(transverse_direction,2),
                                             'buckling_resistance_shear_direction':round(shear_direction,2),
                                             'equivalent_stress':round(equivalent_stress,5)
                                             }

        return buckling_resistance_serviceability
    
    def usage_factor_serviceability_check(self,cfg,FEA_stress,buckling_resistance_serviceability):

        longtudinal_direction = FEA_stress['longtudinal_stress']/buckling_resistance_serviceability['buckling_resistance_longtudinal_direction']

        transverse_direction = FEA_stress['transverse_stress']/buckling_resistance_serviceability['buckling_resistance_transverse_direction']

        shear_direction = FEA_stress['shear_stress']/buckling_resistance_serviceability['buckling_resistance_shear_direction']

        equivalent_direction = FEA_stress['vonmises_stress']/buckling_resistance_serviceability['equivalent_stress']

        usage_factor_serviceability = {'usage_in_longtudinal_direction':round(longtudinal_direction,2),
                                       'usage_in_transverse_direction':round(transverse_direction,2),
                                       'usage_in_shear_direction':round(shear_direction,2),
                                       'usage_in_equivalent_direction':round(equivalent_direction,2)
                                       }
        return usage_factor_serviceability
    
    def resistance_ultimate_check(self,cfg,reduced_slender_ratio,characteristic_resistance):

        if reduced_slender_ratio['reduced_ratio_longtudinal_direction']<1:
            longtudinal_direction = characteristic_resistance['normal_stress_kx']/(math.sqrt(1+reduced_slender_ratio['reduced_ratio_longtudinal_direction']**4))
        else:
            longtudinal_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(2)/reduced_slender_ratio['reduced_ratio_longtudinal_direction']
        
        if reduced_slender_ratio['reduced_ratio_transverse_direction']<1:
            transverse_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_transverse_direction']**4)
        else:
            transverse_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(2)/reduced_slender_ratio['reduced_ratio_transverse_direction']
        
        if reduced_slender_ratio['reduced_ratio_shear_direction']<1:
            shear_direction = characteristic_resistance['normal_stress_tk']/math.sqrt(1+reduced_slender_ratio['reduced_ratio_shear_direction']**4)
        else:
            shear_direction = characteristic_resistance['normal_stress_tk']/math.sqrt(2)/reduced_slender_ratio['reduced_ratio_shear_direction']
        
        if reduced_slender_ratio['equivalent_slenderness_ratio']<1:
            equivalent_direction = characteristic_resistance['normal_stress_kx']/(math.sqrt(1+reduced_slender_ratio['equivalent_slenderness_ratio']**4))
        else:
            equivalent_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(2)/reduced_slender_ratio['equivalent_slenderness_ratio']

        ultimate_check_resistance = {'longtudinal':round(longtudinal_direction,2),'transverse':round(transverse_direction,2),
                          'shear':round(shear_direction,2),'equivalent':round(equivalent_direction,2)
                          }
        return ultimate_check_resistance
    
    def usage_factor_ultimate_check(self,cfg,resistance_ultimate,FEA_stress):

        longtudinal_direction = FEA_stress['longtudinal_stress']/resistance_ultimate['longtudinal']

        transverse_direction = FEA_stress['transverse_stress']/resistance_ultimate['transverse']

        shear_direction = FEA_stress['shear_stress']/resistance_ultimate['shear']

        equivalent_direction = FEA_stress['vonmises_stress']/resistance_ultimate['equivalent']

        usage_factor_ultimate = {'usage_longtudinal_direction':round(longtudinal_direction,2),'usage_transverse_direction':round(transverse_direction,2),
                                 'usage_shear_direction':round(shear_direction,2),'usage_equivalent_direction':round(equivalent_direction,2)
                                 }
        return usage_factor_ultimate
    
    def buckling_coefficient(self,cfg,plate_properties):

        longtudinal_stress = 7.0
        transverse_stress = 1 + 2.5 *plate_properties['s/l'] **2 +5 *plate_properties['s/l'] ** 4
        shear_stress = 9 + 5.6* plate_properties['s/l'] ** 2

        coefficinet_factor = {'coefficinet_longtudinal_stress':round(longtudinal_stress,2),
                       'coefficinet_transverse_stress' :round(transverse_stress,2),
                       'coefficinet_shear_stress': round(shear_stress,2)
                       }
        return coefficinet_factor
    
    def elastic_resistance(self,cfg,plate_properties,elastic_buckling_resistance,coefficient_factor):

        longtudinal_stress = elastic_buckling_resistance['coefficient'] * coefficient_factor['coefficinet_longtudinal_stress'] * plate_properties['t/s'] ** 2
        transverse_stress = elastic_buckling_resistance['coefficient'] * coefficient_factor['coefficinet_transverse_stress'] * plate_properties['t/s'] ** 2
        shear_stress = elastic_buckling_resistance['coefficient'] * coefficient_factor['coefficinet_shear_stress'] * plate_properties['t/s'] ** 2

        elastic_resistance = {'resistance_longtudinal_stress':round(longtudinal_stress,2),'resistance_transverse_stress':round(transverse_stress,2),
                              'resistance_shear_stress':round(shear_stress,2)
                              }
        return elastic_resistance
    
    def get_reduced_ratio(self,cfg,elastic_resistance,characteristic_resistance,FEA_stress,plate_properties):

        longtudinal_direction = math.sqrt(characteristic_resistance['normal_stress_kx']/elastic_resistance['resistance_longtudinal_stress'])
        transverse_direction = math.sqrt(characteristic_resistance['normal_stress_kx']/elastic_resistance['resistance_transverse_stress'])
        shear_direction = math.sqrt(characteristic_resistance['normal_stress_tk']/elastic_resistance['resistance_shear_stress'])
        equivalent_direction = math.sqrt((plate_properties['yield_strength']/FEA_stress['vonmises_stress'])*((FEA_stress['longtudinal_stress']/
                                               elastic_resistance['resistance_longtudinal_stress'])** plate_properties['c']+
                                               (FEA_stress['transverse_stress']/elastic_resistance['resistance_transverse_stress'])** plate_properties['c']+
                                               (FEA_stress['shear_stress']/elastic_resistance['resistance_shear_stress'])** plate_properties['c'])** (1/plate_properties['c']))
 
        reduced_ratio = {'ratio_longtudinal_direction':round(longtudinal_direction,2),'ratio_transverse_direction':round(transverse_direction,2),
                         'ratio_shear_direction':round(shear_direction,2),'ratio_equivalent_direction':round(equivalent_direction,2)
                         }
        return reduced_ratio
 
    def get_characteristic_buckling_serviceability(self,cfg,characteristic_resistance,reduced_ratio,plate_properties,reduced_slender_ratio):

        longtudinal_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(1 + reduced_ratio['ratio_longtudinal_direction'] ** 4)
        transverse_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(1 + reduced_ratio['ratio_transverse_direction'] ** 4)
        shear_direction = characteristic_resistance['normal_stress_tk']/math.sqrt(1 + reduced_ratio['ratio_shear_direction'] ** 4)
        equialent_direction = plate_properties['yield_strength']/math.sqrt(1 + reduced_slender_ratio['equivalent_slenderness_ratio'] ** 4)

        characteristic_buckling_serviceabilty = {'resistance_longtudinal_direction':round(longtudinal_direction,2),
                                                  'resistance_transverse_direction':round(transverse_direction,2),
                                                  'resistance_shear_direction':round(shear_direction,2),
                                                  'resistance_equivalent_direction':round(equialent_direction,2)
                                                  }
        return characteristic_buckling_serviceabilty
    
    def usage_factor(self,cfg,FEA_stress,characteristic_buckling_serviceability):

        longtudinal_direction = FEA_stress['longtudinal_stress']/characteristic_buckling_serviceability['resistance_longtudinal_direction']

        transverse_direction = FEA_stress['transverse_stress']/characteristic_buckling_serviceability['resistance_transverse_direction']

        shear_direction = FEA_stress['shear_stress']/characteristic_buckling_serviceability['resistance_shear_direction']

        equialent_direction = FEA_stress['vonmises_stress']/characteristic_buckling_serviceability['resistance_equivalent_direction']
        
        usage_factor = {'usage_longtudinal_direction':round(longtudinal_direction,3),'usage_transverse_direction':round(transverse_direction,2),
                        'usage_shear_direction':round(shear_direction,2),'usage_equialent_direction':round(equialent_direction,3)
                        }
        return usage_factor
    
    def get_resistance_ultimate_check(self,cfg,reduced_ratio,characteristic_resistance):

        if reduced_ratio['ratio_longtudinal_direction']<1:
            longtudinal_direction = characteristic_resistance['normal_stress_kx']/(math.sqrt(1+ reduced_ratio['ratio_longtudinal_direction']**4))
        else:
            longtudinal_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(2)/reduced_ratio['ratio_longtudinal_direction']
        
        if reduced_ratio['ratio_transverse_direction']<1:
            transverse = characteristic_resistance['normal_stress_kx']
        else:
            transverse = characteristic_resistance['normal_stress_kx']/math.sqrt(2)/reduced_ratio['ratio_transverse_direction']
        
        if reduced_ratio['ratio_shear_direction']<1:
            shear_direction = characteristic_resistance['normal_stress_tk']/(math.sqrt(1+ reduced_ratio['ratio_shear_direction']** 4))
        else:
            shear_direction = characteristic_resistance['normal_stress_tk']/math.sqrt(2)/reduced_ratio['ratio_shear_direction']

        if reduced_ratio['ratio_equivalent_direction']<1:
            equialent_direction = characteristic_resistance['normal_stress_kx']/(math.sqrt(1+ reduced_ratio['ratio_equivalent_direction']** 4))
        else:
            equialent_direction = characteristic_resistance['normal_stress_kx']/math.sqrt(2)/reduced_ratio['ratio_equivalent_direction']
        
        resistance_ultimate = {'longtudinal_resistance':round(longtudinal_direction,2),'transverse_resistance':round(transverse,2),
                               'shear_resistance':round(shear_direction,2),'equialent_resistance':round(equialent_direction,2)
                               }
        return resistance_ultimate
    
    def usage_factor_ultimate(self,cfg,FEA_stress,char_resistance_ultimate):

        longtudinal = FEA_stress['longtudinal_stress']/char_resistance_ultimate['longtudinal_resistance']

        transverse = FEA_stress['transverse_stress']/char_resistance_ultimate['transverse_resistance']

        shear = FEA_stress['shear_stress']/char_resistance_ultimate['shear_resistance']

        equivalent = FEA_stress['vonmises_stress']/char_resistance_ultimate['equialent_resistance']

        usage_factor_ultimate_check = {'usage_longtudinal':round(longtudinal,3),'usage_transverse':round(transverse,3),
                                       'usage_shear':round(shear,2),'usage_equivalent':round(equivalent,3)
                                       }
        return usage_factor_ultimate_check
    
    def get_stress_longitudinal_direction(self,cfg,plate_properties):

        reduced_ratio_value = 0.525 * plate_properties['s/t']*math.sqrt(plate_properties['yield_strength']/plate_properties['young_modulus'])

        Cx = (reduced_ratio_value-0.22)/reduced_ratio_value**2

        resulting_factor = 1.15

        design_resistance = Cx*plate_properties['yield_strength']/resulting_factor

        longtudinal_stress = {'design_resistance':round(design_resistance,2)
                             }
        return longtudinal_stress
    
    def get_stress_transverse_direction(self,cfg,plate_properties):

        k_value = 0.26
        reduction_factor = 1.00
        resulting_factor = 1.15
        R_value = (1.3 *plate_properties['thickness']/plate_properties['length']*math.sqrt(plate_properties['young_modulus']/plate_properties['yield_strength'])+
                   k_value*(1-1.3*plate_properties['thickness']/plate_properties['length']*math.sqrt(plate_properties['young_modulus']/plate_properties['yield_strength'])))*plate_properties['yield_strength']*reduction_factor
        
        design_resistance_transverse = R_value/resulting_factor

        transverse_stress = {'design_resistance_transverse':round(design_resistance_transverse,2)
                            }
        return transverse_stress
    
    def get_stress_shear_direction(self,cfg,plate_properties):

        kl_value = 5.34 +4 *(plate_properties['s/l'])** 2
        w_value = 0.795* plate_properties['s/t']* math.sqrt(plate_properties['yield_strength']/(plate_properties['young_modulus']*kl_value))
        C_value = 1

        resulting_factor = 1.15
        shear_resistance = C_value / resulting_factor * plate_properties['yield_strength']/math.sqrt(3)

        shear_stress = {'shear_resistance':round(shear_resistance,2)
                       }
        return shear_stress
    
    def get_bi_axial_direction(self,cfg,plate_properties,stress_longtudinal,stress_transverse,stress_shear,FEA_stress):

        ci_value = 1 - plate_properties['breadth']/(120 * plate_properties['thickness'])
        C_value = 1.00
        resulting_factor = 1.15
        stress_biaxial_with_shear = round(C_value/resulting_factor * plate_properties['yield_strength']/ math.sqrt(3),2)

        longtudinal_stress = stress_longtudinal['design_resistance']
        transverse_stress = stress_transverse['design_resistance_transverse']
        
        bi_axial_loading = ((FEA_stress['longtudinal_stress']/longtudinal_stress)**2+ (FEA_stress['longtudinal_stress']/transverse_stress)**2 - ci_value*(FEA_stress['longtudinal_stress']/longtudinal_stress)*(FEA_stress['longtudinal_stress']/transverse_stress)+(FEA_stress['shear_stress']/stress_biaxial_with_shear)**2)

        bi_axial_stress = {'bi_axial_loading_shear_condition':bi_axial_loading}
        return bi_axial_stress

    def DNV_RP_C201_usage_factor(self,cfg,FEA_stress,stress_longtudinal,stress_transverse,stress_shear,stress_bi_axial):

        longtudinal = FEA_stress['longtudinal_stress']/stress_longtudinal['design_resistance']

        transverse = FEA_stress['longtudinal_stress']/stress_transverse['design_resistance_transverse']

        shear = FEA_stress['shear_stress']/stress_shear['shear_resistance']

        bi_axial_with_shear = math.sqrt(stress_bi_axial['bi_axial_loading_shear_condition'])

        dnv_rp_usage_factor = {'usage_longtudinal':round(longtudinal,4),'usage_transverse':round(transverse,3),
                               'usage_shear':round(shear,3),'usage_bi_axial_with_shear':round(bi_axial_with_shear,3)
                               }
        return dnv_rp_usage_factor






    







        
