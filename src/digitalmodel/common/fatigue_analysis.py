import os
import pandas as pd
from assetutilities.common.utilities import add_cwd_to_filename

class FatigueAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg["inputs"]["software"] == "orcaflex":
            raise NotImplementedError
        elif cfg["inputs"]["software"] == "abaqus":
            raise NotImplementedError
        elif cfg["inputs"]["software"] is None:
            if cfg["inputs"]["calculation_type"] == "damage":
                cfg = self.damage_from_sn_data(cfg)
        return cfg

    def damage_from_sn_data(self, cfg):
        fatigue_curve = self.get_fatigue_curve(cfg)
        damage = 0
        for sn in cfg['inputs']['SN']:
            damage += self.damage_from_stress_range_single(sn, fatigue_curve)
            
        cfg = cfg.update({'fatigue_analysis': {'damage': damage}})
        
        return cfg
    
    def damage_from_stress_range_single(self, sn, fatigue_curve):
        s = sn['S']/1e+6
        n = sn['N']
        N = self.get_cycles_to_failure(fatigue_curve, s)

        damage = n/N
        
        return damage

    def get_cycles_to_failure(self, fatigue_curve, s):
        N = fatigue_curve['a1']*s**fatigue_curve['m1']
        return N

    def get_fatigue_curve(self, cfg):
        fatigue_curve_data = self.get_fatigue_curve_data(cfg)
        fatigue_curve_df = fatigue_curve_data[fatigue_curve_data['Curve Label'] == cfg['inputs']['fatigue_curve']]
        fatigue_curve = fatigue_curve_df.to_dict('records')[0]
        
        return fatigue_curve

    def get_fatigue_curve_data(self, cfg):
        if cfg["fatigue_data"]['csv']:
            fatigue_data_file = cfg["fatigue_data"]['io']
        cwd = os.getcwd()
        fatigue_data_file = add_cwd_to_filename(fatigue_data_file, cwd)
        fatigue_curve_data = pd.read_csv(fatigue_data_file)
        
        return fatigue_curve_data