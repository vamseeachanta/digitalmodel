import os
import pandas as pd
from assetutilities.common.yml_utilities import WorkingWithYAML
from digitalmodel.common.time_series_analysis import TimeSeriesAnalysis

wwy = WorkingWithYAML()
tsa = TimeSeriesAnalysis()


class FatigueAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):
        if "software" in cfg["inputs"]:
            if cfg["inputs"]["software"] == "orcaflex":
                raise NotImplementedError
            elif cfg["inputs"]["software"] == "abaqus":
                raise NotImplementedError
        if cfg["inputs"]["calculation_type"] == "damage":
            if cfg['inputs']['stress_input'] == 'timetrace':
                cfg = self.damage_from_timetrace(cfg) 
            elif cfg['inputs']['stress_input'] == 'sn':
                cfg = self.damage_from_sn_data(cfg) 
            else:
                raise Exception("Only SN and timetrace data inputs are supported")   

        return cfg

    def damage_from_sn_data(self, cfg):
        fatigue_curve = self.get_fatigue_curve(cfg)
        damage = 0
        for sn in cfg["inputs"]["SN"]:
            s = sn['s']
            n = sn['n_cycles']
            N = self.get_cycles_to_failure(fatigue_curve, s)
            damage += n/N

        cfg.update({"fatigue_analysis": {"damage": damage}})

        return cfg

    def damage_from_rainflow_cycles(self, rainflow_df, fatigue_curve):
        damage = 0
        for index, row in rainflow_df.iterrows():
            s = row["range"]
            n = row["count"]
            N = self.get_cycles_to_failure(fatigue_curve, s)

            damage += n / N

        return damage

    def damage_from_timetrace(self, cfg):
        fatigue_curve = self.get_fatigue_curve(cfg)
        damage = 0
        cfg_timetraces = cfg["inputs"]["timetraces"].copy()
        for timetrace in cfg_timetraces:
            s_trace = timetrace['s_trace']
            n_traces = timetrace['n_traces']
            rainflow_df, rainflow_dict = self.get_rainflow_from_timetrace(s_trace)
            timetrace.update({'rainflow': rainflow_dict})

            damage_timetrace = self.damage_from_rainflow_cycles(rainflow_df, fatigue_curve)
            damage_n_timetraces = damage_timetrace * n_traces
            timetrace.update({'damage': {'timetrace': float(damage_timetrace), 'n_timetraces': float(damage_n_timetraces)}})

            damage += damage_n_timetraces

        cfg.update({"fatigue_analysis": {"total_damage": float(damage), "timetraces": cfg_timetraces.copy()}})

        return cfg

    def get_cycles_to_failure(self, fatigue_curve, s):
        N = fatigue_curve["a1"] * s ** fatigue_curve["m1"]
        return N

    def get_fatigue_curve(self, cfg):
        fatigue_curve_data = self.get_fatigue_curve_data(cfg)
        fatigue_curve_df = fatigue_curve_data[
            fatigue_curve_data["Curve Label"] == cfg["inputs"]["fatigue_curve"]
        ]
        fatigue_curve = fatigue_curve_df.to_dict("records")[0]

        return fatigue_curve

    def get_fatigue_curve_data(self, cfg):

        if cfg["fatigue_data"]["csv"]:
            fatigue_data_file = cfg["fatigue_data"]["io"]

        library_name = 'digitalmodel'
        library_cfg = {
            'filename': fatigue_data_file,
            'library_name': library_name
        }

        fatigue_data_file = wwy.get_library_filename(library_cfg)
        fatigue_curve_data = pd.read_csv(fatigue_data_file)

        return fatigue_curve_data

    def get_default_sn_cfg(self):
        default_cfg = {
            "basename": "fatigue_analysis",
            "inputs": {
                "calculation_type": "damage",
                "stress_input": 'sn',
                "SN": [{"s": 2000000, "n_cycles": 1, "thickness": 15}],
                "fatigue_curve": "DnV 2005 C2 Seawater CP",
            },
            "fatigue_data": {
                "csv": True,  # True
                "io": "tests/test_data/fatigue_analysis/fatigue_data.csv",
            },
        }

        return default_cfg

    def get_default_timetrace_cfg(self):
        default_cfg = {
            "basename": "fatigue_analysis",
            "inputs": {
                "calculation_type": "damage",
                "stress_input": 'timetrace',
                "timetraces":[{
                        's_trace':
                        [
                            -7565518.75,
                            23879775.0,
                            -7565518.75,
                            23879775.0,
                            -7565518.75,
                            23879775.0,
                        ],
                        'n_traces': 1,
                        'thickness': 15,
                    }],
                "fatigue_curve": "DnV 2005 C2 Seawater CP",
            },
            "fatigue_data": {
                "csv": True,  # True
                "io": "tests/test_data/fatigue_analysis/fatigue_data.csv",
            },
        }

        return default_cfg

    def get_rainflow_from_timetrace(self, timetrace):
        rainflow_df, rainflow_dict = tsa.get_rainflow_count_from_time_series(timetrace)

        return rainflow_df, rainflow_dict