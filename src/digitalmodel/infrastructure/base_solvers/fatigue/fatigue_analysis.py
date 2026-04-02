# Third party imports
import pandas as pd
from assetutilities.common.yml_utilities import WorkingWithYAML

# Reader imports
from digitalmodel.signal_processing.time_series.time_series_analysis import TimeSeriesAnalysis

wwy = WorkingWithYAML()
tsa = TimeSeriesAnalysis()


class FatigueAnalysis:
    """Fatigue damage analysis using S-N curves and rainflow cycle counting.

    Supports damage calculation from direct S-N data inputs and from
    time-domain stress traces via rainflow counting.
    """

    def __init__(self):
        """Initialize FatigueAnalysis."""
        pass

    def router(self, cfg):
        """Route fatigue analysis to the appropriate calculation method.

        Args:
            cfg: Configuration dictionary with 'inputs' containing
                'calculation_type' and 'stress_input' keys.

        Returns:
            dict: Updated configuration with fatigue analysis results.

        Raises:
            NotImplementedError: If 'orcaflex' or 'abaqus' software is specified.
            Exception: If stress_input is not 'sn' or 'timetrace'.
        """
        if "software" in cfg["inputs"]:
            if cfg["inputs"]["software"] == "orcaflex":
                raise NotImplementedError
            elif cfg["inputs"]["software"] == "abaqus":
                raise NotImplementedError
        if cfg["inputs"]["calculation_type"] == "damage":
            if cfg["inputs"]["stress_input"] == "timetrace":
                cfg = self.damage_from_timetrace(cfg)
            elif cfg["inputs"]["stress_input"] == "sn":
                cfg = self.damage_from_sn_data(cfg)
            else:
                raise Exception("Only SN and timetrace data inputs are supported")

        return cfg

    def damage_from_sn_data(self, cfg):
        """Calculate cumulative fatigue damage from S-N data pairs.

        Args:
            cfg: Configuration dictionary with 'inputs.SN' list of
                stress-cycle pairs and fatigue curve specification.

        Returns:
            dict: Updated configuration with 'fatigue_analysis.damage' result.
        """
        fatigue_curve = self.get_fatigue_curve(cfg)
        damage = 0
        for sn in cfg["inputs"]["SN"]:
            s = sn["s"]
            n = sn["n_cycles"]
            N = self.get_cycles_to_failure(fatigue_curve, s)
            damage += n / N

        cfg.update({"fatigue_analysis": {"damage": damage}})

        return cfg

    def damage_from_rainflow_cycles(self, rainflow_df, fatigue_curve):
        """Calculate cumulative damage from rainflow-counted stress cycles.

        Args:
            rainflow_df: DataFrame with 'range' and 'count' columns from
                rainflow cycle counting.
            fatigue_curve: Dictionary with 'a1' and 'm1' S-N curve parameters.

        Returns:
            float: Cumulative fatigue damage (Miner's rule summation).
        """
        damage = 0
        for index, row in rainflow_df.iterrows():
            s = row["range"]
            n = row["count"]
            N = self.get_cycles_to_failure(fatigue_curve, s)

            damage += n / N

        return damage

    def damage_from_timetrace(self, cfg):
        """Calculate fatigue damage from stress time traces.

        Performs rainflow counting on each timetrace and accumulates damage.

        Args:
            cfg: Configuration dictionary with 'inputs.timetraces' list.

        Returns:
            dict: Updated configuration with 'fatigue_analysis' results.
        """
        fatigue_curve = self.get_fatigue_curve(cfg)
        damage = 0
        cfg_timetraces = cfg["inputs"]["timetraces"].copy()
        for timetrace in cfg_timetraces:
            s_trace = timetrace["s_trace"]
            n_traces = timetrace["n_traces"]
            rainflow_df, rainflow_dict = self.get_rainflow_from_timetrace(s_trace)
            timetrace.update({"rainflow": rainflow_dict})

            damage_timetrace = self.damage_from_rainflow_cycles(
                rainflow_df, fatigue_curve
            )
            damage_n_timetraces = damage_timetrace * n_traces
            timetrace.update(
                {
                    "damage": {
                        "timetrace": float(damage_timetrace),
                        "n_timetraces": float(damage_n_timetraces),
                    }
                }
            )

            damage += damage_n_timetraces

        cfg.update(
            {
                "fatigue_analysis": {
                    "total_damage": float(damage),
                    "timetraces": cfg_timetraces.copy(),
                }
            }
        )

        return cfg

    def get_cycles_to_failure(self, fatigue_curve, s):
        """Calculate cycles to failure for a given stress range using S-N curve.

        Args:
            fatigue_curve: Dictionary with 'a1' and 'm1' S-N curve parameters.
            s: Stress range value.

        Returns:
            float: Number of cycles to failure (N).
        """
        N = fatigue_curve["a1"] * s ** fatigue_curve["m1"]
        return N

    def get_fatigue_curve(self, cfg):
        """Retrieve the specified fatigue curve from the fatigue data.

        Args:
            cfg: Configuration with 'inputs.fatigue_curve' label.

        Returns:
            dict: Fatigue curve parameters as a dictionary.
        """
        fatigue_curve_data = self.get_fatigue_curve_data(cfg)
        fatigue_curve_df = fatigue_curve_data[
            fatigue_curve_data["Curve Label"] == cfg["inputs"]["fatigue_curve"]
        ]
        fatigue_curve = fatigue_curve_df.to_dict("records")[0]

        return fatigue_curve

    def get_fatigue_curve_data(self, cfg):
        """Load fatigue curve data from a CSV file.

        Args:
            cfg: Configuration with 'fatigue_data.io' file path.

        Returns:
            pd.DataFrame: DataFrame containing fatigue curve data.
        """

        if cfg["fatigue_data"]["csv"]:
            fatigue_data_file = cfg["fatigue_data"]["io"]

        library_name = "digitalmodel"
        library_cfg = {"filename": fatigue_data_file, "library_name": library_name}

        fatigue_data_file = wwy.get_library_filename(library_cfg)
        fatigue_curve_data = pd.read_csv(fatigue_data_file)

        return fatigue_curve_data

    def get_default_sn_cfg(self):
        """Get default configuration for S-N based fatigue analysis.

        Returns:
            dict: Default configuration dictionary for S-N damage calculation.
        """
        default_cfg = {
            "basename": "fatigue_analysis",
            "inputs": {
                "calculation_type": "damage",
                "stress_input": "sn",
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
        """Get default configuration for timetrace-based fatigue analysis.

        Returns:
            dict: Default configuration dictionary for timetrace damage calculation.
        """
        default_cfg = {
            "basename": "fatigue_analysis",
            "inputs": {
                "calculation_type": "damage",
                "stress_input": "timetrace",
                "timetraces": [
                    {
                        "s_trace": [
                            -7565518.75,
                            23879775.0,
                            -7565518.75,
                            23879775.0,
                            -7565518.75,
                            23879775.0,
                        ],
                        "n_traces": 1,
                        "thickness": 15,
                    }
                ],
                "fatigue_curve": "DnV 2005 C2 Seawater CP",
            },
            "fatigue_data": {
                "csv": True,  # True
                "io": "tests/test_data/fatigue_analysis/fatigue_data.csv",
            },
        }

        return default_cfg

    def get_rainflow_from_timetrace(self, timetrace):
        """Perform rainflow cycle counting on a stress timetrace.

        Args:
            timetrace: List of stress values forming the time trace.

        Returns:
            tuple: A pair of (rainflow_df, rainflow_dict) containing the
                cycle counting results as DataFrame and dictionary.
        """
        rainflow_df, rainflow_dict = tsa.count_cycles(timetrace)

        return rainflow_df, rainflow_dict
