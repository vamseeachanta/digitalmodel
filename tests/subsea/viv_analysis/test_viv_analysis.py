"""Real offline tubular routing input with numerical output readback."""
import json
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.subsea.viv_analysis.viv_analysis import VIVAnalysis


def test_run_process(tmp_path):
    root = Path(__file__).resolve().parents[3]
    cfg = yaml.safe_load((root / 'examples/workflows/viv-analysis/input.yml').read_text())
    cfg['Analysis'].update(result_folder=str(tmp_path), file_name='offline_viv')
    result = VIVAnalysis().router(cfg)
    natural = pd.read_csv(tmp_path / 'offline_viv_natural_frequencies.csv')
    shedding = pd.read_csv(tmp_path / 'offline_viv_vs_frequencies.csv')
    safety = pd.read_csv(tmp_path / 'offline_viv_safety_factors.csv')
    # C205 (2010) added mass and excitation; derivation in test_legacy_audit.py.
    assert natural.loc[0, 'mode 1'] == pytest.approx(1.9290322268, abs=1e-9)
    assert shedding.loc[0, 'shredding_frequency_inline'] == pytest.approx(1.1294117647)
    assert safety.loc[0, 'safety_factor_inline'] == pytest.approx(1.7079972842)
    assert json.loads(result['viv']['natural_frequencies'])['mode 1']['0'] == pytest.approx(
        1.9290322268, abs=1e-9,
    )
