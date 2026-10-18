"""Hand-calculated imperial tubular regressions; no solver or engine import."""
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.subsea.viv_analysis.viv_analysis import VIVAnalysis
from digitalmodel.subsea.viv_analysis.viv_tubular_members import VIVTubularMembers


@pytest.fixture
def legacy_cfg():
    root = Path(__file__).resolve().parents[3]
    cfg = yaml.safe_load((root / 'examples/workflows/viv-analysis/input.yml').read_text())
    return VIVAnalysis().get_pipe_properties(cfg)


def test_legacy_wet_frequencies_and_excitation(legacy_cfg):
    # C205 (2010) §9.1.11 / App. D: m_a=Ca*rho*pi*D²/4.
    # E=30e6 psi, I=361.5439319 in^4, L=720 in,
    # weight=5.589872732 lb/in (steel+coating), added=4.750834711 lb/in.
    # Euler-Bernoulli pinned beam f=pi/(2 L²)*sqrt(E I g / (w+w_a)).
    route = VIVTubularMembers()
    natural = route.get_natural_frequencies(legacy_cfg)
    shedding = route.get_vs_frequencies(legacy_cfg)
    safety = route.get_safety_factors(legacy_cfg, natural, shedding)
    assert natural.loc[0, 'mode 1'] == pytest.approx(1.9290322268, abs=1e-9)
    assert natural.loc[1, 'mode 1'] == pytest.approx(1.7375267386, abs=1e-9)
    # C205 (2010) §9.1.3 and §9.3.3.2: fs=.2*36/12.75, fIL=2fs.
    assert shedding.loc[0, 'shredding_frequency_crossflow'] == pytest.approx(.56470588235)
    assert shedding.loc[0, 'shredding_frequency_inline'] == pytest.approx(1.12941176471)
    assert shedding.loc[0, 'Re'] == pytest.approx(219354.4)
    # KC=U*T/D needs an imposed oscillatory period; steady current has none.
    assert pd.isna(shedding.loc[0, 'KC'])
    assert safety.loc[0, 'safety_factor_inline'] == pytest.approx(1.70799728415)
    assert safety.loc[0, 'safety_factor_crossflow'] == pytest.approx(3.41599456830)


def test_legacy_speed_units(legacy_cfg):
    legacy_cfg['viv']['current_speed_unit'] = 'm/s'
    legacy_cfg['environment']['current'][0]['data'][0]['speed'] = .9144
    result = VIVTubularMembers().get_vs_frequencies(legacy_cfg)
    assert result.loc[0, 'shredding_frequency_crossflow'] == pytest.approx(.56470588235)
    assert result.loc[0, 'Re'] == pytest.approx(219354.4)
    legacy_cfg['viv']['current_speed_unit'] = 'knots'
    with pytest.raises(ValueError, match='unit'):
        VIVTubularMembers().get_vs_frequencies(legacy_cfg)


def test_legacy_added_mass_coefficient(legacy_cfg):
    legacy_cfg['viv']['added_mass_coefficient'] = 0.
    dry = VIVTubularMembers().get_natural_frequencies(legacy_cfg)
    assert dry.loc[0, 'mode 1'] == pytest.approx(2.6236968182, rel=1e-8)
    legacy_cfg['viv']['added_mass_coefficient'] = -1.
    with pytest.raises(ValueError, match='added.mass'):
        VIVTubularMembers().get_natural_frequencies(legacy_cfg)
