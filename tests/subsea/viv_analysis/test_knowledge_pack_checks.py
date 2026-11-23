"""Knowledge-pack gates must bind a package calculation to an asserted result."""
from pathlib import Path


def test_gates_name_package_outputs():
    pack = Path(__file__).resolve().parents[3] / 'src/digitalmodel/subsea/viv_analysis/docs/agent-pack.md'
    text = pack.read_text()
    expected = {
        'C01': 'get_material',
        'C02': 'cli.natural_frequency',
        'C03': 'VIVScreening.screen_member',
        'C04': 'VIVAnalysis.router',
        'C05': 'FrequencyCalculator.calculate_natural_frequency',
        'C06': 'VIVFatigueCalculator.calculate_allowable_cycles',
        'C07': 'VIVFatigueCalculator.calculate_damage',
        'C08': 'VIVFatigueCalculator.calculate_damage',
        'C09': 'VIVFatigueResult.citations',
        'C10': 'VIVScreening.screen_member',
    }
    for gate, symbol in expected.items():
        row = next(line for line in text.splitlines() if line.startswith(f'| {gate} |'))
        assert symbol in row
        assert 'assert' in row.lower()
    assert 'A4 — identity checks' in text
    assert '1.9290322268' in text
    assert '1.46e6' in text
