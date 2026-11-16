"""Rendered worked-card figure retains physical axes and hypothesis labels."""
import importlib.util
from pathlib import Path
import xml.etree.ElementTree as ET

import matplotlib.image as mpimg
import numpy as np
from matplotlib.figure import Figure

HERE = Path(__file__).resolve().parents[2] / 'examples/report-evals/dynacard-pump-card'


def test_figure_exports_physical_cards_and_ten_reference_shapes(tmp_path, monkeypatch):
    raw_svg = []
    savefig = Figure.savefig
    def capture_svg(figure, path, *args, **kwargs):
        savefig(figure, path, *args, **kwargs)
        if Path(path).suffix == '.svg':
            raw_svg.append(Path(path).read_text(encoding='utf-8'))
    monkeypatch.setattr(Figure, 'savefig', capture_svg)
    spec = importlib.util.spec_from_file_location('dynacard_plotting', HERE / 'plotting.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    theta = np.linspace(0, 2 * np.pi, 100)
    card = {'position_in': (27 - 27 * np.cos(theta)).tolist(),
            'load_lbf': (1000 + 2200 * np.sin(theta)).tolist()}
    labels = ['Normal operation', 'Insufficient supply', 'Gas interference',
              'Rod parting', 'Tagging up', 'Tagging down', 'Traveling valve leakage',
              'Standing valve leakage', 'Plunger out of barrel', 'Sand production']
    data = {'condition_name': 'public example well',
            'cards': {'surface': dict(card, load_lbf=(np.array(card['load_lbf']) + 7000).tolist()),
                      'default': card, 'gibbs': card, 'reference': card},
            'key_results': {'surface_stroke_in': 54, 'pump_stroke_in': 54,
                            'fluid_load_lbf': 4000, 'fillage_pct': 90,
                            'effective_travel_in': 48.6},
            'fluid_load_levels': {'upstroke_lbf': 5000, 'downstroke_lbf': 1000},
            'plate_shapes': [{'label': label, 'position': [0, 0, 1, 1],
                              'load': [0, 1, 1, 0],
                              'crosswalk_status': 'ASSUMED' if i in (1, 9) else 'LITERAL'}
                             for i, label in enumerate(labels)],
            'shape_hypothesis': {'nearest_three': [{'label': label, 'rank': i + 1}
                                                  for i, label in enumerate(labels[:3])]}}
    target = tmp_path / 'figure.png'
    module.render_figure(data, target)
    assert target.stat().st_size > 40000
    pixels = mpimg.imread(target)
    assert pixels.shape[:2] == (1300, 2400)
    assert np.count_nonzero(pixels[:, :, :3] < .9) > 10000
    svg = target.with_suffix('.svg')
    assert svg.stat().st_size > 20000
    text = ' '.join(ET.parse(svg).getroot().itertext())
    for phrase in ('Position (in)', 'Load (lbf)', 'Public reference card shapes',
                   'nearest by shape distance', 'hypothesis', 'assumed inputs', 'fluid load', 'fillage'):
        assert phrase.lower() in text.lower()
    assert text.count('(label mapping assumed)') == 2
    assert '−' not in text  # The load axis is labelled from zero; no negative load is implied.
    assert all(line == line.rstrip() for line in svg.read_text().splitlines())
    def parsed(text):
        return [(node.tag, {key: ' '.join(value.split()) for key, value in node.attrib.items()},
                 ' '.join((node.text or '').split())) for node in ET.fromstring(text).iter()]
    assert parsed(svg.read_text()) == parsed(raw_svg[0])
