"""Simple RAO plot generation with minimal dependencies."""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

# Direct imports to avoid module init issues
import importlib.util

# Load parser module directly
parser_path = Path(__file__).parent.parent / 'src' / 'digitalmodel' / 'modules' / 'marine_analysis' / 'parsers' / 'aqwa_lis_parser.py'
spec = importlib.util.spec_from_file_location("aqwa_parser", parser_path)
parser_module = importlib.util.module_from_spec(spec)

# Mock the imports that the parser needs
import numpy as np
from dataclasses import dataclass
from typing import Dict, List, Optional

# Create minimal dataclasses
@dataclass
class DOFData:
    amplitude: np.ndarray
    phase: np.ndarray

@dataclass
class StructureRAO:
    structure_name: str
    location: str = ""
    displacement_raos: Optional[Dict] = None
    velocity_raos: Optional[Dict] = None
    acceleration_raos: Optional[Dict] = None

# Inject into parser module namespace
parser_module.DOFData = DOFData
parser_module.StructureRAO = StructureRAO
parser_module.np = np
parser_module.Dict = Dict
parser_module.List = List
parser_module.Optional = Optional

# Now execute the module
spec.loader.exec_module(parser_module)

# Get the parser class
AQWALISParser = parser_module.AQWALISParser

# Test
lis_file = Path('docs/modules/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS')

if __name__ == '__main__':
    import matplotlib
    matplotlib.use('Agg')  # Non-interactive backend
    import matplotlib.pyplot as plt

    # Parse data
    parser = AQWALISParser(str(lis_file))
    rao_data = parser.parse()

    if rao_data:
        struct = rao_data[0]
        if struct.displacement_raos:
            # Extract data
            freqs = sorted(struct.displacement_raos.keys())
            headings_set = set()
            for freq_data in struct.displacement_raos.values():
                headings_set.update(freq_data.keys())
            headings = sorted(list(headings_set))

            periods = [2 * np.pi / f for f in freqs]

            # Create amplitude matrix for heave (example)
            amp_matrix = np.zeros((len(freqs), len(headings)))
            for i, freq in enumerate(freqs):
                for j, heading in enumerate(headings):
                    if freq in struct.displacement_raos and heading in struct.displacement_raos[freq]:
                        dof_data = struct.displacement_raos[freq][heading]
                        if 'heave' in dof_data:
                            amp_matrix[i, j] = dof_data['heave']['amplitude']

            # Simple plot
            fig, ax = plt.subplots(figsize=(12, 8))
            for j, heading in enumerate(headings):
                ax.plot(periods, amp_matrix[:, j], 'o-', label=f'{heading:.0f}°', markersize=5)

            ax.set_xlabel('Wave Period (s)', fontsize=12)
            ax.set_ylabel('Heave Amplitude (m/m)', fontsize=12)
            ax.set_title(f'Heave RAO - Fixed Parser Test\n{len(headings)} headings × {len(freqs)} periods', fontsize=14, fontweight='bold')
            ax.legend(loc='best', ncol=2)
            ax.grid(True, alpha=0.3)
            ax.invert_xaxis()

            output_dir = Path('tests/marine_engineering/plots/rao_qa')
            output_dir.mkdir(parents=True, exist_ok=True)
            plt.savefig(output_dir / 'rao_heave_test.png', dpi=300, bbox_inches='tight')

            # Summary
            print(f'✓ Parsed successfully')
            print(f'  Frequencies: {len(freqs)}')
            print(f'  Headings: {len(headings)}')
            print(f'  Heading values: {headings}')
            print(f'  Total data points: {len(freqs) * len(headings)}')
            print(f'\n✓ Plot saved: {output_dir / "rao_heave_test.png"}')
