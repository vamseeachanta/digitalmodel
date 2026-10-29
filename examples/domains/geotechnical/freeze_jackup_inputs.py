# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Freeze synthetic particulars and selected public facts; retain originals externally.

Usage: python freeze_jackup_inputs.py NDBC_GZIP RESEARCH_DIRECTORY OUTPUT_DIRECTORY
Particulars are explicit synthetic assumptions, not collection statistics.
This script never retrieves data or copies names, source tables or source prose.
"""

import gzip
import hashlib
import json
import sys
from datetime import datetime
from pathlib import Path

from digitalmodel.marine_ops.jackup_inputs import observation_summary

HOSSAIN = 'https://api.research-repository.uwa.edu.au/ws/portalfiles/portal/45701479/hossaintxt.pdf'
HU = 'https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf'
NDBC = 'https://www.ndbc.noaa.gov/data/historical/stdmet/42019h2024.txt.gz'


def digest(path):
    """Return the SHA-256 of exact input bytes."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def synthetic_unit():
    """Return explicit synthetic inputs without unsupported collection claims."""
    def assumed(value, unit, basis):
        return {'value': value, 'unit': unit, 'status': 'illustrative assumption',
                'basis': 'Assumed, based on previous experience; ' + basis}

    return {
        'leg_count': 3,
        'hull_length_m': assumed(72.0, 'm', 'generic hull dimension; not used in load transfer.'),
        'hull_breadth_m': assumed(66.0, 'm', 'generic hull dimension; not used in load transfer.'),
        'hull_depth_m': assumed(8.0, 'm', 'generic hull dimension; not used in load transfer.'),
        'leg_length_m': assumed(150.0, 'm', 'generic leg length; not a water-depth rating.'),
        'variable_load_t': assumed(3470.0, 't',
            'synthetic mass retained for numerical continuity; no collection median or equipment rating is claimed.'),
        'leg_spacing_m': assumed([40.0, 44.0], 'm',
            'synthetic isosceles support triangle chosen to exercise biaxial equilibrium.'),
        'lightship_t': assumed(8000.0, 't',
            'synthetic mass chosen to exercise three-support equilibrium.'),
        'preload_per_leg_kn': assumed(45000.0, 'kN',
            'synthetic load comparator; not an equipment rating.'),
        'spudcan_area_m2': assumed(160.0, 'm2',
            'synthetic circular footing area; equivalent diameter 14.3 m; actual geometry is unqualified.'),
    }


def freeze(ndbc_archive, research, output):
    """Build synthetic particulars, selected public facts and byte-integrity hashes."""
    from digitalmodel.data_systems.data_procurement.metocean.api_clients import NOAAClient

    # Replay the fetched public archive through the existing NOAA date-query parser.
    class ArchiveClient(NOAAClient):
        def stream_lines(self, endpoint, **kwargs):
            with gzip.open(ndbc_archive, 'rt', encoding='ascii') as stream:
                yield from stream
    with ArchiveClient() as noaa_client:
        observations = observation_summary(noaa_client.query_by_date(
            start_date=datetime(2024, 1, 1), end_date=datetime(2024, 12, 31, 23, 59),
            station_id='42019'))
    def assumption(value, unit, basis):
        return {'value': value, 'unit': unit, 'status': 'illustrative assumption', 'basis': basis}
    data = {
        'qualification': 'generic_screening_only', 'revision': 1, 'date': '2026-10-27',
        'unit': synthetic_unit(),
        'particulars_basis': 'Synthetic inputs assumed, based on previous experience; not recovered specifications or collection statistics.',
        'soil': {
            'source': HOSSAIN, 'locator': 'Table 1, Site 3; equation 1 interface definition',
            'scope': 'Published anonymized Gulf profile; not colocated with the buoy or bathymetry query.',
            'sand_thickness_m': 4.9, 'interface_su_kpa': 22.13, 'clay_gradient_kpa_m': 0.93,
            'sand_submerged_weight_kn_m3': 9.4, 'clay_submerged_weight_kn_m3': 7.3,
            'relative_density': 0.637, 'qualified_bottom_m': 52.1,
            'datum_note': 'Strength is evaluated relative to the interface, following equation 1; no datum is inferred from a collection column name.',
        },
        'method_assumptions': {
            'source': HU, 'critical_state_friction_deg': 31.0,
            'log_crushing_strength_kpa': 10.0, 'base_cone_angle_deg': 13.0,
            'basis': 'Illustrative assumptions for silica sand and a conical underside; actual mineralogy and geometry are unqualified.',
            'curve_connections': 'Illustrative linear mobilization and post-peak segments; only peak and clay branches follow research equations.',
        },
        'structural_sensitivity': {
            'force_coefficient_kn_m2': assumption(1000.0, 'kN/m2',
                'Synthetic F=C Hs squared sensitivity; no physical wave transfer is established.'),
            'application_height_m': assumption(50.0, 'm',
                'Synthetic force lever arm above the seabed; no water depth is inferred.'),
            'sliding_resistance_kn': assumption(10000.0, 'kN',
                'Independent synthetic resistance to exercise the sliding inequality; not derived from the soil.'),
        },
        'environment': {
            'noaa': {'source': NDBC, 'station': '42019', 'summary': observations,
                     'reuse': 'Selected US government observation facts only; no raw archive is copied.'},
            'era5': {'status': 'unavailable', 'reason': 'Existing client requires authenticated access; no authenticated request was made.',
                     'source': 'https://cds.climate.copernicus.eu/'},
            'gebco': {'status': 'unavailable', 'reason': 'Existing numerical query endpoint returned HTTP 404; a WMS map image is not numerical depth evidence.',
                      'source': 'https://www.gebco.net/data-products/gebco-web-services/web-map-service'},
            'return_period_years': None, 'water_depth_m': None,
            'qualification': 'No site-specific extremes or colocated environmental model is established.',
        },
        'reuse': 'Synthetic particulars and selected cited facts only; original tables, text and figures are excluded. Public access does not establish an unrestricted licence.',
    }
    output.mkdir(parents=True, exist_ok=True)
    raw = (json.dumps(data, indent=2, sort_keys=True, allow_nan=False) + '\n').encode()
    (output / 'inputs.json').write_bytes(raw)
    manifest = {
        'algorithm': 'sha256', 'files': {'inputs.json': hashlib.sha256(raw).hexdigest()},
        'external_source_digests': {
            'ndbc_archive': digest(ndbc_archive),
            'hu_2015_manuscript': digest(research / 'hu2015.pdf'),
            'hossain_2019_manuscript': digest(research / 'hossain2019.pdf')},
        'source_copy_policy': 'Originals remain external; digests refer to original bytes, not engineering qualification.',
    }
    (output / 'manifest.json').write_text(json.dumps(manifest, indent=2, sort_keys=True) + '\n', encoding='utf-8')


if __name__ == '__main__':
    freeze(*(Path(value) for value in sys.argv[1:]))
