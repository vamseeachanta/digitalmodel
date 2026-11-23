# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Invented interpolation data: these are not values from a standard."""

import json


def make_synthetic_tables(root):
    wiki = root / 'wikis/synthetic/e305.md'
    wiki.parent.mkdir(parents=True, exist_ok=True)
    wiki.write_text('---\ncode_id: dnv-rp-e305\npublisher: DNV\nrevision: 1988\n---\nSynthetic metadata for tests only.\n')
    citation = dict(code_id='dnv-rp-e305', publisher='DNV', revision='1988',
                    section='Synthetic interpolation example',
                    wiki_path='wikis/synthetic/e305.md', source_sibling='generic',
                    note='Synthetic data; no licensed table values')
    tables = {}
    for name in ('roughness', 'R', 'clay_friction'):
        axis = 'grain_size_m' if name == 'roughness' else 'synthetic_coordinate'
        tables[name] = dict(axes=[dict(name=axis, units='m' if name == 'roughness' else '1', values=[0.0, 2.0])],
                            units='m' if name == 'roughness' else '1', values=[2.0, 6.0], citation=citation.copy())
    tables['Fw'] = dict(axes=[dict(name=axis, units='1', values=[0.0, 2.0]) for axis in ('K', 'M')],
                        units='1', values=[[2.0, 6.0], [6.0, 10.0]], citation=citation.copy())
    payload = dict(schema_version=1, tables=tables)
    path = root / 'synthetic_e305_tables.json'
    path.write_text(json.dumps(payload, allow_nan=False))
    return path, payload, wiki
