"""Validate staged ship structure research without computing engineering results.

The schema follows JSON Schema 2020-12 Validation (2022), sections 6 and 7:
https://json-schema.org/draft/2020-12/json-schema-validation
Relationship checks implement the authored contract in the reference README.
No source downloads, dataset publication or buckling computation is performed.
"""

import argparse
import json
from pathlib import Path
from urllib.parse import urlsplit

from jsonschema import Draft202012Validator, FormatChecker, ValidationError

SCHEMA_PATH = (
    Path(__file__).resolve().parents[2]
    / 'data/ship_structure_reference/research.schema.json'
)


def validate_research(data):
    """Reject invalid provenance, units, nonfinite values and source relationships.

    Valid staging metadata establishes neither source rights nor engineering
    readiness. Inputs are not mutated; missing evidence is never inferred.
    """
    # RFC 8259 (2017), section 6: NaN and Infinity are not JSON numbers.
    # Round-trip also rejects non-JSON Python objects before schema validation.
    data = json.loads(json.dumps(data, allow_nan=False))
    schema = json.loads(SCHEMA_PATH.read_text(encoding='utf-8'))
    Draft202012Validator(schema, format_checker=FormatChecker()).validate(data)
    for source in data['sources']:
        pointer = urlsplit(source['url'])
        if not pointer.hostname:
            raise ValueError('Source URL requires a hostname')
        # Access validates port syntax and range without resolving the hostname.
        _ = pointer.port
    for collection, key in (('sources', 'key'), ('products', 'id'), ('sections', 'id')):
        identifiers = [row[key] for row in data[collection]]
        if len(identifiers) != len(set(identifiers)):
            raise ValueError(f'Duplicate identifiers in {collection}')
    source_keys = {row['key'] for row in data['sources']}
    for collection in ('products', 'sections'):
        if any(row['source_key'] not in source_keys for row in data[collection]):
            raise ValueError(f'Unknown source in {collection}')


def read_research(path):
    """Read local JSON while rejecting duplicate object keys."""
    def unique_object(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError('Duplicate JSON object key')
            result[key] = value
        return result

    return json.loads(Path(path).read_text(encoding='utf-8'), object_pairs_hook=unique_object)


def main(argv=None):
    """Validate one staging record; return a nonzero exit code on invalid input."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', type=Path, help='Local research JSON, schema version 2')
    args = parser.parse_args(argv)
    try:
        validate_research(read_research(args.input))
    except (OSError, UnicodeError, ValueError, TypeError, ValidationError):
        # Do not echo source records, local paths or private metadata in diagnostics.
        print(json.dumps({'status': 'invalid_metadata', 'engineering_status': 'NOT_EVALUATED'}))
        return 2
    print(json.dumps({'status': 'metadata_valid', 'engineering_status': 'NOT_EVALUATED',
                      'publication_allowed': False}))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
