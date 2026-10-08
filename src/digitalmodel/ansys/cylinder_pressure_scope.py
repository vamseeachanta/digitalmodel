"""Exact pressure continuation scopes; no execution or inferred authorization."""
from digitalmodel.ansys.analysis_records import canonical_bytes

COARSE_SCOPE = dict(case_ids=['ocv-t60-p10-n4'], ordinal=2, max_attempts=1,
    qualification='diagnostic_only', capture_only=True)
INTERMEDIATE_SCOPE = dict(case_ids=['ocv-t60-p10-n8'], ordinal=3, max_attempts=1,
    qualification='diagnostic_only', capture_only=True)


def pressure_step(config):
    """Refuse missing, extra, changed or incorrectly typed scope fields."""
    if not isinstance(config, dict) or not isinstance(config.get('scope'), dict):
        raise ValueError('Explicit pressure scope required')
    scope = config['scope']
    for expected in (COARSE_SCOPE, INTERMEDIATE_SCOPE):
        if canonical_bytes(scope) == canonical_bytes(expected):
            return expected['ordinal'], expected['case_ids'][0]
    raise ValueError('Exact typed coarse or intermediate pressure scope required')
