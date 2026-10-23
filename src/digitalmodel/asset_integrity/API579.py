"""API 579 fitness-for-service assessment entry point."""

from .common.API579_components import API579_components
from .common.validation import not_evaluated


def API579(cfg):
    from datetime import datetime
    t_start = datetime.now()

    checks = cfg['default']['Analysis']
    cfg['Result'] = {}
    statuses = {
        name: not_evaluated(
            'check disabled' if not enabled else 'check has not completed',
            cfg.get('input_revision'))
        for name, enabled in (('GML', checks['GML']['Circumference']),
                              ('LML', checks['LML']), ('B31G', checks['B31G']))
    }
    cfg['Result']['assessment_status'] = statuses
    api579_components = API579_components(cfg)

    if cfg['default']['Analysis']['GML']['Circumference']:
        cfg = api579_components.gml()
        statuses['GML'] = {
            'status': 'EVALUATED', 'reason': 'GML calculation completed',
            'criterion': 'local wall checks passed; pressure demand bracketed',
            'input_revision': cfg.get('input_revision', 'unspecified'),
        }

    if cfg['default']['Analysis']['LML']:
        cfg = api579_components.lml()

    if cfg['default']['Analysis']['B31G']:
        cfg = api579_components.b31g()
        statuses['B31G'] = {
            'status': 'EVALUATED', 'reason': 'B31G calculation completed',
            'criterion': 'calculation completed; disposition remains in its check outputs',
            'input_revision': cfg.get('input_revision', 'unspecified'),
        }

    # Per-check records survive legacy components replacing the Result mapping.
    cfg['Result']['assessment_status'] = statuses

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format(
        (t_end - t_start).seconds))

    return cfg
