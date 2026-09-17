"""Actual descriptor identity, without solver or licence access."""
import importlib
import os
from contextlib import ExitStack

import pytest

from . import test_cylinder_preparation_history as fixtures

history = fixtures.history


def api():
    return importlib.import_module('digitalmodel.ansys.cylinder_preparation_streams')


@pytest.mark.parametrize('fault', ['none', 'replica', 'swapped', 'closed', 'pipe', 'hardlink'])
def test_actual_stream_identity(history, tmp_path, fault):
    module = api()
    paths = module.expected_stream_paths(history)
    with ExitStack() as stack:
        streams = {key: stack.enter_context(open(path, 'xb')) for key, path in paths.items()}
        if fault == 'replica':
            streams['stdout'] = stack.enter_context((tmp_path/'replica').open('xb'))
        elif fault == 'swapped':
            streams['stdout'], streams['stderr'] = streams['stderr'], streams['stdout']
        elif fault == 'closed':
            streams['stdout'].close()
        elif fault == 'pipe':
            read_fd, write_fd = os.pipe()
            stack.callback(os.close, read_fd)
            streams['stdout'] = stack.enter_context(os.fdopen(write_fd, 'wb'))
        elif fault == 'hardlink':
            os.link(paths['stdout'], tmp_path/'alias')
        if fault == 'none':
            assert module.verify_streams(history, **streams) == paths
        else:
            with pytest.raises(ValueError):
                module.verify_streams(history, **streams)


def test_streams_missing_refuse(history):
    with pytest.raises(ValueError):
        api().verify_streams(history)


def test_stream_paths_reject_foreign_campaign(history):
    history['operational']['output_directory'] += '-foreign'
    with pytest.raises(ValueError):
        api().expected_stream_paths(history)
