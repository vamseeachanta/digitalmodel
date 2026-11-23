"""Verify live driver descriptors against lineage-bound retained stream files."""
import os
from pathlib import Path
import re
import stat
import sys

from .cylinder_pressure_resources import _checked_path
from .cylinder_pressure_scope import pressure_step


def expected_stream_paths(config):
    """Expected destinations are names, not evidence of an actual open descriptor."""
    if pressure_step(config)[0] != 3:
        return {}
    output = Path(config['operational']['output_directory'])
    original = Path(config['lineage']['original_root'])
    match = re.fullmatch(r'ansys-2121-pressure-intermediate-(\d{8}-r[1-9]\d*)', output.name)
    if (not match or not output.is_absolute() or not original.is_absolute()
            or output.parent != original.parent):
        raise ValueError('Intermediate stream campaign namespace differs')
    directory = _checked_path(original.parent.parent/'_coordination', True)
    stem = 'SOLVERS-intermediate-driver-' + match.group(1)
    return {name: str(directory/(stem+suffix)) for name, suffix in
            [('stdout', '.stdout.json'), ('stderr', '.stderr.txt')]}


def _same_file(stream, path):
    try:
        opened = os.fstat(stream.fileno())
        named = _checked_path(path, False).stat()
        if (not stat.S_ISREG(opened.st_mode) or not opened.st_ino
                or opened.st_nlink != 1 or named.st_nlink != 1
                or not os.path.samestat(opened, named)):
            raise ValueError('Driver descriptor is not its retained regular file')
    except (OSError, ValueError, AttributeError) as exc:
        raise ValueError('Original driver stream identity unavailable or different') from exc


def verify_streams(config, *, stdout=None, stderr=None):
    """Observe actual descriptors; pipes, consoles, aliases and replicas refuse."""
    paths = expected_stream_paths(config)
    if paths:
        _same_file(sys.stdout if stdout is None else stdout, paths['stdout'])
        _same_file(sys.stderr if stderr is None else stderr, paths['stderr'])
    return paths
