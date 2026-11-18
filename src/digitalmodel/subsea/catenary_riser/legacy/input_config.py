"""Argument selection for archived YAML entry points; no implicit error fallback."""

from pathlib import PureWindowsPath


def configuration_inputs(arguments, default, prefix):
    """Return update path and filename stem; only an omitted argument uses default.

    A supplied argument must be one nonempty YAML path. Both path separators
    are accepted. File existence and YAML errors belong to the caller and
    propagate; they never cause selection of a different engineering input.
    """
    if len(arguments) > 1:
        raise ValueError("expected at most one YAML argument")
    if not arguments:
        return None, PureWindowsPath(default).stem
    value = arguments[0]
    if not isinstance(value, str) or not value.strip():
        raise ValueError("argument must be a nonempty YAML path")
    path = PureWindowsPath(value)
    if path.suffix.lower() not in {".yml", ".yaml"}:
        raise ValueError("argument must name a YAML file")
    return (PureWindowsPath(prefix) / path).as_posix(), path.stem
