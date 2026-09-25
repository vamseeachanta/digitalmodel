"""Quarantine gate for cathodic-protection models that are not yet physical.

Some functions in this package were written as first-pass screening models
whose arithmetic does not follow a recognised standard (issue #2209). Rather
than delete them, they are gated: calling one without ``experimental=True``
raises :class:`ExperimentalModelError` naming the model, the defect, and the
standard a re-model must follow. The functions stay importable from their
own modules so existing scripts fail loudly instead of silently.
"""

from __future__ import annotations


class ExperimentalModelError(RuntimeError):
    """Raised when a quarantined model is called without ``experimental=True``."""

    def __init__(self, model: str, reason: str, standard: str) -> None:
        self.model = model
        self.reason = reason
        self.standard = standard
        super().__init__(
            f"{model} is quarantined as experimental: {reason}. "
            f"A re-model must follow {standard}. "
            "Pass experimental=True to run it anyway (results are not for design use)."
        )


def require_experimental(flag: bool, model: str, reason: str, standard: str) -> None:
    """Raise :class:`ExperimentalModelError` unless ``flag`` is true.

    Parameters
    ----------
    flag : bool
        The caller's ``experimental`` keyword.
    model : str
        Dotted name of the quarantined function, e.g. ``"stray_current.assess_stray_current"``.
    reason : str
        One-sentence statement of why the model is not physical.
    standard : str
        Standard (and clause) a replacement must follow, e.g. ``"EN 50162 Table 1"``.
    """
    if not flag:
        raise ExperimentalModelError(model, reason, standard)
