"""Backward compatibility shim. Use digitalmodel.modules.data_scraping instead."""
import warnings

warnings.warn(
    "digitalmodel.modules.data_procurement is deprecated. "
    "Use digitalmodel.modules.data_scraping instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.modules.data_scraping import *  # noqa: F401, F403, E402
