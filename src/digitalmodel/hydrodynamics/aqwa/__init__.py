"""Backward compatibility wrapper for aqwa module."""

from digitalmodel.aqwa.aqwa_router import (
    Aqwa,
    a_post,
    a_pre,
    mes_files,
    update_deep_dictionary,
)

__all__ = [
    "Aqwa",
    "a_pre",
    "a_post",
    "mes_files",
    "update_deep_dictionary",
]
