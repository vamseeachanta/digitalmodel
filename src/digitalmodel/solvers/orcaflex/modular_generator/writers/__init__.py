"""Writers that emit OrcaFlex model files from generated data.

``basefile`` emits a variation model composed with ``BaseFile:``, which clears
all existing model data and then loads the named file.  The flat
``- includefile:`` list emitted by :mod:`modular_generator` is the other
composition form, and it merges incrementally instead.
"""

from .basefile import build_variation_document, write_variation_model

__all__ = ["build_variation_document", "write_variation_model"]
