# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Read the four benchmark decks with strict pyNastran cross-referencing."""

from pathlib import Path

import pyNastran
from pyNastran.bdf.bdf import BDF

SUPPORTED = {"GRID", "CQUAD4", "PSHELL", "MAT1", "SPC1", "SPC", "FORCE", "ENDDATA"}


def main() -> None:
    """Print FE0 counts, raising on parsing, validation or reference errors."""
    print(f"pyNastran {pyNastran.__version__}")
    for deck in sorted((Path(__file__).resolve().parents[1] / "inputs").glob("*.bdf")):
        model = BDF(debug=False)
        model.set_error_storage(stop_on_parsing_error=True, stop_on_xref_error=True)
        model.read_bdf(deck, xref=True, validate=True)
        model.pop_parse_errors()
        model.pop_xref_errors()
        unsupported = set(model.card_count) - SUPPORTED
        rejected = len(model.reject_cards) + len(model.reject_lines)
        unresolved = len(model._stored_xref_errors)
        assert not unsupported and not rejected and not unresolved
        print(f"{deck.name}: read_errors=0 unresolved_references={unresolved} "
              f"unsupported_cards={len(unsupported)} rejected_cards={rejected} "
              f"grids={len(model.nodes)} elements={len(model.elements)} PASS")


if __name__ == "__main__":
    main()
