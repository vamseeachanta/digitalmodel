# ABOUTME: Loader for the dynacard example-card library (synthetic + field archive).
# ABOUTME: Cards live in data/example_cards.json, assembled by a capabilities script.
"""Load the dynacard **example-card library**.

The library collects every identified example card from three sources:

* ``synthetic-verified`` — generator-library variants (several seeds and
  severity parameters per mode), each verified at build time to classify
  back to its own mode; oilfield units (inches / lbs).
* ``field-measured`` — surface cards parsed from dynamometer test files of
  real wells (anonymized), with SPM / stroke context; oilfield units.
* ``field-archive-digitized`` — pump-card shapes digitized from a
  hand-labeled card-image archive (phenomena include butterfly, incomplete
  fillage, fillage collapse, full card); normalized (unitless) shapes,
  anonymized wells, rendered as marker dots.

Assembled by ``scripts/capabilities/build_dynacard_example_library.py``.
"""

from __future__ import annotations

import json
from functools import lru_cache
from pathlib import Path
from typing import Any, Dict, List, Optional

from pydantic import BaseModel

from .models import CardData

_DATA_PATH = Path(__file__).parent / "data" / "example_cards.json"

VALID_SOURCES = {"synthetic-verified", "field-measured", "field-archive-digitized"}
VALID_UNITS = {"oilfield", "normalized"}


class ExampleCard(BaseModel):
    """One example card in the library."""

    id: str
    phenomenon: str
    source: str  # see VALID_SOURCES
    units: str  # "oilfield" (in / lbs) or "normalized" (0..1 shape)
    render: str = "line"  # "line" (ordered trace) or "dots" (markers only)
    position: List[float]
    load: List[float]
    meta: Dict[str, Any] = {}

    def as_card_data(self) -> CardData:
        """The card as the module's CardData (for calculators/diagnostics).

        Note: the ML classifier is trained on oilfield-unit cards; do not
        feed it ``normalized`` shapes.
        """
        return CardData(position=self.position, load=self.load)


@lru_cache(maxsize=1)
def _library() -> List[ExampleCard]:
    raw = json.loads(_DATA_PATH.read_text(encoding="utf-8"))
    return [ExampleCard(**c) for c in raw["cards"]]


def load_example_cards(
    phenomenon: Optional[str] = None,
    source: Optional[str] = None,
) -> List[ExampleCard]:
    """All example cards, optionally filtered by phenomenon and/or source."""
    cards = list(_library())
    if phenomenon is not None:
        cards = [c for c in cards if c.phenomenon == phenomenon]
    if source is not None:
        cards = [c for c in cards if c.source == source]
    return cards


def phenomena() -> List[str]:
    """Distinct phenomenon labels in the library, sorted."""
    return sorted({c.phenomenon for c in _library()})


def get_card(card_id: str) -> ExampleCard:
    """Fetch one card by id; raises KeyError with the valid-id hint."""
    for c in _library():
        if c.id == card_id:
            return c
    raise KeyError(
        f"unknown example card {card_id!r}; see load_example_cards() for "
        f"the {len(_library())} available cards"
    )
