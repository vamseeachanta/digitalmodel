# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Original synthetic controller row: inputs, arithmetic and inventory.

Run with PYTHONPATH=src python examples/domains/artificial_lift/scada_surface_export.py.
All numeric inputs are invented for this example, not field measurements.
Hand calculation: stroke = 4 - 0 = 4; load range = 30 - 10 = 20;
surface span / declared surface stroke = 4 / 4 = 1.
"""

import io
import json

from digitalmodel.marine_ops.artificial_lift.dynacard.scada_export import (
    read_scada_export,
    summarize_cards,
)


def worked_example() -> dict:
    """Return a surface-only inventory; the duplicated downhole trace is flagged."""
    text = (
        "Card Time|Card Type|Stroke Period|SPM|Stroke Length|"
        "Surface Positions|Surface Loads|Downhole Positions|Downhole Loads\n"
        "2020-01-01T00:00:00Z|Current|6|10|4|"
        "0,2,4,2,0|10,30,30,10,10|0,1,0|0,1,0\n"
    )
    cards = read_scada_export(io.StringIO(text))
    return summarize_cards(cards)


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
