# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Convert explicitly declared beam input ordering during serialization."""

from numbers import Integral
from typing import Any

import numpy as np


def validate_beam_node_order(node_order: str) -> None:
    """Require an explicit supported convention without guessing from geometry."""
    if not isinstance(node_order, str) or node_order not in ("calculix", "gmsh"):
        raise ValueError("beam_node_order must be 'calculix' or 'gmsh'")


def beam_connectivity(connectivity: Any, element_type: str,
                      node_order: str, node_count: int) -> Any:
    """Return B32 rows in CalculiX order without changing caller storage.

    Only declared Gmsh B32 rows are validated here. IDs are dense zero-based
    indices into the supplied nodes, independent of coordinate units. Object
    dtype preserves boolean and floating-point IDs until their rejection.
    """
    if element_type != "B32" or node_order != "gmsh":
        return connectivity
    rows = np.asarray(connectivity, dtype=object)
    if rows.ndim != 2 or rows.shape[1] != 3:
        raise ValueError("Gmsh B32 connectivity requires rows of three node IDs")
    for row in rows:
        if any(not isinstance(n, Integral) or isinstance(n, (bool, np.bool_))
               or not 0 <= int(n) < node_count for n in row):
            raise ValueError("Gmsh B32 node IDs must be nonboolean integers in range")
        if len(set(row)) != 3:
            raise ValueError("Gmsh B32 requires three distinct node IDs per row")
    return rows[:, [0, 2, 1]].copy()
