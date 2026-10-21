# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented product-level lookup, including rejected requests."""

import doctest
from pathlib import Path


def test_psl1_worked_example():
    """Catch stale answers or missing examples in the API 5L lookup guidance.

    Reference answer: UNIASEN, API 5L Pipe Specification, Mechanical Requirement
    PSL1 table, L360/X52 row, https://uniasen.com/standards/api-5l/, accessed
    2026-10-21. The page identifies API 5L, 46th edition (2018).
    The example tests the public lookup contract; licensed-text qualification
    remains pending.
    """
    document = (
        Path(__file__).resolve().parents[2]
        / "docs/domains/materials/grade-matrix-validation-2026-06-28.md"
    )
    result = doctest.testfile(
        str(document), module_relative=False, encoding="utf-8"
    )
    assert result.attempted > 0, "the product-level worked example is missing"
    assert result.failed == 0
