# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented synthetic product-level lookup example."""
import doctest
from pathlib import Path


def test_psl1_worked_example():
    document = (Path(__file__).resolve().parents[2]
                / "docs/domains/materials/grade-matrix-validation-2026-06-28.md")
    result = doctest.testfile(str(document), module_relative=False, encoding="utf-8")
    assert result.attempted > 0, "the product-level worked example is missing"
    assert result.failed == 0
