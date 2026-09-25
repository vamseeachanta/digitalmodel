"""Owner decisions C13/C16: a .dat with a proven .yml twin is removed.

OrcaFlex binary .dat files embed the saving user and machine, so each one
whose .yml twin loads to the same model was deleted from the public tree. A
model or catalogue that named the .dat must name the twin instead.
"""

from __future__ import annotations

import re
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[3]
ORCAFLEX = REPO / "docs" / "domains" / "orcaflex"
CATALOG = ORCAFLEX / "library" / "templates" / "catalog.yaml"
_FIELD = re.compile(r"^﻿?\s*(BaseFile|Model):\s*(.+?)\s*$", re.M)
_SOURCE = re.compile(r"^\s*source:\s*(.+?)\s*$", re.M)

_MULTIBODY = "aqwa/to_orcaflex/multibody_test1/orcaflex"
#: Every model file that named a removed .dat (BaseFile or a script's Model).
CONSUMERS = [
    f"{_MULTIBODY}/fsts_fst1_l015_fst2_l015.yml",
    f"{_MULTIBODY}/fsts_fst1_l015_fst2_l015_mwl.yml",
    f"{_MULTIBODY}/fsts_fst1_l095_fst2_l095.yml",
    f"{_MULTIBODY}/fsts_fst1_l095_fst2_l095_hwl.yml",
    f"{_MULTIBODY}/fsts_fst1_l095_fst2_l095_lwl.yml",
    f"{_MULTIBODY}/fsts_fst1_l095_fst2_l095_mwl.yml",
    "examples/raw/C10/MultipleStatics.yml",
    "examples/modular/C10/MultipleStatics/includes/01_general.yml",
]


@pytest.mark.parametrize("rel", CONSUMERS)
def test_consumers_name_the_yml_twin(rel):
    text = (ORCAFLEX / rel).read_text(encoding="utf-8-sig")
    values = [m.group(2).strip() for m in _FIELD.finditer(text)]
    assert values and all(v.lower().endswith(".yml") for v in values), values


@pytest.mark.parametrize("rel", CONSUMERS[:7])
def test_the_named_twin_exists(rel):
    path = ORCAFLEX / rel
    text = path.read_text(encoding="utf-8-sig")
    for m in _FIELD.finditer(text):
        assert (path.parent / m.group(2).strip()).is_file(), m.group(2)


def test_every_catalogue_source_exists():
    text = CATALOG.read_text(encoding="utf-8-sig")
    sources = [m.group(1).strip().strip("'\"") for m in _SOURCE.finditer(text)]
    assert sources
    missing = [s for s in sources if not (ORCAFLEX / s).is_file()]
    assert not missing, missing
