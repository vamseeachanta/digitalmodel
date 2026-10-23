# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inspect original IGES unit metadata before the kernel supplies defaults."""

import re
from pathlib import Path

from .models import CadReadError


def original_unit_flag(path: Path) -> int:
    """Read the original flag, preserving zero/blank as absent.

    OCCT 7.9 IGESData_GlobalSection::Init parameters 14/15 and its correction
    block: an absent flag/name is silently replaced with inches by OCCT.
    IGESFile/igesread.c defines 72 data columns and comma/semicolon defaults.
    Hollerith strings can contain separators and span physical records.
    https://github.com/Open-Cascade-SAS/OCCT/blob/V7_9_0/src/IGESData/IGESData_GlobalSection.cxx
    https://github.com/Open-Cascade-SAS/OCCT/blob/V7_9_0/src/IGESFile/igesread.c
    """
    data = "".join(
        line[:72]
        for line in path.read_text(encoding="ascii").splitlines()
        if len(line) > 72 and line[72] == "G"
    )
    if not data:
        raise CadReadError("Missing IGES global section")
    first = re.match(r"\s*1[Hh](.)", data)
    separator = first.group(1) if first else ","
    fields: list[str] = []
    position = 0
    terminator = ";"
    while position < len(data):
        while position < len(data) and data[position].isspace():
            position += 1
        match = re.match(r"(\d+)[Hh]", data[position:])
        if match:
            start = position + match.end()
            position = start + int(match.group(1))
            value = data[start:position]
            while position < len(data) and data[position].isspace():
                position += 1
        else:
            start = position
            while position < len(data) and data[position] not in {
                separator,
                terminator,
            }:
                position += 1
            value = data[start:position].strip()
        fields.append(value)
        if len(fields) == 2 and value:
            if len(value) != 1:
                raise CadReadError("Invalid IGES record delimiter")
            terminator = value
        if position >= len(data) or data[position] not in {separator, terminator}:
            raise CadReadError("Malformed IGES global parameter")
        ended = data[position] == terminator
        position += 1
        if len(fields) == 14:
            try:
                return int(value) if value else 0
            except ValueError as exc:
                raise CadReadError("Invalid IGES unit flag") from exc
        if ended:
            break
    raise CadReadError("Missing IGES unit flag parameter")
