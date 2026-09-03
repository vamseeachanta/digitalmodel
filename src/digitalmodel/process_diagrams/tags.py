"""ISA-5.1 tag and line-number grammar.

Parsing and validation for the two identifier schemes that carry most of the
information on a piping and instrumentation diagram:

- **Instrument tags** (``PDIC-1204``) — ANSI/ISA-5.1 alphanumeric identification.
- **Line numbers** (``12"-PG-1001-A1A-IH``) — the compound pipe identifier.

The tag reads left to right as a first letter (the measured variable), an
optional variable modifier, one or more succeeding letters (the functions the
device performs), and the loop number. Every device on a loop shares the number,
so ``PDIC-1204`` and its final element ``PDV-1204`` are the same loop.

A ``Z`` in the modifier position marks a safety-instrumented function, which is
how a reader distinguishes a trip from an alarm without tracing bubble geometry
(ISA-5.1-2009 permits this or an explicit ``(SIS)`` suffix).
"""

from __future__ import annotations

import re
from dataclasses import dataclass

# ISA-5.1 first letters (measured variable / initiating variable).
VARIABLES: dict[str, str] = {
    "A": "analysis", "B": "burner or combustion", "C": "user's choice",
    "D": "density", "E": "voltage", "F": "flow", "G": "gauging or dimension",
    "H": "hand", "I": "current", "J": "power", "K": "time or time schedule",
    "L": "level", "M": "moisture or humidity", "N": "user's choice",
    "O": "user's choice", "P": "pressure", "Q": "quantity",
    "R": "radiation", "S": "speed or frequency", "T": "temperature",
    "U": "multivariable", "V": "vibration", "W": "weight or force",
    "X": "unclassified", "Y": "event, state or presence", "Z": "position",
}

# Letters that act as a variable modifier when they follow the first letter.
VARIABLE_MODIFIERS: dict[str, str] = {
    "D": "differential", "F": "ratio", "Q": "integrate or totalise",
    "S": "safety", "Z": "safety instrumented system", "X": "unclassified",
}

# ISA-5.1 succeeding letters (readout / passive / output function).
FUNCTIONS: dict[str, str] = {
    "A": "alarm", "B": "user's choice", "C": "control", "E": "primary element",
    "G": "glass or viewing device", "I": "indicate", "K": "control station",
    "L": "light", "N": "user's choice", "O": "orifice or restriction",
    "P": "test point", "Q": "integrate or totalise", "R": "record",
    "S": "switch", "T": "transmit", "U": "multifunction",
    "V": "valve, damper or louver", "W": "well or probe",
    "X": "unclassified", "Y": "relay, compute or convert", "Z": "actuator",
}

# Trailing function modifiers. Longest first — HH must be tried before H.
FUNCTION_MODIFIERS: tuple[str, ...] = ("HH", "LL", "H", "L")

_TAG_RE = re.compile(
    r"^(?P<letters>[A-Z]{1,6})-(?P<loop>[0-9]{1,6}[A-Z]?)(?:\s*\((?P<sis>SIS)\))?$"
)
_LINE_RE = re.compile(
    r'^(?P<size>\d+(?:\.\d+)?|\d+/\d+|\d+-\d+/\d+)"'
    r"-(?P<service>[A-Z]{1,4})"
    r"-(?P<sequence>[0-9]{2,6})"
    r"-(?P<piping_class>[A-Z0-9]{2,6})"
    r"(?:-(?P<insulation>[A-Z]{1,3}))?$"
)


class TagError(ValueError):
    """Raised when a string is not a well-formed ISA-5.1 tag or line number."""


@dataclass(frozen=True)
class Tag:
    """A parsed ISA-5.1 instrument tag."""

    raw: str
    variable: str
    variable_modifier: str | None
    functions: str
    function_modifier: str | None
    loop: str
    sis_suffix: bool = False

    @property
    def letters(self) -> str:
        """The full letter block, e.g. ``PDIC``."""
        parts = [self.variable, self.variable_modifier or "", self.functions,
                 self.function_modifier or ""]
        return "".join(parts)

    @property
    def is_safety_function(self) -> bool:
        """True where the tag marks a safety-instrumented function.

        Either the ``Z`` variable modifier or an explicit ``(SIS)`` suffix.
        ISA-5.1-2009 allows both; a drawing set should declare which it uses
        on its legend sheet and then stay consistent.
        """
        return self.variable_modifier == "Z" or self.sis_suffix

    @property
    def is_trip(self) -> bool:
        """True where the tag carries a high-high or low-low function modifier."""
        return self.function_modifier in ("HH", "LL")

    def describe(self) -> str:
        """Plain-language expansion, e.g. 'pressure, differential, indicate, transmit'."""
        parts = [VARIABLES.get(self.variable, "unknown variable")]
        if self.variable_modifier:
            parts.append(VARIABLE_MODIFIERS.get(self.variable_modifier, "unknown modifier"))
        parts.extend(FUNCTIONS.get(letter, "unknown function") for letter in self.functions)
        if self.function_modifier:
            parts.append({"HH": "high-high", "LL": "low-low",
                          "H": "high", "L": "low"}[self.function_modifier])
        return ", ".join(parts)

    def same_loop(self, other: "Tag") -> bool:
        """True where two tags belong to the same control loop."""
        return self.loop == other.loop

    def __str__(self) -> str:  # pragma: no cover - trivial
        return self.raw


def parse_tag(text: str) -> Tag:
    """Parse an ISA-5.1 instrument tag.

    >>> t = parse_tag("PDIC-1204")
    >>> (t.variable, t.variable_modifier, t.functions, t.loop)
    ('P', 'D', 'IC', '1204')
    >>> parse_tag("PZHH-401").is_safety_function
    True

    Two disambiguation rules, both inherent to the grammar rather than stated
    by the standard — a real drawing's legend sheet is the tie-breaker:

    - ``Z`` in second position is always the safety-system variable modifier,
      because as a succeeding letter it means "actuator" and never appears
      there (position tags put Z first: ``ZV``, ``ZC``, ``ZT``).
    - Any other modifier letter is read as a modifier only when letters follow
      it, so ``PDT-101`` is a differential pressure transmitter.

    Project-specific tags that are common in oil and gas but outside strict
    ISA usage — ``SDV`` shutdown valve, ``BDV`` blowdown valve — parse, but
    they parse against the ISA reading of each letter, not the house meaning.
    """
    m = _TAG_RE.match(text.strip().upper())
    if not m:
        raise TagError(f"not a well-formed ISA-5.1 tag: {text!r}")
    letters = m.group("letters")

    variable = letters[0]
    if variable not in VARIABLES:
        raise TagError(f"{variable!r} is not an ISA-5.1 first letter (in {text!r})")
    rest = letters[1:]

    modifier = None
    if rest.startswith("Z"):
        # Z in second position is always read as the safety-system variable
        # modifier. As a succeeding letter Z means "actuator", which does not
        # occur in second position on real tags (position tags put Z first:
        # ZV, ZC, ZT).
        modifier, rest = "Z", rest[1:]
    elif len(rest) >= 2 and rest[0] in VARIABLE_MODIFIERS:
        modifier, rest = rest[0], rest[1:]

    function_modifier = None
    for suffix in FUNCTION_MODIFIERS:
        if len(rest) >= len(suffix) and rest.endswith(suffix):
            function_modifier, rest = suffix, rest[: -len(suffix)]
            break

    if not rest and modifier is None and function_modifier is None:
        raise TagError(f"tag {text!r} has no succeeding (function) letter")
    for letter in rest:
        if letter not in FUNCTIONS:
            raise TagError(f"{letter!r} is not an ISA-5.1 succeeding letter (in {text!r})")

    return Tag(
        raw=text.strip(),
        variable=variable,
        variable_modifier=modifier,
        functions=rest,
        function_modifier=function_modifier,
        loop=m.group("loop"),
        sis_suffix=m.group("sis") is not None,
    )


@dataclass(frozen=True)
class LineNumber:
    """A parsed pipe line number, e.g. ``12"-PG-1001-A1A-IH``."""

    raw: str
    size_in: float
    service: str
    sequence: str
    piping_class: str
    insulation: str | None = None

    @property
    def is_insulated(self) -> bool:
        return self.insulation is not None

    def __str__(self) -> str:  # pragma: no cover - trivial
        return self.raw


def parse_line_number(text: str) -> LineNumber:
    """Parse a compound line number.

    >>> ln = parse_line_number('12"-PG-1001-A1A-IH')
    >>> (ln.size_in, ln.service, ln.piping_class, ln.insulation)
    (12.0, 'PG', 'A1A', 'IH')

    A new line number is assigned whenever size, service, or piping class
    changes, so these three fields together are what makes a run distinct.
    """
    m = _LINE_RE.match(text.strip().upper())
    if not m:
        raise TagError(f"not a well-formed line number: {text!r}")
    size = m.group("size")
    if "-" in size:  # e.g. 1-1/2"
        whole, frac = size.split("-")
        num, den = frac.split("/")
        size_in = float(whole) + float(num) / float(den)
    elif "/" in size:
        num, den = size.split("/")
        size_in = float(num) / float(den)
    else:
        size_in = float(size)
    return LineNumber(
        raw=text.strip(),
        size_in=size_in,
        service=m.group("service"),
        sequence=m.group("sequence"),
        piping_class=m.group("piping_class"),
        insulation=m.group("insulation"),
    )
