"""Major Accident Hazard inventory with a validated CSV boundary.

Basis: UK SCR-2015, SI 2015/398, regulation 16(1)(c)-(d) requires hazard
identification, likelihood/consequence assessment and control measures.
IMO MSC-MEPC.2/Circ.12/Rev.2 (2018), annex sections 5.2.1.2 and 5.3 describe
the hazard/scenario inventory; appendix 5 section 3.2.3 defines potential loss
of life (PLL), the expected fatalities/year used here. See:
https://www.legislation.gov.uk/uksi/2015/398/regulation/16
https://www.imo.org/en/OurWork/Safety/Pages/FormalSafetyAssessment.aspx

ISO 17776:2016, Major accident hazard management during the design of new
installations, is a contextual reference. Its clause text was unavailable;
this schema does not certify compliance with it or exhaust hazard identification.
No licensed failure-rate table or acceptance limit is embedded.

Pydantic is deliberate at the external inventory/CSV boundary, where unknown
fields, non-finite values and malformed records must be rejected. Existing
ALARP dataclasses remain the deterministic calculation interface.

CSV has exactly the model fields as columns. Barriers and responsible_roles
are JSON arrays within quoted CSV cells. Blank numeric cells mean unknown;
zero is a known value. Bases remain required even for unknown quantities.
Worked synthetic example: 0.02 events/year * 0.5 fatalities/event = 0.01
fatalities/year. These are arithmetic assumptions, not operating data.
"""

from __future__ import annotations

import csv
import io
import json
import math
from contextlib import nullcontext
from enum import Enum
from pathlib import Path
from typing import Annotated, TextIO

from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    StringConstraints,
    ValidationError,
    model_validator,
)

Text = Annotated[str, StringConstraints(strip_whitespace=True, min_length=1)]
# IMO FSA (2018), annex section 2: frequency is non-negative; fatalities are
# counts. Zero is permitted; there is no empirical rate or acceptance threshold.
NonNegative = Annotated[float, Field(strict=True, ge=0, allow_inf_nan=False)]
TextItems = Annotated[tuple[Text, ...], Field(min_length=1)]


class EvaluationStatus(str, Enum):
    """Whether the scenario's expected fatality rate can be calculated."""

    EVALUATED = "EVALUATED"
    NOT_EVALUATED = "NOT_EVALUATED"


class MajorAccidentHazard(BaseModel):
    """One scenario; consequences are conditional means, not probabilities.

    Barrier names document controls; they do not automatically reduce frequency.
    A basis identifies the supplied data, assumptions and control credit applied.
    """

    model_config = ConfigDict(extra="forbid", frozen=True)

    hazard_id: Text
    description: Text
    frequency_per_year: NonNegative | None
    fatalities_per_event: NonNegative | None
    frequency_basis: Text
    consequence_basis: Text
    barriers: tuple[Text, ...]
    responsible_roles: TextItems

    @model_validator(mode="after")
    def finite_product(self) -> MajorAccidentHazard:
        value = self.expected_fatalities_per_year
        if value is not None and not math.isfinite(value):
            raise ValueError("expected fatalities/year must be finite")
        return self

    @property
    def expected_fatalities_per_year(self) -> float | None:
        """Single-scenario PLL contribution, never an individual-risk estimate."""
        if self.frequency_per_year is None or self.fatalities_per_event is None:
            return None
        # IMO FSA (2018), appendix 5 section 3.2.3: PLL = sum(f_i * N_i).
        return self.frequency_per_year * self.fatalities_per_event

    @property
    def status(self) -> EvaluationStatus:
        if self.expected_fatalities_per_year is None:
            return EvaluationStatus.NOT_EVALUATED
        return EvaluationStatus.EVALUATED


class MAHValidationError(ValueError):
    """CSV error with physical line and field, without echoing row contents."""

    def __init__(self, row: int, field: str, reason: str):
        self.row = row
        self.field = field
        super().__init__(f"CSV line {row}, field {field}: {reason}")


class MAHRegister(BaseModel):
    """Immutable inventory; duplicate identifiers are rejected.

    An empty register is representable but establishes no hazard coverage or
    ALARP demonstration. No total is returned: overlapping scenarios and unknown
    entries require assessment before summing risk contributions.
    """

    model_config = ConfigDict(extra="forbid", frozen=True)
    hazards: tuple[MajorAccidentHazard, ...]

    @model_validator(mode="after")
    def unique_ids(self) -> MAHRegister:
        ids = [hazard.hazard_id for hazard in self.hazards]
        if len(ids) != len(set(ids)):
            raise ValueError("hazard_id must be unique")
        return self

    @classmethod
    def from_csv(cls, source: str | Path | TextIO) -> MAHRegister:
        """Read a UTF-8 CSV path or open text stream; reject the entire bad input."""
        context = (
            Path(source).open(encoding="utf-8-sig", newline="")
            if isinstance(source, (str, Path))
            else nullcontext(source)
        )
        with context as stream:
            reader = csv.DictReader(stream, strict=True)
            hazards = []
            seen = set()
            try:
                columns = tuple(MajorAccidentHazard.model_fields)
                if (
                    reader.fieldnames is None
                    or len(reader.fieldnames) != len(columns)
                    or set(reader.fieldnames) != set(columns)
                ):
                    raise MAHValidationError(
                        1, "header", "expected exact schema columns"
                    )
                for row in reader:
                    line = reader.line_num
                    if None in row or any(value is None for value in row.values()):
                        raise MAHValidationError(line, "row", "column count mismatch")
                    for field in ("barriers", "responsible_roles"):
                        try:
                            items = json.loads(row[field])
                            if not isinstance(items, list):
                                raise ValueError
                            row[field] = tuple(items)
                        except (ValueError, TypeError):
                            raise MAHValidationError(
                                line, field, "expected JSON array"
                            ) from None
                    for field in ("frequency_per_year", "fatalities_per_event"):
                        try:
                            row[field] = (
                                float(row[field]) if row[field].strip() else None
                            )
                        except ValueError:
                            raise MAHValidationError(
                                line, field, "expected number or blank"
                            ) from None
                    try:
                        hazard = MajorAccidentHazard.model_validate(row)
                    except ValidationError as exc:
                        location = exc.errors(include_input=False)[0]["loc"]
                        field = ".".join(map(str, location)) or "row"
                        raise MAHValidationError(
                            line, field, "schema validation failed"
                        ) from None
                    if hazard.hazard_id in seen:
                        raise MAHValidationError(
                            line, "hazard_id", "duplicate identifier"
                        )
                    seen.add(hazard.hazard_id)
                    hazards.append(hazard)
            except csv.Error:
                raise MAHValidationError(
                    reader.line_num, "row", "malformed CSV"
                ) from None
        return cls(hazards=tuple(hazards))

    def to_csv(self) -> str:
        """Return CSV that round-trips known and unknown values without defaults."""
        stream = io.StringIO(newline="")
        writer = csv.DictWriter(stream, fieldnames=MajorAccidentHazard.model_fields)
        writer.writeheader()
        for hazard in self.hazards:
            row = hazard.model_dump()
            for field in ("barriers", "responsible_roles"):
                row[field] = json.dumps(row[field])
            writer.writerow(row)
        return stream.getvalue()
