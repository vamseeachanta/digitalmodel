"""Strict, provenance-bearing BHA records (physical quantities stay as received)."""
from __future__ import annotations

import math
from typing import Literal

import pint
from pydantic import BaseModel, ConfigDict, Field, model_validator

UNITS = pint.UnitRegistry()
DataClass = Literal['STANDARD-NOMINAL', 'VENDOR-PUBLISHED', 'REAL-EXTERNAL',
                    'SYNTHETIC-EXAMPLE', 'CLIENT-PRIVATE']
COMPONENT_TYPES = {
    'bit': 'Bit', 'drill_collar': 'DrillCollar', 'nm_drill_collar': 'DrillCollar',
    'pony_collar': 'DrillCollar', 'hwdp': 'HeavyWeightDrillPipe',
    'drill_pipe': 'DrillPipe', 'jar': 'Jar', 'mwd': 'Mwd',
    'stabilizer': 'Stabilizer', 'rss': 'SteerableRotaryTool',
    **dict.fromkeys(('bit_sub', 'float_sub', 'motor', 'lwd', 'roller_reamer',
                     'hole_opener', 'crossover_sub', 'accelerator', 'shock_sub',
                     'circulating_sub', 'other'), 'Other'),
}


class StrictModel(BaseModel):
    model_config = ConfigDict(extra='forbid', strict=True)


class Quantity(StrictModel):
    value: float
    unit: str = Field(min_length=1)
    source_key: str = Field(min_length=1)
    basis: Literal['published', 'computed', 'assumed']
    as_published: str = Field(min_length=1, pattern=r'\S')
    as_published_on: str = Field(min_length=1, pattern=r'\S')
    formula: str | None = None

    @model_validator(mode='after')
    def finite_value(self):
        if not math.isfinite(self.value):
            raise ValueError('nonfinite quantity')
        if self.basis == 'computed' and not self.formula:
            raise ValueError('computed quantity needs formula')
        self.to(self.unit)
        return self

    def to(self, unit: str) -> float:
        return float(UNITS.Quantity(self.value, self.unit).to(unit).magnitude)


def physical(value: Quantity, unit: str, *, zero: bool = False) -> float:
    result = value.to(unit)
    if not math.isfinite(result) or result < 0 or (not zero and result == 0):
        raise ValueError('physical quantity outside positive range')
    return result


class Source(StrictModel):
    key: str
    type: Literal['standard', 'vendor', 'open-data', 'open-code', 'book', 'synthetic']
    title: str
    publisher: str
    edition: str
    locator: str
    url: str | None = None
    terms: str
    checked: str


class Connection(StrictModel):
    label: str = Field(pattern=r'^(NC(2[3-9]|[3-6][0-9]|7[0-7])|[678] 5/8 REG)$')
    gender: Literal['pin', 'box']


class Component(StrictModel):
    component_id: str
    type: str
    data_class: DataClass
    source_key: str
    od: Quantity
    id: Quantity
    od_max: Quantity
    length: Quantity
    weight_per_length: Quantity
    top_connection: Connection
    bottom_connection: Connection | None = None
    blade_od: Quantity | None = None
    blade_length: Quantity | None = None
    blade_bottom_to_component_bottom: Quantity | None = None
    role: Literal['near_bit', 'string'] | None = None

    @model_validator(mode='after')
    def geometry(self):
        if self.type not in COMPONENT_TYPES:
            raise ValueError('unknown component type')
        od = physical(self.od, 'm')
        if physical(self.id, 'm', zero=True) >= od:
            raise ValueError('bore must be less than OD')
        if physical(self.od_max, 'm') < od:
            raise ValueError('maximum OD below body OD')
        physical(self.length, 'm')
        physical(self.weight_per_length, 'N/m')
        for name in ('blade_od', 'blade_length', 'blade_bottom_to_component_bottom'):
            if (q := getattr(self, name)) is not None:
                physical(q, 'm', zero=name.endswith('bottom'))
        if (self.blade_length is None) != (self.blade_bottom_to_component_bottom is None):
            raise ValueError('blade length and offset must be provided together')
        if self.blade_length and self.blade_bottom_to_component_bottom:
            envelope = self.blade_length.to('m') + self.blade_bottom_to_component_bottom.to('m')
            if envelope > self.length.to('m'):
                raise ValueError('blade envelope exceeds component length')
        if self.type == 'stabilizer' and not self.blade_od:
            raise ValueError('stabilizer needs blade OD')
        if self.type != 'stabilizer' and self.role is not None:
            raise ValueError('stabilizer role on another component')
        return self


class Item(StrictModel):
    pos: int = Field(ge=1)
    component_id: str
    count: int = Field(default=1, ge=1)
    role: Literal['near_bit', 'string', 'nm_spacing', 'crossover'] | None = None
    length_override: Quantity | None = None
    note: str | None = None


class Assembly(StrictModel):
    bha_id: str
    name: str
    data_class: DataClass
    purpose: Literal['pendulum', 'building', 'holding', 'motor', 'rss', 'other']
    hole_size: Quantity
    source_key: str
    checked: str
    mud_weight_basis: Quantity | None = None
    material_density: Quantity | None = None
    planned_wob: Quantity | None = None
    wob_margin: Quantity | None = None
    items: list[Item] = Field(min_length=1)

    @model_validator(mode='after')
    def inputs(self):
        physical(self.hole_size, 'm')
        for name, unit in (('mud_weight_basis', 'kg/m^3'),
                           ('material_density', 'kg/m^3'), ('planned_wob', 'N')):
            if (q := getattr(self, name)) is not None:
                physical(q, unit, zero=name != 'material_density')
        if self.wob_margin and not 0 <= self.wob_margin.to('dimensionless') < 1:
            raise ValueError('margin must be in [0,1)')
        if self.mud_weight_basis and self.material_density:
            if self.mud_weight_basis.to('kg/m^3') >= self.material_density.to('kg/m^3'):
                raise ValueError('mud density must be below material density')
        return self


class Document(StrictModel):
    schema_version: Literal[1]
    sources: list[Source]
    components: list[Component]
    assemblies: list[Assembly]
