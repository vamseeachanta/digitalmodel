"""Assembly errors R1–R6; weight advisory R7 is calculated separately."""
from math import isclose

from .loader import BhaDatabaseError
from .schema import physical


def fail(rule, message):
    raise BhaDatabaseError(f'{rule}: {message}')


def mate(lower, upper):
    return (upper is not None and lower.label == upper.label
            and lower.gender != upper.gender)


def check_assembly(assembly, components):
    items = assembly.items
    if any(i.component_id not in components for i in items):
        fail('R6', 'unknown component_id')
    parts = [components[i.component_id] for i in items]
    if [i.pos for i in items] != list(range(1, len(items) + 1)):
        fail('R1', 'positions must be contiguous, ascending, bit at position 1')
    if parts[0].type != 'bit' or sum(c.type == 'bit' for c in parts) != 1 or items[0].count != 1:
        fail('R1', 'exactly one bit must occupy position 1')
    hole_size = assembly.hole_size.to('m')
    if not isclose(parts[0].od.to('m'), hole_size, rel_tol=1e-9):
        fail('R3', 'nominal bit OD must match hole size')
    for index, (item, c) in enumerate(zip(items, parts)):
        if item.role == 'near_bit' or c.role == 'near_bit':
            if (c.type != 'stabilizer' or index != 1 or item.count != 1
                    or c.bottom_connection is None or c.bottom_connection.gender != 'box'):
                fail('R2', 'near-bit stabilizer must sit directly on bit with bottom box')
        max_od = max(q.to('m') for q in (c.od, c.od_max, c.blade_od or c.od))
        if max_od > hole_size and not isclose(max_od, hole_size, rel_tol=1e-9):
            fail('R3', 'component or blade OD exceeds hole size')
        if item.length_override:
            physical(item.length_override, 'm')
        if c.blade_length and c.blade_bottom_to_component_bottom:
            envelope = c.blade_length.to('m') + c.blade_bottom_to_component_bottom.to('m')
            if envelope > (item.length_override or c.length).to('m'):
                fail('R3', 'blade envelope exceeds selected component length')
        if item.count > 1 and not mate(c.top_connection, c.bottom_connection):
            fail('R5', 'repeated joints have incompatible connections')
    ranks = {'drill_collar': 0, 'nm_drill_collar': 0, 'pony_collar': 0,
             'hwdp': 1, 'drill_pipe': 2}
    sequence = [ranks[c.type] for c in parts if c.type in ranks]
    if sequence != sorted(sequence):
        fail('R4', 'bit-up order must be collars, then HWDP, then drill pipe')
    for lower, upper in zip(parts, parts[1:]):
        if not mate(lower.top_connection, upper.bottom_connection):
            fail('R5', 'adjacent thread labels/genders do not match; use a compatible crossover')
