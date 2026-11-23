"""Elementary mechanics for assumed examples; results retain calculation basis."""
from dataclasses import dataclass
import math

from pydantic import BaseModel

from .schema import COMPONENT_TYPES, Quantity


def derived(value, unit, formula, sources):
    if not math.isfinite(value):
        from .loader import BhaDatabaseError
        raise BhaDatabaseError('nonfinite calculation result')
    return {'value': value, 'unit': unit, 'basis': 'computed',
            'formula': formula, 'source_keys': sorted(set(sources))}


def quantity_sources(model):
    if isinstance(model, Quantity):
        return [model.source_key]
    if isinstance(model, BaseModel):
        return [key for name in type(model).model_fields
                for key in quantity_sources(getattr(model, name))]
    if isinstance(model, list):
        return [key for value in model for key in quantity_sources(value)]
    return []


@dataclass
class Database:
    sources: dict
    components: dict
    assemblies: dict

    def assembly(self, bha_id):
        from .loader import BhaDatabaseError
        if bha_id not in self.assemblies:
            raise BhaDatabaseError('unknown bha_id')
        return self.assemblies[bha_id]

    def sections(self, bha_id):
        sections, distance, prior_lengths = [], 0.0, []
        for item in self.assembly(bha_id).items:
            c = self.components[item.component_id]
            length = (item.length_override or c.length).to('m') * item.count
            length_keys = quantity_sources(item.length_override or c.length)
            section = {
                'component_id': c.component_id, 'osdc_type': COMPONENT_TYPES[c.type],
                'od': derived(c.od.to('m'), 'm', 'SI(od)', [c.od.source_key]),
                'id': derived(c.id.to('m'), 'm', 'SI(id)', [c.id.source_key]),
                'length': derived(length, 'm', 'SI(length)*count', length_keys),
                'distance_from_bit': derived(distance, 'm', 'sum(preceding lengths)', prior_lengths or [self.assembly(bha_id).source_key]),
            }
            if c.blade_length and c.blade_bottom_to_component_bottom:
                centre = distance + c.blade_bottom_to_component_bottom.to('m') + c.blade_length.to('m') / 2
                section['blade_centre_from_bit'] = derived(centre, 'm', 'distance+blade_offset+blade_length/2',
                    prior_lengths + quantity_sources(c.blade_length) + quantity_sources(c.blade_bottom_to_component_bottom))
            sections.append(section)
            distance += length
            prior_lengths += length_keys
        return sections

    def summary(self, bha_id):
        a = self.assembly(bha_id)
        sections = self.sections(bha_id)
        parts = [self.components[s['component_id']] for s in sections]
        keys = [a.source_key] + quantity_sources(a)
        keys += [key for c in parts for key in quantity_sources(c)]
        total = sum(s['length']['value'] for s in sections)
        air = sum(s['length']['value'] * c.weight_per_length.to('N/m') for s, c in zip(sections, parts))
        factor = None
        if a.mud_weight_basis and a.material_density:
            factor = 1 - a.mud_weight_basis.to('kg/m^3') / a.material_density.to('kg/m^3')
        below = 0.0
        for section, c in zip(sections, parts):
            if c.type == 'hwdp':
                break
            below += section['length']['value'] * c.weight_per_length.to('N/m')
        r7, messages = wob_advisory(a, below, factor, keys)
        max_od = max(q.to('m') for c in parts for q in (c.od, c.od_max, c.blade_od or c.od))
        return {
            'bha_id': bha_id,
            'total_length_m': derived(total, 'm', 'sum(length*count)', keys),
            'max_od_m': derived(max_od, 'm', 'max(body,maximum,blade OD)', keys),
            'air_weight_N': derived(air, 'N', 'sum(length*weight_per_length)', keys),
            'buoyed_weight_N': None if factor is None else derived(
                air * factor, 'N', 'air_weight*(1-rho_mud/rho_material)', keys),
            'r7': r7, 'warnings': messages,
        }


def wob_advisory(assembly, below_weight, factor, keys):
    if factor is None or assembly.planned_wob is None or assembly.wob_margin is None:
        return {'state': 'NOT_EVALUATED'}, ['R7 NOT_EVALUATED: missing density, planned WOB or margin']
    available = below_weight * factor * (1 - assembly.wob_margin.to('dimensionless'))
    planned = assembly.planned_wob.to('N')
    state = 'EXCEEDED' if planned > available else 'WITHIN_ASSUMED_LIMIT'
    advisory = {'state': state, 'available_wob_N': derived(
        available, 'N', 'below_HWDP_air_weight*(1-rho_mud/rho_material)*(1-margin)', keys)}
    messages = [] if planned <= available else [
        f'R7 EXCEEDED: planned WOB {planned:.3f} N > available {available:.3f} N (assumed margin)']
    return advisory, messages
