
select a.id,c.ref_code_name, a.nominal_size,b.unit_info -> 'nominal_size' nominal_size_units,
a.outer_diameter , b.unit_info -> 'outer_diameter' outer_diameter_units,
a.wall_thickness , b.unit_info -> 'wall_thickness' wall_thickness_units,
a.inside_diameter , b.unit_info -> 'inside_diameter' inside_diameter_units
from pipe_metrics a, pipe_units b, pipe_reference_codes c
where a.unit_id = b.unit_id 
and a.ref_code_id = c.ref_code_id;

select a.id,c.description, a.loading_type,a.hs, b.unit_info -> 'hs' hs_units,
a.hmax , b.unit_info -> 'hmax' hmax_units,
a.tp , b.unit_info -> 'tp' tp_units,
a.gamma,a.alpha,a.return_period, 
b.unit_info -> 'return_period' return_period_units,
a.probability, b.unit_info -> 'probability' probability_units
from wave_info a, wave_units b, location c
where a.unit_id = b.unit_id 
and a.location_id = c.location_id;

select a.id,c.description, a.loading_type,a.current_type,a.current_profile_id,
a.depth, b.unit_info -> 'depth' depth_units,
a.current_speed , b.unit_info -> 'current_speed' current_speed_units,
a.direction , b.unit_info -> 'direction' direction_units,
a.return_period, b.unit_info -> 'return_period' return_period_units,
a.probability, b.unit_info -> 'probability' probability_units
from current_info a, current_units b, location c
where a.unit_id = b.unit_id 
and a.location_id = c.location_id;



