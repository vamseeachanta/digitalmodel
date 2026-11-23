drop table if exists material_grades;
drop table if exists pipe_reference_codes;
drop table if exists pipe_units;
drop function if exists calculate_inside_diameter;


create table pipe_reference_codes(
	ref_code_id serial primary key not null,
	ref_code_name varchar(50) not null);
	
create table pipe_units(
	unit_id serial primary key not null,
	unit_info hstore not null);
	
create table pipe_metrics(
id serial primary key not null,
ref_code_id integer not null references pipe_reference_codes(ref_code_id),
nominal_size varchar(20) not null,
outer_diameter float not null,
wall_thickness numeric(6,2) not null,
inside_diameter float not null,
unit_id integer not null references pipe_units(unit_id)
);

create function calculate_inside_diameter() returns trigger as $$
begin
	if NEW.inside_diameter is null then
		NEW.inside_diameter := (NEW.outer_diameter - (NEW.wall_thickness * 2 /1000) );
	end if;
	return new;
end;
$$ language plpgsql;

create trigger trig_insert_pipe_metrics_insidediameter
before insert
on pipe_metrics
for each row
execute procedure calculate_inside_diameter();