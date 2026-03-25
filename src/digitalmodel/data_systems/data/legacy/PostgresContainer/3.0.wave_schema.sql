drop table if exists wave_info;
drop table if exists wave_units;

create table wave_units(
	unit_id serial primary key not null,
	unit_info hstore not null);

create table wave_info(
id serial primary key not null,
location_id integer not null references location(location_id),
loading_type loading_type,
hs float not null,
hmax float null,
tp float not null,
gamma float not null,
alpha float null,
return_period integer null,
probability float null,
unit_id integer not null references wave_units(unit_id)
);