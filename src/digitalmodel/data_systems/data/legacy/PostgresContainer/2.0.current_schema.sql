drop table if exists current_info;
drop type if exists current_type;
drop table if exists current_units;


create TYPE current_type AS ENUM ('Normal','Loop','NA' );

create table current_units(
	unit_id serial primary key not null,
	unit_info hstore not null);
	
create table current_info(
id serial primary key not null,
location_id integer not null references location(location_id),
loading_type loading_type,
current_type current_type,
current_profile_id int not null,
depth int not null,
current_speed float null,
direction varchar(30) null,
return_period integer null,
probability float null,
unit_id integer not null references current_units(unit_id)
);