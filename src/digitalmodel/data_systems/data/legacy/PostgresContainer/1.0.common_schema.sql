CREATE EXTENSION hstore;

drop table if exists location;
drop type if exists loading_type;
drop type if exists reference_codes;


create table location(
	location_id serial primary key not null,
	latitude float null,
	longitude float null,
	description varchar(100) null,
	business_area varchar(100) null
);

create type loading_type as enum ('Extreme','long-term');

create table reference_codes(
	ref_code_id serial primary key not null,
	ref_code varchar(50) not null)
	release_year SMALLINT not null)
	release_date DATE null);
-- Add constraints for unique pair of ref_code and release_date
