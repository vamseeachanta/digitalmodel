drop table if exists wave_info;
drop table if exists current_info;
drop table if exists location;
drop table if exists wave_units;
drop type if exists current_type;
drop type if exists loading_type;
drop table if exists current_units;


create table location(
	location_id serial primary key not null,
	latitude float null,
	longiture float null,
	description varchar(100) null,
	business_area varchar(100) null
);

insert into location (business_area) values ('GOM - Mexico Side');
insert into location (business_area) values ('GOM US Side');
insert into location (business_area) values ('South America, Offshore Brazil');
insert into location (business_area) values ('South America, Offshore Guyana');
insert into location (business_area) values ('West Africa, Offshore Angola');

create type loading_type as enum ('Extreme','long-term');
create table wave_units(
	unit_id serial primary key not null,
	unit_info hstore not null);

insert into wave_units (unit_info) values 
('hs => "ft", 
  hmax => "ft", 
  tp => "s",
  return_period => "years",
  probability => "%"');
 
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

insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (1,'Extreme',23.1955380577428,43.1430446194226,12.15,3,null,5,null,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (1,'Extreme',25.1968503937008,46.8503937007874,12.51,3,null,10,null,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (1,'Extreme',31.7257217847769,59.0223097112861,13.73,3,null,100,null,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',0.820209973753281,null,2.5,1,null,null,0.661057692307692,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',0.820209973753281,null,3.5,1,null,null,7.8525641025641,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',0.820209973753281,null,4.5,1,null,null,7.04126602564102,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',2.46062992125984,null,3.5,1.3,null,null,6.01963141025641,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',2.46062992125984,null,4.5,1,null,null,18.8100961538461,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',2.46062992125984,null,5.5,1,null,null,15.9254807692308,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',2.46062992125984,null,10.5,1,null,null,0.120192307692308,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',4.1010498687664,null,4.5,1.3,null,null,2.49399038461538,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',4.1010498687664,null,5.5,1.3,null,null,12.3898237179487,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',4.1010498687664,null,6.5,1,null,null,9.73557692307692,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',5.74146981627297,null,6.5,1.3,null,null,7.94270833333333,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',5.74146981627297,null,7.5,1,null,null,5.18830128205128,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',7.38188976377953,null,10.5,1,null,null,0.300480769230769,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',9.02230971128609,null,7.5,1.3,null,null,5.13822115384615,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',10.6627296587927,null,13.5,1,null,null,0.0500801282051282,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',15.5839895013123,null,9.5,1.3,null,null,0.270432692307692,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (2,'long-term',22.1456692913386,null,10.5,1.3,null,null,0.0600961538461538,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',4.75721784776903,null,6.23,6,null,null,9.6,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',5.54461942257218,null,7.99,6,null,null,26.99,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.46325459317585,null,9.94,6,null,null,17.28,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.16797900262467,null,12.02,6,null,null,18.99,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.13517060367454,null,13.99,6,null,null,16.39,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.26640419947507,null,15.5,6,null,null,5.2,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.36482939632546,null,16.5,6,null,null,2.4,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.33202099737533,null,17.5,6,null,null,1.74,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',6.10236220472441,null,18.5,6,null,null,0.76,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (5,'long-term',5.80708661417323,null,20.18,6,null,null,0.65,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,3,2.2,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,3,2.2,null,null,0.181,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,5,2.2,null,null,0.025,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,5,2.2,null,null,1.882,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,5,2.2,null,null,6.318,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,5,2.2,null,null,1.137,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,5,2.2,null,null,0.028,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,7,2.2,null,null,0.025,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,7,2.2,null,null,2.704,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,7,2.2,null,null,11.084,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,7,2.2,null,null,15.938,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,7,2.2,null,null,8.279,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,7,2.2,null,null,1.661,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,7,2.2,null,null,0.092,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',13.1233595800525,null,7,2.2,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,9,2.2,null,null,0.067,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,9,2.2,null,null,1.695,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,9,2.2,null,null,4.034,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,9,2.2,null,null,5.472,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,9,2.2,null,null,5.159,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,9,2.2,null,null,4.258,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,9,2.2,null,null,2.311,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',13.1233595800525,null,9,2.2,null,null,0.739,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',14.7637795275591,null,9,2.2,null,null,0.196,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',16.4041994750656,null,9,2.2,null,null,0.043,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',18.0446194225722,null,9,2.2,null,null,0.006,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,11,2.2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,11,2.2,null,null,1.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,11,2.2,null,null,2.572,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,11,2.2,null,null,2.906,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,11,2.2,null,null,2.679,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,11,2.2,null,null,1.965,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,11,2.2,null,null,1.143,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',13.1233595800525,null,11,2.2,null,null,0.647,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',14.7637795275591,null,11,2.2,null,null,0.494,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',16.4041994750656,null,11,2.2,null,null,0.34,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',18.0446194225722,null,11,2.2,null,null,0.147,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',19.6850393700787,null,11,2.2,null,null,0.034,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,13,2.2,null,null,0.031,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,13,2.2,null,null,0.576,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,13,2.2,null,null,1.986,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,13,2.2,null,null,2.06,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,13,2.2,null,null,1.658,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,13,2.2,null,null,1.192,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,13,2.2,null,null,0.644,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',13.1233595800525,null,13,2.2,null,null,0.386,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',14.7637795275591,null,13,2.2,null,null,0.291,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',16.4041994750656,null,13,2.2,null,null,0.135,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',18.0446194225722,null,13,2.2,null,null,0.132,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',19.6850393700787,null,13,2.2,null,null,0.055,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',21.3254593175853,null,13,2.2,null,null,0.037,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',22.9658792650919,null,13,2.2,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',24.6062992125984,null,13,2.2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,15,2.2,null,null,0.018,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,15,2.2,null,null,0.441,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,15,2.2,null,null,0.742,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,15,2.2,null,null,0.607,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,15,2.2,null,null,0.46,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,15,2.2,null,null,0.288,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,15,2.2,null,null,0.205,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',13.1233595800525,null,15,2.2,null,null,0.095,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',14.7637795275591,null,15,2.2,null,null,0.074,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',16.4041994750656,null,15,2.2,null,null,0.095,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',18.0446194225722,null,15,2.2,null,null,0.089,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',19.6850393700787,null,15,2.2,null,null,0.034,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',21.3254593175853,null,15,2.2,null,null,0.025,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',22.9658792650919,null,15,2.2,null,null,0.015,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',24.6062992125984,null,15,2.2,null,null,0.018,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',26.246719160105,null,15,2.2,null,null,0.012,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',1.64041994750656,null,17,2.2,null,null,0.006,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',3.28083989501312,null,17,2.2,null,null,0.018,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,17,2.2,null,null,0.049,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,17,2.2,null,null,0.028,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,17,2.2,null,null,0.028,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,17,2.2,null,null,0.015,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,17,2.2,null,null,0.025,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',13.1233595800525,null,17,2.2,null,null,0.031,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',14.7637795275591,null,17,2.2,null,null,0.018,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',16.4041994750656,null,17,2.2,null,null,0.006,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',18.0446194225722,null,17,2.2,null,null,0.046,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',19.6850393700787,null,17,2.2,null,null,0.012,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',4.92125984251969,null,19,2.2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',6.56167979002625,null,19,2.2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',8.20209973753281,null,19,2.2,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',9.84251968503937,null,19,2.2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (3,'long-term',11.4829396325459,null,19,2.2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',2.46062992125984,null,7.5,1,null,null,0.055,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',2.46062992125984,null,8.5,1,null,null,0.038,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,4.5,1,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,5.5,1,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,6.5,1,null,null,0.022,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,7.5,1,null,null,1.733,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,8.5,1,null,null,1.309,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,9.5,2,null,null,0.059,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,10.5,2,null,null,0.015,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,11.5,2,null,null,0.007,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,12.5,8,null,null,0.005,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',3.28083989501312,null,13.5,8,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,4.5,1,null,null,0.022,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,5.5,1,null,null,0.112,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,6.5,1,null,null,0.415,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,7.5,1,null,null,4.383,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,8.5,1,null,null,4.853,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,9.5,2,null,null,0.485,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,10.5,2,null,null,0.135,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,11.5,2,null,null,0.044,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,12.5,8,null,null,0.026,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,13.5,8,null,null,0.015,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,14.5,8,null,null,0.013,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.1010498687664,null,15.5,8,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,4.5,1,null,null,0.011,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,5.5,1,null,null,0.264,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,6.5,1,null,null,1.283,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,7.5,1,null,null,5.304,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,8.5,1,null,null,5.315,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,9.5,2,null,null,1.174,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,10.5,2,null,null,0.457,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,11.5,2,null,null,0.266,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,12.5,8,null,null,0.136,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,13.5,8,null,null,0.07,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,14.5,8,null,null,0.086,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,15.5,8,null,null,0.059,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,16.5,8,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',4.92125984251969,null,17.5,8,null,null,0.006,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,5.5,1,null,null,0.15,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,6.5,1,null,null,1.694,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,7.5,1,null,null,6.173,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,8.5,1,null,null,5.873,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,9.5,2,null,null,2.225,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,10.5,2,null,null,0.87,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,11.5,2,null,null,0.562,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,12.5,8,null,null,0.308,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,13.5,8,null,null,0.196,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,14.5,8,null,null,0.111,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,15.5,8,null,null,0.057,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,16.5,8,null,null,0.027,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,17.5,8,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,18.5,8,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',5.74146981627297,null,19.5,8,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,5.5,1,null,null,0.021,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,6.5,1,null,null,0.971,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,7.5,1,null,null,5.325,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,8.5,1,null,null,6.739,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,9.5,2,null,null,2.148,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,10.5,2,null,null,0.942,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,11.5,2,null,null,0.641,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,12.5,8,null,null,0.539,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,13.5,8,null,null,0.247,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,14.5,8,null,null,0.15,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,15.5,8,null,null,0.096,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,16.5,8,null,null,0.027,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,17.5,8,null,null,0.013,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',6.56167979002625,null,18.5,8,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,5.5,1,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,6.5,1,null,null,0.281,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,7.5,1,null,null,3.244,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,8.5,1,null,null,6.856,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,9.5,2,null,null,1.966,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,10.5,2,null,null,0.625,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,11.5,2,null,null,0.497,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,12.5,8,null,null,0.537,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,13.5,8,null,null,0.346,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,14.5,8,null,null,0.231,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,15.5,8,null,null,0.118,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,16.5,8,null,null,0.028,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',7.38188976377953,null,17.5,8,null,null,0.007,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,6.5,1,null,null,0.029,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,7.5,1,null,null,1.262,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,8.5,1,null,null,5.291,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,9.5,2,null,null,1.999,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,10.5,2,null,null,0.368,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,11.5,2,null,null,0.236,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,12.5,8,null,null,0.313,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,13.5,8,null,null,0.355,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,14.5,8,null,null,0.236,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,15.5,8,null,null,0.141,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,16.5,8,null,null,0.015,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',8.20209973753281,null,17.5,8,null,null,0.004,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,7.5,1,null,null,0.263,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,8.5,1,null,null,2.974,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,9.5,2,null,null,1.649,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,10.5,2,null,null,0.247,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,11.5,2,null,null,0.051,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,12.5,8,null,null,0.143,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,13.5,8,null,null,0.276,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,14.5,8,null,null,0.227,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,15.5,8,null,null,0.098,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,16.5,8,null,null,0.019,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.02230971128609,null,17.5,8,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,7.5,1,null,null,0.022,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,8.5,1,null,null,1.159,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,9.5,2,null,null,1.204,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,10.5,2,null,null,0.14,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,11.5,2,null,null,0.022,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,12.5,8,null,null,0.053,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,13.5,8,null,null,0.084,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,14.5,8,null,null,0.115,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,15.5,8,null,null,0.057,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',9.84251968503937,null,16.5,8,null,null,0.013,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,7.5,1,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,8.5,1,null,null,0.26,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,9.5,2,null,null,0.65,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,10.5,2,null,null,0.109,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,11.5,2,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,12.5,8,null,null,0.011,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,13.5,8,null,null,0.015,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,14.5,8,null,null,0.03,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,15.5,8,null,null,0.026,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',10.6627296587927,null,16.5,8,null,null,0.007,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,8.5,1,null,null,0.05,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,9.5,2,null,null,0.261,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,10.5,2,null,null,0.095,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,12.5,8,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,13.5,8,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,14.5,8,null,null,0.019,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,15.5,8,null,null,0.013,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',11.4829396325459,null,16.5,8,null,null,0.008,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,8.5,1,null,null,0.006,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,9.5,2,null,null,0.09,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,10.5,2,null,null,0.074,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,11.5,2,null,null,0.007,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,13.5,8,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,14.5,8,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,15.5,8,null,null,0.018,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,16.5,8,null,null,0.004,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',12.3031496062992,null,17.5,8,null,null,0.004,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.1233595800525,null,8.5,1,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.1233595800525,null,9.5,2,null,null,0.02,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.1233595800525,null,10.5,2,null,null,0.042,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.1233595800525,null,14.5,8,null,null,0.004,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.1233595800525,null,16.5,8,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.1233595800525,null,17.5,8,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.9435695538058,null,9.5,2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.9435695538058,null,10.5,2,null,null,0.023,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',13.9435695538058,null,11.5,2,null,null,0.002,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',14.7637795275591,null,9.5,2,null,null,0.001,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',14.7637795275591,null,10.5,2,null,null,0.009,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',14.7637795275591,null,11.5,2,null,null,0.003,1);
insert into wave_info (location_id,loading_type,hs,hmax,tp,gamma,alpha,return_period,probability,unit_id) values (4,'long-term',15.5839895013123,null,11.5,2,null,null,0.001,1);


create TYPE current_type AS ENUM ('Normal','Loop','NA' );

create table current_units(
	unit_id serial primary key not null,
	unit_info hstore not null);

insert into current_units (unit_info) values 
('depth => "m",
  current_speed => "m/s", 
  direction => "Deg CCW N",
  return_period => "years",
  probability => "%"');
 
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


INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,0,0.1028,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,25,0.1028,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,50,0.1023888,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,75,0.0934452,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,100,0.0792588,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,125,0.069904,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,200,0.0520168,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,300,0.0366996,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,400,0.02827,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,500,0.0223076,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,600,0.0178872,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,700,0.01542,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,750,0.014392,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,800,0.0124388,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,900,0.0064764,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,1000,0.08,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',1,2896,0.08,null,null,25.926616623661,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,0,0.2056,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,25,0.2056,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,50,0.2047776,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,75,0.1868904,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,100,0.1585176,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,125,0.139808,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,200,0.1040336,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,300,0.0733992,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,400,0.05654,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,500,0.0446152,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,600,0.0357744,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,700,0.03084,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,750,0.028784,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,800,0.0248776,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,900,0.0129528,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,1000,0.08,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',2,2896,0.08,null,null,22.1033700836657,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,0,0.3084,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,25,0.3084,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,50,0.3071664,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,75,0.2803356,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,100,0.2377764,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,125,0.209712,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,200,0.1560504,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,300,0.1100988,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,400,0.08481,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,500,0.0669228,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,600,0.0536616,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,700,0.04626,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,750,0.043176,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,800,0.0373164,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,900,0.0194292,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,1000,0.08,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',3,2896,0.08,null,null,14.3295801079052,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,0,0.4112,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,25,0.4112,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,50,0.4095552,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,75,0.3737808,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,100,0.3170352,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,125,0.279616,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,200,0.2080672,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,300,0.1467984,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,400,0.11308,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,500,0.0892304,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,600,0.0715488,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,700,0.06168,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,750,0.057568,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,800,0.0497552,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,900,0.0259056,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,1000,0.08,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',4,2896,0.08,null,null,6.44151223707874,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,0,0.514,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,25,0.514,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,50,0.511944,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,75,0.467226,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,100,0.396294,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,125,0.34952,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,200,0.260084,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,300,0.183498,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,400,0.14135,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,500,0.111538,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,600,0.089436,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,700,0.0771,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,750,0.07196,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,800,0.062194,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,900,0.032382,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,1000,0.08,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',5,2896,0.08,null,null,2.53436547032606,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,0,0.6168,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,25,0.6168,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,50,0.6143328,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,75,0.5606712,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,100,0.4755528,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,125,0.419424,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,200,0.3121008,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,300,0.2201976,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,400,0.16962,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,500,0.1338456,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,600,0.1073232,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,700,0.09252,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,750,0.086352,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,800,0.0746328,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,900,0.0388584,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,1000,0.08,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',6,2896,0.08,null,null,1.46536085698647,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,0,0.7196,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,25,0.7196,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,50,0.7167216,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,75,0.6541164,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,100,0.5548116,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,125,0.489328,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,200,0.3641176,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,300,0.2568972,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,400,0.19789,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,500,0.1561532,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,600,0.1252104,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,700,0.10794,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,750,0.100744,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,800,0.0870716,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,900,0.0453348,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,1000,0.08,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',7,2896,0.08,null,null,0.83032293377121,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,0,0.8224,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,25,0.8224,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,50,0.8191104,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,75,0.7475616,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,100,0.6340704,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,125,0.559232,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,200,0.4161344,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,300,0.2935968,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,400,0.22616,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,500,0.1784608,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,600,0.1430976,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,700,0.12336,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,750,0.115136,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,800,0.0995104,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,900,0.0518112,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,1000,0.08,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',8,2896,0.08,null,null,0.214090233794667,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,0,0.9252,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,25,0.9252,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,50,0.9214992,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,75,0.8410068,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,100,0.7133292,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,125,0.629136,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,200,0.4681512,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,300,0.3302964,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,400,0.25443,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,500,0.2007684,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,600,0.1609848,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,700,0.13878,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,750,0.129528,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,800,0.1119492,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,900,0.0582876,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,1000,0.08,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Normal',9,2896,0.08,null,null,0.154781452811009,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,0,0.1286,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,25,0.1286,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,50,0.1286,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,75,0.1280856,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,100,0.1168974,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,125,0.0991506,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,200,0.087448,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,300,0.0650716,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,400,0.0459102,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,500,0.035365,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,600,0.0279062,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,700,0.0223764,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,750,0.01929,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,800,0.018004,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,900,0.0155606,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,1000,0.0081018,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',10,2896,0.08,null,null,0,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,0,0.2572,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,25,0.2572,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,50,0.2572,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,75,0.2561712,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,100,0.2337948,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,125,0.1983012,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,200,0.174896,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,300,0.1301432,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,400,0.0918204,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,500,0.07073,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,600,0.0558124,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,700,0.0447528,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,750,0.03858,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,800,0.036008,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,900,0.0311212,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,1000,0.0162036,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',11,2896,0.08,null,null,1.83976608187135,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,0,0.3858,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,25,0.3858,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,50,0.3858,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,75,0.3842568,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,100,0.3506922,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,125,0.2974518,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,200,0.262344,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,300,0.1952148,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,400,0.1377306,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,500,0.106095,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,600,0.0837186,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,700,0.0671292,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,750,0.05787,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,800,0.054012,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,900,0.0466818,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,1000,0.0243054,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',12,2896,0.08,null,null,4.07347155768208,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,0,0.5144,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,25,0.5144,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,50,0.5144,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,75,0.5123424,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,100,0.4675896,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,125,0.3966024,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,200,0.349792,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,300,0.2602864,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,400,0.1836408,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,500,0.14146,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,600,0.1116248,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,700,0.0895056,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,750,0.07716,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,800,0.072016,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,900,0.0622424,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,1000,0.0324072,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',13,2896,0.08,null,null,3.95736310473153,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,0,0.643,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,25,0.643,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,50,0.643,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,75,0.640428,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,100,0.584487,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,125,0.495753,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,200,0.43724,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,300,0.325358,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,400,0.229551,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,500,0.176825,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,600,0.139531,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,700,0.111882,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,750,0.09645,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,800,0.09002,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,900,0.077803,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,1000,0.040509,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',14,2896,0.08,null,null,3.91727804359383,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,0,0.7716,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,25,0.7716,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,50,0.7716,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,75,0.7685136,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,100,0.7013844,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,125,0.5949036,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,200,0.524688,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,300,0.3904296,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,400,0.2754612,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,500,0.21219,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,600,0.1674372,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,700,0.1342584,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,750,0.11574,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,800,0.108024,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,900,0.0933636,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,1000,0.0486108,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',15,2896,0.08,null,null,3.75002658160553,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,0,0.9002,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,25,0.9002,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,50,0.9002,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,75,0.8965992,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,100,0.8182818,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,125,0.6940542,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,200,0.612136,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,300,0.4555012,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,400,0.3213714,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,500,0.247555,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,600,0.1953434,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,700,0.1566348,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,750,0.13503,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,800,0.126028,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,900,0.1089242,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,1000,0.0567126,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',16,2896,0.08,null,null,2.92206273258905,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,0,1.0288,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,25,1.0288,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,50,1.0288,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,75,1.0246848,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,100,0.9351792,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,125,0.7932048,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,200,0.699584,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,300,0.5205728,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,400,0.3672816,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,500,0.28292,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,600,0.2232496,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,700,0.1790112,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,750,0.15432,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,800,0.144032,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,900,0.1244848,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,1000,0.0648144,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',17,2896,0.08,null,null,2.14109516214779,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,0,1.1574,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,25,1.1574,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,50,1.1574,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,75,1.1527704,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,100,1.0520766,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,125,0.8923554,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,200,0.787032,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,300,0.5856444,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,400,0.4131918,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,500,0.318285,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,600,0.2511558,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,700,0.2013876,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,750,0.17361,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,800,0.162036,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,900,0.1400454,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,1000,0.0729162,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',18,2896,0.08,null,null,1.59372674109516,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,0,1.286,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,25,1.286,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,50,1.286,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,75,1.280856,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,100,1.168974,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,125,0.991506,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,200,0.87448,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,300,0.650716,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,400,0.459102,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,500,0.35365,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,600,0.279062,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,700,0.223764,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,750,0.1929,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,800,0.18004,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,900,0.155606,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,1000,0.081018,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',19,2896,0.08,null,null,1.00074428495481,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,0,1.4146,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,25,1.4146,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,50,1.4146,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,75,1.4089416,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,100,1.2858714,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,125,1.0906566,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,200,0.961928,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,300,0.7157876,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,400,0.5050122,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,500,0.389015,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,600,0.3069682,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,700,0.2461404,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,750,0.21219,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,800,0.198044,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,900,0.1711666,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,1000,0.0891198,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',20,2896,0.08,null,null,0.471345029239766,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,0,1.5432,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,25,1.5432,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,50,1.5432,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,75,1.5370272,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,100,1.4027688,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,125,1.1898072,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,200,1.049376,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,300,0.7808592,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,400,0.5509224,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,500,0.42438,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,600,0.3348744,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,700,0.2685168,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,750,0.23148,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,800,0.216048,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,900,0.1867272,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,1000,0.0972216,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',21,2896,0.08,null,null,0.192131844763424,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,0,1.6718,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,25,1.6718,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,50,1.6718,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,75,1.6651128,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,100,1.5196662,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,125,1.2889578,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,200,1.136824,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,300,0.8459308,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,400,0.5968326,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,500,0.459745,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,600,0.3627806,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,700,0.2908932,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,750,0.25077,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,800,0.234052,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,900,0.2022878,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,1000,0.1053234,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',22,2896,0.08,null,null,0.085699096225412,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,0,1.8004,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,25,1.8004,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,50,1.8004,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,75,1.7931984,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,100,1.6365636,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,125,1.3881084,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,200,1.224272,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,300,0.9110024,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,400,0.6427428,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,500,0.49511,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,600,0.3906868,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,700,0.3132696,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,750,0.27006,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,800,0.252056,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,900,0.2178484,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,1000,0.1134252,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',23,2896,0.08,null,null,0.0442317916002127,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,0,1.929,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,25,1.929,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,50,1.929,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,75,1.921284,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,100,1.753461,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,125,1.487259,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,200,1.31172,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,300,0.976074,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,400,0.688653,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,500,0.530475,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,600,0.418593,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,700,0.335646,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,750,0.28935,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,800,0.27006,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,900,0.233409,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,1000,0.121527,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (2,'long-term','Loop',24,2896,0.08,null,null,0.0110579479000532,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,0,0.2,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,10,0.2,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,20,0.16,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,50,0.144,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,100,0.102,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,125,0.086,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,150,0.074,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,200,0.058,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,300,0.06,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,400,0.056,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,450,0.05,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,500,0.046,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,627,0.04,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,800,0.042,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,1000,0.058,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,1250,0.104,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,1500,0.086,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',1,1650,0.062,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,0,0.4,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,10,0.4,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,20,0.32,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,50,0.288,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,100,0.204,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,125,0.172,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,150,0.148,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,200,0.116,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,300,0.12,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,400,0.112,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,450,0.1,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,500,0.092,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,627,0.08,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,800,0.084,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,1000,0.116,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,1250,0.208,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,1500,0.172,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',2,1650,0.124,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,0,0.6,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,10,0.6,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,20,0.48,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,50,0.432,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,100,0.306,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,125,0.258,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,150,0.222,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,200,0.174,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,300,0.18,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,400,0.168,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,450,0.15,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,500,0.138,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,627,0.12,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,800,0.126,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,1000,0.174,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,1250,0.312,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,1500,0.258,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',3,1650,0.186,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,0,0.8,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,10,0.8,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,20,0.64,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,50,0.576,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,100,0.408,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,125,0.344,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,150,0.296,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,200,0.232,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,300,0.24,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,400,0.224,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,450,0.2,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,500,0.184,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,627,0.16,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,800,0.168,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,1000,0.232,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,1250,0.416,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,1500,0.344,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',4,1650,0.248,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,0,1,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,10,1,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,20,0.8,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,50,0.72,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,100,0.51,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,125,0.43,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,150,0.37,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,200,0.29,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,300,0.3,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,400,0.28,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,450,0.25,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,500,0.23,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,627,0.2,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,800,0.21,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,1000,0.29,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,1250,0.52,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,1500,0.43,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',5,1650,0.31,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,0,1.2,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,10,1.2,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,20,0.96,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,50,0.864,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,100,0.612,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,125,0.516,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,150,0.444,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,200,0.348,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,300,0.36,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,400,0.336,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,450,0.3,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,500,0.276,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,627,0.24,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,800,0.252,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,1000,0.348,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,1250,0.624,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,1500,0.516,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',6,1650,0.372,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,0,1.4,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,10,1.4,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,20,1.12,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,50,1.008,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,100,0.714,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,125,0.602,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,150,0.518,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,200,0.406,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,300,0.42,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,400,0.392,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,450,0.35,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,500,0.322,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,627,0.28,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,800,0.294,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,1000,0.406,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,1250,0.728,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,1500,0.602,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',7,1650,0.434,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,0,1.6,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,10,1.6,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,20,1.28,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,50,1.152,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,100,0.816,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,125,0.688,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,150,0.592,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,200,0.464,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,300,0.48,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,400,0.448,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,450,0.4,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,500,0.368,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,627,0.32,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,800,0.336,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,1000,0.464,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,1250,0.832,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,1500,0.688,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (4,'long-term','NA',8,1650,0.496,null,null,0.05,1);

INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,0,0.103,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,11.4,0.103,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,28.5,0.0686666666666667,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,113.9,0.0480666666666667,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,284.8,0.0274666666666667,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,569.6,0.0274666666666667,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,854.4,0.0274666666666667,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',1,1367,0.0274666666666667,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,0,0.146,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,11.4,0.146,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,28.5,0.0973333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,113.9,0.0681333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,284.8,0.0389333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,569.6,0.0389333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,854.4,0.0389333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',2,1367,0.0389333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,0,0.21,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,11.4,0.21,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,28.5,0.14,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,113.9,0.098,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,284.8,0.056,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,569.6,0.056,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,854.4,0.056,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',3,1367,0.056,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,0,0.264,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,11.4,0.264,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,28.5,0.176,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,113.9,0.1232,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,284.8,0.0704,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,569.6,0.0704,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,854.4,0.0704,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',4,1367,0.0704,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,0,0.315,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,11.4,0.315,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,28.5,0.21,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,113.9,0.147,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,284.8,0.084,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,569.6,0.084,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,854.4,0.084,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',5,1367,0.084,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,0,0.365,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,11.4,0.365,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,28.5,0.243333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,113.9,0.170333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,284.8,0.0973333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,569.6,0.0973333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,854.4,0.0973333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',6,1367,0.0973333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,0,0.419,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,11.4,0.419,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,28.5,0.279333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,113.9,0.195533333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,284.8,0.111733333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,569.6,0.111733333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,854.4,0.111733333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',7,1367,0.111733333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,0,0.478,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,11.4,0.478,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,28.5,0.318666666666667,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,113.9,0.223066666666667,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,284.8,0.127466666666667,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,569.6,0.127466666666667,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,854.4,0.127466666666667,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',8,1367,0.127466666666667,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,0,0.551,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,11.4,0.551,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,28.5,0.367333333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,113.9,0.257133333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,284.8,0.146933333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,569.6,0.146933333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,854.4,0.146933333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',9,1367,0.146933333333333,null,null,10,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,0,0.597,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,11.4,0.597,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,28.5,0.398,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,113.9,0.2786,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,284.8,0.1592,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,569.6,0.1592,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,854.4,0.1592,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',10,1367,0.1592,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,0,0.656,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,11.4,0.656,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,28.5,0.437333333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,113.9,0.306133333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,284.8,0.174933333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,569.6,0.174933333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,854.4,0.174933333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',11,1367,0.174933333333333,null,null,5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,0,0.695,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,11.4,0.695,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,28.5,0.463333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,113.9,0.324333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,284.8,0.185333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,569.6,0.185333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,854.4,0.185333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',12,1367,0.185333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,0,0.746,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,11.4,0.746,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,28.5,0.497333333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,113.9,0.348133333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,284.8,0.198933333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,569.6,0.198933333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,854.4,0.198933333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',13,1367,0.198933333333333,null,null,2.5,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,0,0.805,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,11.4,0.805,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,28.5,0.536666666666667,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,113.9,0.375666666666667,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,284.8,0.214666666666667,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,569.6,0.214666666666667,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,854.4,0.214666666666667,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',14,1367,0.214666666666667,null,null,2,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,0,0.849,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,11.4,0.849,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,28.5,0.566,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,113.9,0.3962,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,284.8,0.2264,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,569.6,0.2264,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,854.4,0.2264,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',15,1367,0.2264,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,0,0.919,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,11.4,0.919,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,28.5,0.612666666666667,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,113.9,0.428866666666667,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,284.8,0.245066666666667,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,569.6,0.245066666666667,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,854.4,0.245066666666667,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',16,1367,0.245066666666667,null,null,1,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,0,0.947,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,11.4,0.947,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,28.5,0.631333333333333,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,113.9,0.441933333333333,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,284.8,0.252533333333333,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,569.6,0.252533333333333,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,854.4,0.252533333333333,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',17,1367,0.252533333333333,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,0,0.984,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,11.4,0.984,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,28.5,0.656,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,113.9,0.4592,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,284.8,0.2624,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,569.6,0.2624,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,854.4,0.2624,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',18,1367,0.2624,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,0,1.045,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,11.4,1.045,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,28.5,0.696666666666667,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,113.9,0.487666666666667,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,284.8,0.278666666666667,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,569.6,0.278666666666667,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,854.4,0.278666666666667,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',19,1367,0.278666666666667,null,null,0.25,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,0,1.119,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,11.4,1.119,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,28.5,0.746,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,113.9,0.5222,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,284.8,0.2984,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,569.6,0.2984,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,854.4,0.2984,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',20,1367,0.2984,null,null,0.15,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,0,1.173,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,11.4,1.173,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,28.5,0.782,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,113.9,0.5474,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,284.8,0.3128,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,569.6,0.3128,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,854.4,0.3128,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (5,'long-term','NA',21,1367,0.3128,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,0,0.2,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,10,0.2,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,20,0.16,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,50,0.144,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,100,0.102,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,125,0.086,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,150,0.074,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,200,0.058,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,300,0.06,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,400,0.056,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,450,0.05,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,500,0.046,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,627,0.04,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,800,0.042,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,1000,0.058,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,1250,0.104,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,1500,0.086,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',1,1650,0.062,null,null,11.931,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,0,0.4,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,10,0.4,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,20,0.32,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,50,0.288,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,100,0.204,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,125,0.172,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,150,0.148,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,200,0.116,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,300,0.12,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,400,0.112,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,450,0.1,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,500,0.092,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,627,0.08,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,800,0.084,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,1000,0.116,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,1250,0.208,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,1500,0.172,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',2,1650,0.124,null,null,31.025,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,0,0.6,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,10,0.6,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,20,0.48,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,50,0.432,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,100,0.306,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,125,0.258,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,150,0.222,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,200,0.174,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,300,0.18,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,400,0.168,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,450,0.15,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,500,0.138,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,627,0.12,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,800,0.126,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,1000,0.174,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,1250,0.312,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,1500,0.258,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',3,1650,0.186,null,null,29.645,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,0,0.8,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,10,0.8,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,20,0.64,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,50,0.576,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,100,0.408,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,125,0.344,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,150,0.296,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,200,0.232,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,300,0.24,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,400,0.224,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,450,0.2,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,500,0.184,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,627,0.16,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,800,0.168,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,1000,0.232,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,1250,0.416,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,1500,0.344,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',4,1650,0.248,null,null,17.183,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,0,1,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,10,1,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,20,0.8,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,50,0.72,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,100,0.51,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,125,0.43,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,150,0.37,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,200,0.29,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,300,0.3,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,400,0.28,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,450,0.25,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,500,0.23,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,627,0.2,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,800,0.21,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,1000,0.29,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,1250,0.52,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,1500,0.43,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',5,1650,0.31,null,null,6.799,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,0,1.2,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,10,1.2,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,20,0.96,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,50,0.864,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,100,0.612,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,125,0.516,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,150,0.444,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,200,0.348,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,300,0.36,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,400,0.336,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,450,0.3,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,500,0.276,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,627,0.24,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,800,0.252,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,1000,0.348,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,1250,0.624,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,1500,0.516,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',6,1650,0.372,null,null,2.407,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,0,1.4,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,10,1.4,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,20,1.12,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,50,1.008,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,100,0.714,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,125,0.602,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,150,0.518,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,200,0.406,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,300,0.42,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,400,0.392,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,450,0.35,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,500,0.322,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,627,0.28,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,800,0.294,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,1000,0.406,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,1250,0.728,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,1500,0.602,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',7,1650,0.434,null,null,0.96,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,0,1.6,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,10,1.6,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,20,1.28,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,50,1.152,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,100,0.816,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,125,0.688,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,150,0.592,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,200,0.464,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,300,0.48,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,400,0.448,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,450,0.4,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,500,0.368,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,627,0.32,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,800,0.336,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,1000,0.464,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,1250,0.832,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,1500,0.688,null,null,0.05,1);
INSERT INTO public.current_info(
location_id, loading_type, current_type, current_profile_id, depth, current_speed, direction, return_period, probability, unit_id)
 VALUES (3,'long-term','NA',8,1650,0.496,null,null,0.05,1);
