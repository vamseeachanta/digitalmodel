SELECT
     "Well Name"
    ,"Rigs"
    ,"Wellbore Status Date" as "Last BSEE Date"
    ,"Water Depth"
    ,"Total Measured Depth"
    ,"Total Vertical Depth"
    ,coalesce(drilling_footage_ft,0) as "Drilling Footage"
    ,coalesce("Drilling Days", 0) as "Drilling Days"
    ,coalesce(drilling_days_per_10000_ft,0) as "Drilling Days per 10,000 ft"
    ,"Completion Days"
    ,"Spud Date"
    ,coalesce("Total Depth Date", '1900-01-01') as "Total Depth Date"
    ,coalesce("RIG_LAST_DATE_ON_WELL", '1900-01-01') as "Rig Last Date on Well"
    ,coalesce("MAX_DRILL_FLUID_WGT",0) as "Max Mud Weight"
FROM bsee.output_data_well
WHERE
    "Field NickName" = '{}'
ORDER BY "Well Name";
