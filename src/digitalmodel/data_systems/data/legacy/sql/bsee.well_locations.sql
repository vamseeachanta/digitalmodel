SELECT
     "Well Name"
    ,"Wellbore Status"
    ,"Wellbore Status Date" as "Last BSEE Date"
    ,"Completion Stub Code"
    ,ROUND(coalesce("HORZ_DEPARTURE",0) /0.3048) as "Horizontal Departure"
    ,ROUND(coalesce("SURF_x_rel",0) /0.3048) as "Wellhead Rel X"
    ,ROUND(coalesce("SURF_y_rel",0) /0.3048) as "Wellhead Rel Y"
    ,ROUND(coalesce("BOT_x_rel",0) /0.3048) as "Bottom Hole Rel X"
    ,ROUND(coalesce("BOT_y_rel",0) /0.3048) as "Bottom Hole Rel Y"
    ,"Surface Latitude"
    ,"Surface Longitude"
    ,"Bottom Latitude"
    ,"Bottom Longitude"
FROM bsee.output_data_well
WHERE
    "Field NickName" = '{}'
ORDER BY "Well Name";
