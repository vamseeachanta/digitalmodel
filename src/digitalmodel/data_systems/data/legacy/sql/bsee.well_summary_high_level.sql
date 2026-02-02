SELECT
    "Company Name" as Company
    ,"Water Depth" as "Water Depth (ft)"
    ,"Well Name"
    ,"Well Purpose"
    ,"Rigs"
    ,"Side Tracks"
    ,"Spud Date"
    ,"Wellbore Status"
    ,"Wellbore Status Date"  as "Last BSEE Date"
    ,"Drilling Days" as "Well Construction Days"
    ,"Completion Days" as "Well Completion Days"
    ,"RIG_LAST_DATE_ON_WELL" as "Rig Last Date on Well"
    ,"Tree Height Above Mudline" as "Tree Height AML (ft)"
    ,"Field Name" as "BSEE Field"
    ,"API10"
FROM bsee.output_data_well
WHERE
    "Field NickName" = '{}'
ORDER BY "Well Name";
