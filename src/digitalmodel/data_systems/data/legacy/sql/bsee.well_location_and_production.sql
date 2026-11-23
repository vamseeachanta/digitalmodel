SELECT
     "Well Name"
    ,"Wellbore Status"
    ,"Wellbore Status Date" as "Last BSEE Date"
    ,"Completion Stub Code"
    ,"Sidetrack and Bypass"
    ,ROUND(coalesce("HORZ_DEPARTURE",0) /0.3048) as "Horizontal Departure"
    ,ROUND(cast("O_CUMMULATIVE_PROD_MMBBL" as numeric), 2) as "Cumulative Oil Production to Date"
    ,ROUND("O_MEAN_PROD_RATE_BOPD") as "Mean Oil Production Rate"
    ,ROUND("SURF_x_rel" /0.3048) as "Wellhead Rel X"
    ,ROUND("SURF_y_rel"/0.3048) as "Wellhead Rel Y"
    ,'Well:' || "Well Name" || ';<br> ' || 'Cummulative Oil (MMbl):' || ROUND(cast("O_CUMMULATIVE_PROD_MMBBL" as numeric), 2) AS plot_text
FROM bsee.output_data_well
WHERE
    "Field NickName" = '{}'
    and "O_CUMMULATIVE_PROD_MMBBL" > 0
ORDER BY "Well Name";
