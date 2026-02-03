SELECT
    "Well Name"
    ,ROUND(cast("DAYS_ON_PROD" as numeric)) as "DAYS_ON_PROD"
    ,ROUND(cast("O_CUMMULATIVE_PROD_MMBBL" as numeric), 2) as "O_CUMMULATIVE_PROD_MMBBL"
    ,ROUND(cast("O_MEAN_PROD_RATE_BOPD" as numeric), 3) as "O_MEAN_PROD_RATE_BOPD"
    , "Side Tracks"
    ,ROUND("HORZ_DEPARTURE" /0.3048) as "Horizontal Departure"
    ,'Well: ' || "Well Name" || ';<br> ' || 'Sidetracks: ' || "Side Tracks" || ';<br> ' || 'Days on Production: ' || ROUND(cast("DAYS_ON_PROD" as numeric), 0) AS plot_text
FROM bsee.output_data_well
WHERE
    "Field NickName" = '{}'
ORDER BY  "O_CUMMULATIVE_PROD_MMBBL" DESC;