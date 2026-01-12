SELECT
     "Well Name"
	,monthly_production
    ,'Well Departure (ft):' || ROUND("HORZ_DEPARTURE" /0.3048) || ';<br> ' || 'Cummulative Oil (MMbl):' || ROUND(cast("O_CUMMULATIVE_PROD_MMBBL" as numeric), 2) AS plot_text
FROM bsee.output_data_api12
WHERE
    "Field NickName" = '{}'
    and "O_CUMMULATIVE_PROD_MMBBL" > 0
    and monthly_production is not null
ORDER BY "Well Name";
