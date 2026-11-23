SELECT
	"PRODUCTION_DATETIME"
	, round(cast("Production Rate, BOPD" as numeric), 2) as "Production Rate, BOPD"
	, round(cast("CUMULATIVE_MONTLY_PRODUCTION_MMbbl" as numeric), 3) as "CUMULATIVE_MONTLY_PRODUCTION_MMbbl"
FROM bsee.output_production_summary
WHERE
    "Field NickName" = '{}'
    and "CUMULATIVE_MONTLY_PRODUCTION_MMbbl" > 0;