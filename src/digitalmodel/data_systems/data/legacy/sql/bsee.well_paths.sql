select distinct on ("API10")
	"API12",
     "Well Name"
	,xyz
FROM bsee.output_data_api12
WHERE
    "Field NickName" = '{}'
    and xyz is not null
ORDER BY "API10", "API12" desc;