-- INCOMPLETE. Not working ---

SELECT
    WELL_NAME_UPDATED as [Well Name]
    ,[CSNG_INTV_TYPE_CD]
    ,[CSNG_HOLE_SIZE]
    ,CASE WHEN CSNG_SETTING_TOP_MD=0 then  [Water Depth] else CSNG_SETTING_TOP_MD  end as CSNG_SETTING_TOP_MD
    ,[CSNG_SETTING_BOTM_MD]
    ,[CASING_SIZE]
    ,[CASING_WEIGHT]
    ,[CASING_GRADE]
    ,[CSNG_LINER_TEST_PRSS]
    ,[CSNG_SHOE_TEST_PRSS]
    ,[CSNG_CEMENT_VOL]
    FROM
        (SELECT
            API12
            ,[CSNG_INTV_TYPE_CD]
            ,[CSNG_HOLE_SIZE]
            ,[CSNG_SETTING_TOP_MD]
            ,[CSNG_SETTING_BOTM_MD]
            ,[CASING_SIZE]
            ,[CASING_WEIGHT]
            ,[CASING_GRADE]
            ,[CSNG_LINER_TEST_PRSS]
            ,[CSNG_SHOE_TEST_PRSS]
            ,[CSNG_CEMENT_VOL]
        FROM bsee.output_casing_tubulars

        WHERE
            "Field NickName" = '{}'
        ) ) as casing_Query

        JOIN
            (SELECT [API12], [API10] FROM [master].[dbo].[output_data_api12]
            )
            AS Updated_Well_Name_api12
            on casing_Query.API12= Updated_Well_Name_api12.API12

        JOIN
            (SELECT [API10], [Water Depth], [WELL NAME] as [WELL_NAME_UPDATED] FROM [master].[dbo].[output_data_well]
            )
            AS Updated_Well_Name_QUERY
            on Updated_Well_Name_api12.API10= Updated_Well_Name_QUERY.API10

        ORDER BY [WELL_NAME_UPDATED], Updated_Well_Name_api12.API12, [CSNG_HOLE_SIZE] DESC, [CSNG_SETTING_TOP_MD]
