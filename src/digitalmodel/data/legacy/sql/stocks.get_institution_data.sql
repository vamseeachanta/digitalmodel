SELECT a.ticker ,
       a.name ,
       a.summary ,
       a.quarter_end_date ,
       a.signaturedate ,
       a.updated_time ,
       a.sec ,
       a.yf ,
       a.status ,
       1 AS query_id
FROM stocks.institution a
WHERE ticker ='{0}'
