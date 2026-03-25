SELECT a.ticker, a.insider insider_info, a.summary, a.breakout_trends, a.updated_time, a.option option_analysis, a.ta, a.status, e.yf daily_data , i.yf institution_data, k.name, k.sector
FROM stocks.analysis a
left outer join stocks.eod e on e.ticker = a.ticker
left outer join stocks.institution i on e.ticker = i.ticker
left outer join stocks.keys k on e.ticker = k.ticker
WHERE a.ticker = '{0}';
