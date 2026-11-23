UPDATE stocks.keys SET cik = '{0}', ticker = '{1}', name = '{2}', exchange= '{3}', sector = '{4}', cusip = '{5}', status = '{6}', updated_time = NOW() WHERE ticker = '{1}' RETURNING ticker;
INSERT INTO stocks.keys (cik,  ticker, name, exchange, sector, cusip, status, updated_time)
       SELECT '{0}', '{1}', '{2}', '{3}', '{4}', '{5}', '{6}', NOW()
       WHERE NOT EXISTS (SELECT 1 FROM stocks.keys WHERE ticker = '{1}') RETURNING ticker;
