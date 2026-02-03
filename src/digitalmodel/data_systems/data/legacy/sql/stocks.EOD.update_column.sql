UPDATE stocks.EOD SET {0} = '{1}', status = '{2}', updated_time = NOW() WHERE ticker = '{3}'  RETURNING ticker;;
INSERT INTO stocks.EOD (ticker, {0}, status, updated_time)
       SELECT '{3}', '{1}', '{2}', NOW()
       WHERE NOT EXISTS (SELECT 1 FROM stocks.EOD WHERE ticker='{3}') RETURNING ticker;