UPDATE stocks.analysis SET {0} = '{1}', status = '{2}', updated_time = NOW() WHERE ticker = '{3}'  RETURNING ticker;;
INSERT INTO stocks.analysis (ticker, {0}, status, updated_time)
       SELECT '{3}', '{1}', '{2}', NOW()
       WHERE NOT EXISTS (SELECT 1 FROM stocks.analysis WHERE ticker='{3}') RETURNING ticker;