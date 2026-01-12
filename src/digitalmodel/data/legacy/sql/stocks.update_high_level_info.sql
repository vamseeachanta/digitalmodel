UPDATE stocks.info SET description = '{1}', data = '{2}', updated_time = NOW() WHERE label = '{0}'  RETURNING label;
INSERT INTO stocks.info (label, description, data, updated_time)
       SELECT '{0}', '{1}', '{2}', NOW()
       WHERE NOT EXISTS (SELECT 1 FROM stocks.info WHERE label='{0}') RETURNING label;