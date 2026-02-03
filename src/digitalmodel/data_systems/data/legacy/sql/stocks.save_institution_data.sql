UPDATE stocks.institution SET cik = '{0}', name = '{1}', summary = '{2}', quarter_end_date = '{3}', signatureDate= '{4}', data = '{5}', status = '{6}', updated_time = NOW() WHERE cik = '{0}' and quarter_end_date = '{3}' RETURNING cik;
INSERT INTO stocks.institution (cik,  name, summary, quarter_end_date, signatureDate, data, status, updated_time)
       SELECT '{0}', '{1}', '{2}', '{3}', '{4}', '{5}', '{6}', NOW()
       WHERE NOT EXISTS (SELECT 1 FROM stocks.institution WHERE cik = '{0}' and quarter_end_date = '{3}') RETURNING cik;
