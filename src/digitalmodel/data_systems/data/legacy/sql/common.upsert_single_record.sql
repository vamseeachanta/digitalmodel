INSERT INTO {custom_dict[table_name]} ({custom_dict[columns]})
VALUES {custom_dict[values]}
ON CONFLICT (custom_dict[primary_key]) DO UPDATE
  SET {custom_dict[set_code_block]};

