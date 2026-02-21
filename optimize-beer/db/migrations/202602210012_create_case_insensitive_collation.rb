Sequel.migration do
  up do
    execute <<~SQL
      CREATE COLLATION case_insensitive (
        provider = icu,
        locale = 'und-u-=ks-level2',
        deterministic = false
      );
    SQL
  end

  down do
    execute <<~SQL
      DROP COLLATION case_insensitive;
    SQL
  end
end
