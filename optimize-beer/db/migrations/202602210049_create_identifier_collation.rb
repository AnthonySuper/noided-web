Sequel.migration do
  up do
    execute <<~SQL
      CREATE COLLATION identifier (
        provider = icu,
        locale = 'und-u-ks-level1',
        deterministic = false
      );
    SQL
  end

  down do
    execute <<~SQL
      DROP COLLATION identifier;
    SQL
  end
end
