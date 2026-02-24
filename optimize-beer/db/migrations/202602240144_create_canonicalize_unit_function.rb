Sequel.migration do
  up do
    execute <<~SQL
      CREATE OR REPLACE FUNCTION to_unit_category(u unit)
      RETURNS unit_category
      IMMUTABLE
      STRICT
      LANGUAGE SQL
      AS $$
        SELECT CASE u
          WHEN 'gram' THEN 'mass'::unit_category
          WHEN 'kilogram' THEN 'mass'::unit_category
          WHEN 'ounce' THEN 'mass'::unit_category
          WHEN 'pound' THEN 'mass'::unit_category
          WHEN 'milliliter' THEN 'volume'::unit_category
          WHEN 'liter' THEN 'volume'::unit_category
          WHEN 'hectoliter' THEN 'volume'::unit_category
          WHEN 'fluid_ounce' THEN 'volume'::unit_category
          WHEN 'gallon' THEN 'volume'::unit_category
          WHEN 'us_beer_barrel' THEN 'volume'::unit_category
          WHEN 'each' THEN 'count'::unit_category
          WHEN 'minute' THEN 'time'::unit_category
          WHEN 'hour' THEN 'time'::unit_category
        END;
      $$;

      COMMENT ON FUNCTION to_unit_category IS
        'returns the category of a given unit';

      CREATE OR REPLACE FUNCTION to_canonical_qty(qty NUMERIC, u unit)
      RETURNS NUMERIC(20,6)
      IMMUTABLE
      STRICT
      LANGUAGE SQL
      AS $$
        SELECT (CASE u
          WHEN 'gram' THEN qty
          WHEN 'kilogram' THEN qty * 1000
          WHEN 'ounce' THEN qty * 28.3495
          WHEN 'pound' THEN qty * 453.592
          WHEN 'milliliter' THEN qty
          WHEN 'liter' THEN qty * 1000
          WHEN 'hectoliter' THEN qty * 100000
          WHEN 'fluid_ounce' THEN qty * 29.5735
          WHEN 'gallon' THEN qty * 3785.41
          WHEN 'us_beer_barrel' THEN qty * 117348
          WHEN 'each' THEN qty
          WHEN 'minute' THEN qty
          WHEN 'hour' THEN qty * 60
        END)::NUMERIC(20,6);
      $$;

      COMMENT ON FUNCTION to_canonical_qty IS
        'converts a quantity in some unit-of-measure to its equivalent quantity in the base unit (gram, ml, each, minute)';
    SQL
  end

  down do
    execute <<~SQL
      DROP FUNCTION to_canonical_qty;
      DROP FUNCTION to_unit_category;
    SQL
  end
end
