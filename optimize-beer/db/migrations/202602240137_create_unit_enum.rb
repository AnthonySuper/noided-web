Sequel.migration do
  change do
    create_enum(
      :unit,
      %w[gram kilogram ounce pound
         milliliter liter hectoliter fluid_ounce gallon us_beer_barrel
         each
         minute hour]
    )
  end
end
