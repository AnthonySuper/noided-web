Sequel.migration do
  change do
    create_enum(
      :unit_category,
      %w[mass volume time count]
    )
  end
end
