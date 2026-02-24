Sequel.migration do
  change do
    create_table(:items) do
      primary_key :id, type: :Bignum
      foreign_key(
        :organization_id,
        :organizations,
        type: :Bignum,
        null: false,
        on_delete: :cascade
      )
      String(:name, null: false, collate: 'identifier')
      String(:description, null: false, default: '')
      column(:default_unit, :unit, null: false)
      column(
        :measure_category, :unit_category,
        null: false,
        generated_always_as: Sequel.function(:to_unit_category, :default_unit),
        stored: true
      )
      timestamps
    end
  end
end
