Sequel.migration do
  change do
    create_table :actors do
      primary_key :id, type: :Bignum
      String :name, null: false, collate: "identifier"
      timestamps

      index :name, unique: true
    end
  end
end
