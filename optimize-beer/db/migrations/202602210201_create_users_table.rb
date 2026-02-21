Sequel.migration do
  change do
    create_table :users do
      foreign_key :id, :actors, primary_key: true, on_delete: :cascade, type: :Bignum
      String :email, null: false, collate: "case_insensitive"
      timestamps

      index :email, unique: true
    end
  end
end
