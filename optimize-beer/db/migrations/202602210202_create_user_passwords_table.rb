Sequel.migration do
  change do
    create_table :user_passwords do
      foreign_key :user_id, :users, primary_key: true, on_delete: :cascade, type: :Bignum
      String :password_digest, null: false
      timestamps
    end
  end
end
