Sequel.migration do
  change do
    create_table :sessions do
      primary_key :id, type: :Bignum
      foreign_key(
        :user_id,
        :users,
        on_delete: :cascade, type: :Bignum, null: false
      )
      String(:user_agent, null: true)
      column(:remote_ip, :inet, null: false)
      column(:valid_during, :tstzrange, null: false)

      index :user_id
      index :valid_during, type: :gist

      timestamps

    end
  end
end
