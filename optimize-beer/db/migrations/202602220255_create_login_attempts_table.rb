Sequel.migration do
  change do
    create_table :login_attempts do
      primary_key :id, type: :Bignum
      foreign_key(
        :user_id,
        :users,
        on_delete: :cascade, type: :Bignum, null: false
      )
      String(:user_agent, null: true)
      column(:remote_ip, :inet, null: false)
      column(:attempt_at, :timestamptz, null: false)

      index %i[user_id attempt_at]
      index %i[remote_ip attempt_at]
    end
  end
end
