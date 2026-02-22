Sequel.migration do
  change do
    create_table(:organizations) do
      primary_key :id, type: :Bignum
      String(
        :name,
        null: false,
        collate: "identifier"
      )
      timestamps

      index :name, unique: true
    end

    create_enum(:organization_access_level, %w[guest member admin])

    create_table(:organization_user_accesses) do
      foreign_key(
        :organization_id,
        :organizations,
        type: :bigint,
        null: false,
        on_delete: :cascade
      )
      foreign_key(
        :user_id,
        :users,
        type: :bigint,
        null: false,
        on_delete: :cascade
      )

      column(:access_level, :organization_access_level, null: false)

      primary_key(%i[organization_id user_id])

      index :user_id

      timestamps
    end

    create_table(:user_default_organizations) do
      foreign_key(
        :user_id,
        :users,
        type: :bigint,
        null: false,
        on_delete: :cascade,
        primary_key: true
      )
      foreign_key(
        :organization_id,
        :organizations,
        type: :bigint,
        null: false,
        on_delete: :cascade
      )
      index :organization_id

      timestamps
    end
  end
end
