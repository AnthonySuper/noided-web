# Agent Instructions for `noided-web`

## Database Migrations (optimize-beer)

The `optimize-beer` sub-project uses a Ruby-based migration system (Sequel) instead of Haskell-native migrations.

### Preferred Workflow

1.  **Generate a migration**: Use the `script/db.rb` script.
    ```bash
    ./optimize-beer/script/db.rb gen_migration <migration_name>
    ```
    This creates a new file in `optimize-beer/db/migrations/`.

2.  **Apply migrations**: Run the `migrate` command.
    ```bash
    ./optimize-beer/script/db.rb migrate
    ```
    This will:
    - Apply migrations to both `development` and `test` databases (as configured in `config/db.yml`).
    - Automatically update `optimize-beer/db/schema.sql` by running `pg_dump`.

### Migration Conventions

-   **Timestamps**: Use the `timestamps` helper instead of manual columns. This adds `created_at` and `updated_at` with `timestamptz` and default `now()`.
-   **Collations**: Use `identifier` collation for names/identifiers where case-insensitive (but case-preserving) behavior is desired.
-   **Enums**: Use PostgreSQL enums where appropriate; the script supports the `pg_enum` extension.

### Common Pitfalls

-   **Foreign Key Types**: In Sequel migrations, `foreign_key` defaults to `Integer` (4-byte `int4`). However, our primary keys (like `actors.id`) use `Bignum` (8-byte `int8`/`bigint`) to match Haskell's `Int64`.
    -   **Always** specify `type: :Bignum` for foreign keys:
        ```ruby
        foreign_key :id, :actors, primary_key: true, type: :Bignum
        ```
    -   Failure to do this will result in `UnexpectedColumnTypeStatementError` during tests because of the size mismatch (4 vs 8 bytes).

-   **Single Column Queries**: When you need to select only a single column from a table (e.g., for existence checks), use `Element` from `Data.HKD`. This avoids the need to define a single-field HKD structure.
    -   `Noided.Sql` re-exports `Data.HKD`, so `Element` is available directly when you import `Noided.Sql`.
    -   Example:
        ```haskell
        exists <- queryMaybe $ do
          row <- addFrom_ (fromBase_ myTable)
          addWhere_ (row.someField ==. bindParam val)
          select_ $ Element row.someField
        ```
