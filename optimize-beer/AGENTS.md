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

-   **Ambiguous Field Names**: Many tables and forms share common field names (e.g., `id`, `name`, `email`).
    -   **Always** use `{-# LANGUAGE NoFieldSelectors #-}` and `{-# LANGUAGE OverloadedRecordDot #-}` in modules that work with multiple HKD types.
    -   Use `DuplicateRecordFields` to allow multiple types to define the same field names.

## Testing Best Practices

### Database Isolation
To prevent test data from polluting the database, all tests should run inside a transaction that automatically rolls back.
-   Use `runDB` from `OptBeer.DB.Table.SpecHelper`, which uses `transactDryRun`.
-   **Crucial**: If a test requires database setup (e.g., pre-inserting a record), that setup **must** be done inside the same `runDB` block as the code being tested. Otherwise, the setup data will be rolled back before the test runs.
    ```haskell
    it "finds a user" $ \pool -> do
      res <- runDB (do
        _ <- querySingleRow (insertUser ...)
        performBusinessLogic -- This can see the inserted user
        ) pool
    ```

### Direct Row Construction
When setting up test data, you don't always need to construct a full HKD record (like `User { ... }`). You can use `WrappedRow` syntax to specify only the columns you care about.
-   Use `values_` with the label operator `:==>` and the mutation helper `mutateVal_`.
-   Example:
    ```haskell
    let insertActor = insertReturning actorsTable
          (values_ ((#name :==> mutateVal_ (bindParam @Text "Alice") :::%? EmptyWrappedRow) :| [])
          (\row -> row.id)
    ```
-   `values_` takes a `Data.List.NonEmpty.NonEmpty`, so it is sometimes easier to turn on the `OverloadedLists`
    extension so that you can use a list literal to build its argument.
