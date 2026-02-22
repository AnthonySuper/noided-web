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

## Form Development Workflow

When implementing a new form, you must complete the following steps to ensure technical and aesthetic integrity:

1.  **Define the Form Type**: Create a module in `OptBeer.Form.Type`.
2.  **Implement Validation**: Create a module in `OptBeer.Form.Validate`.
3.  **Create the Renderer**: Create a module in `OptBeer.Form.Render`.
4.  **Add Translations**: Add the necessary keys to `config/translations/`.
    -   Check if common fields (like `email` or `password`) already exist in the global `form.attributes` scope before adding them to a specific form scope.
5.  **Write a Renderer Test**: Every new form renderer **must** have a corresponding spec in `test/OptBeer/Form/Render/`.
    -   Use `assertHasNoBadTranslations` to ensure all fields and labels are properly localized.
    -   This is our primary defense against "broken" UIs in production.
6.  **Write an Action Test**: Create a functional test in `test/OptBeer/Action/` to verify the form submission logic, database side-effects, and redirects.

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

### Transaction Monad (TransactM)
-   **Crucial**: `TransactM` does **not** have an instance of `MonadIO`. You cannot use `liftIO` inside a database transaction.
-   This is a deliberate design choice to ensure transactions are deterministic and can be retried safely.
-   **Timestamps**: Because you cannot call `getCurrentTime` inside `TransactM`, you must fetch the current time *before* starting the transaction and pass it in as an argument.
-   Example:
    ```haskell
    now <- getCurrentTime
    runTransaction $ do
      performActionWithTime now
    ```

### Direct Row Construction
When setting up test data or performing manual inserts, you don't always need to construct a full HKD record. You can use `WrappedRow` syntax to specify only the columns you care about.
-   Use `values_` with the label operator `:==>` and the mutation helper `mutateVal_`.
-   Chain multiple fields in a single row using the `:::%?` operator and end with `EmptyWrappedRow`.
-   **Important**: `values_` takes a `NonEmpty` list of *rows*. Even if you are only inserting one row, it must be inside a list (if `OverloadedLists` is on) or wrapped with `:| []`.
-   Example (with `OverloadedLists`):
    ```haskell
    let vals = values_
          [ #name :==> mutateVal_ (bindParam @Text "Alice")
            :::%? #email :==> mutateVal_ (bindParam @Text "alice@example.com")
            :::%? EmptyWrappedRow
          ]
    ```

### Querying Multiple Tables
When selecting from multiple tables or returning multiple values from `SelectM`:
-   Use the `:-:` operator to combine multiple HKD structures in a `select_` or `return` statement.
-   The result will be wrapped in the `:--:` data constructor for pattern matching.
-   Example:
    ```haskell
    userAndPw <- queryMaybe $ do
      user <- addFrom_ (fromBase_ usersTable)
      pw <- addFrom_ (fromBase_ userPasswordsTable)
      addWhere_ (user.id ==. pw.userId)
      return $ user :-: pw
    case userAndPw of
      Just (u :--: p) -> ...
    ```

### Range Types
-   When working with PostgreSQL ranges (like `tstzrange`), use the `Range` type from `PostgreSQL.Binary.Range`.
-   The bounds are constructed using `Incl` (inclusive) and `Excl` (exclusive).
-   Example: `Range (Incl start) (Excl end)`

### Custom Enum Types
When wrapping a Postgres ENUM type in Haskell:
1.  **Define the Data Type**: Use a simple sum type and derive `Generic`, `Show`, `Eq`, etc.
2.  **`PGType` Instance**: Provide the name of the enum type in Postgres.
3.  **`AsBindParam` Instance**: Use `EncodeNonNull` with `Hasql.Encoders.enum`.
4.  **`AsHaskellValue` Instance**: Use `Hasql.Decoders.enum`.
-   **Crucial**: Both `Enc.enum` and `Dec.enum` require the **schema** (usually `Just "public"`) and the **type name** as arguments before the mapping function.
-   Example:
    ```haskell
    instance AsBindParam MyEnum where
      bindParamEncoder = EncodeNonNull $ Enc.enum (Just "public") "my_enum_type" $ \case
        A -> "a"
        B -> "b"

    instance AsHaskellValue MyEnum where
      type HaskellTypeOf MyEnum = MyEnum
      decodeHaskellValue _ = Dec.enum (Just "public") "my_enum_type" $ \case
        "a" -> Just A
        "b" -> Just B
        _ -> Nothing
    ```

### Translation Testing
When testing renderers, we want to ensure that all required translation keys exist.
-   Use `withTranslationsInLocale` from `OptBeer.Form.Render.SpecHelper` to provide a translation environment to your specs.
-   Use `assertHasNoBadTranslations` to verify that the rendered output does not contain `<noided-bad-translation>` tags.
-   **Note**: `hspec-discover` automatically applies the `hook` in the `test/OptBeer/Form/Render/` directory, which loads translations from `config/translations`.

## Form Rendering Conventions

### Structure
-   **Model Scoping**: Use `fieldWrapModelName` to scope translation keys for an entire form (e.g., `fieldWrapModelName "CreateUser"`).
-   **Subforms**: When rendering a subform, use `subformField` to wrap the specific renderer for that HKD type.
-   **Reusability**: Extract common field layouts into helpers (e.g., a `fieldWrapper` that handles labels and error lists).

### Translations (i18n)
-   Translation files are located in `config/translations/`.
-   The loader is **recursive**, so you can organize keys into subdirectories by locale (e.g., `config/translations/en/form.yaml`).
-   Use the `pluralize` syntax for counts:
    ```yaml
    password_policy:
      min_length: "{pluralize($count) { one { At least $count character long } default { At least $count characters long } }}"
    ```

## Frontend Architecture (Vite)

-   **Source Assets**: All frontend source files (TypeScript, CSS) live in the `frontend/` directory.
-   **Built Assets**: Vite builds production assets into the `static/` directory.
    -   **Important**: Do not manually modify files in `static/`.
    -   The Haskell server uses the `FrontendAssets` effect to look up hashed filenames from `static/.vite/manifest.json` in production.
-   **Dev Server**: In development mode, the app points to the Vite dev server at `http://localhost:5173`.

## CSS Conventions

-   **Design System**: We use type-safe CSS variables defined with `@property` in `frontend/style/variables.css`.
    -   Always prefer using variables (e.g., `var(--color-primary)`) over hardcoded values.
-   **Modern Features**: We use modern CSS features like `:has()` for state-based styling.
    -   Example: `.form-field-wrapper:has(.form-field-errors)` is used to style inputs when errors are present.
-   **Color Calculations**: We use the `rgb(from ...)` syntax to derive colors (like focus rings with custom opacity) from base variables.
    -   Example: `box-shadow: 0 0 0 3px rgb(from var(--color-primary) r g b / var(--ring-opacity));`
