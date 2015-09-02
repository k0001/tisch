{- | @opaleye-sot@ is a different API for the core @opaleye@
infraestructure with the following goals in mind:

* Type safety everywhere.

  While it is true that @opaleye@, by relying in the type system,
  makes it impossible to write malformed queries, it doesn't keep
  us for accidentaly referring to a wrong column, comparing two
  columns we are not supposed to compare, confusing two tables
  that happen to have the same shape, or similar scenarios.
  @opaleye-sot@, by making a heavy use of the GHC type system,
  provides an additional layer of safety that can prevent these
  undesired scenarios.

* Boilerplate removal.

  Working with @opaleye@ can get a bit boilerplatey. @opaleye-sot@
  never requires us to say the same thing more than once, and it
  provides us with generic machinery so that we can skip altogether
  the provision of the types that @opaleye@ requires.

* Maintenance.

  As a consequence of the extended type safety and small amount of
  boilerplate that @opaleye-sot@ requires, maintaining code that uses
  the @opaleye-sot@ API is easy. For example, when writing queries,
  columns are identified by their PostgreSQL name, so, if we ever
  change the name of a column in the table description, then our
  querying code using the old name will not compile.

-}
module Opaleye.SOT.Tutorial
  (
  ) where

