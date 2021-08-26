## composite

[![Build Status](https://travis-ci.com/ConferOpenSource/composite.svg?branch=master)](https://travis-ci.com/ConferOpenSource/composite)

Composite is a group of libraries focusing on practical uses of composite records, in particular [Vinyl](https://github.com/VinylRecords/Vinyl/), such as querying records from a database and converting them to JSON. These libraries are based on the excellent [Frames](https://github.com/acowley/Frames) style use of Vinyl records, though composite implements its own derived from Frames to make for a smaller dependency graph, as Frames is a full CSV parsing/printing and data manipulation library.

### `composite-aeson`

`composite-aeson` provides JSON formatting facilities for records. JSON formats can be derived automatically when default formats are available, explicitly assembled, combined, or a mix. Aeson's use of `FromJSON`/`ToJSON` type classes is mostly avoided to make using JSON formats first-class while still convenient.

Example:

```haskell
{-# LANGUAGE DataKinds, OverloadedStrings, PatternSynonyms, TypeOperators #-}
import qualified Data.Aeson as Aeson
import Composite.Aeson (JsonFormat, defaultJsonFormatRecord, recordJsonFormat, toJsonWithFormat)
import Composite.Record (Record, Rec(RNil), (:->), pattern (:*:))
import Data.Text (Text)

type FId   = "id"   :-> Int
type FName = "name" :-> Text
type User = '[FId, FName]

userFormat :: JsonFormat e (Record User)
userFormat = recordJsonFormat defaultJsonFormatRecord

alice :: Record User
alice = 1 :*: "Alice" :*: RNil

aliceJson :: Aeson.Value
aliceJson = toJsonWithFormat userFormat alice
```

### `composite-aeson-path`

`composite-aeson` support for the [path](https://hackage.haskell.org/package/path) library.

### `composite-aeson-refined`

`composite-aeson` support for the [refined](https://hackage.haskell.org/package/refined) library.

### `composite-base`

Definitions shared by the other composite libraries or generally useful when using Vinyl records. Includes some Template Haskell splices to generate various optics for records, as well as a specialization of `MonadReader` which works on a context record, providing general environment for a computation.

### `composite-binary`

Instance of `Binary` from the [binary](https://hackage.haskell.org/package/binary) library for composite records.

### `composite-ekg`

Autoconfiguration of [EKG](https://hackage.haskell.org/package/ekg) from a record of EKG metrics.

### `composite-hashable`

Instance of `Hashable` from the [hashable](https://hackage.haskell.org/package/hashable) library for composite records.

### `composite-opaleye`

`composite-opaleye` provides the necessary instances to use a Vinyl record with the [opaleye](https://github.com/tomjaguarpaw/haskell-opaleye) library, letting you use records for query expressions as well as result rows.

Example:

```haskell
{-# LANGUAGE Arrows, DataKinds, FlexibleContexts, OverloadedStrings, PatternSynonyms, TemplateHaskell, TypeOperators #-}
import Control.Arrow (returnA)
import Composite.Opaleye (defaultRecTable)
import Composite.Record (Record, (:->))
import Composite.TH (withLensesAndProxies)
import Control.Lens (view)
import Data.Int (Int64)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Opaleye (Column, PGInt8, PGText, Query, Table(Table), (./=), asc, constant, orderBy, queryTable, restrict)

-- For each field type defined with, withLensesAndProxies will expand to the type, a record lens for the type,
-- and a proxy for the type, so for example FId is the type, fId is a lens which accesses the "id" field of any
-- record which contains that field, and fId_ is a proxy for the field type in case it's needed.
withLensesAndProxies [d|
  type FId   = "id"   :-> Int64
  type CId   = "id"   :-> Column PGInt8
  type FName = "name" :-> Text
  type CName = "name" :-> Column PGText
  |]

type User     = '[FId, FName]
type UserCols = '[CId, CName]

userTable :: Table (Record UserCols) (Record UserCols)
userTable = Table "users" defaultRecTable

userQuery :: Query (Record UserCols)
userQuery =
  orderBy (asc $ view cName) $ proc () -> do
    user <- queryTable userTable -< ()
    let recId = view cId user
    restrict -< recId ./= constant (1 :: Int64)
    returnA -< user
```

### `composite-swagger`

Automatic derivation of Swagger 2 (ala [swagger2](https://hackage.haskell.org/package/swagger2)) definitions for composite records.

### Related work

- [`compdoc`](https://hackage.haskell.org/package/compdoc) provides functionality for reading a Pandoc into a record.
- [`composite-dhall`](https://hackage.haskell.org/package/composite-dhall) provides `ToDhall` and `FromDhall` instances for composite records.
- [`composite-tuple`](https://hackage.haskell.org/package/composite-tuple) provides utility functions for treating composite records as tuples, ala `Relude.Extra.Tuple` from [relude](https://hackage.haskell.org/package/relude).
- [`fcf-composite`](https://hackage.haskell.org/package/fcf-composite) provides integration with first-class-families for type-level computation of records.
- [`polysemy-methodology-composite`](https://hackage.haskell.org/package/polysemy-methodology-composite) provides functions for using polysemy-methodology with composite.

### `example`

A small servant based server which uses `composite-opaleye` to pull records from the database, reshape
the record to an API type, and send the records out to the client as JSON via `composite-aeson`.

## Maturity

As of writing, we use these libraries in all our Haskell projects internally and have had no major issues. There are spots using either composite or vinyl where the compiler error messages could use improvement. There are certain use cases that can cause the simplifier to crash, though we have not observed any runtime errors as yet. They have not been proven out for performance at larger scale. We'd appreciate any fixes, improvements, or experience reports.

## Contributing

Contributions and feedback welcome! File an issue or make a PR.
