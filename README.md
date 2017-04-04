## composite

Composite is a group of small libraries to integrate [Vinyl](https://github.com/VinylRecords/Vinyl/) and [Frames](https://github.com/acowley/Frames) extensible and compositional records with other libraries to make them practical to use in a server setting.

### `composite-aeson`

`composite-aeson` provides JSON formatting facilities for records. JSON formats can be derived automatically when default formats are available, explicitly assembled, combined, or a mix. Aeson's use of `FromJSON`/`ToJSON` type classes is mostly avoided to make using JSON formats first-class while still convenient.

Example:

```haskell
import qualified Data.Aeson as Aeson
import Composite.Aeson (RecJsonFormat, defaultJsonFormatRec, recFormatJson)
import Composite.Record (Record, (:->), pattern (:*:), pattern Nil)

type FId   = "id"   :-> Int
type FName = "name" :-> Text
type User = '[FId, FName]

userFormat :: RecJsonFormat e User
userFormat = recFormatJson defaultJsonFormatRec

alice :: Record '[User]
alice = 1 :*: "Alice" :*: Nil

aliceJson :: Aeson.Value
aliceJson = toJsonWithFormat userFormat alice
```

### `composite-base`

Definitions shared by the other composite libraries or generally useful when using Vinyl/Frames records.

### `composite-opaleye`

`composite-opaleye` provides the necessary instances to use a Frames record with the [opaleye](https://github.com/tomjaguarpaw/haskell-opaleye) library, letting you use records for query expressions as well as result rows.

Example:

```haskell
import Composite.Opaleye (defaultRecTable)
import Composite.Record (Record, (:->))
import Composite.TH (withLensesAndProxies)
import Control.Lens (view)
import Data.Proxy (Proxy(Proxy))
import Frames ((:->), Record, rlens)
import Opaleye (Column, PGInt8, PGText, Query, Table, (./=), asc, constant, orderBy, queryTable, restrict)

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

userQuery :: Query (Record User)
userQuery =
  orderBy (asc $ view cName) $ proc () -> do
    user@(view cId -> recId) <- queryTable userTable -< ()
    restrict -< recId ./= constant 1
    returnA -< user
```

### `example`

A small servant based server which uses `composite-opaleye` to pull records from the database, reshape
the record to an API type, and send the records out to the client as JSON via `composite-aeson`.

## Contributing

Contributions and feedback welcome! File and issue or make a PR.

## Chat

Asa (@asa) and Ross (@dridus) hang out on [fpchat](https://fpchat.com).

