# yesod-dsl

A domain specific language and a code generator desined to create RESTful
JSON-only web services for managing an SQL database with [Yesod web
framework](http://www.yesodweb.com/),
[Persistent](http://www.yesodweb.com/book/persistent), and
[Esqueleto](http://hackage.haskell.org/package/esqueleto-1.2).

## Features
 * generates a Yesod 1.2-compatible subsite
 * generates boilerplate code for entity validation
 * supports following field types : Word32, Word64, Int32, Int64, Text, Bool, Double, TimeOfDay, Day, UTCTime, ZonedTime
 * generates filtering and sorting code compatible with ExtJS grids
 * can
 * generates support code for implementing polymorphic relations and accessing common fields

## License
 * The code generator is distributed under the terms of [Simplified BSD license](LICENSE)



