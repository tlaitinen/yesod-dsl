# enterdsl - Enterprise DSL

A domain specific language and a code generator desined to create RESTful
web-based administration systems for managing a MongoDB database. 

The generated server-side Haskell code implements a JSON web service that provides:
 * data versioning
 * data validation
 * per-user-limited-database-views
 * per-document and per-field user rights management
 * file storage and retrieval in GridFS

The generated client-side Javascript code implements a reactive user interface that provides:
 * three layer navigation structure (pages, sub-pages, tabs)
 * displaying only pages and fields that the user can see
 * displaying db-views in tables whose rows can be ordered, selected and searched using different filters
 * database-document editors with fast field validation
 * multiple file upload
 * internationalization support

## Building blocks
The generated code uses:
 * Yesod web framework
 * MongoDB database
 * EmberJS framework

## Status
 * work in progress

## License
 * This library is distributed under the terms of [Simplified BSD license](LICENSE)
