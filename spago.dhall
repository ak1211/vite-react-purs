{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "apexcharts"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "dom-filereader"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "integers"
  , "js-bigints"
  , "maybe"
  , "newtype"
  , "options"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "refs"
  , "routing-duplex"
  , "strings"
  , "transformers"
  , "tuples"
  , "unique"
  , "unsafe-coerce"
  , "web-dom"
  , "web-file"
  , "web-html"
  , "web-router"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
