{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
