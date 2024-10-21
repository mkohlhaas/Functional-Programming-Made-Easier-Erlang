{ name = "my-project"
, dependencies =
  [ "console"
  , "contravariant"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "prelude"
  , "profunctor"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
