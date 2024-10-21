{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "nonempty"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
