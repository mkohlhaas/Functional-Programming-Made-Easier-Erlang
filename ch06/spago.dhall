{ name = "my-project"
, dependencies =
  [ "console", "effect", "lists", "maybe", "newtype", "prelude", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
