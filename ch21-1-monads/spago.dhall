{ name = "my-project"
, dependencies =
  [ "console", "effect", "integers", "maybe", "prelude", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
