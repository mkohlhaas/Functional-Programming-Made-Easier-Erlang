{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
