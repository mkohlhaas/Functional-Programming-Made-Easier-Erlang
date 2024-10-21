{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
