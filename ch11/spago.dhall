{ name = "my-project"
, dependencies = [ "console", "effect", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
