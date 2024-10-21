{ name = "my-project"
, dependencies = [ "console", "effect", "lists", "maybe", "prelude", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
