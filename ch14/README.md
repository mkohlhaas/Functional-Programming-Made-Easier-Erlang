Installation Notes Erlang and Rebar3
==
[Kerl and Rebar3](https://gist.github.com/mkohlhaas/dfbd14a13a483ac086e6a55509a0df84)

Setting Up Basic Purerl Project
==
Just copy this project or:
```shell
spago init --no-comments
# add purerl backend to spago.dhall
# add /ebin/ to .gitignore
rm -rf test
rm packages.dhall
wget https://raw.githubusercontent.com/id3as/demo-ps/refs/heads/main/server/packages.dhall
wget https://raw.githubusercontent.com/mkohlhaas/PureScript-Erlang/refs/heads/main/Makefile
make
```

Check Compiler Versions
==
[Purerl version table](https://github.com/purerl/purerl?tab=readme-ov-file#versions)

```shell
purs   --version   # 0.15.14
purerl --version   # 0.0.22
which erlc         # ~/Kerl/26/bin/erlc; OTP 27 doesn't work
```
