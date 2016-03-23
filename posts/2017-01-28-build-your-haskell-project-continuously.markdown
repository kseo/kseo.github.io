---
title: Build your Haskell project continuously
tags: build, stack, ghcid, steeloverseer
author: Kwang Yul Seo
---
Today I am going to introduce handy tools which help you build your Haskell project continuously so that you can see the list of errors and warnings quickly as you program.

<!--more-->

# Stack

`stack build` command has `--file-watch` option. When turned on, *stack* watches for changes in local files and automatically rebuild.

```
stack build --file-watch
```

Use `--fast` option if you want fast build which turns off optimizations (`-O0`). Also use `--pedantic` flag if you want to fix all warnings(`-Wall` and `-Werror`).

```
stack build --file-watch --fast --pedantic
```

# ghcid

Neil Mitchell's [ghcid][ghcid] provides a similar functionality in a different way. It runs *GHCi* as a daemon and runs `:reload` whenever your source code changes.

`ghcid` executes `stack ghci` by default if you have `stack.yaml` file and `.stack-work` directory.

```
ghcid
```

If you would like to give a custom command, use `--command` option.

```
ghcid "--command=ghci Main.hs"
```

`ghcid` is much faster than `stack build` because it uses *GHCi*.

# Steel Overseer

If you want to run arbitrary commands when arbitrary files change, use [Steel Overseer][steeloverseer] instead. You can specify the pattern and commands in `.sosrc` file using YAML syntax. The following example has two rules.

* Watch `*.hs` files under `System` directory and run `stack build`.
* Watch `*.hs` files under `test` directory and run `stack test`.

```yaml
- pattern: src/(.*)\.hs
  commands:
  - stack build
- pattern: test/(.*)\.hs
  commands:
  - stack test
```

`sos` command watches the specified files and runs the corresponding commands.

```
sos
```

# Wrap-up

These small tools greatly increase your productivity. Please choose one and enjoy instant feedback!

[ghcid]: https://github.com/ndmitchell/ghcid
[steeloverseer]: https://github.com/schell/steeloverseer
