# Introduction

An OTP application to auto compile changed files into an erlang node.

# Dependencies

`edt` uses [fs](https://github.com/synrc/fs) to be notified on files
changes and requires a platform specific `fs` backend to be installed
which can be installed like

## Mac

```
brew isntall fswatch
```

## Linux
```
<install> inotify-tools

```

# How to use

Add the following line to the `rebar3` profile of your choice

```
 {edt, "", {git, "git@github.com:raghavkarol/edt.git", {branch, "master"}}},
```

Then start and erlang node like

```
(REBAR3_PROFILE=<profile> ; rebar3 shell )
```

Now editing and changing a file will automatically reload

# Post actions

Post actions are executed on successful compile and reload of a
module. For example to run tests eunit tests in a module do:

```erlang
 1> edt_post_action:add(
    _Name = test_1, _
    Action = fun() ->
      edt_api:test(module_with_eunit_tests)
    end).

 2> edt_post_action:add(
     _Name = test_2,
     _Action = fun() ->
       edt_api:test(ct_SUITE)
     end).
```

There can be multiple post actions are are executed in the `Name` order.

# Emacs flycheck backend

To use as a flymake backend we can extend
[flycheck-rebar3](https://github.com/joedevivo/flycheck-rebar3)
available on ELPA with the following configuration.

```elisp
(flycheck-define-checker erlang-edt-flycheck
    "An Erlang syntax checker using the edt  server."
    :command ("~/bin/flycheck_edt" (eval (buffer-file-name)))
    :error-parser flycheck-parse-with-patterns-without-color
    :error-patterns
    ((warning line-start
              (file-name) ":" line ": Warning:" (message) line-end)
     (error line-start
            (file-name) ":" line ": " (message) line-end))
    :modes erlang-mode
    :enabled flycheck-rebar3-project-root
    :predicate flycheck-buffer-saved-p
    :working-directory flycheck-rebar3-project-root)

  (add-to-list 'flycheck-checkers 'erlang-edt-flycheck)

```

See [flycheck_edt](examples/flycheck_edt)
