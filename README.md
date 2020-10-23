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
<pkg-mgr-install> inotify-tools
```

# Using

Add the following line to the `rebar3` profile of your choice

```
 {edt, "", {git, "git@github.com:raghavkarol/edt.git", {branch, "master"}}},
```

Then start and erlang node like

```
(REBAR3_PROFILE=<profile> ; rebar3 shell )
```

Now editing and changing a file will automatically reload

# Other features

- Post actions on successful compile and reload
- Flycheck backend
