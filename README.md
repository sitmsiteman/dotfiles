# Personal desktop configurations

## General setups

### tmux configuration

```
set -g history-limit 10000
```

### .desktop and shell scripts

### emacs configuration

### Plan9port

#### fonts

Use 9front font to display Hangul characters.


#### .bashrc

```
## If inside Acme...
if [ "$winid" ]; then
  ## ... then patch the `cd` command
  _cd () {
    \cd "$@" && awd
  }
  alias cd=_cd
fi
```

## NetBSD (9.x)

### Mercusys MW150US V2

