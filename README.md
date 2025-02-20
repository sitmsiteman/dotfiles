## General setups

### Plan9port

#### fonts
Use modified font to display Hangul characters.

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

