alias df	df -k
alias du	du -k
alias f		finger
alias h		'history -r | more'
alias j		jobs -l
alias la	ls -a
alias lf	ls -FA
alias ll	ls -lsA
alias tset	'set noglob histchars=""; eval `\tset -s \!*`; unset noglob histchars'
alias z		suspend

setenv	EDITOR	vim

# vi settings: set show-match auto-indent always-redraw shift-width=4
#setenv	EXINIT	"se sm ai redraw sw=4"

setenv	VISUAL	${EDITOR}

setenv	PAGER	more

#setenv	PRINTER	change-this-to-a-printer

# Set the search path for programs.
set path = (~/bin /bin /sbin /usr/{bin,sbin,X11R7/bin,pkg/{,s}bin,games} \
	    /usr/local/{,s}bin)

if ($?prompt) then
	# An interactive shell -- set some stuff up

	# Filename completion.
	set filec
	set autolist

	# Size of the history buffer.
	set history = 1000
	set savehist = (10000 merge)
	set histfile = $HOME/.cshhistory

	# Do not exit on EOF condition (e.g. ^D typed)
	# (disabled by default, not default behavior)
	set ignoreeof

	# Set the location of your incoming email for mail notification.
	set mail = (/var/mail/$USER)

	# Set the prompt to include the hostname.
	set mch = `hostname -s`
	alias prompt 'set prompt = "$mch:q"":$cwd:t {\!} "'
	alias cd 'cd \!*; prompt'
	alias chdir 'cd \!*; prompt'
	alias popd 'popd \!*; prompt'
	alias pushd 'pushd \!*; prompt'
	cd .
	umask 022

endif
