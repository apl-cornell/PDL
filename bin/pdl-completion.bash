 #/usr/bin/bash

_pdl()
{
    local cur=${COMP_WORDS[COMP_CWORD]}

    if [[ $cur == -* ]] ; then
	local opts="--help --out --printStages --debug --portwarn --autocast --addrLockModule --reglockModule"
	local fword=${COMP_WORDS[1]}
	if [ $fword == "interpret" ]; then
	    opts="$opts --maxIterations --memoryInput"
	elif [ $fword == "gen" ]; then
	     opts="$opts --memInit"
	fi
	COMPREPLY=( $( compgen -W "$opts" -- "$cur"))
    else
	if [ "${#COMP_WORDS[@]}" != "2" ]; then
	    COMPREPLY=( $( compgen -o plusdirs -f -X "!*.pdl" -- $cur ))
	else
	    COMPREPLY=( $( compgen -W "gen typecheck parse interpret" -- "$cur"))
	fi
    fi
}


complete -o filenames -F _pdl ./bin/pdl pdl
