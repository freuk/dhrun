_dhall-executor.hs()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(dhall-executor.hs "${CMDLINE[@]}") )
}

complete -o filenames -F _dhall-executor.hs dhall-executor.hs
