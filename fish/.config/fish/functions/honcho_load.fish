function honcho_load --description "Load HONCHO_API_KEY from 1Password"
    set -gx HONCHO_API_KEY (op read 'op://Personal/Honcho/credential' 2>/dev/null)
end
