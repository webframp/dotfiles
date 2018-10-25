function dnsrecords
    command dig +nocmd $argv any +multiline +noall +answer @8.8.8.8
end
