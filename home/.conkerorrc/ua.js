// Option 1: Just say we're Firefox
session_pref("general.useragent.compatMode.firefox", true);

// Option 2: per domain policy
require("user-agent-policy");

user_agent_policy.define_policy("default",
                                user_agent_firefox(),
                                "images.google.com",
                                build_url_regexp($domain = /(.*\.)?google/, $path = /images|search\?tbm=isch/),
                                "plus.google.com");

// user_agent_firefox() comes with conkeror
function user_agent_chrome () {
    return "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36";
}

//Mozilla/5.0 (Unknown; Linux x86_64) AppleWebKit/538.1 (KHTML, like Gecko) PhantomJS/2.0.0 Safari/538.1
