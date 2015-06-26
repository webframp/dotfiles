define_browser_object_class(
    "history-url", null,
    function (I, prompt) {
        check_buffer (I.buffer, content_buffer);
        var result = yield I.buffer.window.minibuffer.read_url(
            $prompt = prompt,  $use_webjumps = false, $use_history = true, $use_bookmarks = false);
        yield co_return (result);
    },
    $hint = "choose url from history");



interactive("find-url-from-history",
    "Find a page from history in the current buffer",
    "find-url",
    $browser_object = browser_object_history_url);

interactive("find-url-from-history-new-buffer",
    "Find a page from history in the current buffer",
    "find-url-new-buffer",
    $browser_object = browser_object_history_url);



function history_clear () {
    var history = Cc["@mozilla.org/browser/nav-history-service;1"]
        .getService(Ci.nsIBrowserHistory);
    history.removeAllPages();
}
interactive("history-clear",
    "Clear the history.",
    history_clear);


/*
 * History cleanup
 */

function function_pretty_name (fn) {
    if (fn.name)
        return fn.name;
    var s = fn.toString().trimRight();
    var m = s.match(/^[^)]+\)\s*([^{;]+)$/)
    if (m)
        return m[1];
    return s;
}

define_keywords("$verbose", "$dry_run");
function history_clean (predicates) {
    keywords(arguments, $verbose = false, $dry_run = false);
    predicates = make_array(predicates);
    var npred = predicates.length;
    var predhits = [];
    var verbose = arguments.$verbose;
    var dry_run = arguments.$dry_run;
    var query = nav_history_service.getNewQuery();
    query.searchTerms = "";
    var options = nav_history_service.getNewQueryOptions();
    options.queryType = options.QUERY_TYPE_HISTORY;
    options.includeHidden = true;
    var root = nav_history_service.executeQuery(query, options).root;
    root.containerOpen = true;
    var count = root.childCount;
    if (verbose)
        dump("History items: "+count);
    var now = Date.now() / 86400000; // now in days
    var history = Cc["@mozilla.org/browser/nav-history-service;1"]
        .getService(Ci.nsIBrowserHistory);
    for (var i = count - 1; i >= 0; --i) {
        var o = root.getChild(i); // nsINavHistoryResultNode
        var age = now - o.time / 86400000000; // age in days
        for (var j = 0; j < npred; ++j) {
            var p = predicates[j];
            if (p(o.uri, age, o.accessCount)) {
                predhits[j] = (predhits[j] || 0) + 1;
                if (! dry_run)
                    history.removePage(make_uri(o.uri));
                continue;
            }
        }
    }
    if (verbose) {
        dumpln(" -> " + (count - predhits.reduce(function (a, b) a + b, 0)) +
               (dry_run ? " [DRY_RUN]" : ""));
        for (j = 0; j < npred; ++j) {
            var name = predicates[j].name;
            if (! name)
                name = function_pretty_name(predicates[j]);
            var hits = predhits[j] || 0;
            dumpln("  " + name + ": " + hits);
        }
    }
}

history_clean(function (uri, age) age > 670, $verbose);
