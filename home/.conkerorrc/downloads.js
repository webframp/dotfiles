download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

generate_filename_safely_fn = function (filename) {
    function sanitize (str) {
        return str
            .replace(/[\W ]+/g, "-")
            .replace(/^-+/, "")
            .replace(/-+$/, "")
            .toLowerCase();
    }
    var i = filename.lastIndexOf(".");
    if (i > -1) {
        var ext = filename.substring(i + 1);
        filename = filename.substring(0, i);
    }
    return sanitize(filename) +
        (ext ? ("." + sanitize(ext)) : "");
};
