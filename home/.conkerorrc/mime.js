external_content_handlers.set("application/pdf", "mupdf");
external_content_handlers.set("application/postscript", "gv");
external_content_handlers.set("application/x-dvi", "xdvi");
external_content_handlers.set("video/*", "urxvt -e mplayer");

// OR use Google Docs Viewer (http://docs.google.com/viewer) for pdf files
// TODO: add this as a option for content_handler_prompt (the default dialog)
// or maybe override "view internally" the option to trigger this handler
// function content_handler_doc_viewer (ctx) {
//     ctx.abort(); // abort the download
//     let uri = ctx.launcher.source.spec;
//     let docviewuri = "http://docs.google.com/viewer?url=" + encodeURI(uri);
//     ctx.frame.location = docviewuri;

//     // copy original url to clipboard
//     writeToClipboard(uri);
//     p    ctx.window.minibuffer.message("Copied: " + uri);
// }

// content_handlers.set("application/pdf", content_handler_doc_viewer);
