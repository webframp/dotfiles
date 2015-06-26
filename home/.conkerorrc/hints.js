// Solarized Dark hint colors:
hint_background_color = "#cb4b16";
active_hint_background_color = "#859900";

register_user_stylesheet(
    "data:text/css," +
        escape(
            "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
                "span.__conkeror_hint {\n"+
                "  font-size: 12px !important;\n"+
                "  line-height: 12px !important;\n"+
                "}"));

// base2 fg and blue bg
register_user_stylesheet(
    "data:text/css," +
        escape (
            "span.__conkeror_hint {" +
                " border: 1px solid #dddddd !important;" +
                " color: #eee8d5 !important;" +
                " background-color: #268bd2 !important;" +
                "}"));
