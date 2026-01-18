#include <mongoose.h>

static void ev_handler(struct mg_connection *c, int ev, void *ev_data) {
    if (ev == MG_EV_HTTP_MSG) {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;

        if (mg_match(hm->uri, mg_str("/html"), NULL)) {
            mg_http_reply(c, 200, "Content-Type: text/html\r\n",
                          "<!DOCTYPE html><html><head><meta "
                          "charset=\"utf-8\"></head><body>hello</body></html>");
        } else {
            struct mg_http_serve_opts opts = {.root_dir = "."};
            mg_http_serve_dir(c, hm, &opts);
        }
    }
}

int main() {
    mg_log_set(MG_LL_ERROR);
    struct mg_mgr mgr;
    mg_mgr_init(&mgr);
    mg_http_listen(&mgr, "http://localhost:8000", ev_handler, NULL);

    while (true) {
        mg_mgr_poll(&mgr, 1000);
    }

    return 0;
}
