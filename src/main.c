#include <errno.h>
#include <limits.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <h2o.h>
#include <h2o/http1.h>
#include <h2o/http2.h>
#include <lmdb.h>

static h2o_pathconf_t *register_handler(h2o_hostconf_t *hostconf, const char *path, int (*on_req)(h2o_handler_t *, h2o_req_t *))
{
    h2o_pathconf_t *pathconf = h2o_config_register_path(hostconf, path);
    h2o_handler_t *handler = h2o_create_handler(pathconf, sizeof(*handler));
    handler->on_req = on_req;
    return pathconf;
}

static int handle_migrate(h2o_handler_t *self, h2o_req_t *req)
{
  static h2o_generator_t generator = {NULL, NULL};

  if (!h2o_memis(req->method.base, req->method.len, H2O_STRLIT("GET")))
    return -1;

  h2o_iovec_t body = h2o_strdup(&req->pool, "hello world\n", SIZE_MAX);
  req->res.status = 200;
  req->res.reason = "OK";
  h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, H2O_STRLIT("text/plain"));
  h2o_start_response(req, &generator);
  h2o_send(req, &body, 1, 1);

  return 0;
}

static MDB_env *env;
static int handle_data(h2o_handler_t *self, h2o_req_t *req)
{
  struct MDB_txn *txn;
  MDB_dbi dbi;
  struct MDB_cursor *cursor;
  if (h2o_memis(req->method.base, req->method.len, H2O_STRLIT("POST")) &&
      h2o_memis(req->path_normalized.base, req->path_normalized.len, H2O_STRLIT("/data/"))) {
    printf("put\n");

    mdb_txn_begin(env, NULL, 0, &txn);
    mdb_dbi_open(txn, "test", MDB_CREATE, &dbi);
    int curres = mdb_cursor_open(txn, dbi, &cursor);
    printf("cur res: %i\n", curres);
    if (curres != 0) {
      perror("cur error?");
    }

    char *strval = "hello world";
    char *strkey = "key";

    MDB_val key = {strlen(strkey), strdup(strkey)};
    MDB_val value = {strlen(strval), strdup(strval)};

    int res = mdb_cursor_put(cursor, &key, &value, MDB_NOOVERWRITE);
    printf("res put: %i\n", res);
    if (res != 0) {
      perror("put error?");
    }

    mdb_cursor_close(cursor);
    //    mdb_dbi_close(env, dbi);
    mdb_txn_commit(txn);

    static h2o_generator_t generator = {NULL, NULL};
    req->res.status = 200;
    req->res.reason = "OK";
    h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, H2O_STRLIT("text/plain; charset=utf-8"));
    h2o_start_response(req, &generator);
    h2o_send(req, &req->entity, 1, 1);
    return 0;
  } else if (h2o_memis(req->method.base, req->method.len, H2O_STRLIT("GET")) &&
             h2o_memis(req->path_normalized.base, req->path_normalized.len, H2O_STRLIT("/data/"))) {
    int txnres = mdb_txn_begin(env, NULL, 0, &txn);
    printf("txn res: %i\n", txnres);
    mdb_dbi_open(txn, "test", MDB_CREATE, &dbi);
    int curres = mdb_cursor_open(txn, dbi, &cursor);
    printf("cur res: %i\n", curres);
    if (curres != 0) {
      perror("cur error?");
    }
    printf("get\n");

    char *strkey = "key";

    MDB_val key = {strlen(strkey), strdup(strkey)};
    MDB_val value = {0, NULL};

    int res = mdb_cursor_get(cursor, &key, &value, MDB_FIRST);
    printf("res get: %i\n", res);
    h2o_iovec_t response;
    if (res == EINVAL) {
      response.base = "einval";
    } else if (res == EACCES) {
      response.base = "eacces";
    } else if (res == MDB_TXN_FULL) {
      response.base = "full";
    } else if (res == MDB_MAP_FULL) {
      response.base = "mapfull";
    } else if (res == 0) {
      response.base = strdup(value.mv_data);
    } else {
      perror("get error?");
      response.base = "found not :(\r\n";
    }
    response.len = strlen(response.base);

    mdb_cursor_close(cursor);
    //    mdb_dbi_close(env, dbi);
    mdb_txn_commit(txn);

    static h2o_generator_t generator = {NULL, NULL};
    req->res.status = 200;
    req->res.reason = "OK";
    h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, H2O_STRLIT("text/plain; charset=utf-8"));
    h2o_start_response(req, &generator);
    h2o_send(req, &response, 1, 1);
    return 0;
  }
  printf("not found\n");

  return -1;
}

static h2o_globalconf_t config;
static h2o_context_t ctx;
static h2o_accept_ctx_t accept_ctx;

static void on_accept(uv_stream_t *listener, int status)
{
  uv_tcp_t *conn;
  h2o_socket_t *sock;

  if (status != 0)
    return;

  conn = h2o_mem_alloc(sizeof(*conn));
  uv_tcp_init(listener->loop, conn);

  if (uv_accept(listener, (uv_stream_t *)conn) != 0) {
    uv_close((uv_handle_t *)conn, (uv_close_cb)free);
    return;
  }

  sock = h2o_uv_socket_create((uv_stream_t *)conn, (uv_close_cb)free);
  h2o_accept(&accept_ctx, sock);
}

static int create_listener(void)
{
  static uv_tcp_t listener;
  struct sockaddr_in addr;
  int r;

  uv_tcp_init(ctx.loop, &listener);
  uv_ip4_addr("0.0.0.0", 7890, &addr);
  if ((r = uv_tcp_bind(&listener, (struct sockaddr *)&addr, 0)) != 0) {
    fprintf(stderr, "uv_tcp_bind:%s\n", uv_strerror(r));
    goto Error;
  }
  if ((r = uv_listen((uv_stream_t *)&listener, 128, on_accept)) != 0) {
    fprintf(stderr, "uv_listen:%s\n", uv_strerror(r));
    goto Error;
  }

  return 0;
 Error:
  uv_close((uv_handle_t *)&listener, NULL);
  return r;
}

int main(int argc, char **argv)
{
  // TODO: get dynamic host and port
  mdb_env_create(&env);
  mdb_env_set_maxdbs(env, 10);
  mdb_env_open(env, "/tmp/", 0, 700);
  h2o_hostconf_t *hostconf;

  signal(SIGPIPE, SIG_IGN);

  h2o_config_init(&config);
  hostconf = h2o_config_register_host(&config, h2o_iovec_init(H2O_STRLIT("default")), 65535);
  register_handler(hostconf, "/data", handle_data);
  register_handler(hostconf, "/migrate", handle_migrate);

  uv_loop_t loop;
  uv_loop_init(&loop);
  h2o_context_init(&ctx, &loop, &config);

  /* disabled by default: uncomment the line below to enable access logging */
  /* h2o_access_log_register(&config.default_host, "/dev/stdout", NULL); */

  accept_ctx.ctx = &ctx;
  accept_ctx.hosts = config.hosts;

  if (create_listener() != 0) {
    fprintf(stderr, "failed to listen to 0.0.0.0:7890:%s\n", strerror(errno));
    goto Error;
  }

  uv_run(ctx.loop, UV_RUN_DEFAULT);

 Error:
  mdb_env_close(env);
  return 1;
}
