#import <Foundation/Foundation.h>
#import <crt_externs.h>
#import <erl_nif.h>

extern void erl_start(int argc, char **argv);

void ElixirKitStart()
{
    int argc = *_NSGetArgc();
    char** argv = *_NSGetArgv();
    erl_start(argc, argv);
}

void ElixirKitPublish(NSString* name, NSString* data)
{
    ErlNifEnv* env = enif_alloc_env();
    ErlNifPid pid;
    ERL_NIF_TERM server = enif_make_atom(env, "Elixir.ElixirKit.Server");

    if (!enif_whereis_pid(NULL, server, &pid)) {
        fprintf(stderr, "enif_whereis_pid failed\\n");
    }

    ERL_NIF_TERM tagTerm = enif_make_atom(env, "publish");
    ERL_NIF_TERM nameTerm = enif_make_atom(env, [name UTF8String]);
    ERL_NIF_TERM dataTerm = enif_make_string(env, [data UTF8String], ERL_NIF_LATIN1);
    ERL_NIF_TERM msg = enif_make_tuple3(env, tagTerm, nameTerm, dataTerm);

    if (!enif_send(NULL, &pid, env, msg)) {
        fprintf(stderr, "enif_send failed\\n");
    }
}
