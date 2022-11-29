#include <Foundation/Foundation.h>
#include <erl_nif.h>

static ERL_NIF_TERM elixirkit_nif_publish(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char name[128];
    if (!enif_get_atom(env, argv[0], name, 128, ERL_NIF_LATIN1)) {
        return enif_raise_exception(env, enif_make_atom(env, "badarg"));
    }
    NSString* nameObj = [NSString stringWithFormat:@"ElixirKit:%s", name];

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_raise_exception(env, enif_make_atom(env, "badarg"));
    }
    NSString* dataObj = [[NSString alloc] initWithBytes:bin.data length:bin.size encoding:NSUTF8StringEncoding];

    [[NSNotificationCenter defaultCenter] postNotificationName:nameObj object:dataObj];
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"publish", 2, elixirkit_nif_publish}
};

ERL_NIF_INIT(Elixir.ElixirKit.NIF, nif_funcs, NULL, NULL, NULL, NULL)
