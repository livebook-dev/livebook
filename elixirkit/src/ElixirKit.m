#import <Foundation/Foundation.h>
#import <erl_nif.h>

#define ERTS_VERSION "13.1.2"

extern void erl_start(int argc, char **argv);

void ElixirKitStartRelease()
{
    NSBundle* bundle = [NSBundle mainBundle];
    NSString* rootdir = [NSString stringWithFormat:@"%@/Contents/Resources/rel", [bundle bundlePath]];
    NSString* version = [bundle objectForInfoDictionaryKey:@"CFBundleVersion"];
    NSString* homedir = NSHomeDirectory();
    NSString* bindir = [NSString stringWithFormat:@"%@/erts-%s/bin", rootdir, ERTS_VERSION];
    NSString* sysconfig = [NSString stringWithFormat:@"%@/releases/%@/sys", rootdir, version];
    NSString* bootdir = [NSString stringWithFormat:@"%@/releases/%@/start", rootdir, version];
    NSString* libdir = [NSString stringWithFormat:@"%@/lib", rootdir];

    setenv("BINDIR", [bindir UTF8String], 1);
    setenv("RELEASE_ROOT", [rootdir UTF8String], 1);
    setenv("RELEASE_SYS_CONFIG", [sysconfig UTF8String], 1);

    NSString* additionalPathsPath = [bundle pathForResource:@"additional_paths" ofType:nil inDirectory:@"rel"];
    NSString* additionalPathsString = [NSString stringWithContentsOfFile:additionalPathsPath encoding:NSUTF8StringEncoding error:NULL];
    NSArray *additionalPaths = [additionalPathsString componentsSeparatedByString:@":"];
    NSString* finalPath = @"";
    for (NSString* path in additionalPaths) {
        if ([path hasPrefix:@"/"]) {
            finalPath = [NSString stringWithFormat:@"%@%@:", finalPath, path];
        } else {
            finalPath = [NSString stringWithFormat:@"%@%@/%@:", finalPath, rootdir, path];
        }
    }
    finalPath = [NSString stringWithFormat:@"%@%@", finalPath, [[[NSProcessInfo processInfo] environment] objectForKey:@"PATH"]];
    setenv("PATH", [finalPath UTF8String], 1);

    const char *args[] = {
        "app",
        "-sbwt",
        "none",
        "--",
        "-root",
        [rootdir UTF8String],
        "--",
        "-home",
        [homedir UTF8String],
        "-bindir",
        [bindir UTF8String],
        "-config",
        [sysconfig UTF8String],
        "-boot",
        [bootdir UTF8String],
        "-boot_var",
        "RELEASE_LIB",
        [libdir UTF8String],
        "-noshell",
        "-elixir",
        "ansi_enabled",
        "true",
        "-s",
        "elixir",
        "start_cli",
        "-mode",
        "interactive",
        "--",
        "-extra",
        "--no-halt",
    };

    erl_start(sizeof(args) / sizeof(args[0]), (char **)args);
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
