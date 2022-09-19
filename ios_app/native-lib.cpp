#ifndef native_lib_hpp
#define native_lib_hpp

#include <string>
#include <thread>
#include <unistd.h>
#include <dlfcn.h>


#include <CoreFoundation/CoreFoundation.h>
extern "C" {
CF_EXPORT void CFLog(int32_t level, CFStringRef format, ...);
}

static std::string log_file;
extern "C" {
extern void erl_start(int argc, char **argv);
}

void ensure_slash(std::string& str)
{
    if (!str.empty() && str[str.size()-1] != '/') {
        str.append("/");
    }
}

#define ERROR(x) { printf(x); return x; }
const char* startErlang(std::string root_dir, std::string log_dir, const char *app_version, const char *erts_version)
{
    std::string bin_dir = getenv("BINDIR");
    // keeping it static to keep the environment variable alive
    char *path = getenv("PATH");
    // keeping it static to keep the environment variable alive
    static std::string env_path = std::string("PATH=").append(path).append(":").append(bin_dir);

    chdir(root_dir.c_str());
    putenv((char *)env_path.c_str());

    std::string config_path = root_dir + "releases/" + app_version + "/sys";
    std::string boot_path = root_dir + "releases/" + app_version + "/start";
    std::string lib_path = root_dir + "lib";
    std::string home_dir;
    std::string update_dir = root_dir + "update";
    if (const char* h = getenv("HOME")) {
        home_dir = h;
    } else {
        home_dir = root_dir + "home";
    }

    const char *args[] = {
            "test_main",
            "-sbwt",
            "none",
            // Reduced literal super carrier to 10mb because of spurious error message on 9th gen iPads
            // "erts_mmap: Failed to create super carrier of size 1024 MB"
            "-MIscs",
            "10",
            //"-Mea",
            //"min",
            "--",
            // "-init_debug",
            "-root",
            root_dir.c_str(),
            "-progname",
            "erl",
            "--",
            "-home",
            home_dir.c_str(), // Was without slash / at the end
            "--",
            "-kernel",
            "shell_history",
            "enabled",
            "--",
            // "-heart",
            // "-pa",
            // update_dir.c_str(),
            "-start_epmd",
            "false",
            //"-kernel",
            //"inet_dist_use_interface",
            //"{127,0,0,1}",
            "-elixir",
            "ansi_enabled",
            "true",
            "-noshell",
            "-s",
            "elixir",
            "start_cli",
            "-mode",
            "interactive",
            "-config",
            config_path.c_str(),
            "-boot",
            boot_path.c_str(),
            "-bindir",
            bin_dir.c_str(),
            "-boot_var",
            "RELEASE_LIB",
            lib_path.c_str(),
            "--",
            "--",
            "-extra",
            "--no-halt",
    };

    erl_start(sizeof(args) / sizeof(args[0]), (char **)args);
    return "ok";
}

extern "C" {
const char* start_erlang(const char* root, const char* home) {
    static std::string root_dir = root;
    static std::string log_dir = home;
    
    ensure_slash(root_dir);
    ensure_slash(log_dir);
    log_file = log_dir + "elixir.log";

    std::string boot_file = root_dir + "releases/start_erl.data";
    FILE *fp = fopen(boot_file.c_str(), "rb");
    if (!fp) ERROR("Could not locate start_erl.data");

    static char line_buffer[128];
    size_t read = fread(line_buffer, 1, sizeof(line_buffer) - 1, fp);
    fclose(fp);
    line_buffer[read] = 0;

    char* erts_version = strtok(line_buffer, " ");
    if (!erts_version) ERROR("Could not identify erts version in start_erl.data file");

    char* app_version = strtok(0, " ");
    if (!app_version) ERROR("Could not idenfity app version in start_erl.data file");
        
    std::thread erlang([=]{
        return startErlang(root_dir, log_dir, app_version, erts_version);
    });
    erlang.detach();
    return "ok";
}
}


#endif /* native_lib_hpp */
