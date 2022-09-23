import Cocoa
import Cxx

func startRelease(_ input : String) {
    let appdir = "\(FileManager.default.currentDirectoryPath)/rel/"
    let info = "\(appdir)releases/start_erl.data"
    do {
        let versions = try String(contentsOfFile: info)
        let rel = versions.components(separatedBy: .whitespaces)[1]
        let erts = versions.components(separatedBy: .whitespaces)[0]
        setEnv(name: "BINDIR", value: "\(appdir)erts-\(erts)/bin")    
        setEnv(name: "RELEASE_SYS_CONFIG", value: "\(appdir)releases/\(rel)/sys")
    } catch {
        print("Startup failed: couldn't locate \(info)")
    }

    setEnv(name: "RELEASE_ROOT", value: appdir)    
    let ret = start_erlang(appdir, appdir)
    print("Ret: " + String(cString: ret!))
}

func setEnv(name: String, value: String) {
    print("setenv \(name) \(value)")
    setenv(name, value, 1)
}

let fm = FileManager.default
let appName = Bundle.main.object(forInfoDictionaryKey: "CFBundleDisplayName") as! String
let home = NSHomeDirectory()
let logPath = "\(home)/Library/Logs/\(appName).log"
if !fm.fileExists(atPath: logPath) { fm.createFile(atPath: logPath, contents: Data()) }
let logFile = FileHandle(forUpdatingAtPath: logPath)!
logFile.seekToEndOfFile()

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.run()
