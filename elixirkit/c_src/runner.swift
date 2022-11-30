<%

path =
  (release.options[:app][:additional_paths] || [])
  |> Enum.map(fn path ->
    if String.starts_with?(path, "/") do
      path
    else
      "\\(relDir)/#{path}"
    end
  end)
  |> Enum.concat(["\\(path)"])
  |> Enum.join(":")

%>import AppKit

let homeDir = NSHomeDirectory()
let relDir = Bundle.main.path(forResource: "rel", ofType: "")!

<%= if root_dir = release.options[:app][:root_dir] do %>
let rootDir = "\(relDir)/<%= root_dir %>"
<% else %>
let rootDir = relDir
<% end %>

let binDir = "\(rootDir)/erts-<%= release.erts_version %>/bin"
let appName = Bundle.main.infoDictionary!["CFBundleName"] as! String
let launcherPath = Bundle.main.path(forAuxiliaryExecutable: appName)!

let logPath = "\(homeDir)/Library/Logs/\(appName).log"
if !FileManager.default.fileExists(atPath: logPath) { FileManager.default.createFile(atPath: logPath, contents: Data()) }
let logFile = FileHandle(forUpdatingAtPath: logPath)!
logFile.seekToEndOfFile()

let task = Process()
task.standardOutput = logFile
task.standardError = logFile
task.launchPath = "\(relDir)/bin/app"
task.arguments = ["start"]
task.environment = ProcessInfo.processInfo.environment
let path = task.environment!["PATH"]!
task.environment!["PATH"] = "<%= path %>"
task.environment!["RELEASE_ERLEXEC"] = "\(launcherPath) -- -home \(homeDir) -root \(rootDir) -bindir \(binDir) "

task.terminationHandler = {(t: Process) in
    if t.terminationStatus != 0 {
        let alert = NSAlert()
        alert.alertStyle = .critical
        alert.messageText = "\(appName) exited with error status \(t.terminationStatus)."
        alert.informativeText = "Logs available at: \(logPath)"
        alert.runModal()
    }
}

try task.run()
task.waitUntilExit()
