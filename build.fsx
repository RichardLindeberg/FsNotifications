// --------------------------------------------------------------------------------------
// A simple FAKE build script that:
//  1) Hosts Suave server locally & reloads web part that is defined in 'app.fsx'
//  2) Deploys the web application to Azure web sites when called with 'build deploy'
// --------------------------------------------------------------------------------------

#r "packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FAKE/tools/FakeLib.dll"

open Fake
open System
open System.IO
open Suave
open Suave.Logging
open Suave.Web
open Suave.Http
open System.Net
open Microsoft.FSharp.Compiler.Interactive.Shell

// --------------------------------------------------------------------------------------
// The following uses FileSystemWatcher to look for changes in 'app.fsx'. When
// the file changes, we run `#load "app.fsx"` using the F# Interactive service
// and then get the `App.app` value (top-level value defined using `let app = ...`).
// The loaded WebPart is then hosted at localhost:8083.
// --------------------------------------------------------------------------------------
let webDir = sprintf "%s\\Notifications.WebApi" __SOURCE_DIRECTORY__
log webDir

let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()

let fsiSession = 
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
  let argv = Array.append [|"/fake/fsi.exe"; "--quiet"; "--noninteractive"; "-d:DO_NOT_START_SERVER"|] [||]
  FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream)

let reportFsiError (e:exn) =
  traceError "Reloading app.fsx script failed."
  traceError (sprintf "Message: %s\nError: %s" e.Message (sbErr.ToString().Trim()))
  sbErr.Clear() |> ignore

let reloadScript () = 
  try
    traceImportant "Reloading app.fsx script..."
    let appFsx = webDir @@ "app.fsx"
    fsiSession.EvalInteraction(sprintf "#load @\"%s\"" appFsx)
    fsiSession.EvalInteraction("open App")
    match fsiSession.EvalExpression("app") with
    | Some app -> Some(app.ReflectionValue :?> WebPart)
    | None -> failwith "Couldn't get 'app' value"
  with e -> reportFsiError e; None

// --------------------------------------------------------------------------------------
// Suave server that redirects all request to currently loaded version
// --------------------------------------------------------------------------------------

let currentApp = ref (fun _ -> async { return None })
let port = Sockets.Port.Parse <| getBuildParamOrDefault "port" "8083"

let serverConfig =
  { defaultConfig with
      homeFolder = Some webDir
      bindings = [ HttpBinding.create HTTP IPAddress.Loopback port] }

let reloadAppServer () =
  reloadScript() |> Option.iter (fun app -> 
    currentApp.Value <- app
    traceImportant "New version of app.fsx loaded!" )

Target "run" (fun _ ->
  let app ctx = currentApp.Value ctx
  let _, server = startWebServerAsync serverConfig app

  // Start Suave to host it on localhost
  reloadAppServer()
  Async.Start(server)
  // Open web browser with the loaded file
  System.Diagnostics.Process.Start("http://localhost:8083") |> ignore
  
  // Watch for changes & reload when app.fsx changes
  use watcher = {BaseDirectory = webDir; Includes = ["*.*"]; Excludes = []} |> WatchChanges (fun _ -> reloadAppServer())
  traceImportant "Waiting for app.fsx edits. Press any key to stop."
  System.Console.ReadLine() |> ignore
)

// --------------------------------------------------------------------------------------
// Minimal Azure deploy script - just overwrite old files with new ones
// --------------------------------------------------------------------------------------

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server


// Target "deploy" (fun _ ->
//   let sourceDirectory = __SOURCE_DIRECTORY__
//   let wwwrootDirectory = __SOURCE_DIRECTORY__ @@ "../wwwroot"
//   CleanDir wwwrootDirectory
//   CopyRecursive sourceDirectory wwwrootDirectory false |> ignore
// )


// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

// Target "Deploy" (fun _ ->
//     !! (buildDir + "/**/*.*")
//     -- "*.zip"
//     |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
// )

// Build order
"Clean"
  ==> "Build"
  ==> "run"


RunTargetOrDefault "Build"