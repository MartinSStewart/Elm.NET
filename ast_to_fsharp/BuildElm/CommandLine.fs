module CommandLine

open System.Diagnostics

let run (text: string): string =
    let cmd = new Process()
    do cmd.StartInfo.FileName <- "cmd.exe"
    do cmd.StartInfo.RedirectStandardInput <- true
    do cmd.StartInfo.RedirectStandardOutput <- true
    do cmd.StartInfo.CreateNoWindow <- true
    do cmd.StartInfo.UseShellExecute <- false
    let _ = cmd.Start()

    do cmd.StandardInput.WriteLine(text)
    do cmd.StandardInput.Flush()
    do cmd.StandardInput.Close()
    do cmd.WaitForExit()
    cmd.StandardOutput.ReadToEnd()