module CommandLine

open System.Diagnostics
open System.Threading

let run (text: string): string =
    let cmd = new Process()
    do cmd.StartInfo.FileName <- "cmd.exe"
    do cmd.StartInfo.RedirectStandardInput <- true
    do cmd.StartInfo.RedirectStandardOutput <- true
    do cmd.StartInfo.CreateNoWindow <- false
    do cmd.StartInfo.UseShellExecute <- false
    let _ = cmd.Start()

    do cmd.StandardInput.WriteLine(text)
    do cmd.StandardInput.Flush()
    do cmd.StandardInput.Close()
    //do cmd.WaitForExit()

    // Ugly hack to get the console to return the output.
    do Thread.Sleep(1000)
    let _ = cmd.CloseMainWindow()

    cmd.StandardOutput.ReadToEnd()