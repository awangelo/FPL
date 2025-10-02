def bufSize : USize := 20 * 1024

def helpMessage : String :=
"feline - concatenate files and print on the standard output

USAGE:
    feline [FILE]...

ARGUMENTS:
    [FILE]...    Files to concatenate. Use '-' or no arguments to read from stdin.

OPTIONS:
    --help       Show this help message

EXAMPLES:
    feline file1.txt file2.txt    Concatenate and display file1.txt and file2.txt
    feline -                      Read from standard input
    feline                        Read from standard input (same as above)
    cat file.txt | feline         Read from standard input via pipe

If no files are specified, feline reads from standard input.
If a file does not exist, an error message is printed to stderr and processing continues with remaining files."

partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let block ← stream.read bufSize
  if block.isEmpty then
    pure ()
  else
    let stdout ← IO.getStdout
    stdout.write block
    dump stream

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

def process (exitCode : UInt32) (args : List String) : IO UInt32 := do
  match args with
  | [] => pure exitCode
  | "-" :: args =>
    let stdin ← IO.getStdin
    dump stdin
    process exitCode args
  | fileName :: args =>
    let stream ← fileStream ⟨fileName⟩
    match stream with
    | none =>
      process 1 args
    | some stream =>
      dump stream
      process exitCode args

def printHelp : IO UInt32 := do
  let stdout ← IO.getStdout
  stdout.putStrLn helpMessage
  pure 0

def main : List String → IO UInt32
  | ["--help"]     => printHelp
  | []             => process 0 ["-"]
  | args           => process 0 args
