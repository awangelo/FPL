def bufSize : USize := 20 * 1024

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

def main : List String → IO UInt32
  | []   => process 0 ["-"]
  | args => process 0 args
