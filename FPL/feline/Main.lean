def bufSize : USize := 20 * 1024

partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let block ← stream.read bufSize
  if block.isEmpty then
    pure ()
  else
    let stdout ← IO.getStdout
    stdout.write block
    dump stream


def main : IO Unit :=
  IO.println s!"Hello, world!"
