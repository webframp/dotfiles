# ABOUTME: OpenCode - AI coding agent for the terminal
# ABOUTME: Wraps bunx invocation to avoid glibc 2.42 segfault with bun --compile binaries
{
  lib,
  stdenv,
  writeShellApplication,
  bun,
  ...
}:
writeShellApplication {
  name = "opencode";
  runtimeInputs = [bun];
  text = ''
    exec bunx opencode-ai "$@"
  '';

  meta = with lib; {
    description = "AI coding agent built for the terminal";
    homepage = "https://opencode.ai";
    license = licenses.mit;
    platforms = platforms.unix;
    mainProgram = "opencode";
  };
}
