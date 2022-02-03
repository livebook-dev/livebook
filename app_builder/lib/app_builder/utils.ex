defmodule AppBuilder.Utils do
  @moduledoc false

  def cmd!(bin, args, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {_, 0} = System.cmd(bin, args, opts)
  end

  def shell!(command, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {_, 0} = System.shell(command, opts)
  end
end
