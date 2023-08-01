defmodule Livebook.Utils do
  @moduledoc false

  require Logger

  @type id :: binary()

  @doc """
  Generates a random binary id.
  """
  @spec random_id() :: id()
  def random_id() do
    :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
  end

  @doc """
  Generates a random short binary id.
  """
  @spec random_short_id() :: id()
  def random_short_id() do
    :crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)
  end

  @doc """
  Generates a random cookie for a distributed node.
  """
  @spec random_cookie() :: atom()
  def random_cookie() do
    :"c_#{Base.url_encode64(:crypto.strong_rand_bytes(39))}"
  end

  @doc """
  Generates a random binary id that includes node information.

  ## Format

  The id is formed from the following binary parts:

    * 3B - random boot id
    * 16B - hashed node name
    * 9B - random bytes

  The binary is base32 encoded.
  """
  @spec random_node_aware_id() :: id()
  def random_node_aware_id() do
    boot_id = Livebook.Config.random_boot_id()
    node_part = node_hash(node())
    random_part = :crypto.strong_rand_bytes(11)
    binary = <<boot_id::binary, node_part::binary, random_part::binary>>
    # 3B + 16B + 11B = 30B is suitable for base32 encoding without padding
    Base.encode32(binary, case: :lower)
  end

  # Note: the result is always 16 bytes long
  defp node_hash(node) do
    content = Atom.to_string(node)
    :erlang.md5(content)
  end

  @doc """
  Extracts node name from the given node aware id.

  The node in question must be connected, otherwise it won't be found.
  """
  @spec node_from_node_aware_id(id()) :: {:ok, node(), boot_id :: binary()} | :error
  def node_from_node_aware_id(id) do
    case Base.decode32(id, case: :lower) do
      {:ok,
       <<boot_id::binary-size(3), node_part::binary-size(16), _random_part::binary-size(11)>>} ->
        known_nodes = [node() | Node.list()]

        Enum.find_value(known_nodes, :error, fn node ->
          node_hash(node) == node_part && {:ok, node, boot_id}
        end)

      _ ->
        :error
    end
  end

  @doc """
  Returns the host part of a node.
  """
  @spec node_host() :: binary()
  def node_host do
    [_, host] = node() |> Atom.to_string() |> :binary.split("@")
    host
  end

  @doc """
  Registers the given process under `name` for the time of `fun` evaluation.
  """
  @spec temporarily_register(pid(), atom(), (... -> any())) :: any()
  def temporarily_register(pid, name, fun) do
    Process.register(pid, name)
    fun.()
  after
    Process.unregister(name)
  end

  @doc """
  Returns a function that accesses list items by the given id.

  ## Examples

      iex> list = [%{id: 1, name: "Jake"}, %{id: 2, name: "Amy"}]
      iex> get_in(list, [Livebook.Utils.access_by_id(2), Access.key(:name)])
      "Amy"

      iex> list = [%{id: 1, name: "Jake"}, %{id: 2, name: "Amy"}]
      iex> put_in(list, [Livebook.Utils.access_by_id(2), Access.key(:name)], "Amy Santiago")
      [%{id: 1, name: "Jake"}, %{id: 2, name: "Amy Santiago"}]

  An error is raised if the accessed structure is not a list:

      iex> get_in(%{}, [Livebook.Utils.access_by_id(1)])
      ** (RuntimeError) Livebook.Utils.access_by_id/1 expected a list, got: %{}

  """
  @spec access_by_id(term()) ::
          Access.access_fun(data :: struct() | map(), current_value :: term())
  def access_by_id(id) do
    fn
      :get, data, next when is_list(data) ->
        data
        |> Enum.find(fn item -> item.id == id end)
        |> next.()

      :get_and_update, data, next when is_list(data) ->
        case Enum.split_while(data, fn item -> item.id != id end) do
          {prev, [item | cons]} ->
            case next.(item) do
              {get, update} ->
                {get, prev ++ [update | cons]}

              :pop ->
                {item, prev ++ cons}
            end

          _ ->
            {nil, data}
        end

      _op, data, _next ->
        raise "Livebook.Utils.access_by_id/1 expected a list, got: #{inspect(data)}"
    end
  end

  @doc """
  Validates if the given URL is syntactically valid.

  ## Examples

      iex> Livebook.Utils.valid_url?("not_a_url")
      false

      iex> Livebook.Utils.valid_url?("https://example.com")
      true

      iex> Livebook.Utils.valid_url?("http://localhost")
      true

      iex> Livebook.Utils.valid_url?("http://")
      false

  """
  @spec valid_url?(String.t()) :: boolean()
  def valid_url?(url) do
    uri = URI.parse(url)
    uri.scheme != nil and uri.host not in [nil, ""]
  end

  @doc """
  Validates a change is a valid URL.
  """
  @spec validate_url(Ecto.Changeset.t(), atom()) :: Ecto.Changeset.t()
  def validate_url(changeset, field) do
    Ecto.Changeset.validate_change(changeset, field, fn ^field, url ->
      if valid_url?(url) do
        []
      else
        [{field, "must be a valid URL"}]
      end
    end)
  end

  @doc ~S"""
  Validates if the given string forms valid CLI flags.

  ## Examples

      iex> Livebook.Utils.valid_cli_flags?("")
      true

      iex> Livebook.Utils.valid_cli_flags?("--arg1 value --arg2 'value'")
      true

      iex> Livebook.Utils.valid_cli_flags?("--arg1 \"")
      false

  """
  @spec valid_cli_flags?(String.t()) :: boolean()
  def valid_cli_flags?(flags) do
    try do
      OptionParser.split(flags)
      true
    rescue
      _ -> false
    end
  end

  @doc """
  Changes the first letter in the given string to upper case.

  ## Examples

      iex> Livebook.Utils.upcase_first("sippin tea")
      "Sippin tea"

      iex> Livebook.Utils.upcase_first("short URL")
      "Short URL"

      iex> Livebook.Utils.upcase_first("")
      ""

  """
  @spec upcase_first(String.t()) :: String.t()
  def upcase_first(string) do
    {first, rest} = String.split_at(string, 1)
    String.upcase(first) <> rest
  end

  @doc """
  Changes the first letter in the given string to lower case.

  ## Examples

      iex> Livebook.Utils.downcase_first("Sippin tea")
      "sippin tea"

      iex> Livebook.Utils.downcase_first("Short URL")
      "short URL"

      iex> Livebook.Utils.downcase_first("")
      ""

  """
  @spec downcase_first(String.t()) :: String.t()
  def downcase_first(string) do
    {first, rest} = String.split_at(string, 1)
    String.downcase(first) <> rest
  end

  @doc """
  Expands a relative path in terms of the given URL.

  ## Examples

      iex> Livebook.Utils.expand_url("file:///home/user/lib/file.ex", "../root.ex")
      "file:///home/user/root.ex"

      iex> Livebook.Utils.expand_url("https://example.com/lib/file.ex?token=supersecret", "../root.ex")
      "https://example.com/root.ex?token=supersecret"

      iex> Livebook.Utils.expand_url("https://example.com", "./root.ex")
      "https://example.com/root.ex"

  """
  @spec expand_url(String.t(), String.t()) :: String.t()
  def expand_url(url, relative_path) do
    url
    |> URI.parse()
    |> Map.update!(:path, fn path ->
      Livebook.FileSystem.Utils.resolve_unix_like_path(path || "/", relative_path)
    end)
    |> URI.to_string()
  end

  @doc """
  Infers file name from the given URL.

  ## Examples

      iex> Livebook.Utils.url_basename("https://example.com/data.csv")
      "data.csv"

      iex> Livebook.Utils.url_basename("https://example.com")
      ""

  """
  @spec url_basename(String.t()) :: String.t()
  def url_basename(url) do
    uri = URI.parse(url)

    (uri.path || "/")
    |> String.split("/")
    |> List.last()
  end

  @doc ~S"""
  Wraps the given line into lines that fit in `width` characters.

  Words longer than `width` are not broken apart.

  ## Examples

      iex> Livebook.Utils.wrap_line("cat on the roof", 7)
      "cat on\nthe\nroof"

      iex> Livebook.Utils.wrap_line("cat in the cup", 7)
      "cat in\nthe cup"

      iex> Livebook.Utils.wrap_line("cat in the cup", 2)
      "cat\nin\nthe\ncup"

  """
  @spec wrap_line(String.t(), pos_integer()) :: String.t()
  def wrap_line(line, width) do
    line
    |> String.split()
    |> Enum.reduce({[[]], 0}, fn part, {[group | groups], group_size} ->
      size = String.length(part)

      cond do
        group == [] ->
          {[[part] | groups], size}

        group_size + 1 + size <= width ->
          {[[part, " " | group] | groups], group_size + 1 + size}

        true ->
          {[[part], group | groups], size}
      end
    end)
    |> elem(0)
    |> Enum.map(&Enum.reverse/1)
    |> Enum.reverse()
    |> Enum.intersperse("\n")
    |> IO.iodata_to_binary()
  end

  @doc """
  Reads file contents and encodes it into a data URL.
  """
  @spec read_as_data_url!(Path.t()) :: binary()
  def read_as_data_url!(path) do
    content = File.read!(path)
    mime = MIME.from_path(path)
    data = Base.encode64(content)
    "data:#{mime};base64,#{data}"
  end

  @doc """
  Expands URL received from the Desktop App for opening in the browser.
  """
  def expand_desktop_url("") do
    LivebookWeb.Endpoint.access_url()
  end

  def expand_desktop_url("/settings") do
    to_string(%{LivebookWeb.Endpoint.access_struct_url() | path: "/settings"})
  end

  def expand_desktop_url("file://" <> path) do
    notebook_open_url(path)
  end

  def expand_desktop_url("livebook://" <> rest) do
    notebook_import_url("https://#{rest}")
  end

  @doc """
  Opens the given `url` in the browser.
  """
  def browser_open(url) do
    win_cmd_args = ["/c", "start", String.replace(url, "&", "^&")]

    cmd_args =
      case :os.type() do
        {:win32, _} ->
          {"cmd", win_cmd_args}

        {:unix, :darwin} ->
          {"open", [url]}

        {:unix, _} ->
          cond do
            System.find_executable("xdg-open") ->
              {"xdg-open", [url]}

            # When inside WSL
            System.find_executable("cmd.exe") ->
              {"cmd.exe", win_cmd_args}

            true ->
              nil
          end
      end

    case cmd_args do
      {cmd, args} -> System.cmd(cmd, args)
      nil -> Logger.warning("could not open the browser, no open command found in the system")
    end

    :ok
  end

  @doc """
  Splits the given string at the last occurrence of `pattern`.

  ## Examples

      iex> Livebook.Utils.split_at_last_occurrence("1,2,3", ",")
      {:ok, "1,2", "3"}

      iex> Livebook.Utils.split_at_last_occurrence("123", ",")
      :error

  """
  @spec split_at_last_occurrence(String.t(), String.pattern()) ::
          {:ok, left :: String.t(), right :: String.t()} | :error
  def split_at_last_occurrence(string, pattern) when is_binary(string) do
    case :binary.matches(string, pattern) do
      [] ->
        :error

      parts ->
        {start, _} = List.last(parts)
        size = byte_size(string)
        {:ok, binary_part(string, 0, start), binary_part(string, start + 1, size - start - 1)}
    end
  end

  @doc ~S"""
  Finds CR characters and removes leading text in the same line.

  Note that trailing CRs are kept.

  ## Examples

      iex> Livebook.Utils.apply_rewind("Hola\nHmm\rHey")
      "Hola\nHey"

      iex> Livebook.Utils.apply_rewind("\rHey")
      "Hey"

      iex> Livebook.Utils.apply_rewind("Hola\r\nHey\r")
      "Hola\r\nHey\r"

  """
  @spec apply_rewind(String.t()) :: String.t()
  def apply_rewind(text) when is_binary(text) do
    apply_rewind(text, "", "")
  end

  defp apply_rewind(<<?\n, rest::binary>>, acc, line),
    do: apply_rewind(rest, <<acc::binary, line::binary, ?\n>>, "")

  defp apply_rewind(<<?\r, byte, rest::binary>>, acc, _line) when byte != ?\n,
    do: apply_rewind(rest, acc, <<byte>>)

  defp apply_rewind(<<byte, rest::binary>>, acc, line),
    do: apply_rewind(rest, acc, <<line::binary, byte>>)

  defp apply_rewind("", acc, line), do: acc <> line

  @doc ~S"""
  Limits `text` to last `max_lines`.

  Replaces the removed lines with `"..."`.

  ## Examples

      iex> Livebook.Utils.cap_lines("Line 1\nLine 2\nLine 3\nLine 4", 2)
      "...\nLine 3\nLine 4"

      iex> Livebook.Utils.cap_lines("Line 1\nLine 2", 2)
      "Line 1\nLine 2"

      iex> Livebook.Utils.cap_lines("Line 1\nLine 2", 3)
      "Line 1\nLine 2"

  """
  @spec cap_lines(String.t(), non_neg_integer()) :: String.t()
  def cap_lines(text, max_lines) do
    text
    |> :binary.matches("\n")
    |> Enum.at(-max_lines)
    |> case do
      nil ->
        text

      {pos, _len} ->
        <<_ignore::binary-size(pos), rest::binary>> = text
        "..." <> rest
    end
  end

  @doc """
  Returns an absolute URL to import notebook from the given `url`.

  ## Examples

      iex> Livebook.Utils.notebook_import_url("https://example.com/foo.livemd")
      "http://localhost:4002/import?url=https%3A%2F%2Fexample.com%2Ffoo.livemd"

      iex> Livebook.Utils.notebook_import_url("https://my_host", "https://example.com/foo.livemd")
      "https://my_host/import?url=https%3A%2F%2Fexample.com%2Ffoo.livemd"

  """
  def notebook_import_url(base_url \\ LivebookWeb.Endpoint.access_struct_url(), url) do
    base_url
    |> URI.parse()
    |> Map.replace!(:path, "/import")
    |> URI.append_query("url=#{URI.encode_www_form(url)}")
    |> URI.to_string()
  end

  @doc """
  Returns an absolute URL to open notebook from the given local `path`.

  A directory `path` may also be given, in which case the URL directs
  to a file open page with the directory open.

  ## Examples

      iex> Livebook.Utils.notebook_open_url("/data/foo.livemd")
      "http://localhost:4002/open?path=%2Fdata%2Ffoo.livemd"

      iex> Livebook.Utils.notebook_open_url("https://my_host", "/data/foo.livemd")
      "https://my_host/open?path=%2Fdata%2Ffoo.livemd"

  """
  def notebook_open_url(base_url \\ LivebookWeb.Endpoint.access_struct_url(), path) do
    base_url
    |> URI.parse()
    |> Map.replace!(:path, "/open")
    |> URI.append_query("path=#{URI.encode_www_form(path)}")
    |> URI.to_string()
  end

  @doc """
  Formats the given number of bytes into a human-friendly text.

  ## Examples

      iex> Livebook.Utils.format_bytes(0)
      "0 B"

      iex> Livebook.Utils.format_bytes(900)
      "900 B"

      iex> Livebook.Utils.format_bytes(1100)
      "1.1 KB"

      iex> Livebook.Utils.format_bytes(1_228_800)
      "1.2 MB"

      iex> Livebook.Utils.format_bytes(1_363_148_800)
      "1.4 GB"

      iex> Livebook.Utils.format_bytes(1_503_238_553_600)
      "1.5 TB"

  """
  @spec format_bytes(non_neg_integer()) :: String.t()
  def format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= memory_unit(:TB) -> format_bytes(bytes, :TB)
      bytes >= memory_unit(:GB) -> format_bytes(bytes, :GB)
      bytes >= memory_unit(:MB) -> format_bytes(bytes, :MB)
      bytes >= memory_unit(:KB) -> format_bytes(bytes, :KB)
      true -> format_bytes(bytes, :B)
    end
  end

  defp format_bytes(bytes, :B) when is_integer(bytes), do: "#{bytes} B"

  defp format_bytes(bytes, unit) when is_integer(bytes) do
    value = bytes / memory_unit(unit)
    "#{:erlang.float_to_binary(value, decimals: 1)} #{unit}"
  end

  defp memory_unit(:TB), do: 1_000_000_000_000
  defp memory_unit(:GB), do: 1_000_000_000
  defp memory_unit(:MB), do: 1_000_000
  defp memory_unit(:KB), do: 1_000

  @doc """
  Gets the port for an existing listener.

  The listener references usually follow the pattern `plug.HTTP`
  and `plug.HTTPS`.
  """
  @spec get_port(:ranch.ref(), :inet.port_number()) :: :inet.port_number()
  def get_port(ref, default) do
    try do
      :ranch.get_addr(ref)
    rescue
      _ -> default
    else
      {_, port} when is_integer(port) -> port
      _ -> default
    end
  end

  @doc """
  Converts the given IP address into a valid hostname.

  ## Examples

      iex> Livebook.Utils.ip_to_host({192, 168, 0, 1})
      "192.168.0.1"

      iex> Livebook.Utils.ip_to_host({127, 0, 0, 1})
      "localhost"

      iex> Livebook.Utils.ip_to_host({0, 0, 0, 0})
      "localhost"

  """
  @spec ip_to_host(:inet.ip_address()) :: String.t()
  def ip_to_host(ip)

  def ip_to_host({0, 0, 0, 0}), do: "localhost"
  def ip_to_host({127, 0, 0, 1}), do: "localhost"

  def ip_to_host(ip) do
    ip |> :inet.ntoa() |> List.to_string()
  end
end
