defmodule Livebook.Utils do
  @moduledoc false

  @type id :: binary()

  @doc """
  Generates a random binary id.
  """
  @spec random_id() :: binary()
  def random_id() do
    :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
  end

  @doc """
  Generates a random short binary id.
  """
  @spec random_short_id() :: binary()
  def random_short_id() do
    :crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)
  end

  @doc """
  Generates a random cookie for a distributed node.
  """
  @spec random_cookie() :: atom()
  def random_cookie() do
    :crypto.strong_rand_bytes(42) |> Base.url_encode64() |> String.to_atom()
  end

  @doc """
  Converts the given name to node identifier.
  """
  @spec node_from_name(String.t()) :: atom()
  def node_from_name(name) do
    if name =~ "@" do
      String.to_atom(name)
    else
      # Default to the same host as the current node
      :"#{name}@#{node_host()}"
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
  """
  @spec valid_url?(String.t()) :: boolean()
  def valid_url?(url) do
    uri = URI.parse(url)
    uri.scheme != nil and uri.host != nil and uri.host =~ "."
  end

  @doc """
  Validates if the given hex color is the correct format

  ## Examples

      iex> Livebook.Utils.valid_hex_color?("#111111")
      true

      iex> Livebook.Utils.valid_hex_color?("#ABC123")
      true

      iex> Livebook.Utils.valid_hex_color?("ABCDEF")
      false

      iex> Livebook.Utils.valid_hex_color?("#111")
      false
  """
  @spec valid_hex_color?(String.t()) :: boolean()
  def valid_hex_color?(hex_color), do: hex_color =~ ~r/^#[0-9a-fA-F]{6}$/

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
  """
  @spec expand_url(String.t(), String.t()) :: String.t()
  def expand_url(url, relative_path) do
    url
    |> URI.parse()
    |> Map.update!(:path, fn path ->
      path |> Path.dirname() |> Path.join(relative_path) |> Path.expand()
    end)
    |> URI.to_string()
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
end
