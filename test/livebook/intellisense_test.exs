defmodule Livebook.IntellisenseTest.Utils do
  @moduledoc false

  @doc """
  Returns `{binding, env}` resulting from evaluating
  the given block of code in a fresh context.
  """
  defmacro eval(do: block) do
    quote do
      block = unquote(Macro.escape(block))
      binding = []
      env = :elixir.env_for_eval([])
      {_, binding, env} = :elixir.eval_quoted(block, binding, env)
      {binding, env}
    end
  end
end

defmodule Livebook.IntellisenseTest do
  use ExUnit.Case, async: true

  import Livebook.IntellisenseTest.Utils

  alias Livebook.Intellisense

  describe "get_completion_items/3" do
    test "completion when no hint given" do
      {binding, env} = eval(do: nil)

      length_item = %{
        label: "length/1",
        kind: :function,
        detail: "Kernel.length(list)",
        documentation: """
        Returns the length of `list`.

        ```
        @spec length(list()) :: non_neg_integer()
        ```\
        """,
        insert_text: "length($0)"
      }

      assert length_item in Intellisense.get_completion_items("", binding, env)
      assert length_item in Intellisense.get_completion_items("to_string(", binding, env)
      assert length_item in Intellisense.get_completion_items("Enum.map(list, ", binding, env)
    end

    @tag :erl_docs
    test "Erlang module completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: ":zlib",
                 kind: :module,
                 detail: "module",
                 documentation:
                   "This module provides an API for the zlib library ([www.zlib.net](http://www.zlib.net)). It is used to compress and decompress data. The data format is described by [RFC 1950](https://www.ietf.org/rfc/rfc1950.txt), [RFC 1951](https://www.ietf.org/rfc/rfc1951.txt), and [RFC 1952](https://www.ietf.org/rfc/rfc1952.txt).",
                 insert_text: "zlib"
               }
             ] = Intellisense.get_completion_items(":zl", binding, env)
    end

    test "Erlang module no completion" do
      {binding, env} = eval(do: nil)

      assert [] = Intellisense.get_completion_items(":unknown", binding, env)
    end

    test "Erlang module multiple values completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: ":user",
                 kind: :module,
                 detail: "module",
                 documentation: _user_doc,
                 insert_text: "user"
               },
               %{
                 label: ":user_drv",
                 kind: :module,
                 detail: "module",
                 documentation: _user_drv_doc,
                 insert_text: "user_drv"
               },
               %{
                 label: ":user_sup",
                 kind: :module,
                 detail: "module",
                 documentation: _user_sup_doc,
                 insert_text: "user_sup"
               }
             ] = Intellisense.get_completion_items(":user", binding, env)
    end

    @tag :erl_docs
    test "Erlang root completion" do
      {binding, env} = eval(do: nil)

      lists_item = %{
        label: ":lists",
        kind: :module,
        detail: "module",
        documentation: "This module contains functions for list processing.",
        insert_text: "lists"
      }

      assert lists_item in Intellisense.get_completion_items(":", binding, env)
      assert lists_item in Intellisense.get_completion_items("  :", binding, env)
    end

    @tag :erl_docs
    test "Erlang completion doesn't include quoted atoms" do
      {binding, env} = eval(do: nil)

      assert [] = Intellisense.get_completion_items(~s{:Elixir}, binding, env)
    end

    @tag :erl_docs
    test "Erlang module completion with 'in' operator in spec" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "open_port/2",
                 kind: :function,
                 detail: ":erlang.open_port/2",
                 documentation: _open_port_doc,
                 insert_text: "open_port($0)"
               }
             ] = Intellisense.get_completion_items(":erlang.open_por", binding, env)
    end

    test "Elixir proxy" do
      {binding, env} = eval(do: nil)

      assert %{
               label: "Elixir",
               kind: :module,
               detail: "module",
               documentation: "No documentation available",
               insert_text: "Elixir"
             } in Intellisense.get_completion_items("E", binding, env)
    end

    test "Elixir module completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "Enum",
                 kind: :module,
                 detail: "module",
                 documentation: "Provides a set of algorithms to work with enumerables.",
                 insert_text: "Enum"
               },
               %{
                 label: "Enumerable",
                 kind: :interface,
                 detail: "protocol",
                 documentation: "Enumerable protocol used by `Enum` and `Stream` modules.",
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("En", binding, env)

      assert [
               %{
                 label: "Enumerable",
                 kind: :interface,
                 detail: "protocol",
                 documentation: "Enumerable protocol used by `Enum` and `Stream` modules.",
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("Enumera", binding, env)

      assert [
               %{
                 label: "RuntimeError",
                 kind: :struct,
                 detail: "exception",
                 documentation: "No documentation available",
                 insert_text: "RuntimeError"
               }
             ] = Intellisense.get_completion_items("RuntimeE", binding, env)
    end

    test "Elixir struct completion lists nested options" do
      {binding, env} = eval(do: nil)

      assert %{
               label: "File.Stat",
               kind: :struct,
               detail: "struct",
               documentation: "A struct that holds file information.",
               insert_text: "File.Stat"
             } in Intellisense.get_completion_items("%Fi", binding, env)
    end

    test "Elixir type completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "from/0",
                 kind: :type,
                 detail: "typespec",
                 documentation: "Tuple describing the client of a call request.",
                 insert_text: "from"
               }
             ] = Intellisense.get_completion_items("GenServer.fr", binding, env)

      assert [
               %{
                 label: "name/0",
                 kind: :type,
                 detail: "typespec",
                 documentation: _name_doc,
                 insert_text: "name"
               },
               %{
                 label: "name_all/0",
                 kind: :type,
                 detail: "typespec",
                 documentation: _name_all_doc,
                 insert_text: "name_all"
               }
             ] = Intellisense.get_completion_items(":file.nam", binding, env)
    end

    test "Elixir module completion with self" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "Enumerable",
                 kind: :interface,
                 detail: "protocol",
                 documentation: "Enumerable protocol used by `Enum` and `Stream` modules.",
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("Enumerable", binding, env)
    end

    test "Elixir completion on modules from load path" do
      {binding, env} = eval(do: nil)

      assert %{
               label: "Jason",
               kind: :module,
               detail: "module",
               documentation: "A blazing fast JSON parser and generator in pure Elixir.",
               insert_text: "Jason"
             } in Intellisense.get_completion_items("Jas", binding, env)
    end

    test "Elixir no completion" do
      {binding, env} = eval(do: nil)

      assert [] = Intellisense.get_completion_items(".", binding, env)
      assert [] = Intellisense.get_completion_items("Xyz", binding, env)
      assert [] = Intellisense.get_completion_items("x.Foo", binding, env)
      assert [] = Intellisense.get_completion_items("x.Foo.get_by", binding, env)
    end

    test "Elixir private module no completion" do
      {binding, env} = eval(do: nil)

      assert [] =
               Intellisense.get_completion_items(
                 "Livebook.TestModules.Hidd",
                 binding,
                 env
               )
    end

    test "Elixir private module members completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 detail: "Livebook.TestModules.Hidden.visible()",
                 documentation: "No documentation available",
                 insert_text: "visible()",
                 kind: :function,
                 label: "visible/0"
               }
             ] = Intellisense.get_completion_items("Livebook.TestModules.Hidden.", binding, env)
    end

    test "Elixir root submodule completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "Access",
                 kind: :interface,
                 detail: "behaviour",
                 documentation: "Key-based access to data structures.",
                 insert_text: "Access"
               }
             ] = Intellisense.get_completion_items("Elixir.Acce", binding, env)
    end

    test "Elixir submodule completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "ANSI",
                 kind: :module,
                 detail: "module",
                 documentation: "Functionality to render ANSI escape sequences.",
                 insert_text: "ANSI"
               }
             ] = Intellisense.get_completion_items("IO.AN", binding, env)
    end

    test "Elixir submodule no completion" do
      {binding, env} = eval(do: nil)

      assert [] = Intellisense.get_completion_items("IEx.Xyz", binding, env)
    end

    test "Elixir function completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "version/0",
                 kind: :function,
                 detail: "System.version()",
                 documentation: """
                 Elixir version information.

                 ```
                 @spec version() :: String.t()
                 ```\
                 """,
                 insert_text: "version()"
               }
             ] = Intellisense.get_completion_items("System.ve", binding, env)
    end

    test "Elixir sigil completion" do
      {binding, env} = eval(do: nil)

      regex_item = %{
        label: "~r/2",
        kind: :function,
        detail: "Kernel.sigil_r(term, modifiers)",
        documentation: "Handles the sigil `~r` for regular expressions.",
        insert_text: "~r"
      }

      assert regex_item in Intellisense.get_completion_items("~", binding, env)

      assert [^regex_item] = Intellisense.get_completion_items("~r", binding, env)
    end

    @tag :erl_docs
    test "Erlang function completion" do
      {binding, env} = eval(do: nil)

      assert %{
               label: "gzip/1",
               kind: :function,
               detail: ":zlib.gzip/1",
               documentation: """
               Compresses data with gz headers and checksum.

               ```
               @spec gzip(data) :: compressed
                     when data: iodata(),
                          compressed: binary()
               ```\
               """,
               insert_text: "gzip($0)"
             } in Intellisense.get_completion_items(":zlib.gz", binding, env)
    end

    test "function completion with arity" do
      {binding, env} = eval(do: nil)

      assert %{
               label: "concat/1",
               kind: :function,
               detail: "Enum.concat(enumerables)",
               documentation: """
               Given an enumerable of enumerables, concatenates the `enumerables` into
               a single list.

               ```
               @spec concat(t()) :: t()
               ```\
               """,
               insert_text: "concat($0)"
             } in Intellisense.get_completion_items("Enum.concat/", binding, env)

      assert [
               %{label: "count/1"},
               %{label: "count/2"}
             ] = Intellisense.get_completion_items("Enum.count/", binding, env)
    end

    test "function completion same name with different arities" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "concat/1",
                 kind: :function,
                 detail: "Enum.concat(enumerables)",
                 documentation: """
                 Given an enumerable of enumerables, concatenates the `enumerables` into
                 a single list.

                 ```
                 @spec concat(t()) :: t()
                 ```\
                 """,
                 insert_text: "concat($0)"
               },
               %{
                 label: "concat/2",
                 kind: :function,
                 detail: "Enum.concat(left, right)",
                 documentation: """
                 Concatenates the enumerable on the `right` with the enumerable on the
                 `left`.

                 ```
                 @spec concat(t(), t()) :: t()
                 ```\
                 """,
                 insert_text: "concat($0)"
               }
             ] = Intellisense.get_completion_items("Enum.concat", binding, env)
    end

    test "function completion when has default args then documentation all arities have docs" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "join/1",
                 kind: :function,
                 detail: ~S{Enum.join(enumerable, joiner \\ "")},
                 documentation: """
                 Joins the given `enumerable` into a string using `joiner` as a
                 separator.

                 ```
                 @spec join(t(), String.t()) :: String.t()
                 ```\
                 """,
                 insert_text: "join($0)"
               },
               %{
                 label: "join/2",
                 kind: :function,
                 detail: ~S{Enum.join(enumerable, joiner \\ "")},
                 documentation: """
                 Joins the given `enumerable` into a string using `joiner` as a
                 separator.

                 ```
                 @spec join(t(), String.t()) :: String.t()
                 ```\
                 """,
                 insert_text: "join($0)"
               }
             ] = Intellisense.get_completion_items("Enum.jo", binding, env)
    end

    test "function completion using a variable bound to a module" do
      {binding, env} =
        eval do
          mod = System
        end

      assert [
               %{
                 label: "version/0",
                 kind: :function,
                 detail: "System.version()",
                 documentation: """
                 Elixir version information.

                 ```
                 @spec version() :: String.t()
                 ```\
                 """,
                 insert_text: "version()"
               }
             ] = Intellisense.get_completion_items("mod.ve", binding, env)
    end

    test "operator completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "++/2",
                 kind: :function,
                 detail: "left ++ right",
                 documentation: """
                 List concatenation operator. Concatenates a proper list and a term, returning a list.

                 ```
                 @spec list() ++ term() ::
                         maybe_improper_list()
                 ```\
                 """,
                 insert_text: "++"
               },
               %{
                 label: "+/1",
                 kind: :function,
                 detail: "+value",
                 documentation: """
                 Arithmetic positive unary operator.

                 ```
                 @spec +integer() :: integer()
                 @spec +float() :: float()
                 ```\
                 """,
                 insert_text: "+"
               },
               %{
                 label: "+/2",
                 kind: :function,
                 detail: "left + right",
                 documentation: """
                 Arithmetic addition operator.

                 ```
                 @spec integer() + integer() :: integer()
                 @spec float() + float() :: float()
                 @spec integer() + float() :: float()
                 @spec float() + integer() :: float()
                 ```\
                 """,
                 insert_text: "+"
               }
             ] = Intellisense.get_completion_items("+", binding, env)

      assert [
               %{label: "+/1"},
               %{label: "+/2"}
             ] = Intellisense.get_completion_items("+/", binding, env)

      assert [
               %{label: "++/2"}
             ] = Intellisense.get_completion_items("++/", binding, env)
    end

    test "map atom key completion" do
      {binding, env} =
        eval do
          map = %{
            foo: 1,
            bar_1: ~r/pattern/,
            bar_2: true
          }
        end

      assert [
               %{
                 label: "bar_1",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "bar_1"
               },
               %{
                 label: "bar_2",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "bar_2"
               },
               %{
                 label: "foo",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.", binding, env)

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.f", binding, env)
    end

    test "nested map atom key completion" do
      {binding, env} =
        eval do
          map = %{
            nested: %{
              deeply: %{
                foo: 1,
                bar_1: 23,
                bar_2: 14,
                mod: System
              }
            }
          }
        end

      assert [
               %{
                 label: "nested",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "nested"
               }
             ] = Intellisense.get_completion_items("map.nest", binding, env)

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.nested.deeply.f", binding, env)

      assert [
               %{
                 label: "version/0",
                 kind: :function,
                 detail: "System.version()",
                 documentation: """
                 Elixir version information.

                 ```
                 @spec version() :: String.t()
                 ```\
                 """,
                 insert_text: "version()"
               }
             ] = Intellisense.get_completion_items("map.nested.deeply.mod.ve", binding, env)

      assert [] = Intellisense.get_completion_items("map.non.existent", binding, env)
    end

    test "map string key completion is not supported" do
      {binding, env} =
        eval do
          map = %{"foo" => 1}
        end

      assert [] = Intellisense.get_completion_items("map.f", binding, env)
    end

    test "autocompletion off a bound variable only works for modules and maps" do
      {binding, env} =
        eval do
          num = 5
          map = %{nested: %{num: 23}}
        end

      assert [] = Intellisense.get_completion_items("num.print", binding, env)
      assert [] = Intellisense.get_completion_items("map.nested.num.f", binding, env)
    end

    test "autocompletion using access syntax does is not supported" do
      {binding, env} =
        eval do
          map = %{nested: %{deeply: %{num: 23}}}
        end

      assert [] = Intellisense.get_completion_items("map[:nested][:deeply].n", binding, env)
      assert [] = Intellisense.get_completion_items("map[:nested].deeply.n", binding, env)
      assert [] = Intellisense.get_completion_items("map.nested.[:deeply].n", binding, env)
    end

    test "macro completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "is_nil/1",
                 kind: :function,
                 detail: "Kernel.is_nil(term)",
                 documentation: "Returns `true` if `term` is `nil`, `false` otherwise.",
                 insert_text: "is_nil($0)"
               }
             ] = Intellisense.get_completion_items("Kernel.is_ni", binding, env)
    end

    test "special forms completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "quote/2",
                 kind: :function,
                 detail: "Kernel.SpecialForms.quote(opts, block)",
                 documentation: "Gets the representation of any expression.",
                 insert_text: "quote "
               }
             ] = Intellisense.get_completion_items("quot", binding, env)
    end

    test "kernel import completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "put_in/2",
                 kind: :function,
                 detail: "Kernel.put_in(path, value)",
                 documentation: "Puts a value in a nested structure via the given `path`.",
                 insert_text: "put_in($0)"
               },
               %{
                 label: "put_in/3",
                 kind: :function,
                 detail: "Kernel.put_in(data, keys, value)",
                 documentation: """
                 Puts a value in a nested structure.

                 ```
                 @spec put_in(
                         Access.t(),
                         [term(), ...],
                         term()
                       ) :: Access.t()
                 ```\
                 """,
                 insert_text: "put_in($0)"
               }
             ] = Intellisense.get_completion_items("put_i", binding, env)
    end

    test "variable name completion" do
      {binding, env} =
        eval do
          number = 3
          numbats = ["numbat", "numbat"]
          nothing = nil
        end

      assert [
               %{
                 label: "numbats",
                 kind: :variable,
                 detail: "variable",
                 documentation: nil,
                 insert_text: "numbats"
               }
             ] = Intellisense.get_completion_items("numba", binding, env)

      assert [
               %{
                 label: "numbats",
                 kind: :variable,
                 detail: "variable",
                 documentation: nil,
                 insert_text: "numbats"
               },
               %{
                 label: "number",
                 kind: :variable,
                 detail: "variable",
                 documentation: nil,
                 insert_text: "number"
               }
             ] = Intellisense.get_completion_items("num", binding, env)

      assert [
               %{
                 label: "nothing",
                 kind: :variable,
                 detail: "variable",
                 documentation: nil,
                 insert_text: "nothing"
               },
               %{label: "node/0"},
               %{label: "node/1"},
               %{label: "not/1"}
             ] = Intellisense.get_completion_items("no", binding, env)
    end

    test "completion of manually imported functions and macros" do
      {binding, env} =
        eval do
          import Enum
          import System, only: [version: 0]
          import Protocol
        end

      assert [
               %{label: "take/2"},
               %{label: "take_every/2"},
               %{label: "take_random/2"},
               %{label: "take_while/2"}
             ] = Intellisense.get_completion_items("take", binding, env)

      assert %{
               label: "version/0",
               kind: :function,
               detail: "System.version()",
               documentation: """
               Elixir version information.

               ```
               @spec version() :: String.t()
               ```\
               """,
               insert_text: "version()"
             } in Intellisense.get_completion_items("v", binding, env)

      assert [
               %{label: "derive/2"},
               %{label: "derive/3"}
             ] = Intellisense.get_completion_items("der", binding, env)

      assert [
               %{label: "count/1"},
               %{label: "count/2"}
             ] = Intellisense.get_completion_items("count/", binding, env)
    end

    test "ignores quoted variables when performing variable completion" do
      {binding, env} =
        eval do
          quote do
            var!(my_var_1, Elixir) = 1
          end

          my_var_2 = 2
        end

      assert [
               %{label: "my_var_2"}
             ] = Intellisense.get_completion_items("my_var", binding, env)
    end

    test "completion inside expression" do
      {binding, env} = eval(do: nil)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("1 En", binding, env)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("foo(En", binding, env)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("Test En", binding, env)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("foo(x,En", binding, env)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("[En", binding, env)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("{En", binding, env)
    end

    test "ampersand completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("&En", binding, env)

      assert [
               %{label: "all?/1"},
               %{label: "all?/2"}
             ] = Intellisense.get_completion_items("&Enum.al", binding, env)

      assert [
               %{label: "all?/1"},
               %{label: "all?/2"}
             ] = Intellisense.get_completion_items("f = &Enum.al", binding, env)
    end

    test "negation operator completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{label: "is_binary/1"}
             ] = Intellisense.get_completion_items("!is_bin", binding, env)
    end

    test "pin operator completion" do
      {binding, env} =
        eval do
          my_variable = 2
        end

      assert [
               %{label: "my_variable"}
             ] = Intellisense.get_completion_items("^my_va", binding, env)
    end

    defmodule SublevelTest.LevelA.LevelB do
    end

    test "Elixir completion sublevel" do
      {binding, env} = eval(do: nil)

      assert [
               %{label: "LevelA"}
             ] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.SublevelTest.",
                 binding,
                 env
               )
    end

    test "complete aliases of Elixir modules" do
      {binding, env} =
        eval do
          alias List, as: MyList
        end

      assert [
               %{label: "MyList"}
             ] = Intellisense.get_completion_items("MyL", binding, env)

      assert [
               %{label: "to_integer/1"},
               %{label: "to_integer/2"}
             ] = Intellisense.get_completion_items("MyList.to_integ", binding, env)
    end

    @tag :erl_docs
    test "complete aliases of Erlang modules" do
      {binding, env} =
        eval do
          alias :lists, as: EList
        end

      assert [
               %{label: "EList"}
             ] = Intellisense.get_completion_items("EL", binding, env)

      assert [
               %{label: "map/2"},
               %{label: "mapfoldl/3"},
               %{label: "mapfoldr/3"}
             ] = Intellisense.get_completion_items("EList.map", binding, env)

      assert %{
               label: "max/1",
               kind: :function,
               detail: ":lists.max/1",
               documentation: """
               Returns the first element of `List` that compares greater than or equal to all other elements of `List`.

               ```
               @spec max(list) :: max
                     when list: [t, ...], max: t, t: term()
               ```\
               """,
               insert_text: "max($0)"
             } in Intellisense.get_completion_items("EList.", binding, env)

      assert [] = Intellisense.get_completion_items("EList.Invalid", binding, env)
    end

    test "completion for functions added when compiled module is reloaded" do
      {binding, env} = eval(do: nil)

      {:module, _, bytecode, _} =
        defmodule Sample do
          def foo(), do: 0
        end

      assert [
               %{label: "foo/0"}
             ] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.Sample.foo",
                 binding,
                 env
               )

      Code.compiler_options(ignore_module_conflict: true)

      defmodule Sample do
        def foo(), do: 0
        def foobar(), do: 0
      end

      assert [
               %{label: "foo/0"},
               %{label: "foobar/0"}
             ] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.Sample.foo",
                 binding,
                 env
               )
    after
      Code.compiler_options(ignore_module_conflict: false)
      :code.purge(Sample)
      :code.delete(Sample)
    end

    defmodule MyStruct do
      defstruct [:my_val]
    end

    test "completion for struct names" do
      {binding, env} = eval(do: nil)

      assert [
               %{label: "MyStruct"}
             ] =
               Intellisense.get_completion_items("Livebook.IntellisenseTest.MyStr", binding, env)
    end

    test "completion for struct keys" do
      {binding, env} =
        eval do
          struct = %Livebook.IntellisenseTest.MyStruct{}
        end

      assert [
               %{label: "my_val"}
             ] = Intellisense.get_completion_items("struct.my", binding, env)
    end

    test "ignore invalid Elixir module literals" do
      {binding, env} = eval(do: nil)

      defmodule(:"Elixir.Livebook.IntellisenseTest.Unicodé", do: nil)

      assert [] =
               Intellisense.get_completion_items("Livebook.IntellisenseTest.Unicod", binding, env)
    after
      :code.purge(:"Elixir.Livebook.IntellisenseTest.Unicodé")
      :code.delete(:"Elixir.Livebook.IntellisenseTest.Unicodé")
    end

    test "known Elixir module attributes completion" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "moduledoc",
                 kind: :variable,
                 detail: "module attribute",
                 documentation: "Provides documentation for the current module.",
                 insert_text: "moduledoc"
               }
             ] = Intellisense.get_completion_items("@modu", binding, env)
    end

    test "handles calls on module attribute" do
      {binding, env} = eval(do: nil)
      assert [] = Intellisense.get_completion_items("@attr.value", binding, env)
    end

    test "includes language keywords" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "do",
                 kind: :keyword,
                 detail: "do-end block",
                 documentation: nil,
                 insert_text: "do\n  $0\nend"
               }
               | _
             ] = Intellisense.get_completion_items("do", binding, env)
    end

    test "includes space instead of parentheses for def* macros" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "defmodule/2",
                 insert_text: "defmodule "
               }
             ] = Intellisense.get_completion_items("defmodu", binding, env)
    end

    test "includes space instead of parentheses for keyword macros" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "import/2",
                 insert_text: "import "
               }
             ] = Intellisense.get_completion_items("impor", binding, env)
    end

    test "includes doesn't include space nor parentheses for macros like __ENV__" do
      {binding, env} = eval(do: nil)

      assert [
               %{
                 label: "__ENV__/0",
                 insert_text: "__ENV__"
               }
             ] = Intellisense.get_completion_items("__EN", binding, env)
    end
  end

  describe "get_details/3" do
    test "returns nil if there are no matches" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_details("Unknown.unknown()", 2, binding, env)
    end

    test "returns subject range" do
      {binding, env} = eval(do: nil)

      assert %{range: %{from: 1, to: 18}} =
               Intellisense.get_details("Integer.to_string(10)", 15, binding, env)

      assert %{range: %{from: 1, to: 8}} =
               Intellisense.get_details("Integer.to_string(10)", 2, binding, env)
    end

    test "does not return duplicate details for functions with default arguments" do
      {binding, env} = eval(do: nil)

      assert %{contents: [_]} =
               Intellisense.get_details("Integer.to_string(10)", 15, binding, env)
    end

    test "returns details only for exactly matching identifiers" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_details("Enum.ma", 6, binding, env)
    end

    test "returns full docs" do
      {binding, env} = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("Enum.map", 6, binding, env)
      assert content =~ "## Examples"
    end

    @tag :erl_docs
    test "returns full Erlang docs" do
      {binding, env} = eval(do: nil)

      assert %{contents: [file]} = Intellisense.get_details(":file.read()", 2, binding, env)
      assert file =~ "## Performance"

      assert %{contents: [file_read]} = Intellisense.get_details(":file.read()", 8, binding, env)
      assert file_read =~ "Typical error reasons:"
    end

    test "properly parses unicode" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_details("msg = '🍵'", 8, binding, env)
    end

    test "handles operators" do
      {binding, env} = eval(do: nil)

      assert %{contents: [match_op]} = Intellisense.get_details("x = 1", 3, binding, env)
      assert match_op =~ "Match operator."
    end

    test "handles local calls" do
      {binding, env} = eval(do: nil)

      assert %{contents: [to_string_fn]} =
               Intellisense.get_details("to_string(1)", 3, binding, env)

      assert to_string_fn =~ "Converts the argument to a string"
    end
  end

  describe "get_signature_items/3" do
    test "returns nil when outside call" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_signature_items("length()", binding, env)
    end

    test "returns nil if there are no matches" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_signature_items("Unknown.unknown(", binding, env)
      assert nil == Intellisense.get_signature_items("Enum.concat(x, y,", binding, env)
    end

    test "supports remote function calls" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"],
                   documentation: """
                   Returns a list where each element is the result of invoking
                   `fun` on each corresponding element of `enumerable`.

                   ---

                   ```
                   @spec map(t(), (element() -> any())) ::
                           list()
                   ```\
                   """
                 }
               ]
             } = Intellisense.get_signature_items("Enum.map(", binding, env)
    end

    test "supports local function calls" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "length(list)",
                   arguments: ["list"],
                   documentation: """
                   Returns the length of `list`.

                   ---

                   ```
                   @spec length(list()) :: non_neg_integer()
                   ```\
                   """
                 }
               ]
             } = Intellisense.get_signature_items("length(", binding, env)
    end

    test "supports manually imported functions and macros" do
      {binding, env} =
        eval do
          import Enum
          import Protocol
        end

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"],
                   documentation: _map_doc
                 }
               ]
             } = Intellisense.get_signature_items("map(", binding, env)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: ~S"derive(protocol, module, options \\ [])",
                   arguments: ["protocol", "module", ~S"options \\ []"],
                   documentation: _derive_doc
                 }
               ]
             } = Intellisense.get_signature_items("derive(", binding, env)
    end

    test "supports remote function calls on aliases" do
      {binding, env} =
        eval do
          alias Enum, as: MyEnum
        end

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"],
                   documentation: _map_doc
                 }
               ]
             } = Intellisense.get_signature_items("MyEnum.map(", binding, env)
    end

    test "supports anonymous function calls" do
      {binding, env} =
        eval do
          add = fn x, y -> x + y end
        end

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "add.(arg1, arg2)",
                   arguments: ["arg1", "arg2"],
                   documentation: """
                   No documentation available\
                   """
                 }
               ]
             } = Intellisense.get_signature_items("add.(", binding, env)
    end

    test "supports captured remote function calls" do
      {binding, env} =
        eval do
          map = &Enum.map/2
        end

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"],
                   documentation: _map_doc
                 }
               ]
             } = Intellisense.get_signature_items("map.(", binding, env)
    end

    @tag :erl_docs
    test "shows signature with arguments for erlang modules" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "map(fun, list1)",
                   arguments: ["fun", "list1"],
                   documentation: _map_doc
                 }
               ]
             } = Intellisense.get_signature_items(":lists.map(", binding, env)
    end

    test "returns call active argument" do
      {binding, env} = eval(do: nil)

      assert %{active_argument: 0, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, ", binding, env)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, 2], ", binding, env)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, 2], fn", binding, env)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items(
                 "Enum.map([1, 2], fn x -> x * x end",
                 binding,
                 env
               )

      assert %{active_argument: 2, signature_items: [_item]} =
               Intellisense.get_signature_items("IO.ANSI.color(1, 2, 3", binding, env)
    end

    test "returns correct active argument when using pipe operator" do
      {binding, env} = eval(do: nil)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("[1, 2] |> Enum.map(", binding, env)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("[1, 2] |> Enum.map(fn", binding, env)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items(
                 "[1, 2] |> Enum.map(fn x -> x * x end",
                 binding,
                 env
               )

      assert %{active_argument: 2, signature_items: [_item]} =
               Intellisense.get_signature_items("1 |> IO.ANSI.color(2, 3", binding, env)
    end

    test "returns a single signature for fnuctions with default arguments" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: ~S"to_string(integer, base \\ 10)",
                   arguments: ["integer", ~S"base \\ 10"],
                   documentation: """
                   Returns a binary which corresponds to the text representation
                   of `integer` in the given `base`.

                   ---

                   ```
                   @spec to_string(integer(), 2..36) ::
                           String.t()
                   ```\
                   """
                 }
               ]
             } = Intellisense.get_signature_items("Integer.to_string(", binding, env)
    end

    test "returns multiple signatures for function with multiple arities" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "concat(enumerables)",
                   arguments: ["enumerables"],
                   documentation: _concat_1_docs
                 },
                 %{
                   signature: "concat(left, right)",
                   arguments: ["left", "right"],
                   documentation: _concat_2_docs
                 }
               ]
             } = Intellisense.get_signature_items("Enum.concat(", binding, env)
    end

    test "returns only signatures where active argument is at valid position" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 1,
               signature_items: [
                 %{
                   signature: "concat(left, right)",
                   arguments: ["left", "right"],
                   documentation: _concat_1_docs
                 }
               ]
             } = Intellisense.get_signature_items("Enum.concat([1, 2], ", binding, env)
    end

    test "does not return any signatures when in do-end block" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_signature_items("if true do ", binding, env)
    end

    test "does not return any signatures for module attributes" do
      {binding, env} = eval(do: nil)

      assert nil == Intellisense.get_signature_items("@length(", binding, env)
    end

    test "does not returns signatures for calls in attribute value" do
      {binding, env} = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   arguments: ["list"],
                   documentation: _length_doc,
                   signature: "length(list)"
                 }
               ]
             } = Intellisense.get_signature_items("@attr length(", binding, env)
    end
  end
end
