defmodule Livebook.IntellisenseTest do
  use ExUnit.Case, async: true

  alias Livebook.Intellisense

  # Returns intellisense context resulting from evaluating
  # the given block of code in a fresh context.
  defmacrop eval(do: block) do
    quote do
      block = unquote(Macro.escape(block))
      binding = []
      # TODO: Use Code.env_for_eval and eval_quoted_with_env on Elixir v1.14+
      env = :elixir.env_for_eval([])
      {_, binding, env} = :elixir.eval_quoted(block, binding, env)
      # TODO: Remove this line on Elixir v1.14 as binding propagates to env correctly
      {_, binding, env} = :elixir.eval_forms(:ok, binding, env)

      %{
        env: env,
        map_binding: fn fun -> fun.(binding) end
      }
    end
  end

  describe "format_code/1" do
    test "formats valid code" do
      assert %{code: "1 + 1", code_error: nil} = Intellisense.format_code("1+1")
    end

    test "returns a syntax error when invalid code is given" do
      assert %{
               code: nil,
               code_error: %{
                 line: 1,
                 description: "syntax error: expression is incomplete"
               }
             } = Intellisense.format_code("1+")
    end
  end

  describe "get_completion_items/3" do
    test "completion when no hint given" do
      context = eval(do: nil)

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

      assert length_item in Intellisense.get_completion_items("", context)
      assert length_item in Intellisense.get_completion_items("to_string(", context)
      assert length_item in Intellisense.get_completion_items("Enum.map(list, ", context)
    end

    @tag :erl_docs
    test "Erlang module completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: ":zlib",
                 kind: :module,
                 detail: "module",
                 documentation:
                   "This module provides an API for the zlib library ([www.zlib.net](http://www.zlib.net)). It is used to compress and decompress data. The data format is described by [RFC 1950](https://www.ietf.org/rfc/rfc1950.txt), [RFC 1951](https://www.ietf.org/rfc/rfc1951.txt), and [RFC 1952](https://www.ietf.org/rfc/rfc1952.txt).",
                 insert_text: "zlib"
               }
             ] = Intellisense.get_completion_items(":zl", context)
    end

    test "Erlang module no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items(":unknown", context)
    end

    test "Erlang module multiple values completion" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items(":user", context)
    end

    @tag :erl_docs
    test "Erlang root completion" do
      context = eval(do: nil)

      lists_item = %{
        label: ":lists",
        kind: :module,
        detail: "module",
        documentation: "This module contains functions for list processing.",
        insert_text: "lists"
      }

      assert lists_item in Intellisense.get_completion_items(":", context)
      assert lists_item in Intellisense.get_completion_items("  :", context)
    end

    @tag :erl_docs
    test "Erlang completion doesn't include quoted atoms" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items(~s{:Elixir}, context)
    end

    @tag :erl_docs
    test "Erlang module completion with 'in' operator in spec" do
      context = eval(do: nil)

      assert [
               %{
                 label: "open_port/2",
                 kind: :function,
                 detail: ":erlang.open_port/2",
                 documentation: _open_port_doc,
                 insert_text: "open_port($0)"
               }
             ] = Intellisense.get_completion_items(":erlang.open_por", context)
    end

    test "Elixir proxy" do
      context = eval(do: nil)

      assert %{
               label: "Elixir",
               kind: :module,
               detail: "module",
               documentation: "No documentation available",
               insert_text: "Elixir"
             } in Intellisense.get_completion_items("E", context)
    end

    test "Elixir module completion" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items("En", context)

      assert [
               %{
                 label: "Enumerable",
                 kind: :interface,
                 detail: "protocol",
                 documentation: "Enumerable protocol used by `Enum` and `Stream` modules.",
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("Enumera", context)

      assert [
               %{
                 label: "RuntimeError",
                 kind: :struct,
                 detail: "exception",
                 documentation: "No documentation available",
                 insert_text: "RuntimeError"
               }
             ] = Intellisense.get_completion_items("RuntimeE", context)
    end

    test "Elixir struct completion lists nested options" do
      context = eval(do: nil)

      assert %{
               label: "File.Stat",
               kind: :struct,
               detail: "struct",
               documentation: "A struct that holds file information.",
               insert_text: "File.Stat"
             } in Intellisense.get_completion_items("%Fi", context)
    end

    test "Elixir type completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "from/0",
                 kind: :type,
                 detail: "typespec",
                 documentation: "Tuple describing the client of a call request.",
                 insert_text: "from"
               }
             ] = Intellisense.get_completion_items("GenServer.fr", context)

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
             ] = Intellisense.get_completion_items(":file.nam", context)
    end

    test "Elixir module completion with self" do
      context = eval(do: nil)

      assert [
               %{
                 label: "Enumerable",
                 kind: :interface,
                 detail: "protocol",
                 documentation: "Enumerable protocol used by `Enum` and `Stream` modules.",
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("Enumerable", context)
    end

    test "Elixir completion on modules from load path" do
      context = eval(do: nil)

      assert %{
               label: "Jason",
               kind: :module,
               detail: "module",
               documentation: "A blazing fast JSON parser and generator in pure Elixir.",
               insert_text: "Jason"
             } in Intellisense.get_completion_items("Jas", context)
    end

    test "Elixir no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items(".", context)
      assert [] = Intellisense.get_completion_items("Xyz", context)
      assert [] = Intellisense.get_completion_items("x.Foo", context)
      assert [] = Intellisense.get_completion_items("x.Foo.get_by", context)
    end

    test "Elixir private module no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items("Livebook.TestModules.Hidd", context)
    end

    test "Elixir private module members completion" do
      context = eval(do: nil)

      assert [
               %{
                 detail: "Livebook.TestModules.Hidden.visible()",
                 documentation: "No documentation available",
                 insert_text: "visible()",
                 kind: :function,
                 label: "visible/0"
               }
             ] = Intellisense.get_completion_items("Livebook.TestModules.Hidden.", context)
    end

    test "Elixir root submodule completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "Access",
                 kind: :interface,
                 detail: "behaviour",
                 documentation: "Key-based access to data structures.",
                 insert_text: "Access"
               }
             ] = Intellisense.get_completion_items("Elixir.Acce", context)
    end

    test "Elixir submodule completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "ANSI",
                 kind: :module,
                 detail: "module",
                 documentation: "Functionality to render ANSI escape sequences.",
                 insert_text: "ANSI"
               }
             ] = Intellisense.get_completion_items("IO.AN", context)
    end

    test "Elixir submodule no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items("IEx.Xyz", context)
    end

    test "Elixir function completion" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items("System.ve", context)
    end

    test "Elixir sigil completion" do
      context = eval(do: nil)

      regex_item = %{
        label: "~r/2",
        kind: :function,
        detail: "Kernel.sigil_r(term, modifiers)",
        documentation: "Handles the sigil `~r` for regular expressions.",
        insert_text: "~r"
      }

      assert regex_item in Intellisense.get_completion_items("~", context)

      assert [^regex_item] = Intellisense.get_completion_items("~r", context)
    end

    @tag :erl_docs
    test "Erlang function completion" do
      context = eval(do: nil)

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
             } in Intellisense.get_completion_items(":zlib.gz", context)
    end

    test "function completion with arity" do
      context = eval(do: nil)

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
             } in Intellisense.get_completion_items("Enum.concat/", context)

      assert [
               %{label: "count/1"},
               %{label: "count/2"}
             ] = Intellisense.get_completion_items("Enum.count/", context)
    end

    test "function completion same name with different arities" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items("Enum.concat", context)
    end

    test "function completion when has default args then documentation all arities have docs" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items("Enum.jo", context)
    end

    test "function completion using a variable bound to a module" do
      context =
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
             ] = Intellisense.get_completion_items("mod.ve", context)
    end

    test "operator completion" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items("+", context)

      assert [
               %{label: "+/1"},
               %{label: "+/2"}
             ] = Intellisense.get_completion_items("+/", context)

      assert [
               %{label: "++/2"}
             ] = Intellisense.get_completion_items("++/", context)
    end

    test "map atom key completion" do
      context =
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
             ] = Intellisense.get_completion_items("map.", context)

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.f", context)
    end

    test "nested map atom key completion" do
      context =
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
             ] = Intellisense.get_completion_items("map.nest", context)

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 detail: "field",
                 documentation: nil,
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.nested.deeply.f", context)

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
             ] = Intellisense.get_completion_items("map.nested.deeply.mod.ve", context)

      assert [] = Intellisense.get_completion_items("map.non.existent", context)
    end

    test "map string key completion is not supported" do
      context =
        eval do
          map = %{"foo" => 1}
        end

      assert [] = Intellisense.get_completion_items("map.f", context)
    end

    test "autocompletion off a bound variable only works for modules and maps" do
      context =
        eval do
          num = 5
          map = %{nested: %{num: 23}}
        end

      assert [] = Intellisense.get_completion_items("num.print", context)
      assert [] = Intellisense.get_completion_items("map.nested.num.f", context)
    end

    test "autocompletion using access syntax does is not supported" do
      context =
        eval do
          map = %{nested: %{deeply: %{num: 23}}}
        end

      assert [] = Intellisense.get_completion_items("map[:nested][:deeply].n", context)
      assert [] = Intellisense.get_completion_items("map[:nested].deeply.n", context)
      assert [] = Intellisense.get_completion_items("map.nested.[:deeply].n", context)
    end

    test "macro completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "is_nil/1",
                 kind: :function,
                 detail: "Kernel.is_nil(term)",
                 documentation: "Returns `true` if `term` is `nil`, `false` otherwise.",
                 insert_text: "is_nil($0)"
               }
             ] = Intellisense.get_completion_items("Kernel.is_ni", context)
    end

    test "special forms completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "quote/2",
                 kind: :function,
                 detail: "Kernel.SpecialForms.quote(opts, block)",
                 documentation: "Gets the representation of any expression.",
                 insert_text: "quote "
               }
             ] = Intellisense.get_completion_items("quot", context)
    end

    test "kernel import completion" do
      context = eval(do: nil)

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
             ] = Intellisense.get_completion_items("put_i", context)
    end

    test "variable name completion" do
      context =
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
             ] = Intellisense.get_completion_items("numba", context)

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
             ] = Intellisense.get_completion_items("num", context)

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
             ] = Intellisense.get_completion_items("no", context)
    end

    test "completion of manually imported functions and macros" do
      context =
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
             ] = Intellisense.get_completion_items("take", context)

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
             } in Intellisense.get_completion_items("v", context)

      assert [
               %{label: "derive/2"},
               %{label: "derive/3"}
             ] = Intellisense.get_completion_items("der", context)

      assert [
               %{label: "count/1"},
               %{label: "count/2"}
             ] = Intellisense.get_completion_items("count/", context)
    end

    test "ignores quoted variables when performing variable completion" do
      context =
        eval do
          quote do
            var!(my_var_1, Elixir) = 1
          end

          my_var_2 = 2
        end

      assert [
               %{label: "my_var_2"}
             ] = Intellisense.get_completion_items("my_var", context)
    end

    test "completion inside expression" do
      context = eval(do: nil)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("1 En", context)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("foo(En", context)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("Test En", context)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("foo(x,En", context)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("[En", context)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("{En", context)
    end

    test "ampersand completion" do
      context = eval(do: nil)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("&En", context)

      assert [
               %{label: "all?/1"},
               %{label: "all?/2"}
             ] = Intellisense.get_completion_items("&Enum.al", context)

      assert [
               %{label: "all?/1"},
               %{label: "all?/2"}
             ] = Intellisense.get_completion_items("f = &Enum.al", context)
    end

    test "negation operator completion" do
      context = eval(do: nil)

      assert [
               %{label: "is_binary/1"}
             ] = Intellisense.get_completion_items("!is_bin", context)
    end

    test "pin operator completion" do
      context =
        eval do
          my_variable = 2
        end

      assert [
               %{label: "my_variable"}
             ] = Intellisense.get_completion_items("^my_va", context)
    end

    defmodule SublevelTest.LevelA.LevelB do
    end

    test "Elixir completion sublevel" do
      context = eval(do: nil)

      assert [%{label: "LevelA"}] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.SublevelTest.",
                 context
               )
    end

    test "complete aliases of Elixir modules" do
      context =
        eval do
          alias List, as: MyList
        end

      assert [
               %{label: "MyList"}
             ] = Intellisense.get_completion_items("MyL", context)

      assert [
               %{label: "to_integer/1"},
               %{label: "to_integer/2"}
             ] = Intellisense.get_completion_items("MyList.to_integ", context)
    end

    @tag :erl_docs
    test "complete aliases of Erlang modules" do
      context =
        eval do
          alias :lists, as: EList
        end

      assert [
               %{label: "EList"}
             ] = Intellisense.get_completion_items("EL", context)

      assert [
               %{label: "map/2"},
               %{label: "mapfoldl/3"},
               %{label: "mapfoldr/3"}
             ] = Intellisense.get_completion_items("EList.map", context)

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
             } in Intellisense.get_completion_items("EList.", context)

      assert [] = Intellisense.get_completion_items("EList.Invalid", context)
    end

    test "completion for functions added when compiled module is reloaded" do
      context = eval(do: nil)

      {:module, _, bytecode, _} =
        defmodule Sample do
          def foo(), do: 0
        end

      assert [%{label: "foo/0"}] =
               Intellisense.get_completion_items("Livebook.IntellisenseTest.Sample.foo", context)

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
                 context
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
      context = eval(do: nil)

      assert [
               %{label: "MyStruct"}
             ] = Intellisense.get_completion_items("Livebook.IntellisenseTest.MyStr", context)
    end

    test "completion for struct keys" do
      context =
        eval do
          struct = %Livebook.IntellisenseTest.MyStruct{}
        end

      assert [
               %{label: "my_val"}
             ] = Intellisense.get_completion_items("struct.my", context)
    end

    test "completion for struct keys inside struct" do
      context = eval(do: nil)

      assert [
               %{
                 label: "my_val",
                 kind: :field,
                 detail: "Livebook.IntellisenseTest.MyStruct struct field",
                 documentation: "```\nmy_val\n```\n\n---\n\n**Default**\n\n```\nnil\n```\n",
                 insert_text: "my_val: "
               }
             ] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{my",
                 context
               )
    end

    test "completion for struct keys inside struct removes filled keys" do
      context =
        eval do
          struct = %Livebook.IntellisenseTest.MyStruct{}
        end

      assert [] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{my_val: 123, ",
                 context
               )
    end

    test "completion for struct keys inside struct ignores `__exception__`" do
      context = eval(do: nil)

      completions = Intellisense.get_completion_items("%ArgumentError{", context)

      refute Enum.find(completions, &match?(%{label: "__exception__"}, &1))
    end

    test "ignore invalid Elixir module literals" do
      context = eval(do: nil)

      defmodule(:"Elixir.Livebook.IntellisenseTest.Unicodé", do: nil)

      assert [] = Intellisense.get_completion_items("Livebook.IntellisenseTest.Unicod", context)
    after
      :code.purge(:"Elixir.Livebook.IntellisenseTest.Unicodé")
      :code.delete(:"Elixir.Livebook.IntellisenseTest.Unicodé")
    end

    test "known Elixir module attributes completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "moduledoc",
                 kind: :variable,
                 detail: "module attribute",
                 documentation: "Provides documentation for the current module.",
                 insert_text: "moduledoc"
               }
             ] = Intellisense.get_completion_items("@modu", context)
    end

    test "handles calls on module attribute" do
      context = eval(do: nil)
      assert [] = Intellisense.get_completion_items("@attr.value", context)
    end

    test "includes language keywords" do
      context = eval(do: nil)

      assert [
               %{
                 label: "do",
                 kind: :keyword,
                 detail: "do-end block",
                 documentation: nil,
                 insert_text: "do\n  $0\nend"
               }
               | _
             ] = Intellisense.get_completion_items("do", context)
    end

    test "includes space instead of parentheses for def* macros" do
      context = eval(do: nil)

      assert [
               %{
                 label: "defmodule/2",
                 insert_text: "defmodule "
               }
             ] = Intellisense.get_completion_items("defmodu", context)
    end

    test "includes space instead of parentheses for keyword macros" do
      context = eval(do: nil)

      assert [
               %{
                 label: "import/2",
                 insert_text: "import "
               }
             ] = Intellisense.get_completion_items("impor", context)
    end

    test "includes doesn't include space nor parentheses for macros like __ENV__" do
      context = eval(do: nil)

      assert [
               %{
                 label: "__ENV__/0",
                 insert_text: "__ENV__"
               }
             ] = Intellisense.get_completion_items("__EN", context)
    end
  end

  describe "get_details/3" do
    test "returns nil if there are no matches" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("Unknown.unknown()", 2, context)
    end

    test "returns subject range" do
      context = eval(do: nil)

      assert %{range: %{from: 1, to: 18}} =
               Intellisense.get_details("Integer.to_string(10)", 15, context)

      assert %{range: %{from: 1, to: 8}} =
               Intellisense.get_details("Integer.to_string(10)", 2, context)
    end

    test "does not return duplicate details for functions with default arguments" do
      context = eval(do: nil)

      assert %{contents: [_]} = Intellisense.get_details("Integer.to_string(10)", 15, context)
    end

    test "returns details only for exactly matching identifiers" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("Enum.ma", 6, context)
    end

    test "returns full docs" do
      context = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("Enum.map", 6, context)
      assert content =~ "## Examples"
    end

    test "returns deprecated docs" do
      context = eval(do: nil)

      assert %{contents: [content | _]} = Intellisense.get_details("Enum.chunk", 6, context)
      assert content =~ "Use Enum.chunk_every/2 instead"
    end

    test "returns since docs" do
      context = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("then", 2, context)
      assert content =~ "Since 1.12.0"
    end

    @tag :erl_docs
    test "returns full Erlang docs" do
      context = eval(do: nil)

      assert %{contents: [file]} = Intellisense.get_details(":file.read()", 2, context)
      assert file =~ "## Performance"

      assert %{contents: [file_read]} = Intellisense.get_details(":file.read()", 8, context)
      assert file_read =~ "Typical error reasons:"
    end

    test "properly parses unicode" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("msg = '🍵'", 8, context)
    end

    test "handles operators" do
      context = eval(do: nil)

      assert %{contents: [match_op]} = Intellisense.get_details("x = 1", 3, context)
      assert match_op =~ "Match operator."
    end

    test "handles local calls" do
      context = eval(do: nil)

      assert %{contents: [to_string_fn]} = Intellisense.get_details("to_string(1)", 3, context)

      assert to_string_fn =~ "Converts the argument to a string"
    end

    test "includes full module name in the docs" do
      context = eval(do: nil)

      assert %{contents: [date_range]} = Intellisense.get_details("Date.Range", 8, context)
      assert date_range =~ "Date.Range"
    end
  end

  describe "get_signature_items/3" do
    test "returns nil when outside call" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("length()", context)
    end

    test "returns nil if there are no matches" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("Unknown.unknown(", context)
      assert nil == Intellisense.get_signature_items("Enum.concat(x, y,", context)
    end

    test "supports remote function calls" do
      context = eval(do: nil)

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
             } = Intellisense.get_signature_items("Enum.map(", context)
    end

    test "supports local function calls" do
      context = eval(do: nil)

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
             } = Intellisense.get_signature_items("length(", context)
    end

    test "supports manually imported functions and macros" do
      context =
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
             } = Intellisense.get_signature_items("map(", context)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: ~S"derive(protocol, module, options \\ [])",
                   arguments: ["protocol", "module", ~S"options \\ []"],
                   documentation: _derive_doc
                 }
               ]
             } = Intellisense.get_signature_items("derive(", context)
    end

    test "supports remote function calls on aliases" do
      context =
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
             } = Intellisense.get_signature_items("MyEnum.map(", context)
    end

    test "supports anonymous function calls" do
      context =
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
             } = Intellisense.get_signature_items("add.(", context)
    end

    test "supports captured remote function calls" do
      context =
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
             } = Intellisense.get_signature_items("map.(", context)
    end

    @tag :erl_docs
    test "shows signature with arguments for erlang modules" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   signature: "map(fun, list1)",
                   arguments: ["fun", "list1"],
                   documentation: _map_doc
                 }
               ]
             } = Intellisense.get_signature_items(":lists.map(", context)
    end

    test "returns call active argument" do
      context = eval(do: nil)

      assert %{active_argument: 0, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, ", context)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, 2], ", context)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, 2], fn", context)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, 2], fn x -> x * x end", context)

      assert %{active_argument: 2, signature_items: [_item]} =
               Intellisense.get_signature_items("IO.ANSI.color(1, 2, 3", context)
    end

    test "returns correct active argument when using pipe operator" do
      context = eval(do: nil)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("[1, 2] |> Enum.map(", context)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("[1, 2] |> Enum.map(fn", context)

      assert %{active_argument: 1, signature_items: [_item]} =
               Intellisense.get_signature_items("[1, 2] |> Enum.map(fn x -> x * x end", context)

      assert %{active_argument: 2, signature_items: [_item]} =
               Intellisense.get_signature_items("1 |> IO.ANSI.color(2, 3", context)
    end

    test "returns a single signature for fnuctions with default arguments" do
      context = eval(do: nil)

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
             } = Intellisense.get_signature_items("Integer.to_string(", context)
    end

    test "returns multiple signatures for function with multiple arities" do
      context = eval(do: nil)

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
             } = Intellisense.get_signature_items("Enum.concat(", context)
    end

    test "returns only signatures where active argument is at valid position" do
      context = eval(do: nil)

      assert %{
               active_argument: 1,
               signature_items: [
                 %{
                   signature: "concat(left, right)",
                   arguments: ["left", "right"],
                   documentation: _concat_1_docs
                 }
               ]
             } = Intellisense.get_signature_items("Enum.concat([1, 2], ", context)
    end

    test "does not return any signatures when in do-end block" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("if true do ", context)
    end

    test "does not return any signatures for module attributes" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("@length(", context)
    end

    test "does not returns signatures for calls in attribute value" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               signature_items: [
                 %{
                   arguments: ["list"],
                   documentation: _length_doc,
                   signature: "length(list)"
                 }
               ]
             } = Intellisense.get_signature_items("@attr length(", context)
    end
  end
end
