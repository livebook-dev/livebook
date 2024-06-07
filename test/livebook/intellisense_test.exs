defmodule Livebook.IntellisenseTest do
  use ExUnit.Case, async: true

  alias Livebook.Intellisense

  # Returns intellisense context resulting from evaluating
  # the given block of code in a fresh context.
  defmacrop eval(do: block) do
    quote do
      block = unquote(Macro.escape(block))
      binding = []
      env = Code.env_for_eval([])
      {value, binding, env} = Code.eval_quoted_with_env(block, binding, env)

      %{
        env: env,
        map_binding: fn fun -> fun.(binding) end
      }
    end
  end

  setup do
    Intellisense.clear_cache(node())
    :ok
  end

  describe "format_code/1" do
    test "formats valid code" do
      assert %{code: "1 + 1", code_markers: []} = Intellisense.format_code("1+1")
    end

    test "returns a syntax error when invalid code is given" do
      assert %{
               code: nil,
               code_markers: [
                 %{
                   line: 1,
                   description: "syntax error: expression is incomplete",
                   severity: :error
                 }
               ]
             } = Intellisense.format_code("1+")
    end
  end

  describe "get_completion_items/3" do
    test "completion when no hint given" do
      context = eval(do: nil)

      length_item = %{
        label: "length/1",
        kind: :function,
        documentation: """
        Returns the length of `list`.

        ```
        Kernel.length(list)
        ```\
        """,
        insert_text: "length(${})"
      }

      assert length_item in Intellisense.get_completion_items("", context, node())
      assert length_item in Intellisense.get_completion_items("to_string(", context, node())
      assert length_item in Intellisense.get_completion_items("Enum.map(list, ", context, node())
    end

    @tag :erl_docs
    test "Erlang module completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: ":zlib",
                 kind: :module,
                 documentation: """
                 zlib compression interface.

                 (module)\
                 """,
                 insert_text: "zlib"
               }
             ] = Intellisense.get_completion_items(":zl", context, node())
    end

    test "Erlang module no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items(":unknown", context, node())
    end

    test "Erlang module multiple values completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: ":orddict",
                 kind: :module,
                 documentation: _orddict_doc,
                 insert_text: "orddict"
               },
               %{
                 label: ":ordsets",
                 kind: :module,
                 documentation: _ordsets_doc,
                 insert_text: "ordsets"
               }
             ] = Intellisense.get_completion_items(":ord", context, node())
    end

    @tag :erl_docs
    test "Erlang root completion" do
      context = eval(do: nil)

      lists_item = %{
        label: ":lists",
        kind: :module,
        documentation: """
        List processing functions.

        (module)\
        """,
        insert_text: "lists"
      }

      assert lists_item in Intellisense.get_completion_items(":", context, node())
      assert lists_item in Intellisense.get_completion_items("  :", context, node())
    end

    @tag :erl_docs
    test "Erlang completion doesn't include quoted atoms" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items(~s{:Elixir}, context, node())
    end

    @tag :erl_docs
    test "Erlang module completion with 'in' operator in spec" do
      context = eval(do: nil)

      assert [
               %{
                 label: "open_port/2",
                 kind: :function,
                 documentation: _open_port_doc,
                 insert_text: "open_port(${})"
               }
             ] = Intellisense.get_completion_items(":erlang.open_por", context, node())
    end

    @tag :erl_docs
    test "Erlang type completion" do
      context = eval(do: nil)

      items = Intellisense.get_completion_items(":maps.iterator", context, node())

      assert %{
               label: "iterator/0",
               kind: :type,
               documentation: """
               No documentation available

               ```
               @type iterator() :: iterator(term(), term())
               ```\
               """,
               insert_text: "iterator()"
             } in items

      assert %{
               label: "iterator/2",
               kind: :type,
               documentation: """
               An iterator representing the associations in a map with keys of type `Key` and
               values of type `Value`.

               ```
               @opaque iterator(key, value)
               ```\
               """,
               insert_text: "iterator(${})"
             } in items
    end

    test "Elixir proxy" do
      context = eval(do: nil)

      assert %{
               label: "Elixir",
               kind: :module,
               documentation: """
               No documentation available

               (module)\
               """,
               insert_text: "Elixir"
             } in Intellisense.get_completion_items("Eli", context, node())
    end

    test "Elixir module completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "Enum",
                 kind: :module,
                 documentation: """
                 Functions for working with collections (known as enumerables).

                 (module)\
                 """,
                 insert_text: "Enum"
               },
               %{
                 label: "Enumerable",
                 kind: :interface,
                 documentation: """
                 Enumerable protocol used by `Enum` and `Stream` modules.

                 (protocol)\
                 """,
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("En", context, node())

      assert [
               %{
                 label: "Enumerable",
                 kind: :interface,
                 documentation: """
                 Enumerable protocol used by `Enum` and `Stream` modules.

                 (protocol)\
                 """,
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("Enumera", context, node())

      assert [
               %{
                 label: "RuntimeError",
                 kind: :struct,
                 documentation: """
                 An exception for a generic runtime error.

                 (exception)\
                 """,
                 insert_text: "RuntimeError"
               }
             ] = Intellisense.get_completion_items("RuntimeE", context, node())
    end

    test "caches all loaded modules" do
      context = eval(do: nil)
      Intellisense.get_completion_items("Hub", context, node())

      key = {Intellisense.IdentifierMatcher, node()}
      assert [_ | _] = :persistent_term.get(key, :error)
      Intellisense.IdentifierMatcher.clear_all_loaded(node())
      assert :error = :persistent_term.get(key, :error)
    end

    test "Elixir struct completion lists nested options" do
      context = eval(do: nil)

      assert %{
               label: "File.Stat",
               kind: :struct,
               documentation: """
               A struct that holds file information.

               (struct)\
               """,
               insert_text: "File.Stat"
             } in Intellisense.get_completion_items("%Fi", context, node())
    end

    test "Elixir type completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "from/0",
                 kind: :type,
                 documentation: """
                 Tuple describing the client of a call request.

                 ```
                 @type from() :: {pid(), tag :: term()}
                 ```\
                 """,
                 insert_text: "from()"
               }
             ] = Intellisense.get_completion_items("GenServer.fr", context, node())

      assert [
               %{
                 label: "internal/1",
                 kind: :type,
                 documentation: """
                 No documentation available

                 ```
                 @opaque internal(value)
                 ```\
                 """,
                 insert_text: "internal(${})"
               }
             ] = Intellisense.get_completion_items("MapSet.intern", context, node())
    end

    test "Elixir module completion with self" do
      context = eval(do: nil)

      assert [
               %{
                 label: "Enumerable",
                 kind: :interface,
                 documentation: """
                 Enumerable protocol used by `Enum` and `Stream` modules.

                 (protocol)\
                 """,
                 insert_text: "Enumerable"
               }
             ] = Intellisense.get_completion_items("Enumerable", context, node())
    end

    test "Elixir completion on modules from load path" do
      context = eval(do: nil)

      assert %{
               label: "Jason",
               kind: :module,
               documentation: """
               A blazing fast JSON parser and generator in pure Elixir.

               (module)\
               """,
               insert_text: "Jason"
             } in Intellisense.get_completion_items("Jas", context, node())
    end

    test "Elixir no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items(".", context, node())
      assert [] = Intellisense.get_completion_items("Xyz", context, node())
      assert [] = Intellisense.get_completion_items("x.Foo", context, node())
      assert [] = Intellisense.get_completion_items("x.Foo.get_by", context, node())
    end

    test "Elixir private module no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items("Livebook.TestModules.Hidd", context, node())
    end

    test "Elixir private module members completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "visible/0",
                 kind: :function,
                 documentation: """
                 No documentation available

                 ```
                 Livebook.TestModules.Hidden.visible()
                 ```\
                 """,
                 insert_text: "visible()"
               }
             ] =
               Intellisense.get_completion_items("Livebook.TestModules.Hidden.", context, node())
    end

    test "Elixir root submodule completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "Access",
                 kind: :interface,
                 documentation: """
                 Key-based access to data structures.

                 (behaviour)\
                 """,
                 insert_text: "Access"
               }
             ] = Intellisense.get_completion_items("Elixir.Acce", context, node())
    end

    test "Elixir submodule completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "ANSI",
                 kind: :module,
                 documentation: """
                 Functionality to render ANSI escape sequences.

                 (module)\
                 """,
                 insert_text: "ANSI"
               }
             ] = Intellisense.get_completion_items("IO.AN", context, node())
    end

    test "Elixir submodule no completion" do
      context = eval(do: nil)

      assert [] = Intellisense.get_completion_items("IEx.Xyz", context, node())
    end

    test "Elixir function completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "version/0",
                 kind: :function,
                 documentation: """
                 Elixir version information.

                 ```
                 System.version()
                 ```\
                 """,
                 insert_text: "version()"
               }
             ] = Intellisense.get_completion_items("System.ve", context, node())
    end

    test "Elixir sigil completion" do
      context = eval(do: nil)

      regex_item = %{
        label: "~r/2",
        kind: :function,
        documentation: """
        Handles the sigil `~r` for regular expressions.

        ```
        Kernel.sigil_r(term, modifiers)
        ```\
        """,
        insert_text: "~r"
      }

      assert regex_item in Intellisense.get_completion_items("~", context, node())

      assert [^regex_item] = Intellisense.get_completion_items("~r", context, node())
    end

    @tag :erl_docs
    test "Erlang function completion" do
      context = eval(do: nil)

      assert %{
               label: "gzip/1",
               kind: :function,
               documentation: """
               Compresses data with gz headers and checksum.

               ```
               :zlib.gzip(Data)
               ```\
               """,
               insert_text: "gzip(${})"
             } in Intellisense.get_completion_items(":zlib.gz", context, node())
    end

    test "function completion with arity" do
      context = eval(do: nil)

      assert %{
               label: "concat/1",
               kind: :function,
               documentation: """
               Given an enumerable of enumerables, concatenates the `enumerables` into
               a single list.

               ```
               Enum.concat(enumerables)
               ```\
               """,
               insert_text: "concat(${})"
             } in Intellisense.get_completion_items("Enum.concat/", context, node())

      assert [
               %{label: "count/1"},
               %{label: "count/2"}
             ] = Intellisense.get_completion_items("Enum.count/", context, node())
    end

    test "function completion same name with different arities" do
      context = eval(do: nil)

      assert [
               %{
                 label: "concat/1",
                 kind: :function,
                 documentation: """
                 Given an enumerable of enumerables, concatenates the `enumerables` into
                 a single list.

                 ```
                 Enum.concat(enumerables)
                 ```\
                 """,
                 insert_text: "concat(${})"
               },
               %{
                 label: "concat/2",
                 kind: :function,
                 documentation: """
                 Concatenates the enumerable on the `right` with the enumerable on the
                 `left`.

                 ```
                 Enum.concat(left, right)
                 ```\
                 """,
                 insert_text: "concat(${})"
               }
             ] = Intellisense.get_completion_items("Enum.concat", context, node())
    end

    test "function completion when has default args then documentation all arities have docs" do
      context = eval(do: nil)

      assert [
               %{
                 label: "utc_today/0",
                 kind: :function,
                 documentation: """
                 Returns the current date in UTC.

                 ```
                 Date.utc_today(calendar \\\\ Calendar.ISO)
                 ```\
                 """,
                 insert_text: "utc_today()"
               },
               %{
                 label: "utc_today/1",
                 kind: :function,
                 documentation: """
                 Returns the current date in UTC.

                 ```
                 Date.utc_today(calendar \\\\ Calendar.ISO)
                 ```\
                 """,
                 insert_text: "utc_today(${})"
               }
             ] = Intellisense.get_completion_items("Date.utc", context, node())
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
                 documentation: """
                 Elixir version information.

                 ```
                 System.version()
                 ```\
                 """,
                 insert_text: "version()"
               }
             ] = Intellisense.get_completion_items("mod.ve", context, node())
    end

    test "operator completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "++/2",
                 kind: :function,
                 documentation: """
                 List concatenation operator. Concatenates a proper list and a term, returning a list.

                 ```
                 left ++ right
                 ```\
                 """,
                 insert_text: "++"
               },
               %{
                 label: "+/1",
                 kind: :function,
                 documentation: """
                 Arithmetic positive unary operator.

                 ```
                 +value
                 ```\
                 """,
                 insert_text: "+"
               },
               %{
                 label: "+/2",
                 kind: :function,
                 documentation: """
                 Arithmetic addition operator.

                 ```
                 left + right
                 ```\
                 """,
                 insert_text: "+"
               }
             ] = Intellisense.get_completion_items("+", context, node())

      assert [
               %{label: "+/1"},
               %{label: "+/2"}
             ] = Intellisense.get_completion_items("+/", context, node())

      assert [
               %{label: "++/2"}
             ] = Intellisense.get_completion_items("++/", context, node())
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
                 documentation: "(field)",
                 insert_text: "bar_1"
               },
               %{
                 label: "bar_2",
                 kind: :field,
                 documentation: "(field)",
                 insert_text: "bar_2"
               },
               %{
                 label: "foo",
                 kind: :field,
                 documentation: "(field)",
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.", context, node())

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 documentation: "(field)",
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.f", context, node())
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
                 documentation: "(field)",
                 insert_text: "nested"
               }
             ] = Intellisense.get_completion_items("map.nest", context, node())

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 documentation: "(field)",
                 insert_text: "foo"
               }
             ] = Intellisense.get_completion_items("map.nested.deeply.f", context, node())

      assert [
               %{
                 label: "version/0",
                 kind: :function,
                 documentation: """
                 Elixir version information.

                 ```
                 System.version()
                 ```\
                 """,
                 insert_text: "version()"
               }
             ] = Intellisense.get_completion_items("map.nested.deeply.mod.ve", context, node())

      assert [] = Intellisense.get_completion_items("map.non.existent", context, node())
    end

    test "map string key completion is not supported" do
      context =
        eval do
          map = %{"foo" => 1}
        end

      assert [] = Intellisense.get_completion_items("map.f", context, node())
    end

    test "autocompletion off a bound variable only works for modules and maps" do
      context =
        eval do
          num = 5
          map = %{nested: %{num: 23}}
        end

      assert [] = Intellisense.get_completion_items("num.print", context, node())
      assert [] = Intellisense.get_completion_items("map.nested.num.f", context, node())
    end

    test "autocompletion using access syntax does is not supported" do
      context =
        eval do
          map = %{nested: %{deeply: %{num: 23}}}
        end

      assert [] = Intellisense.get_completion_items("map[:nested][:deeply].n", context, node())
      assert [] = Intellisense.get_completion_items("map[:nested].deeply.n", context, node())
      assert [] = Intellisense.get_completion_items("map.nested.[:deeply].n", context, node())
    end

    test "macro completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "is_nil/1",
                 kind: :function,
                 documentation: """
                 Returns `true` if `term` is `nil`, `false` otherwise.

                 ```
                 Kernel.is_nil(term)
                 ```\
                 """,
                 insert_text: "is_nil(${})"
               }
             ] = Intellisense.get_completion_items("Kernel.is_ni", context, node())
    end

    test "special forms completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "quote/2",
                 kind: :function,
                 documentation: """
                 Gets the representation of any expression.

                 ```
                 Kernel.SpecialForms.quote(opts, block)
                 ```\
                 """,
                 insert_text: "quote "
               }
             ] = Intellisense.get_completion_items("quot", context, node())
    end

    test "kernel import completion" do
      context = eval(do: nil)

      assert [
               %{
                 label: "put_in/2",
                 kind: :function,
                 documentation: """
                 Puts a value in a nested structure via the given `path`.

                 ```
                 Kernel.put_in(path, value)
                 ```\
                 """,
                 insert_text: "put_in(${})"
               },
               %{
                 label: "put_in/3",
                 kind: :function,
                 documentation: """
                 Puts a value in a nested structure.

                 ```
                 Kernel.put_in(data, keys, value)
                 ```\
                 """,
                 insert_text: "put_in(${})"
               }
             ] = Intellisense.get_completion_items("put_i", context, node())
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
                 documentation: "(variable)",
                 insert_text: "numbats"
               }
             ] = Intellisense.get_completion_items("numba", context, node())

      assert [
               %{
                 label: "numbats",
                 kind: :variable,
                 documentation: "(variable)",
                 insert_text: "numbats"
               },
               %{
                 label: "number",
                 kind: :variable,
                 documentation: "(variable)",
                 insert_text: "number"
               }
             ] = Intellisense.get_completion_items("num", context, node())

      assert [
               %{
                 label: "nothing",
                 kind: :variable,
                 documentation: "(variable)",
                 insert_text: "nothing"
               },
               %{label: "node/0"},
               %{label: "node/1"},
               %{label: "not/1"}
             ] = Intellisense.get_completion_items("no", context, node())
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
             ] = Intellisense.get_completion_items("take", context, node())

      assert %{
               label: "version/0",
               kind: :function,
               documentation: """
               Elixir version information.

               ```
               System.version()
               ```\
               """,
               insert_text: "version()"
             } in Intellisense.get_completion_items("v", context, node())

      assert [
               %{label: "derive/2"},
               %{label: "derive/3"}
             ] = Intellisense.get_completion_items("der", context, node())

      assert [
               %{label: "count/1"},
               %{label: "count/2"}
             ] = Intellisense.get_completion_items("count/", context, node())
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
             ] = Intellisense.get_completion_items("my_var", context, node())
    end

    test "completion inside expression" do
      context = eval(do: nil)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("1 En", context, node())

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("foo(En", context, node())

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("Test En", context, node())

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("foo(x,En", context, node())

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("[En", context, node())

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("{En", context, node())
    end

    test "ampersand completion" do
      context = eval(do: nil)

      assert [
               %{label: "Enum"},
               %{label: "Enumerable"}
             ] = Intellisense.get_completion_items("&En", context, node())

      assert [
               %{label: "all?/1"},
               %{label: "all?/2"}
             ] = Intellisense.get_completion_items("&Enum.al", context, node())

      assert [
               %{label: "all?/1"},
               %{label: "all?/2"}
             ] = Intellisense.get_completion_items("f = &Enum.al", context, node())
    end

    test "negation operator completion" do
      context = eval(do: nil)

      assert [
               %{label: "is_binary/1"}
             ] = Intellisense.get_completion_items("!is_bin", context, node())
    end

    test "pin operator completion" do
      context =
        eval do
          my_variable = 2
        end

      assert [
               %{label: "my_variable"}
             ] = Intellisense.get_completion_items("^my_va", context, node())
    end

    defmodule SublevelTest.LevelA.LevelB do
    end

    test "Elixir completion sublevel" do
      context = eval(do: nil)

      assert [%{label: "LevelA"}] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.SublevelTest.",
                 context,
                 node()
               )
    end

    test "complete aliases of Elixir modules" do
      context =
        eval do
          alias List, as: MyList
        end

      assert [
               %{label: "MyList"}
             ] = Intellisense.get_completion_items("MyL", context, node())

      assert [
               %{label: "to_integer/1"},
               %{label: "to_integer/2"}
             ] = Intellisense.get_completion_items("MyList.to_integ", context, node())
    end

    @tag :erl_docs
    test "complete aliases of Erlang modules" do
      context =
        eval do
          alias :lists, as: EList
        end

      assert [
               %{label: "EList"}
             ] = Intellisense.get_completion_items("EL", context, node())

      assert [
               %{label: "map/2"},
               %{label: "mapfoldl/3"},
               %{label: "mapfoldr/3"}
             ] = Intellisense.get_completion_items("EList.map", context, node())

      assert %{
               label: "max/1",
               kind: :function,
               documentation: """
               Returns the first element of `List` that compares greater than or equal to all
               other elements of `List`.

               ```
               :lists.max(List)
               ```\
               """,
               insert_text: "max(${})"
             } in Intellisense.get_completion_items("EList.", context, node())

      assert [] = Intellisense.get_completion_items("EList.Invalid", context, node())
    end

    test "completion for functions added when compiled module is reloaded" do
      context = eval(do: nil)

      {:module, _, bytecode, _} =
        defmodule Sample do
          def foo(), do: 0
        end

      assert [%{label: "foo/0"}] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.Sample.foo",
                 context,
                 node()
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
                 context,
                 node()
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
             ] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.MyStr",
                 context,
                 node()
               )
    end

    test "completion for struct keys" do
      context =
        eval do
          struct = %Livebook.IntellisenseTest.MyStruct{}
        end

      assert [
               %{label: "my_val"}
             ] = Intellisense.get_completion_items("struct.my", context, node())
    end

    test "completion for struct keys inside struct" do
      context = eval(do: nil)

      assert [
               %{
                 label: "my_val",
                 kind: :field,
                 documentation: """
                 `%Livebook.IntellisenseTest.MyStruct{}` struct field.

                 **Default**

                 ```
                 nil
                 ```\
                 """,
                 insert_text: "my_val: "
               }
             ] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{my",
                 context,
                 node()
               )
    end

    test "completion for struct keys inside struct removes filled keys" do
      context = eval(do: nil)

      assert [] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{my_val: 123, ",
                 context,
                 node()
               )
    end

    test "completion for struct keys inside struct ignores `__exception__`" do
      context = eval(do: nil)

      completions = Intellisense.get_completion_items("%ArgumentError{", context, node())

      refute Enum.find(completions, &match?(%{label: "__exception__"}, &1))
    end

    test "completion for struct keys in update syntax" do
      context = eval(do: nil)

      assert [
               %{
                 label: "my_val",
                 kind: :field,
                 documentation: _my_val_doc,
                 insert_text: "my_val: "
               }
             ] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{struct | ",
                 context,
                 node()
               )

      assert [
               %{
                 label: "my_val",
                 kind: :field,
                 documentation: _my_val_doc,
                 insert_text: "my_val: "
               }
             ] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{struct | my_v",
                 context,
                 node()
               )

      assert [] =
               Intellisense.get_completion_items(
                 "%Livebook.IntellisenseTest.MyStruct{struct | my_val: 123, ",
                 context,
                 node()
               )
    end

    test "completion for map keys in update syntax" do
      context =
        eval do
          map = %{foo: 1}
        end

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 documentation: "(field)",
                 insert_text: "foo: "
               }
             ] = Intellisense.get_completion_items("%{map | ", context, node())

      assert [
               %{
                 label: "foo",
                 kind: :field,
                 documentation: "(field)",
                 insert_text: "foo: "
               }
             ] = Intellisense.get_completion_items("%{map | fo", context, node())

      assert [] = Intellisense.get_completion_items("%{map | foo: 2, ", context, node())
    end

    test "ignore invalid Elixir module literals" do
      context = eval(do: nil)

      defmodule(:"Elixir.Livebook.IntellisenseTest.Unicodé", do: nil)

      assert [] =
               Intellisense.get_completion_items(
                 "Livebook.IntellisenseTest.Unicod",
                 context,
                 node()
               )
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
                 documentation: """
                 Provides documentation for the current module.

                 (module attribute)\
                 """,
                 insert_text: "moduledoc"
               }
             ] = Intellisense.get_completion_items("@modu", context, node())
    end

    test "handles calls on module attribute" do
      context = eval(do: nil)
      assert [] = Intellisense.get_completion_items("@attr.value", context, node())
    end

    test "includes language keywords" do
      context = eval(do: nil)

      assert [
               %{
                 label: "nil",
                 kind: :keyword,
                 documentation: "(special atom)",
                 insert_text: "nil"
               }
               | _
             ] = Intellisense.get_completion_items("nil", context, node())
    end

    test "includes space instead of parentheses for def* macros" do
      context = eval(do: nil)

      assert [
               %{
                 label: "defmodule/2",
                 insert_text: "defmodule "
               }
             ] = Intellisense.get_completion_items("defmodu", context, node())
    end

    test "includes space instead of parentheses for keyword macros" do
      context = eval(do: nil)

      assert [
               %{
                 label: "import/2",
                 insert_text: "import "
               }
             ] = Intellisense.get_completion_items("impor", context, node())
    end

    test "includes doesn't include space nor parentheses for macros like __ENV__" do
      context = eval(do: nil)

      assert [
               %{
                 label: "__ENV__/0",
                 insert_text: "__ENV__"
               }
             ] = Intellisense.get_completion_items("__EN", context, node())
    end

    test "Elixir bitstring modifiers" do
      context = eval(do: nil)

      assert [
               %{
                 label: "integer",
                 kind: :type,
                 documentation: "(bitstring option)",
                 insert_text: "integer"
               }
             ] = Intellisense.get_completion_items("<<a::intege", context, node())

      assert [
               %{
                 label: "size",
                 kind: :type,
                 documentation: "(bitstring option)",
                 insert_text: "size(${})"
               }
             ] = Intellisense.get_completion_items("<<a::siz", context, node())

      assert %{
               label: "integer",
               kind: :type,
               documentation: "(bitstring option)",
               insert_text: "integer"
             } in Intellisense.get_completion_items("<<a::", context, node())
    end

    test "completion for aliases in special forms" do
      context = eval(do: nil)

      assert [
               %{
                 label: "Range",
                 kind: :struct,
                 documentation: """
                 Returns an inclusive range between dates.

                 (struct)\
                 """,
                 insert_text: "Range"
               }
             ] = Intellisense.get_completion_items("alias Date.", context, node())

      assert %{
               label: "Atom",
               kind: :module,
               documentation: """
               Atoms are constants whose values are their own name.

               (module)\
               """,
               insert_text: "Atom"
             } in Intellisense.get_completion_items("alias ", context, node())

      refute Intellisense.get_completion_items("alias ", context, node())
             |> Enum.any?(&(&1.kind == :function))
    end
  end

  describe "get_details/3" do
    test "returns nil if there are no matches" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("Unknown.unknown()", 2, context, node())
    end

    test "returns subject range" do
      context = eval(do: nil)

      assert %{range: %{from: 1, to: 18}} =
               Intellisense.get_details("Integer.to_string(10)", 15, context, node())

      assert %{range: %{from: 1, to: 8}} =
               Intellisense.get_details("Integer.to_string(10)", 2, context, node())
    end

    test "does not return duplicate details for functions with default arguments" do
      context = eval(do: nil)

      assert %{contents: [_]} =
               Intellisense.get_details("Integer.to_string(10)", 15, context, node())
    end

    test "returns details only for exactly matching identifiers" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("Enum.ma", 6, context, node())
    end

    test "returns full docs" do
      context = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("Enum.map", 6, context, node())
      assert content =~ "## Examples"
    end

    test "returns deprecated docs" do
      context = eval(do: nil)

      assert %{contents: [content | _]} =
               Intellisense.get_details("Enum.chunk", 6, context, node())

      assert content =~ "Use Enum.chunk_every/2 instead"
    end

    test "returns since docs" do
      context = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("then", 2, context, node())
      assert content =~ "Since 1.12.0"
    end

    @tag :erl_docs
    test "returns full Erlang docs" do
      context = eval(do: nil)

      assert %{contents: [file]} = Intellisense.get_details(":file.read()", 2, context, node())
      assert file =~ "## Performance"

      assert %{contents: [file_read]} =
               Intellisense.get_details(":file.read()", 8, context, node())

      assert file_read =~ "Typical error reasons:"

      assert %{contents: [crypto]} = Intellisense.get_details(":crypto", 5, context, node())
      assert crypto =~ "This module provides a set of cryptographic functions."
    end

    test "properly parses unicode" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("msg = '🍵'", 8, context, node())
    end

    test "handles operators" do
      context = eval(do: nil)

      assert %{contents: [match_op]} = Intellisense.get_details("x = 1", 3, context, node())
      assert match_op =~ "Match operator."
    end

    test "handles local calls" do
      context = eval(do: nil)

      assert %{contents: [to_string_fn]} =
               Intellisense.get_details("to_string(1)", 3, context, node())

      assert to_string_fn =~ "Converts the argument to a string"
    end

    test "returns nil for bitstring modifiers" do
      context = eval(do: nil)

      assert nil == Intellisense.get_details("<<x :: integer>>", 6, context, node())
      assert nil == Intellisense.get_details("<<x :: integer>>", 10, context, node())
    end

    test "includes full module name in the docs" do
      context = eval(do: nil)

      assert %{contents: [date_range]} =
               Intellisense.get_details("Date.Range", 8, context, node())

      assert date_range =~ "Date.Range"
    end

    test "returns module-prepended type signatures" do
      context = eval(do: nil)

      assert %{contents: [type]} = Intellisense.get_details("Date.t", 6, context, node())
      assert type =~ "Date.t()"
    end

    @tag :erl_docs
    test "returns module-prepended Erlang type signatures" do
      context = eval(do: nil)

      assert %{contents: [type]} =
               Intellisense.get_details(":code.load_error_rsn", 8, context, node())

      assert type =~ ":code.load_error_rsn()"
    end

    test "includes type specs" do
      context = eval(do: nil)

      assert %{contents: [type]} = Intellisense.get_details("Date.t", 6, context, node())
      assert type =~ "@type t() :: %Date"

      # opaque types are listed without internal definition
      assert %{contents: [type]} =
               Intellisense.get_details("MapSet.internal", 10, context, node())

      assert type =~ "@opaque internal(value)\n"
    end

    @tag :erl_docs
    test "includes Erlang type specs" do
      context = eval(do: nil)

      assert %{contents: [type]} =
               Intellisense.get_details(":code.load_error_rsn", 8, context, node())

      assert type =~ "@type load_error_rsn() ::"
    end

    test "returns link to online documentation" do
      context = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("Integer", 1, context, node())
      assert content =~ ~r"https://hexdocs.pm/elixir/[^/]+/Integer.html"

      assert %{contents: [content]} =
               Intellisense.get_details("Integer.to_string(10)", 15, context, node())

      assert content =~ ~r"https://hexdocs.pm/elixir/[^/]+/Integer.html#to_string/2"

      # test elixir types
      assert %{contents: [content]} =
               Intellisense.get_details("GenServer.on_start", 12, context, node())

      assert content =~ ~r"https://hexdocs.pm/elixir/[^/]+/GenServer.html#t:on_start/0"

      # test erlang types
      assert %{contents: [content]} =
               Intellisense.get_details(":code.load_ret", 7, context, node())

      assert content =~ ~r"https://www.erlang.org/doc/man/code.html#type-load_ret"

      # test erlang modules on hexdocs
      assert %{contents: [content]} =
               Intellisense.get_details(":telemetry.span", 13, context, node())

      assert content =~ ~r"https://hexdocs.pm/telemetry/[^/]+/telemetry.html#span/3"

      # test erlang applications
      assert %{contents: [content]} = Intellisense.get_details(":code", 3, context, node())
      assert content =~ ~r"https://www.erlang.org/doc/man/code.html"

      assert %{contents: [content]} =
               Intellisense.get_details(":code.load_binary", 10, context, node())

      assert content =~ ~r"https://www.erlang.org/doc/man/code.html#load_binary-3"

      # test erlang modules
      assert %{contents: [content]} =
               Intellisense.get_details(":atomics.new", 11, context, node())

      assert content =~ ~r"https://www.erlang.org/doc/man/atomics.html#new-2"
    end
  end

  describe "get_signature_items/3" do
    test "returns nil when outside call" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("length()", context, node())
    end

    test "returns nil if there are no matches" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("Unknown.unknown(", context, node())
      assert nil == Intellisense.get_signature_items("Enum.concat(x, y,", context, node())
    end

    test "supports remote function calls" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"]
                 }
               ]
             } = Intellisense.get_signature_items("Enum.map(", context, node())
    end

    test "supports local function calls" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "length(list)",
                   arguments: ["list"]
                 }
               ]
             } = Intellisense.get_signature_items("length(", context, node())
    end

    test "supports manually imported functions and macros" do
      context =
        eval do
          import Enum
          import Protocol
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"]
                 }
               ]
             } = Intellisense.get_signature_items("map(", context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: ~S"derive(protocol, module, options \\ [])",
                   arguments: ["protocol", "module", ~S"options \\ []"]
                 }
               ]
             } = Intellisense.get_signature_items("derive(", context, node())
    end

    test "supports remote function calls on aliases" do
      context =
        eval do
          alias Enum, as: MyEnum
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"]
                 }
               ]
             } = Intellisense.get_signature_items("MyEnum.map(", context, node())
    end

    test "supports anonymous function calls" do
      context =
        eval do
          add = fn x, y -> x + y end
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "add.(arg1, arg2)",
                   arguments: ["arg1", "arg2"]
                 }
               ]
             } = Intellisense.get_signature_items("add.(", context, node())
    end

    test "supports captured remote function calls" do
      context =
        eval do
          map = &Enum.map/2
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"]
                 }
               ]
             } = Intellisense.get_signature_items("map.(", context, node())
    end

    @tag :erl_docs
    test "shows signature with arguments for erlang modules" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(Fun, List1)",
                   arguments: ["Fun", "List1"]
                 }
               ]
             } = Intellisense.get_signature_items(":lists.map(", context, node())
    end

    test "shows signature with default argument being an anonymous function" do
      context = eval(do: nil)

      assert %{
               active_argument: 3,
               items: [
                 %{
                   signature:
                     ~S"max_by(enumerable, fun, sorter \\ &>=/2, empty_fallback \\ fn -> raise Enum.EmptyError end)",
                   arguments: [
                     "enumerable",
                     "fun",
                     ~S"sorter \\ &>=/2",
                     ~S"empty_fallback \\ fn -> raise Enum.EmptyError end"
                   ]
                 }
               ]
             } =
               Intellisense.get_signature_items(
                 "Enum.max_by([1, 2], &Kernel.-/1, &>=/2, ",
                 context,
                 node()
               )
    end

    test "returns call active argument" do
      context = eval(do: nil)

      assert %{active_argument: 0, items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, ", context, node())

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.get_signature_items("Enum.map([1, 2], ", context, node())

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.get_signature_items(
                 "Enum.map([1, 2], fn x -> x * x end",
                 context,
                 node()
               )

      assert %{active_argument: 2, items: [_item]} =
               Intellisense.get_signature_items("IO.ANSI.color(1, 2, 3", context, node())

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.get_signature_items("elem(x, 1 + ", context, node())
    end

    test "returns correct active argument when using pipe operator" do
      context = eval(do: nil)

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.get_signature_items("[1, 2] |> Enum.map(", context, node())

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.get_signature_items(
                 "[1, 2] |> Enum.map(fn x -> x * x end",
                 context,
                 node()
               )

      assert %{active_argument: 2, items: [_item]} =
               Intellisense.get_signature_items("1 |> IO.ANSI.color(2, 3", context, node())
    end

    test "returns a single signature for functions with default arguments" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: ~S"to_string(integer, base \\ 10)",
                   arguments: ["integer", ~S"base \\ 10"]
                 }
               ]
             } = Intellisense.get_signature_items("Integer.to_string(", context, node())
    end

    test "returns multiple signatures for function with multiple arities" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "concat(enumerables)",
                   arguments: ["enumerables"]
                 },
                 %{
                   signature: "concat(left, right)",
                   arguments: ["left", "right"]
                 }
               ]
             } = Intellisense.get_signature_items("Enum.concat(", context, node())
    end

    test "returns only signatures where active argument is at valid position" do
      context = eval(do: nil)

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "concat(left, right)",
                   arguments: ["left", "right"]
                 }
               ]
             } = Intellisense.get_signature_items("Enum.concat([1, 2], ", context, node())
    end

    test "does not return any signatures when in do-end block" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("if true do ", context, node())
    end

    test "does not return any signatures for module attributes" do
      context = eval(do: nil)

      assert nil == Intellisense.get_signature_items("@length(", context, node())
    end

    test "does not returns signatures for calls in attribute value" do
      context = eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   arguments: ["list"],
                   signature: "length(list)"
                 }
               ]
             } = Intellisense.get_signature_items("@attr length(", context, node())
    end
  end
end
