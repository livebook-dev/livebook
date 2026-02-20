defmodule Livebook.Intellisense.ElixirTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.Intellisense

  setup do
    Intellisense.clear_cache(node())
    :ok
  end

  describe "format" do
    test "formats valid code" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               code: "1 + 1",
               code_markers: []
             } = Intellisense.Elixir.handle_request({:format, "1+1"}, context, node())
    end

    test "returns a syntax error when invalid code is given" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               code: nil,
               code_markers: [
                 %{
                   line: 1,
                   description: "syntax error: expression is incomplete",
                   severity: :error
                 }
               ]
             } = Intellisense.Elixir.handle_request({:format, "1+"}, context, node())
    end
  end

  describe "completion" do
    test "completion when no hint given" do
      context = intellisense_context_from_eval(do: nil)

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

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, ""}, context, node())

      assert length_item in items

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "to_string("}, context, node())

      assert length_item in items

      assert %{items: items} =
               Intellisense.Elixir.handle_request(
                 {:completion, "Enum.map(list, "},
                 context,
                 node()
               )

      assert length_item in items
    end

    @tag :erl_docs
    test "Erlang module completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: ":zlib",
                   kind: :module,
                   documentation: """
                   zlib compression interface.

                   (module)\
                   """,
                   insert_text: "zlib"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, ":zl"}, context, node())
    end

    test "Erlang module no completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, ":unknown"}, context, node())
    end

    test "Erlang module multiple values completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, ":ord"}, context, node())
    end

    @tag :erl_docs
    test "Erlang root completion" do
      context = intellisense_context_from_eval(do: nil)

      lists_item = %{
        label: ":lists",
        kind: :module,
        documentation: """
        List processing functions.

        (module)\
        """,
        insert_text: "lists"
      }

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, ":"}, context, node())

      assert lists_item in items

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "  :"}, context, node())

      assert lists_item in items
    end

    @tag :erl_docs
    test "Erlang completion doesn't include quoted atoms" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, ~s{:Elixir}}, context, node())
    end

    @tag :erl_docs
    test "Erlang module completion with 'in' operator in spec" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "open_port/2",
                   kind: :function,
                   documentation: _open_port_doc,
                   insert_text: "open_port(${})"
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, ":erlang.open_por"},
                 context,
                 node()
               )
    end

    @tag :erl_docs
    test "Erlang type completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request(
                 {:completion, ":maps.iterator"},
                 context,
                 node()
               )

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
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "Eli"}, context, node())

      assert %{
               label: "Elixir",
               kind: :module,
               documentation: """
               No documentation available

               (module)\
               """,
               insert_text: "Elixir"
             } in items
    end

    test "Elixir module completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "En"}, context, node())

      assert %{
               items: [
                 %{
                   label: "Enumerable",
                   kind: :interface,
                   documentation: """
                   Enumerable protocol used by `Enum` and `Stream` modules.

                   (protocol)\
                   """,
                   insert_text: "Enumerable"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Enumera"}, context, node())

      assert %{
               items: [
                 %{
                   label: "RuntimeError",
                   kind: :struct,
                   documentation: """
                   An exception for a generic runtime error.

                   (exception)\
                   """,
                   insert_text: "RuntimeError"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "RuntimeE"}, context, node())
    end

    test "Elixir struct completion lists nested options" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "%Fi"}, context, node())

      assert %{
               label: "File.Stat",
               kind: :struct,
               documentation: """
               A struct that holds file information.

               (struct)\
               """,
               insert_text: "File.Stat"
             } in items
    end

    test "Elixir type completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } =
               Intellisense.Elixir.handle_request({:completion, "GenServer.fr"}, context, node())

      assert %{
               items: [
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
               ]
             } =
               Intellisense.Elixir.handle_request({:completion, "MapSet.intern"}, context, node())
    end

    test "Elixir module completion with self" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "Enumerable",
                   kind: :interface,
                   documentation: """
                   Enumerable protocol used by `Enum` and `Stream` modules.

                   (protocol)\
                   """,
                   insert_text: "Enumerable"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Enumerable"}, context, node())
    end

    test "Elixir completion on modules from load path" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "R"}, context, node())

      assert %{
               label: "Req",
               kind: :module,
               documentation: """
               The high-level API.

               (module)\
               """,
               insert_text: "Req"
             } in items
    end

    test "Elixir no completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "."}, context, node())

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "Xyz"}, context, node())

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "x.Foo"}, context, node())

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "x.Foo.get_by"}, context, node())
    end

    test "Elixir private module no completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.TestModules.Hidd"},
                 context,
                 node()
               )
    end

    test "Elixir private module members completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.TestModules.Hidden."},
                 context,
                 node()
               )
    end

    test "Elixir root submodule completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "Access",
                   kind: :interface,
                   documentation: """
                   Key-based access to data structures.

                   (behaviour)\
                   """,
                   insert_text: "Access"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Elixir.Acce"}, context, node())
    end

    test "Elixir submodule completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "ANSI",
                   kind: :module,
                   documentation: """
                   Functionality to render ANSI escape sequences.

                   (module)\
                   """,
                   insert_text: "ANSI"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "IO.AN"}, context, node())
    end

    test "Elixir submodule no completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "IEx.Xyz"}, context, node())
    end

    test "Elixir function completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "System.ve"}, context, node())
    end

    test "Elixir sigil completion" do
      context = intellisense_context_from_eval(do: nil)

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

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "~"}, context, node())

      assert regex_item in items

      assert %{items: [^regex_item]} =
               Intellisense.Elixir.handle_request({:completion, "~r"}, context, node())
    end

    @tag :erl_docs
    test "Erlang function completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, ":zlib.gz"}, context, node())

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
             } in items
    end

    test "function completion with arity" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "Enum.concat/"}, context, node())

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
             } in items

      assert %{
               items: [
                 %{label: "count/1"},
                 %{label: "count/2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Enum.count/"}, context, node())
    end

    test "function completion same name with different arities" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Enum.concat"}, context, node())
    end

    test "function completion when has default args then documentation all arities have docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Date.utc"}, context, node())
    end

    test "function completion using a variable bound to a module" do
      context =
        intellisense_context_from_eval do
          mod = System
        end

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "mod.ve"}, context, node())
    end

    test "operator completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "+"}, context, node())

      assert %{
               items: [
                 %{label: "+/1"},
                 %{label: "+/2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "+/"}, context, node())

      assert %{
               items: [
                 %{label: "++/2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "++/"}, context, node())
    end

    test "map atom key completion" do
      context =
        intellisense_context_from_eval do
          map = %{
            foo: 1,
            bar_1: ~r/pattern/,
            bar_2: true
          }
        end

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "map."}, context, node())

      assert %{
               items: [
                 %{
                   label: "foo",
                   kind: :field,
                   documentation: "(field)",
                   insert_text: "foo"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "map.f"}, context, node())
    end

    test "nested map atom key completion" do
      context =
        intellisense_context_from_eval do
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

      assert %{
               items: [
                 %{
                   label: "nested",
                   kind: :field,
                   documentation: "(field)",
                   insert_text: "nested"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "map.nest"}, context, node())

      assert %{
               items: [
                 %{
                   label: "foo",
                   kind: :field,
                   documentation: "(field)",
                   insert_text: "foo"
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "map.nested.deeply.f"},
                 context,
                 node()
               )

      assert %{
               items: [
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
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "map.nested.deeply.mod.ve"},
                 context,
                 node()
               )

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "map.non.existent"},
                 context,
                 node()
               )
    end

    test "map string key completion is not supported" do
      context =
        intellisense_context_from_eval do
          map = %{"foo" => 1}
        end

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "map.f"}, context, node())
    end

    test "autocompletion off a bound variable only works for modules and maps" do
      context =
        intellisense_context_from_eval do
          num = 5
          map = %{nested: %{num: 23}}
        end

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "num.print"}, context, node())

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "map.nested.num.f"},
                 context,
                 node()
               )
    end

    test "autocompletion using access syntax does is not supported" do
      context =
        intellisense_context_from_eval do
          map = %{nested: %{deeply: %{num: 23}}}
        end

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "map[:nested][:deeply].n"},
                 context,
                 node()
               )

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "map[:nested].deeply.n"},
                 context,
                 node()
               )

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "map.nested.[:deeply].n"},
                 context,
                 node()
               )
    end

    test "macro completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } =
               Intellisense.Elixir.handle_request({:completion, "Kernel.is_ni"}, context, node())
    end

    test "special forms completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "quot"}, context, node())
    end

    test "kernel import completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "put_i"}, context, node())
    end

    test "variable name completion" do
      context =
        intellisense_context_from_eval do
          number = 3
          numbats = ["numbat", "numbat"]
          nothing = nil
        end

      assert %{
               items: [
                 %{
                   label: "numbats",
                   kind: :variable,
                   documentation: "(variable)",
                   insert_text: "numbats"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "numba"}, context, node())

      assert %{
               items: [
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
               ]
             } = Intellisense.Elixir.handle_request({:completion, "num"}, context, node())

      assert %{
               items: [
                 %{
                   label: "nothing",
                   kind: :variable,
                   documentation: "(variable)",
                   insert_text: "nothing"
                 },
                 %{label: "node/0"},
                 %{label: "node/1"},
                 %{label: "not/1"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "no"}, context, node())
    end

    test "completion of manually imported functions and macros" do
      context =
        intellisense_context_from_eval do
          import Enum
          import System, only: [version: 0]
          import Protocol
        end

      assert %{
               items: [
                 %{label: "take/2"},
                 %{label: "take_every/2"},
                 %{label: "take_random/2"},
                 %{label: "take_while/2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "take"}, context, node())

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "v"}, context, node())

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
             } in items

      assert %{
               items: [
                 %{label: "derive/2"},
                 %{label: "derive/3"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "der"}, context, node())

      assert %{
               items: [
                 %{label: "count/1"},
                 %{label: "count/2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "count/"}, context, node())
    end

    test "ignores quoted variables when performing variable completion" do
      context =
        intellisense_context_from_eval do
          quote do
            var!(my_var_1, Elixir) = 1
          end

          my_var_2 = 2
        end

      assert %{
               items: [
                 %{label: "my_var_2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "my_var"}, context, node())
    end

    test "completion inside expression" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "1 En"}, context, node())

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "foo(En"}, context, node())

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "Test En"}, context, node())

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "foo(x,En"}, context, node())

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "[En"}, context, node())

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "{En"}, context, node())
    end

    test "ampersand completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{label: "Enum"},
                 %{label: "Enumerable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "&En"}, context, node())

      assert %{
               items: [
                 %{label: "all?/1"},
                 %{label: "all?/2"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "&Enum.al"}, context, node())

      assert %{
               items: [
                 %{label: "all?/1"},
                 %{label: "all?/2"}
               ]
             } =
               Intellisense.Elixir.handle_request({:completion, "f = &Enum.al"}, context, node())
    end

    test "negation operator completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{label: "is_binary/1"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "!is_bin"}, context, node())
    end

    test "pin operator completion" do
      context =
        intellisense_context_from_eval do
          my_variable = 2
        end

      assert %{
               items: [
                 %{label: "my_variable"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "^my_va"}, context, node())
    end

    @tag :tmp_dir
    test "Elixir completion sublevel", %{tmp_dir: tmp_dir} do
      context =
        intellisense_context_from_eval tmp_dir do
        end

      compile_and_save_bytecode(tmp_dir, ~S'''
      defmodule Livebook.Intellisense.ElixirTest.SublevelTest.LevelA.LevelB do
      end
      ''')

      assert %{items: [%{label: "LevelA"}]} =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.Intellisense.ElixirTest.SublevelTest."},
                 context,
                 node()
               )
    end

    test "complete aliases of Elixir modules" do
      context =
        intellisense_context_from_eval do
          alias List, as: MyList
        end

      assert %{
               items: [
                 %{label: "MyList"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "MyL"}, context, node())

      assert %{
               items: [
                 %{label: "to_integer/1"},
                 %{label: "to_integer/2"}
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "MyList.to_integ"},
                 context,
                 node()
               )
    end

    @tag :erl_docs
    test "complete aliases of Erlang modules" do
      context =
        intellisense_context_from_eval do
          alias :lists, as: EList
        end

      assert %{
               items: [
                 %{label: "EList"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "EL"}, context, node())

      assert %{
               items: [
                 %{label: "map/2"},
                 %{label: "mapfoldl/3"},
                 %{label: "mapfoldr/3"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "EList.map"}, context, node())

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "EList."}, context, node())

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
             } in items

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "EList.Invalid"}, context, node())
    end

    test "completion for functions added when compiled module is reloaded" do
      context = intellisense_context_from_eval(do: nil)

      {:module, _, bytecode, _} =
        defmodule Sample do
          def foo(), do: 0
        end

      assert %{items: [%{label: "foo/0"}]} =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.Intellisense.ElixirTest.Sample.foo"},
                 context,
                 node()
               )

      Code.compiler_options(ignore_module_conflict: true)

      defmodule Sample do
        def foo(), do: 0
        def foobar(), do: 0
      end

      assert %{
               items: [
                 %{label: "foo/0"},
                 %{label: "foobar/0"}
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.Intellisense.ElixirTest.Sample.foo"},
                 context,
                 node()
               )
    after
      Code.compiler_options(ignore_module_conflict: false)
      :code.purge(Sample)
      :code.delete(Sample)
    end

    @tag :tmp_dir
    test "completion for struct names", %{tmp_dir: tmp_dir} do
      context =
        intellisense_context_from_eval tmp_dir do
        end

      compile_and_save_bytecode(tmp_dir, ~S'''
      defmodule Livebook.Intellisense.ElixirTest.MyStruct do
        defstruct [:my_val]
      end
      ''')

      assert %{
               items: [
                 %{label: "MyStruct"}
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.Intellisense.ElixirTest.MyStr"},
                 context,
                 node()
               )
    end

    @tag :tmp_dir
    test "completion for struct keys", %{tmp_dir: tmp_dir} do
      compile_and_save_bytecode(tmp_dir, ~S'''
      defmodule Livebook.Intellisense.ElixirTest.MyStruct do
        defstruct [:my_val]
      end
      ''')

      context =
        intellisense_context_from_eval tmp_dir do
          struct = %Livebook.Intellisense.ElixirTest.MyStruct{}
        end

      assert %{
               items: [
                 %{label: "my_val"}
               ]
             } = Intellisense.Elixir.handle_request({:completion, "struct.my"}, context, node())
    end

    @tag :tmp_dir
    test "completion for struct keys inside struct", %{tmp_dir: tmp_dir} do
      context =
        intellisense_context_from_eval tmp_dir do
        end

      compile_and_save_bytecode(tmp_dir, ~S'''
      defmodule Livebook.Intellisense.ElixirTest.MyStruct do
        defstruct [:my_val]
      end
      ''')

      assert %{
               items: [
                 %{
                   label: "my_val",
                   kind: :field,
                   documentation: """
                   `%Livebook.Intellisense.ElixirTest.MyStruct{}` struct field.

                   **Default**

                   ```
                   nil
                   ```\
                   """,
                   insert_text: "my_val: "
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "%Livebook.Intellisense.ElixirTest.MyStruct{my"},
                 context,
                 node()
               )
    end

    @tag :tmp_dir
    test "completion for struct keys inside struct removes filled keys",
         %{tmp_dir: tmp_dir} do
      context =
        intellisense_context_from_eval tmp_dir do
        end

      compile_and_save_bytecode(tmp_dir, ~S'''
      defmodule Livebook.Intellisense.ElixirTest.MyStruct do
        defstruct [:my_val]
      end
      ''')

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "%Livebook.Intellisense.ElixirTest.MyStruct{my_val: 123, "},
                 context,
                 node()
               )
    end

    test "completion for struct keys inside struct ignores `__exception__`" do
      context = intellisense_context_from_eval(do: nil)

      completions =
        Intellisense.Elixir.handle_request({:completion, "%ArgumentError{"}, context, node())

      refute Enum.find(completions, &match?(%{label: "__exception__"}, &1))
    end

    @tag :tmp_dir
    test "completion for struct keys in update syntax", %{tmp_dir: tmp_dir} do
      context =
        intellisense_context_from_eval tmp_dir do
        end

      compile_and_save_bytecode(tmp_dir, ~S'''
      defmodule Livebook.Intellisense.ElixirTest.MyStruct do
        defstruct [:my_val]
      end
      ''')

      assert %{
               items: [
                 %{
                   label: "my_val",
                   kind: :field,
                   documentation: _my_val_doc,
                   insert_text: "my_val: "
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "%Livebook.Intellisense.ElixirTest.MyStruct{struct | "},
                 context,
                 node()
               )

      assert %{
               items: [
                 %{
                   label: "my_val",
                   kind: :field,
                   documentation: _my_val_doc,
                   insert_text: "my_val: "
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:completion, "%Livebook.Intellisense.ElixirTest.MyStruct{struct | my_v"},
                 context,
                 node()
               )

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion,
                  "%Livebook.Intellisense.ElixirTest.MyStruct{struct | my_val: 123, "},
                 context,
                 node()
               )
    end

    test "completion for map keys in update syntax" do
      context =
        intellisense_context_from_eval do
          map = %{foo: 1}
        end

      assert %{
               items: [
                 %{
                   label: "foo",
                   kind: :field,
                   documentation: "(field)",
                   insert_text: "foo: "
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "%{map | "}, context, node())

      assert %{
               items: [
                 %{
                   label: "foo",
                   kind: :field,
                   documentation: "(field)",
                   insert_text: "foo: "
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "%{map | fo"}, context, node())

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "%{map | foo: 2, "},
                 context,
                 node()
               )
    end

    test "ignore invalid Elixir module literals" do
      context = intellisense_context_from_eval(do: nil)

      defmodule(:"Elixir.Livebook.Intellisense.ElixirTest.Unicodé", do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request(
                 {:completion, "Livebook.Intellisense.ElixirTest.Unicod"},
                 context,
                 node()
               )
    after
      :code.purge(:"Elixir.Livebook.Intellisense.ElixirTest.Unicodé")
      :code.delete(:"Elixir.Livebook.Intellisense.ElixirTest.Unicodé")
    end

    test "known Elixir module attributes completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "moduledoc",
                   kind: :variable,
                   documentation: """
                   Provides documentation for the current module.

                   (module attribute)\
                   """,
                   insert_text: "moduledoc"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "@modu"}, context, node())
    end

    test "handles calls on module attribute" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Elixir.handle_request({:completion, "@attr.value"}, context, node())
    end

    test "includes language keywords" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "nil",
                   kind: :keyword,
                   documentation: "(special atom)",
                   insert_text: "nil"
                 }
                 | _
               ]
             } = Intellisense.Elixir.handle_request({:completion, "nil"}, context, node())
    end

    test "includes space instead of parentheses for def* macros" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "defmodule/2",
                   insert_text: "defmodule "
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "defmodu"}, context, node())
    end

    test "includes space instead of parentheses for keyword macros" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "import/2",
                   insert_text: "import "
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "impor"}, context, node())
    end

    test "includes doesn't include space nor parentheses for macros like __ENV__" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "__ENV__/0",
                   insert_text: "__ENV__"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "__EN"}, context, node())
    end

    test "Elixir bitstring modifiers" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "integer",
                   kind: :type,
                   documentation: "(bitstring option)",
                   insert_text: "integer"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "<<a::intege"}, context, node())

      assert %{
               items: [
                 %{
                   label: "size",
                   kind: :type,
                   documentation: "(bitstring option)",
                   insert_text: "size(${})"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "<<a::siz"}, context, node())

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "<<a::"}, context, node())

      assert %{
               label: "integer",
               kind: :type,
               documentation: "(bitstring option)",
               insert_text: "integer"
             } in items
    end

    test "completion for aliases in special forms" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "Range",
                   kind: :struct,
                   documentation: """
                   Returns an inclusive range between dates.

                   (struct)\
                   """,
                   insert_text: "Range"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, "alias Date."}, context, node())

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "alias "}, context, node())

      assert %{
               label: "Atom",
               kind: :module,
               documentation: """
               Atoms are constants whose values are their own name.

               (module)\
               """,
               insert_text: "Atom"
             } in items

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "alias "}, context, node())

      refute Enum.any?(items, &(&1.kind == :function))
    end
  end

  describe "details" do
    test "returns nil if there are no matches" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Elixir.handle_request(
                 {:details, "Unknown.unknown()", 2},
                 context,
                 node()
               )
    end

    test "returns subject range" do
      context = intellisense_context_from_eval(do: nil)

      assert %{range: %{from: 1, to: 18}} =
               Intellisense.Elixir.handle_request(
                 {:details, "Integer.to_string(10)", 15},
                 context,
                 node()
               )

      assert %{range: %{from: 1, to: 8}} =
               Intellisense.Elixir.handle_request(
                 {:details, "Integer.to_string(10)", 2},
                 context,
                 node()
               )
    end

    test "does not return duplicate details for functions with default arguments" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [_]} =
               Intellisense.Elixir.handle_request(
                 {:details, "Integer.to_string(10)", 15},
                 context,
                 node()
               )
    end

    test "returns details only for exactly matching identifiers" do
      context = intellisense_context_from_eval(do: nil)

      assert nil == Intellisense.Elixir.handle_request({:details, "Enum.ma", 6}, context, node())
    end

    test "returns full docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request({:details, "Enum.map", 6}, context, node())

      assert content =~ "## Examples"
    end

    test "returns deprecated docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [content | _]} =
               Intellisense.Elixir.handle_request({:details, "Enum.chunk", 6}, context, node())

      assert content =~ "Use Enum.chunk_every/2 instead"
    end

    test "returns since docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request({:details, "then", 2}, context, node())

      assert content =~ "Since 1.12.0"
    end

    @tag :erl_docs
    test "returns full Erlang docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [file]} =
               Intellisense.Elixir.handle_request({:details, ":file.read()", 2}, context, node())

      assert file =~ "## Performance"

      assert %{contents: [file_read]} =
               Intellisense.Elixir.handle_request({:details, ":file.read()", 8}, context, node())

      assert file_read =~ "Typical error reasons:"

      assert %{contents: [crypto]} =
               Intellisense.Elixir.handle_request({:details, ":crypto", 5}, context, node())

      assert crypto =~ "This module provides a set of cryptographic functions."
    end

    test "properly parses unicode" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Elixir.handle_request({:details, "msg = '🍵'", 8}, context, node())
    end

    test "handles operators" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [match_op]} =
               Intellisense.Elixir.handle_request({:details, "x = 1", 3}, context, node())

      assert match_op =~ "Match operator."
    end

    test "handles local calls" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [to_string_fn]} =
               Intellisense.Elixir.handle_request({:details, "to_string(1)", 3}, context, node())

      assert to_string_fn =~ "Converts the argument to a string"
    end

    test "returns nil for bitstring modifiers" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Elixir.handle_request(
                 {:details, "<<x :: integer>>", 6},
                 context,
                 node()
               )

      assert nil ==
               Intellisense.Elixir.handle_request(
                 {:details, "<<x :: integer>>", 10},
                 context,
                 node()
               )
    end

    test "includes full module name in the docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [date_range]} =
               Intellisense.Elixir.handle_request({:details, "Date.Range", 8}, context, node())

      assert date_range =~ "Date.Range"
    end

    test "returns module-prepended type signatures" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [type]} =
               Intellisense.Elixir.handle_request({:details, "Date.t", 6}, context, node())

      assert type =~ "Date.t()"
    end

    @tag :erl_docs
    test "returns module-prepended Erlang type signatures" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [type]} =
               Intellisense.Elixir.handle_request(
                 {:details, ":code.load_error_rsn", 8},
                 context,
                 node()
               )

      assert type =~ ":code.load_error_rsn()"
    end

    test "includes type specs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [type]} =
               Intellisense.Elixir.handle_request({:details, "Date.t", 6}, context, node())

      assert type =~ "@type t() :: %Date"

      # opaque types are listed without internal definition
      assert %{contents: [type]} =
               Intellisense.Elixir.handle_request(
                 {:details, "MapSet.internal", 10},
                 context,
                 node()
               )

      assert type =~ "@opaque internal(value)\n"
    end

    @tag :erl_docs
    test "includes Erlang type specs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [type]} =
               Intellisense.Elixir.handle_request(
                 {:details, ":code.load_error_rsn", 8},
                 context,
                 node()
               )

      assert type =~ "@type load_error_rsn() ::"
    end

    test "returns link to online documentation" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request({:details, "Integer", 1}, context, node())

      assert content =~ ~r"https://hexdocs.pm/elixir/[^/]+/Integer.html"

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request(
                 {:details, "Integer.to_string(10)", 15},
                 context,
                 node()
               )

      assert content =~ ~r"https://hexdocs.pm/elixir/[^/]+/Integer.html#to_string/2"

      # test elixir types
      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request(
                 {:details, "GenServer.on_start", 12},
                 context,
                 node()
               )

      assert content =~ ~r"https://hexdocs.pm/elixir/[^/]+/GenServer.html#t:on_start/0"

      # test erlang types
      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request(
                 {:details, ":code.load_ret", 7},
                 context,
                 node()
               )

      assert content =~ ~r"https://www.erlang.org/doc/man/code.html#type-load_ret"

      # test erlang modules on hexdocs
      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request(
                 {:details, ":telemetry.span", 13},
                 context,
                 node()
               )

      assert content =~ ~r"https://hexdocs.pm/telemetry/[^/]+/telemetry.html#span/3"

      # test erlang applications
      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request({:details, ":code", 3}, context, node())

      assert content =~ ~r"https://www.erlang.org/doc/man/code.html"

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request(
                 {:details, ":code.load_binary", 10},
                 context,
                 node()
               )

      assert content =~ ~r"https://www.erlang.org/doc/man/code.html#load_binary-3"

      # test erlang modules
      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request({:details, ":atomics.new", 11}, context, node())

      assert content =~ ~r"https://www.erlang.org/doc/man/atomics.html#new-2"

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request(
                 {:details, ":string.uppercase", 11},
                 context,
                 node()
               )

      assert content =~ ~r"https://www.erlang.org/doc/man/string.html#uppercase-1"
    end

    @tag :tmp_dir
    test "includes definition location for runtime modules", %{tmp_dir: tmp_dir} do
      Code.put_compiler_option(:debug_info, true)

      context =
        intellisense_context_from_eval tmp_dir do
          alias Livebook.Intellisense.ElixirTest.GoToDefinition
        end

      code = ~S'''
      defmodule Livebook.Intellisense.ElixirTest.GoToDefinition do
        @type t :: term()
        @type foo :: foo(:bar)
        @type foo(var) :: {var, t()}

        defmacro with_logging(do: block) do
          quote do
            require Logger
            Logger.debug("Running code")
            result = unquote(block)
            Logger.debug("Result: #{inspect(result)}")
            result
          end
        end

        @spec hello(var :: term()) :: foo(term())
        def hello(message) do
          {:bar, message}
        end
      end
      '''

      file = "#{__ENV__.file}#cell:#{Livebook.Utils.random_short_id()}"
      compile_and_save_bytecode(tmp_dir, code, file)

      assert %{definition: %{line: 1, file: ^file}} =
               Intellisense.Elixir.handle_request(
                 {:details, "GoToDefinition", 14},
                 context,
                 node()
               )

      assert %{definition: %{line: 2, file: ^file}} =
               Intellisense.Elixir.handle_request(
                 {:details, "GoToDefinition.t", 16},
                 context,
                 node()
               )

      # Currently we are fetching location of the lowest arity
      assert %{definition: %{line: 3, file: ^file}} =
               Intellisense.Elixir.handle_request(
                 {:details, "GoToDefinition.foo", 18},
                 context,
                 node()
               )

      assert %{definition: %{line: 6, file: ^file}} =
               Intellisense.Elixir.handle_request(
                 {:details, "GoToDefinition.with_logging", 20},
                 context,
                 node()
               )

      assert %{definition: %{line: 17, file: ^file}} =
               Intellisense.Elixir.handle_request(
                 {:details, "GoToDefinition.hello", 18},
                 context,
                 node()
               )
    after
      Code.put_compiler_option(:debug_info, false)
    end
  end

  describe "signature" do
    test "returns nil when outside call" do
      context = intellisense_context_from_eval(do: nil)

      assert nil == Intellisense.Elixir.handle_request({:signature, "length()"}, context, node())
    end

    test "returns nil if there are no matches" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Elixir.handle_request(
                 {:signature, "Unknown.unknown("},
                 context,
                 node()
               )

      assert nil ==
               Intellisense.Elixir.handle_request(
                 {:signature, "Enum.concat(x, y,"},
                 context,
                 node()
               )
    end

    test "supports remote function calls" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(enumerable, fun)",
                   arguments: ["enumerable", "fun"]
                 }
               ]
             } = Intellisense.Elixir.handle_request({:signature, "Enum.map("}, context, node())
    end

    test "supports local function calls" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "length(list)",
                   arguments: ["list"]
                 }
               ]
             } = Intellisense.Elixir.handle_request({:signature, "length("}, context, node())
    end

    test "supports manually imported functions and macros" do
      context =
        intellisense_context_from_eval do
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
             } = Intellisense.Elixir.handle_request({:signature, "map("}, context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: ~S"derive(protocol, module, options \\ [])",
                   arguments: ["protocol", "module", ~S"options \\ []"]
                 }
               ]
             } = Intellisense.Elixir.handle_request({:signature, "derive("}, context, node())
    end

    test "supports remote function calls on aliases" do
      context =
        intellisense_context_from_eval do
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
             } = Intellisense.Elixir.handle_request({:signature, "MyEnum.map("}, context, node())
    end

    test "supports anonymous function calls" do
      context =
        intellisense_context_from_eval do
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
             } = Intellisense.Elixir.handle_request({:signature, "add.("}, context, node())
    end

    test "supports captured remote function calls" do
      context =
        intellisense_context_from_eval do
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
             } = Intellisense.Elixir.handle_request({:signature, "map.("}, context, node())
    end

    @tag :erl_docs
    test "shows signature with arguments for erlang modules" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "map(Fun, List1)",
                   arguments: ["Fun", "List1"]
                 }
               ]
             } = Intellisense.Elixir.handle_request({:signature, ":lists.map("}, context, node())
    end

    test "shows signature with default argument being an anonymous function" do
      context = intellisense_context_from_eval(do: nil)

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
               Intellisense.Elixir.handle_request(
                 {:signature, "Enum.max_by([1, 2], &Kernel.-/1, &>=/2, "},
                 context,
                 node()
               )
    end

    test "returns call active argument" do
      context = intellisense_context_from_eval(do: nil)

      assert %{active_argument: 0, items: [_item]} =
               Intellisense.Elixir.handle_request({:signature, "Enum.map([1, "}, context, node())

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.Elixir.handle_request(
                 {:signature, "Enum.map([1, 2], "},
                 context,
                 node()
               )

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.Elixir.handle_request(
                 {:signature, "Enum.map([1, 2], fn x -> x * x end"},
                 context,
                 node()
               )

      assert %{active_argument: 2, items: [_item]} =
               Intellisense.Elixir.handle_request(
                 {:signature, "IO.ANSI.color(1, 2, 3"},
                 context,
                 node()
               )

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.Elixir.handle_request({:signature, "elem(x, 1 + "}, context, node())
    end

    test "returns correct active argument when using pipe operator" do
      context = intellisense_context_from_eval(do: nil)

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.Elixir.handle_request(
                 {:signature, "[1, 2] |> Enum.map("},
                 context,
                 node()
               )

      assert %{active_argument: 1, items: [_item]} =
               Intellisense.Elixir.handle_request(
                 {:signature, "[1, 2] |> Enum.map(fn x -> x * x end"},
                 context,
                 node()
               )

      assert %{active_argument: 2, items: [_item]} =
               Intellisense.Elixir.handle_request(
                 {:signature, "1 |> IO.ANSI.color(2, 3"},
                 context,
                 node()
               )
    end

    test "returns a single signature for functions with default arguments" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: ~S"to_string(integer, base \\ 10)",
                   arguments: ["integer", ~S"base \\ 10"]
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:signature, "Integer.to_string("},
                 context,
                 node()
               )
    end

    test "returns multiple signatures for function with multiple arities" do
      context = intellisense_context_from_eval(do: nil)

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
             } = Intellisense.Elixir.handle_request({:signature, "Enum.concat("}, context, node())
    end

    test "returns only signatures where active argument is at valid position" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "concat(left, right)",
                   arguments: ["left", "right"]
                 }
               ]
             } =
               Intellisense.Elixir.handle_request(
                 {:signature, "Enum.concat([1, 2], "},
                 context,
                 node()
               )
    end

    test "does not return any signatures when in do-end block" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Elixir.handle_request({:signature, "if true do "}, context, node())
    end

    test "does not return any signatures for module attributes" do
      context = intellisense_context_from_eval(do: nil)

      assert nil == Intellisense.Elixir.handle_request({:signature, "@length("}, context, node())
    end

    test "does not returns signatures for calls in attribute value" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   arguments: ["list"],
                   signature: "length(list)"
                 }
               ]
             } =
               Intellisense.Elixir.handle_request({:signature, "@attr length("}, context, node())
    end
  end

  # Remote intellisense tests

  setup_all do
    # We use the standalone runtime to mimic a remote node. Note that
    # in the past we used :peer.start, but it was often failing on CI
    # (the start was timing out)

    pid = Livebook.Runtime.Standalone.new() |> Livebook.Runtime.connect()
    assert_receive {:runtime_connect_done, ^pid, {:ok, runtime}}

    parent = self()

    runtime_owner_pid =
      start_supervised!({
        Task,
        fn ->
          Livebook.Runtime.take_ownership(runtime, [])

          code =
            ~S'''
            defmodule RemoteModule do
              @moduledoc """
              RemoteModule module docs
              """

              @doc """
              Hello doc
              """
              def hello(message) do
                message
              end
            end
            '''

          Livebook.Runtime.evaluate_code(runtime, :elixir, code, {:c1, :e1}, [], [])
          receive do: ({:runtime_evaluation_response, :e1, _, _} -> :ok)
          send(parent, :continue)

          receive do: (:done -> :ok)
        end
      })

    receive do: (:continue -> :ok)

    on_exit(fn ->
      send(runtime_owner_pid, :done)
    end)

    [node: runtime.node]
  end

  describe "intellisense completion for remote nodes" do
    test "do not find the RemoteModule inside the Livebook node" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} ==
               Intellisense.Elixir.handle_request({:completion, "RemoteModule"}, context, node())
    end

    test "find the RemoteModule and its docs", %{node: node} do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request({:completion, "RemoteModule"}, context, node)

      assert %{
               label: "RemoteModule",
               kind: :module,
               documentation: """
               No documentation available

               (module)\
               """,
               insert_text: "RemoteModule"
             } in items
    end

    test "find RemoteModule exported functions and its docs", %{node: node} do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Elixir.handle_request(
                 {:completion, "RemoteModule.hel"},
                 context,
                 node
               )

      assert %{
               label: "hello/1",
               kind: :function,
               documentation: """
               No documentation available

               ```
               RemoteModule.hello(arg1)
               ```\
               """,
               insert_text: "hello(${})"
             } in items
    end

    @tag :erl_docs
    test "find modules from apps", %{node: node} do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "unsubscribe/1",
                   kind: :function,
                   documentation: """
                   No documentation available

                   ```
                   :mnesia.unsubscribe(arg1)
                   ```\
                   """,
                   insert_text: "unsubscribe(${})"
                 }
               ]
             } = Intellisense.Elixir.handle_request({:completion, ":mnesia.unsub"}, context, node)
    end

    test "get details", %{node: node} do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [content]} =
               Intellisense.Elixir.handle_request({:details, "RemoteModule", 6}, context, node)

      assert content =~ "No documentation available"
    end
  end

  defp compile_and_save_bytecode(dir, code, file \\ "nofile") do
    [{module, bytecode}] = Code.compile_string(code, file)
    path = Path.join(dir, "#{module}.beam")

    File.write!(path, bytecode)
    Code.prepend_path(dir)

    on_exit(fn ->
      Code.delete_path(dir)
      :code.purge(module)
      :code.delete(module)
    end)
  end
end
