defmodule Livebook.Intellisense.PythonTest do
  use ExUnit.Case, async: true
  @moduletag :python

  import Livebook.TestHelpers

  alias Livebook.Intellisense

  describe "completion" do
    test "when no hint given" do
      context = intellisense_context_from_eval(do: nil)

      length_item = %{
        label: "len",
        kind: :function,
        documentation: """
        Return the number of items in a container.

        ```
        len(obj, /)
        ```\
        """,
        insert_text: "len(${})"
      }

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, ""}, context, node())

      assert length_item in items

      # New context after prefix.

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "repr("}, context, node())

      assert length_item in items

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "foo(bar, "}, context, node())

      assert length_item in items

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "1 +"}, context, node())

      assert length_item in items
    end

    test "hint with prior context" do
      context = intellisense_context_from_eval(do: nil)

      length_item = %{
        label: "len",
        kind: :function,
        documentation: """
        Return the number of items in a container.

        ```
        len(obj, /)
        ```\
        """,
        insert_text: "len(${})"
      }

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "repr(l"}, context, node())

      assert length_item in items

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "foo(bar, l"}, context, node())

      assert length_item in items

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "1 + l"}, context, node())

      assert length_item in items

      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, "dict().l"}, context, node())
    end

    test "ignores within strings and comments" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, ~S/x = "le/}, context, node())

      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, "x = 1 # le"}, context, node())
    end

    test "builtins" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "len",
                   kind: :function,
                   documentation: """
                   Return the number of items in a container.

                   ```
                   len(obj, /)
                   ```\
                   """,
                   insert_text: "len(${})"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "le"}, context, node())

      assert %{
               items: [
                 %{
                   label: "list",
                   kind: :struct,
                   documentation: """
                   Built-in mutable sequence.

                   ```
                   list(iterable=(), /)
                   ```\
                   """,
                   insert_text: "list(${})"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "lis"}, context, node())
    end

    test "globals" do
      context =
        intellisense_context_from_eval do
          elixir_number = 1

          import Pythonx

          ~PY"""
          import time
          number = 1
          var123 = 2
          łódź = 3
          """
        end

      assert %{
               items: [
                 %{
                   label: "elixir_number",
                   kind: :variable,
                   documentation: "(variable)",
                   insert_text: "elixir_number"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "elix"}, context, node())

      assert %{
               items: [
                 %{
                   label: "number",
                   kind: :variable,
                   documentation: "(variable)",
                   insert_text: "number"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "num"}, context, node())

      assert %{
               items: [
                 %{
                   label: "var123",
                   kind: :variable,
                   documentation: "(variable)",
                   insert_text: "var123"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "var12"}, context, node())

      assert %{
               items: [
                 %{
                   label: "łódź",
                   kind: :variable,
                   documentation: "(variable)",
                   insert_text: "łódź"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "ł"}, context, node())

      assert %{
               items: [
                 %{
                   label: "time",
                   kind: :module,
                   documentation: """
                   This module provides various functions to manipulate time values.\
                   """,
                   insert_text: "time"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "tim"}, context, node())
    end

    test "keywords" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "while",
                   kind: :keyword,
                   documentation: "Creates a while loop.",
                   insert_text: "while"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "whi"}, context, node())
    end

    test "module functions" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          import time
          import sys
          """
        end

      # Signature from docs.
      assert %{
               items: [
                 %{
                   label: "monotonic_ns",
                   kind: :function,
                   documentation: """
                   Monotonic clock, cannot go backward, as nanoseconds.

                   ```
                   time.monotonic_ns() -> int
                   ```\
                   """,
                   insert_text: "monotonic_ns()"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "time.monotonic_"},
                 context,
                 node()
               )

      # Signature from inspect.signature.
      assert %{
               items: [
                 %{
                   label: "exit",
                   kind: :function,
                   documentation: """
                   Exit the interpreter by raising SystemExit(status).

                   ```
                   sys.exit(status=None, /)
                   ```\
                   """,
                   insert_text: "exit(${})"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "sys.exi"},
                 context,
                 node()
               )
    end

    test "object methods" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          items = []
          """
        end

      assert %{
               items: [
                 %{
                   label: "append",
                   kind: :function,
                   documentation: """
                   Append object to the end of the list.

                   ```
                   list.append(object, /)
                   ```\
                   """,
                   insert_text: "append(${})"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "items.app"},
                 context,
                 node()
               )
    end

    test "class" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          import ipaddress
          """
        end

      assert %{
               items: [
                 %{
                   label: "IPv6Network",
                   kind: :struct,
                   documentation: """
                   This class represents and manipulates 128-bit IPv6 networks.

                   ```
                   ipaddress.IPv6Network(address, strict=True)
                   ```\
                   """,
                   insert_text: "IPv6Network(${})"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "ipaddress.IPv6Net"},
                 context,
                 node()
               )
    end

    test "multiple dot segments" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          import time
          """
        end

      assert %{
               items: [
                 %{
                   label: "to_bytes",
                   kind: :function,
                   documentation: """
                   Return an array of bytes representing an integer.

                   ```
                   int.to_bytes(length=1, byteorder='big', *, signed=False)
                   ```\
                   """,
                   insert_text: "to_bytes(${})"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "time.timezone.to_by"},
                 context,
                 node()
               )
    end

    test "no completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, "."}, context, node())

      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, "xyz"}, context, node())

      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, "xyz.xyz"}, context, node())
    end

    test "import completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{items: items} =
               Intellisense.Python.handle_request({:completion, "import "}, context, node())

      assert %{
               label: "inspect",
               kind: :module,
               documentation: "Get useful information from live Python objects.",
               insert_text: "inspect"
             } in items

      # No extra items, only importable modules.
      assert %{items: []} =
               Intellisense.Python.handle_request({:completion, "import clas"}, context, node())

      assert %{
               items: [
                 %{
                   label: "inspect",
                   kind: :module,
                   documentation: "Get useful information from live Python objects.",
                   insert_text: "inspect"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "import insp"}, context, node())

      assert %{
               items: [
                 %{
                   label: "http.client",
                   kind: :module,
                   documentation: "HTTP/1.1 client library",
                   insert_text: "http.client"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "import http.cli"},
                 context,
                 node()
               )

      assert %{
               items: [
                 %{
                   label: "inspect",
                   kind: :module,
                   documentation: "Get useful information from live Python objects.",
                   insert_text: "inspect"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "import http.client as client, insp"},
                 context,
                 node()
               )
    end

    test "from import completion" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               items: [
                 %{
                   label: "inspect",
                   kind: :module,
                   documentation: "Get useful information from live Python objects.",
                   insert_text: "inspect"
                 }
               ]
             } = Intellisense.Python.handle_request({:completion, "from insp"}, context, node())

      assert %{
               items: [
                 %{
                   label: "getdoc",
                   kind: :function,
                   documentation: """
                   Get the documentation string for an object.

                   ```
                   inspect.getdoc(object)
                   ```\
                   """,
                   insert_text: "getdoc"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "from inspect import getd"},
                 context,
                 node()
               )

      assert %{
               items: [
                 %{
                   label: "getdoc",
                   kind: :function,
                   documentation: """
                   Get the documentation string for an object.

                   ```
                   inspect.getdoc(object)
                   ```\
                   """,
                   insert_text: "getdoc"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "from inspect import signature, getd"},
                 context,
                 node()
               )

      assert %{
               items: [
                 %{
                   label: "getdoc",
                   kind: :function,
                   documentation: """
                   Get the documentation string for an object.

                   ```
                   inspect.getdoc(object)
                   ```\
                   """,
                   insert_text: "getdoc"
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:completion, "from inspect import signature as sig, getd"},
                 context,
                 node()
               )
    end
  end

  describe "details" do
    test "returns nil if there are no matches" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Python.handle_request(
                 {:details, "Unknown.unknown()", 2},
                 context,
                 node()
               )
    end

    test "returns subject range" do
      context = intellisense_context_from_eval(do: nil)

      assert %{range: %{from: 5, to: 8}} =
               Intellisense.Python.handle_request(
                 {:details, "1 + len([])", 6},
                 context,
                 node()
               )

      assert %{range: %{from: 6, to: 13}} =
               Intellisense.Python.handle_request(
                 {:details, "from inspect import signature as sig, getdoc", 7},
                 context,
                 node()
               )

      assert %{range: %{from: 21, to: 30}} =
               Intellisense.Python.handle_request(
                 {:details, "from inspect import signature as sig, getdoc", 22},
                 context,
                 node()
               )

      assert %{range: %{from: 8, to: 12}} =
               Intellisense.Python.handle_request(
                 {:details, "import http.client as client, inspect", 10},
                 context,
                 node()
               )

      assert %{range: %{from: 8, to: 19}} =
               Intellisense.Python.handle_request(
                 {:details, "import http.client as client, inspect", 12},
                 context,
                 node()
               )

      assert %{range: %{from: 8, to: 19}} =
               Intellisense.Python.handle_request(
                 {:details, "import http.client as client, inspect", 14},
                 context,
                 node()
               )
    end

    test "returns details only for exactly matching identifiers" do
      context = intellisense_context_from_eval(do: nil)

      assert nil == Intellisense.Python.handle_request({:details, "le", 2}, context, node())
    end

    test "returns full docs" do
      context = intellisense_context_from_eval(do: nil)

      assert %{contents: [content]} =
               Intellisense.Python.handle_request({:details, "tuple", 2}, context, node())

      assert content =~ "If the argument is a tuple, the return value is the same object."
    end
  end

  describe "signature" do
    test "returns nil when outside call" do
      context = intellisense_context_from_eval(do: nil)

      assert nil == Intellisense.Python.handle_request({:signature, "len()"}, context, node())
    end

    test "returns nil if there are no matches" do
      context = intellisense_context_from_eval(do: nil)

      assert nil ==
               Intellisense.Python.handle_request(
                 {:signature, "Unknown.unknown("},
                 context,
                 node()
               )

      assert nil ==
               Intellisense.Python.handle_request(
                 {:signature, "len(xs, "},
                 context,
                 node()
               )
    end

    test "builtin function calls" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } = Intellisense.Python.handle_request({:signature, "len("}, context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } = Intellisense.Python.handle_request({:signature, "len(my_list"}, context, node())
    end

    test "module function calls" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          import time
          """
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "time.sleep(object, /)",
                   arguments: ["object"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "time.sleep("},
                 context,
                 node()
               )

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "time.sleep(object, /)",
                   arguments: ["object"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "time.sleep(seconds"},
                 context,
                 node()
               )
    end

    test "class constructor" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          import ipaddress
          """
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "ipaddress.IPv6Network(address, strict=True)",
                   arguments: ["address", "strict=True"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "ipaddress.IPv6Network("},
                 context,
                 node()
               )
    end

    test "with prior context" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } = Intellisense.Python.handle_request({:signature, "1 + len("}, context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "do_something(len(my_list"},
                 context,
                 node()
               )
    end

    test "with brackets in parameters" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } = Intellisense.Python.handle_request({:signature, "len([1, 2, 3"}, context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } =
               Intellisense.Python.handle_request({:signature, "len([1, 2, 3]"}, context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "len(obj, /)",
                   arguments: ["obj"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "len([{'foo': 'bar'}, divmod((1 + 2), 3), "},
                 context,
                 node()
               )
    end

    test "with multi-argument lambda" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "filter(function, iterable, /)",
                   arguments: ["function", "iterable"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "filter(lambda x, "},
                 context,
                 node()
               )

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "filter(function, iterable, /)",
                   arguments: ["function", "iterable"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "filter(lambda x, y:"},
                 context,
                 node()
               )

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "filter(function, iterable, /)",
                   arguments: ["function", "iterable"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "filter(lambda x, y: x + y, "},
                 context,
                 node()
               )
    end

    test "positional active argument" do
      context = intellisense_context_from_eval(do: nil)

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "divmod(x, y, /)",
                   arguments: ["x", "y"]
                 }
               ]
             } =
               Intellisense.Python.handle_request({:signature, "divmod(left, "}, context, node())

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "divmod(x, y, /)",
                   arguments: ["x", "y"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "divmod(left, right"},
                 context,
                 node()
               )

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "exec(source, /, globals=None, locals=None, *, closure=None)",
                   arguments: ["source", "globals=None", "locals=None", "closure=None"]
                 }
               ]
             } =
               Intellisense.Python.handle_request({:signature, "exec(src, glbs"}, context, node())
    end

    test "named active argument" do
      context = intellisense_context_from_eval(do: nil)

      # After a named parameter, we don't know which one is next, unless we know the name.
      assert %{
               active_argument: nil,
               items: [
                 %{
                   signature: "exec(source, /, globals=None, locals=None, *, closure=None)",
                   arguments: ["source", "globals=None", "locals=None", "closure=None"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "exec(src, locals=lcls, "},
                 context,
                 node()
               )

      # When the current parameter is named, we know which one it is.
      assert %{
               active_argument: 2,
               items: [
                 %{
                   signature: "exec(source, /, globals=None, locals=None, *, closure=None)",
                   arguments: ["source", "globals=None", "locals=None", "closure=None"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "exec(src, closure=None, locals="},
                 context,
                 node()
               )
    end

    test "arbitrary parameters" do
      context =
        intellisense_context_from_eval do
          import Pythonx

          ~PY"""
          def fun(arg, *args, kwarg=None, **kwargs):
            pass

          def fun_kwargs(**kwargs):
            pass

          def fun_nonpositional_kwargs(arg, *, kwarg1=None, kwarg2=None):
            pass
          """
        end

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: ~S"print(*args, sep=' ', end='\n', file=None, flush=False)",
                   arguments: ["*args", "sep=' '", ~S"end='\n'", "file=None", "flush=False"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "print(1, 2, 3, 4, 5, 6, 7, 8"},
                 context,
                 node()
               )

      assert %{
               active_argument: 4,
               items: [
                 %{
                   signature: ~S"print(*args, sep=' ', end='\n', file=None, flush=False)",
                   arguments: ["*args", "sep=' '", ~S"end='\n'", "file=None", "flush=False"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "print(1, 2, 3, flush=True"},
                 context,
                 node()
               )

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "fun(arg, *args, kwarg=None, **kwargs)",
                   arguments: ["arg", "*args", "kwarg=None", "**kwargs"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "fun(1"},
                 context,
                 node()
               )

      assert %{
               active_argument: 1,
               items: [
                 %{
                   signature: "fun(arg, *args, kwarg=None, **kwargs)",
                   arguments: ["arg", "*args", "kwarg=None", "**kwargs"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "fun(1, 2, 3, 4"},
                 context,
                 node()
               )

      assert %{
               active_argument: 3,
               items: [
                 %{
                   signature: "fun(arg, *args, kwarg=None, **kwargs)",
                   arguments: ["arg", "*args", "kwarg=None", "**kwargs"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "fun(1, foo=False"},
                 context,
                 node()
               )

      assert %{
               active_argument: 2,
               items: [
                 %{
                   signature: "fun(arg, *args, kwarg=None, **kwargs)",
                   arguments: ["arg", "*args", "kwarg=None", "**kwargs"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "fun(1, foo=False, kwarg=True"},
                 context,
                 node()
               )

      # The last argument is incomplete, it may be named, so signature should still be shown.
      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "fun_kwargs(**kwargs)",
                   arguments: ["**kwargs"]
                 }
               ]
             } =
               Intellisense.Python.handle_request({:signature, "fun_kwargs("}, context, node())

      assert %{
               active_argument: 0,
               items: [
                 %{
                   signature: "fun_kwargs(**kwargs)",
                   arguments: ["**kwargs"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "fun_kwargs(foo=True, "},
                 context,
                 node()
               )

      # Positional argument given to fun_kwargs(**kwargs), so no such function exists.
      assert nil ==
               Intellisense.Python.handle_request({:signature, "fun_kwargs(x, "}, context, node())

      # The last argument is incomplete, it may be named, so signature should still be shown.
      assert %{
               active_argument: nil,
               items: [
                 %{
                   signature: "fun_nonpositional_kwargs(arg, *, kwarg1=None, kwarg2=None)",
                   arguments: ["arg", "kwarg1=None", "kwarg2=None"]
                 }
               ]
             } =
               Intellisense.Python.handle_request(
                 {:signature, "fun_nonpositional_kwargs(True, "},
                 context,
                 node()
               )
    end
  end
end
